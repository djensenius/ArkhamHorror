#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# ///

"""Serving gate for the published locale catalog, run against real nginx.

The catalog's cache policy is a correctness property, not a formatting detail:
an immutable year on a 404 poisons every client that raced a rolling deploy,
and a JSON fetch answered with the SPA shell breaks native clients silently.
Neither can be proven by reading the config, so this gate boots the actual
`prod.nginxconf` (and the config the offline packager generates) in nginx and
asserts the response status, headers, and bytes for:

  * 200 (chunk and manifest) and 304 — cacheable, with the policy their path
    deserves;
  * 404 (missing chunk, unknown path) and 405 (bad method) — never stored,
    and never answered with the SPA shell;
  * `Range` requests — ranges are disabled for the catalog, so they return the
    whole file as a 200 and no 416 can be produced;
  * gzip and brotli negotiation, including that each response carries exactly
    one Cache-Control, Vary, nosniff and Content-Encoding header;
  * JSON content type, `nosniff`, `Vary: Accept-Encoding`, and byte-exact
    payloads that match the manifest's SHA-256;
  * a rolling deploy in both directions: the manifest of one revision fetched
    against the static root of the other. Chunk URLs are content-addressed, so
    every pack that did not change must still resolve; a pack that did change
    must fail *uncacheably* so the client can recover by re-fetching the
    manifest.

Nothing here is skipped when a tool is missing: docker (to run nginx), node
(to build a second revision and to compress/inflate fixtures), git (to assemble
the tracked-sources clone) and bash (to render the offline packager's config)
are all required, and their absence fails the gate.
"""

import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
FRONTEND = ROOT / "frontend"
WORK = FRONTEND / "node_modules" / ".locale-catalog-serving"
NGINX_IMAGE = "nginx:1.27-alpine"
PORT = int(os.environ.get("LOCALE_CATALOG_TEST_PORT", "38199"))
REQUEST_TIMEOUT = 20

CLEAN_CLONE_PREFIXES = ("frontend/src/", "frontend/homebrew/", "frontend/scripts/", "frontend/schemas/", "contracts/", "backend/arkham-api/i18n-emitted-keys.json")
CLEAN_CLONE_FILES = ("frontend/package.json", "frontend/package-lock.json")


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog serving: {message}")


def run(command: list[str], *, capture_output: bool = True, **kwargs) -> subprocess.CompletedProcess:
    text = kwargs.pop("text", capture_output)
    return subprocess.run(command, capture_output=capture_output, text=text, check=False, **kwargs)


def tool(name: str) -> str:
    path = shutil.which(name)
    require(path is not None, f"{name} is required to verify catalog serving")
    return path


def request(path: str, *, method: str = "GET", headers: dict[str, str] | None = None):
    """Returns (status, headers, body). `headers` keeps every occurrence, so a
    duplicated header is visible rather than collapsed."""
    url = f"http://127.0.0.1:{PORT}{path}"
    message = urllib.request.Request(url, method=method, headers=headers or {})
    try:
        # A container that accepts connections but never answers must fail the
        # gate, not hang it.
        with urllib.request.urlopen(message, timeout=REQUEST_TIMEOUT) as response:
            return response.status, Headers(response.headers.items()), response.read()
    except urllib.error.HTTPError as error:
        return error.code, Headers(error.headers.items()), error.read()


class Headers(dict):
    def __init__(self, items):
        self.occurrences: dict[str, list[str]] = {}
        for name, value in items:
            self.occurrences.setdefault(name.lower(), []).append(value)
        super().__init__({name: values[0] for name, values in self.occurrences.items()})

    def count(self, name: str) -> int:
        return len(self.occurrences.get(name.lower(), []))


class Nginx:
    """Runs one nginx container over a given config and static root."""

    def __init__(self, config: Path, static_root: Path, name: str):
        self.config = config
        self.static_root = static_root
        self.name = name
        self.container = None

    def __enter__(self):
        docker = tool("docker")
        run([docker, "rm", "-f", self.name])
        result = run(
            [
                docker, "run", "-d", "--rm", "--name", self.name,
                "-p", f"{PORT}:3000",
                "-v", f"{self.config}:/etc/nginx/nginx.conf:ro",
                "-v", f"{self.static_root}:/opt/arkham/src/frontend/dist:ro",
                NGINX_IMAGE,
            ]
        )
        require(result.returncode == 0, f"could not start nginx: {result.stderr.strip()}")
        self.container = result.stdout.strip()

        for _ in range(60):
            try:
                request("/locale-catalog/manifest.json")
                return self
            except (urllib.error.URLError, ConnectionError, TimeoutError):
                time.sleep(0.25)
        logs_result = run([docker, "logs", self.name])
        logs = f"{logs_result.stdout}{logs_result.stderr}"
        self.__exit__(None, None, None)
        raise SystemExit(f"locale-catalog serving: nginx did not become ready\n{logs}")

    def __exit__(self, *_):
        if self.container is not None:
            run([tool("docker"), "rm", "-f", self.name])
            self.container = None
        return False


def build_catalog(frontend: Path, out: Path) -> dict:
    result = run([tool("node"), str(frontend / "scripts" / "locale-catalog" / "generate.mjs"), "--out", str(out)], cwd=frontend)
    require(result.returncode == 0, f"generation failed: {result.stdout}\n{result.stderr}")

    # The production build precompresses everything it publishes (gzip for
    # gzip_static, brotli for the explicit -f check in prod.nginxconf); mirror
    # that so both negotiation paths are actually exercised here.
    import gzip

    for path in out.rglob("*.json"):
        path.with_suffix(".json.gz").write_bytes(gzip.compress(path.read_bytes(), 9))
    brotli = run(
        [
            tool("node"),
            "-e",
            "const {readdirSync,statSync,readFileSync,writeFileSync}=require('node:fs');"
            "const {join}=require('node:path');const zlib=require('node:zlib');"
            "const walk=(d)=>readdirSync(d,{withFileTypes:true}).flatMap(e=>e.isDirectory()?walk(join(d,e.name)):[join(d,e.name)]);"
            "for (const f of walk(process.argv[1])) if (f.endsWith('.json')) "
            "writeFileSync(f+'.br', zlib.brotliCompressSync(readFileSync(f)));",
            str(out),
        ]
    )
    require(brotli.returncode == 0, f"could not precompress the test catalog: {brotli.stderr}")
    return json.loads((out / "manifest.json").read_text(encoding="utf-8"))


def clean_clone(destination: Path) -> Path:
    """A scratch frontend built only from git-tracked sources."""
    git = tool("git")
    tracked = run([git, "-C", str(ROOT), "ls-files", "-z"]).stdout.split("\0")
    for relative in tracked:
        if not relative:
            continue
        if not (relative.startswith(CLEAN_CLONE_PREFIXES) or relative in CLEAN_CLONE_FILES):
            continue
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, target)
    (destination / "frontend" / "node_modules").symlink_to(FRONTEND / "node_modules")
    return destination / "frontend"


def mutate_one_locale_string(frontend: Path) -> str:
    """Changes exactly one pack's content, so one chunk digest changes."""
    path = frontend / "src" / "locales" / "en" / "base.json"
    messages = json.loads(path.read_text(encoding="utf-8"))
    key = next(key for key, value in messages.items() if isinstance(value, str))
    messages[key] = f"{messages[key]} (serving-gate mutation)"
    path.write_text(json.dumps(messages, ensure_ascii=False, indent=2), encoding="utf-8")
    return "core"


def assert_headers(label: str, status: int, headers: dict[str, str], *, expect_status: int, cache: str, json_type: bool = True):
    require(status == expect_status, f"{label}: expected {expect_status}, got {status}")
    require(
        headers.get("cache-control") == cache,
        f"{label}: expected Cache-Control {cache!r}, got {headers.get('cache-control')!r}",
    )
    require(
        headers.get("x-content-type-options") == "nosniff",
        f"{label}: missing nosniff (got {headers.get('x-content-type-options')!r})",
    )
    if json_type:
        require(
            (headers.get("content-type") or "").startswith("application/json"),
            f"{label}: expected JSON content type, got {headers.get('content-type')!r}",
        )


IMMUTABLE = "public, max-age=31536000, immutable"
REVALIDATE = "public, max-age=0, must-revalidate"
NO_STORE = "no-store"


def brotli_decompress(payload: bytes) -> bytes:
    """Inflates a brotli body with Node's zlib (the stdlib has no brotli)."""
    scratch = WORK / "brotli-body"
    scratch.write_bytes(payload)
    result = run(
        [
            tool("node"),
            "-e",
            "const {readFileSync}=require('node:fs');"
            "process.stdout.write(require('node:zlib').brotliDecompressSync(readFileSync(process.argv[1])));",
            str(scratch),
        ],
        capture_output=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    require(result.returncode == 0, f"the brotli body did not inflate: {result.stderr!r}")
    return result.stdout


# Every Accept-Encoding form the catalog route has to answer the same way in
# prod.nginxconf and in the offline package, and the encoding each one must
# produce. `br;q=0` is a *refusal* (RFC 9110 §12.5.3), so it may never be
# answered with brotli however the rest of the header reads; a `br` carrying a
# parameter the config does not recognise falls back rather than guessing.
NEGOTIATION_CASES: tuple[tuple[str, str | None], ...] = (
    ("br", "br"),
    ("br;q=1", "br"),
    ("br;q=1.000", "br"),
    ("br;q=0.5", "br"),
    ("br; q=0.001", "br"),
    ("BR;Q=0.5", "br"),
    ("gzip, br", "br"),
    ("br, gzip", "br"),
    ("  br ,  gzip  ", "br"),
    ("gzip;q=0, br", "br"),
    ("deflate;q=0.5, br;q=0.9, gzip;q=0.8", "br"),
    ("br;q=0", None),
    ("br;q=0.0", None),
    ("br;q=0.000", None),
    ("br;q=0,gzip", "gzip"),
    ("br;q=0, gzip", "gzip"),
    ("br ; q=0 , gzip", "gzip"),
    ("BR;Q=0, gzip", "gzip"),
    ("br;q=0, br", None),
    ("br;q=0, gzip, br", "gzip"),
    ("br;x=1, gzip", "gzip"),
    ("brotli", None),
    ("brotli, gzip", "gzip"),
    ("xbr, gzip", "gzip"),
    ("*", None),
    ("identity", None),
    ("", None),
)


def check_encoding_negotiation(label: str, path: str, identity: bytes, static_root: Path) -> None:
    """Drives the whole Accept-Encoding matrix against real nginx.

    Each case is checked for the encoding it must select *and* for the bytes it
    hands back, so a config that merely omits `Content-Encoding` while serving
    compressed bytes cannot pass. The cache and safety headers are re-checked on
    every branch, because nginx's `if` is a nested configuration level and only
    the branch that runs contributes its headers.
    """
    import gzip as gzip_module

    stored = static_root / path.lstrip("/")
    for accept, expected in NEGOTIATION_CASES:
        status, headers, body = request(path, headers={"Accept-Encoding": accept})
        shown = accept if accept != "" else "<empty>"
        require(status == 200, f"{label} Accept-Encoding: {shown} returned {status}")
        encoding = headers.get("content-encoding")
        require(
            encoding == expected,
            f"{label} Accept-Encoding: {shown} was answered with "
            f"{encoding or 'identity'}, expected {expected or 'identity'}",
        )
        if expected == "br":
            require(
                body == stored.with_name(f"{stored.name}.br").read_bytes(),
                f"{label} Accept-Encoding: {shown} did not return the stored .br sibling",
            )
            require(
                brotli_decompress(body) == identity,
                f"{label} Accept-Encoding: {shown} returned a brotli body that is not the payload",
            )
        elif expected == "gzip":
            require(
                gzip_module.decompress(body) == identity,
                f"{label} Accept-Encoding: {shown} returned a gzip body that is not the payload",
            )
        else:
            require(
                body == identity,
                f"{label} Accept-Encoding: {shown} did not return the identity payload",
            )
        assert_headers(f"{label} {shown}", status, headers, expect_status=200, cache=IMMUTABLE)
        require(
            "accept-encoding" in headers.get("vary", "").lower(),
            f"{label} Accept-Encoding: {shown} does not vary on Accept-Encoding",
        )
        for header in ("cache-control", "vary", "x-content-type-options"):
            require(
                headers.count(header) == 1,
                f"{label} Accept-Encoding: {shown} carries {headers.count(header)} {header} headers",
            )
        require(
            headers.count("content-encoding") <= 1,
            f"{label} Accept-Encoding: {shown} carries {headers.count('content-encoding')} "
            "content-encoding headers",
        )


def check_status_matrix(manifest: dict, label: str, static_root: Path) -> None:
    chunk = manifest["locales"][0]["chunks"][0]
    path = chunk["path"]

    status, headers, body = request(path)
    assert_headers(f"{label} chunk", status, headers, expect_status=200, cache=IMMUTABLE)
    require(
        hashlib.sha256(body).hexdigest() == chunk["sha256"],
        f"{label} chunk bytes do not match the manifest digest",
    )
    require(
        headers.get("vary", "").lower().find("accept-encoding") != -1,
        f"{label} chunk does not vary on Accept-Encoding",
    )
    etag = headers.get("etag")
    require(etag is not None, f"{label} chunk has no ETag")

    status, headers, _ = request(path, method="HEAD")
    assert_headers(f"{label} chunk HEAD", status, headers, expect_status=200, cache=IMMUTABLE)

    # Ranges are disabled for the catalog, so a Range request must return the
    # whole file (200) rather than a 206 — and can never produce a 416, whose
    # status nginx finalizes after the cache headers are computed.
    status, headers, ranged = request(path, headers={"Range": "bytes=0-31"})
    assert_headers(f"{label} chunk range", status, headers, expect_status=200, cache=IMMUTABLE)
    require(ranged == body, f"{label} range request did not return the whole file")
    require("content-range" not in headers, f"{label} range request produced a partial response")

    # Content negotiation: gzip_static must hand back the stored .gz, and the
    # brotli branch must hand back the stored .br — with one set of headers,
    # not one per nginx `if` level.
    status, headers, gzipped = request(path, headers={"Accept-Encoding": "gzip"})
    require(status == 200, f"{label} gzip request returned {status}")
    require(headers.get("content-encoding") == "gzip", f"{label} did not serve gzip_static")
    import gzip as gzip_module

    require(gzip_module.decompress(gzipped) == body, f"{label} gzip payload differs from the identity body")
    assert_headers(f"{label} gzip", status, headers, expect_status=200, cache=IMMUTABLE)

    status, headers, brotli_body = request(path, headers={"Accept-Encoding": "br"})
    require(status == 200, f"{label} brotli request returned {status}")
    require(headers.get("content-encoding") == "br", f"{label} did not serve the brotli sibling")
    stored = static_root / path.lstrip("/")
    require(
        brotli_body == stored.with_name(f"{stored.name}.br").read_bytes(),
        f"{label} brotli response is not the stored .br sibling",
    )
    require(
        brotli_decompress(brotli_body) == body,
        f"{label} brotli body does not inflate to the identity payload",
    )
    # nginx's `if` block re-declares these headers because add_header does not
    # inherit into a nested context; the response must still carry exactly one
    # of each.
    for header in ("cache-control", "vary", "x-content-type-options", "content-encoding"):
        require(
            headers.count(header) == 1,
            f"{label} brotli response carries {headers.count(header)} {header} headers",
        )
    check_encoding_negotiation(label, path, body, static_root)
    for missing_status, missing_path in (
        (404, "/locale-catalog/c/does-not-exist.json"),
        (405, path),
    ):
        _, error_headers, _ = request(
            missing_path, method="POST" if missing_status == 405 else "GET"
        )
        require(
            error_headers.count("cache-control") == 1,
            f"{label} {missing_status} response carries "
            f"{error_headers.count('cache-control')} Cache-Control headers",
        )
    assert_headers(f"{label} brotli", status, headers, expect_status=200, cache=IMMUTABLE)

    status, headers, _ = request(path, headers={"If-None-Match": etag})
    assert_headers(f"{label} chunk revalidation", status, headers, expect_status=304, cache=IMMUTABLE, json_type=False)

    status, headers, unsatisfiable = request(path, headers={"Range": "bytes=99999999-"})
    assert_headers(f"{label} unsatisfiable range", status, headers, expect_status=200, cache=IMMUTABLE)
    require(unsatisfiable == body, f"{label} unsatisfiable range did not return the whole file")

    status, headers, _ = request(path, method="POST")
    assert_headers(f"{label} bad method", status, headers, expect_status=405, cache=NO_STORE, json_type=False)

    for missing in (
        "/locale-catalog/c/0000000000000000000000000000000000000000000000000000000000000000.json",
        "/locale-catalog/nope.json",
        "/locale-catalog/r/1.deadbeef/manifest.json",
    ):
        status, headers, error_body = request(missing)
        assert_headers(
            f"{label} missing {missing}", status, headers, expect_status=404, cache=NO_STORE, json_type=False
        )
        require(
            b"<!DOCTYPE" not in error_body[:200].upper(),
            f"{label}: a missing catalog path was answered with the SPA shell",
        )

    status, headers, manifest_body = request(manifest["manifestPath"])
    assert_headers(f"{label} manifest", status, headers, expect_status=200, cache=REVALIDATE)
    require(
        json.loads(manifest_body)["catalogRevision"] == manifest["catalogRevision"],
        f"{label} manifest served a different revision",
    )

    status, headers, revision_body = request(manifest["revisionManifestPath"])
    assert_headers(f"{label} revision manifest", status, headers, expect_status=200, cache=IMMUTABLE)
    require(revision_body == manifest_body, f"{label} revision manifest differs from the stable one")


def check_rolling_deploy(other: dict, label: str, changed_packs: set[tuple[str, str]]) -> None:
    """Fetches one revision's chunk URLs against the other revision's replica."""
    unchanged_hits = 0
    for locale in other["locales"]:
        for chunk in locale["chunks"]:
            status, headers, body = request(chunk["path"])
            if status == 200:
                require(
                    hashlib.sha256(body).hexdigest() == chunk["sha256"],
                    f"{label}: {chunk['path']} served the wrong bytes",
                )
                require(headers.get("cache-control") == IMMUTABLE, f"{label}: {chunk['path']} cache policy")
                unchanged_hits += 1
                continue
            require(status == 404, f"{label}: {chunk['path']} returned {status}")
            require(
                headers.get("cache-control") == NO_STORE,
                f"{label}: a missing chunk was cacheable ({headers.get('cache-control')!r})",
            )
            require(
                (locale["locale"], chunk["pack"]) in changed_packs,
                f"{label}: {locale['locale']}/{chunk['pack']} is missing even though its content did not change",
            )
    require(unchanged_hits > 0, f"{label}: no chunk survived the deploy skew")


def render_offline_conf(static_root: Path, destination: Path) -> Path:
    """Renders the config the offline packager generates, without packaging."""
    lines = (ROOT / "offline" / "scripts" / "05-package.sh").read_text(encoding="utf-8").splitlines()
    try:
        start = lines.index("generate_nginx_conf() {")
    except ValueError:
        raise SystemExit("locale-catalog serving: generate_nginx_conf not found in offline/scripts/05-package.sh")
    # The function body contains a heredoc whose lines start with `}`, so the
    # end of the function is the first bare `}` after the heredoc terminator.
    terminator = next(i for i in range(start, len(lines)) if lines[i].strip() == "NGINX_EOF")
    end = next(i for i in range(terminator, len(lines)) if lines[i] == "}")
    body = "\n".join(lines[start + 1 : end])

    harness = destination / "render.sh"
    config_dir = destination / "config"
    config_dir.mkdir(parents=True, exist_ok=True)
    (config_dir / "mime.types").write_text("types { application/json json; text/html html; }\n", encoding="utf-8")
    (destination / "frontend").mkdir(exist_ok=True)
    if not (destination / "frontend" / "dist").exists():
        (destination / "frontend" / "dist").symlink_to(static_root)

    harness.write_text(
        "\n".join(
            [
                "#!/usr/bin/env bash",
                "set -euo pipefail",
                f'SCRIPT_DIR="{destination}"',
                'NGINX_PID="/tmp/nginx.pid"',
                'NGINX_LOG_DIR="/var/log/nginx"',
                'DATA_DIR="/tmp"',
                "NGINX_PORT=3000",
                'API_PORT=3000',
                "detect_resolvers() { echo '127.0.0.11'; }",
                "generate_nginx_conf() {",
                body,
                "}",
                "generate_nginx_conf",
            ]
        ),
        encoding="utf-8",
    )
    result = run([tool("bash"), str(harness)])
    require(result.returncode == 0, f"offline nginx config could not be rendered: {result.stderr}")
    conf = config_dir / "nginx.conf"
    require(conf.is_file(), "offline nginx config was not written")

    # The packaged config points at the packaged tree; the container mounts the
    # static root at the same place prod.nginxconf uses.
    text = conf.read_text(encoding="utf-8")
    text = text.replace(f"{destination}/frontend/dist", "/opt/arkham/src/frontend/dist")
    text = text.replace(f'include "{config_dir}/mime.types"', "include /etc/nginx/mime.types")
    text = text.replace('error_log "/var/log/nginx/error.log" warn;', "error_log /var/log/nginx/error.log warn;")
    text = text.replace('access_log "/var/log/nginx/access.log";', "access_log /var/log/nginx/access.log;")
    text = re.sub(r'pid "[^"]*";', "pid /tmp/nginx.pid;", text)
    conf.write_text(text, encoding="utf-8")
    return conf


def check_offline_serving(manifest: dict, static_root: Path) -> None:
    chunk = manifest["locales"][0]["chunks"][0]

    status, headers, body = request(chunk["path"])
    assert_headers("offline chunk", status, headers, expect_status=200, cache=IMMUTABLE)
    require(hashlib.sha256(body).hexdigest() == chunk["sha256"], "offline chunk bytes differ from the manifest digest")

    status, headers, _ = request(manifest["manifestPath"])
    assert_headers("offline manifest", status, headers, expect_status=200, cache=REVALIDATE)

    # Content negotiation must match prod: the stored .gz and .br are what an
    # offline client gets, with one set of headers.
    import gzip as gzip_module

    status, headers, gzipped = request(chunk["path"], headers={"Accept-Encoding": "gzip"})
    require(status == 200, f"offline gzip request returned {status}")
    require(headers.get("content-encoding") == "gzip", "offline did not serve gzip_static")
    require(gzip_module.decompress(gzipped) == body, "offline gzip payload differs from the identity body")
    assert_headers("offline gzip", status, headers, expect_status=200, cache=IMMUTABLE)

    status, headers, brotli_body = request(chunk["path"], headers={"Accept-Encoding": "br"})
    require(status == 200, f"offline brotli request returned {status}")
    require(headers.get("content-encoding") == "br", "offline did not serve the brotli sibling")
    require(
        brotli_decompress(brotli_body) == body,
        "offline brotli body does not inflate to the identity payload",
    )
    for header in ("cache-control", "vary", "x-content-type-options", "content-encoding"):
        require(
            headers.count(header) == 1,
            f"offline brotli response carries {headers.count(header)} {header} headers",
        )
    check_encoding_negotiation("offline", chunk["path"], body, static_root)

    status, headers, body = request("/locale-catalog/c/0000.json")
    require(status == 404, f"offline: a missing chunk returned {status}, not 404")
    require(b"<!DOCTYPE" not in body.upper(), "offline: a missing chunk was answered with the SPA shell")
    require(
        headers.get("x-content-type-options") == "nosniff",
        "offline: a missing chunk response is missing nosniff",
    )


def main() -> None:
    tool("docker")
    tool("node")
    require((FRONTEND / "node_modules" / "parse5").is_dir(), "frontend dependencies are not installed (npm ci)")

    shutil.rmtree(WORK, ignore_errors=True)
    WORK.mkdir(parents=True)

    # Revision A: the current sources.
    root_a = WORK / "static-a"
    (root_a).mkdir()
    (root_a / "index.html").write_text("<!DOCTYPE html><title>spa</title>", encoding="utf-8")
    manifest_a = build_catalog(FRONTEND, root_a / "locale-catalog")

    # Revision B: the same sources with one pack's content changed.
    clone = clean_clone(WORK / "clone")
    changed_pack = mutate_one_locale_string(clone)
    root_b = WORK / "static-b"
    root_b.mkdir()
    (root_b / "index.html").write_text("<!DOCTYPE html><title>spa</title>", encoding="utf-8")
    manifest_b = build_catalog(clone, root_b / "locale-catalog")
    require(
        manifest_a["catalogRevision"] != manifest_b["catalogRevision"],
        "changing a locale string did not change the catalog revision",
    )

    changed = {("en", changed_pack)}
    packs_a = {(l["locale"], c["pack"]): c["sha256"] for l in manifest_a["locales"] for c in l["chunks"]}
    packs_b = {(l["locale"], c["pack"]): c["sha256"] for l in manifest_b["locales"] for c in l["chunks"]}
    differing = {key for key in packs_a.keys() | packs_b.keys() if packs_a.get(key) != packs_b.get(key)}
    require(
        differing == changed,
        f"expected exactly the mutated pack to change digest, got {sorted(differing)}",
    )

    with Nginx(ROOT / "prod.nginxconf", root_a, "arkham-catalog-a"):
        check_status_matrix(manifest_a, "revision A", root_a)
        check_rolling_deploy(manifest_b, "new manifest against old static root", differing)

    with Nginx(ROOT / "prod.nginxconf", root_b, "arkham-catalog-b"):
        check_status_matrix(manifest_b, "revision B", root_b)
        check_rolling_deploy(manifest_a, "old manifest against new static root", differing)

    offline_conf = render_offline_conf(root_a, WORK / "offline")
    with Nginx(offline_conf, root_a, "arkham-catalog-offline"):
        check_offline_serving(manifest_a, root_a)

    shutil.rmtree(WORK, ignore_errors=True)
    print(
        "locale-catalog serving: verified status/cache/MIME matrix, rolling-deploy skew in both "
        f"directions ({len(packs_a)} chunks), and the offline package's nginx route"
    )


if __name__ == "__main__":
    sys.exit(main())
