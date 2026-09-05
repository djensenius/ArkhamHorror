#!/usr/bin/env python3

"""Exercise exact production and offline nginx artifacts over live HTTP.

This is intentionally not a configuration renderer. Production checks run the
provided final Docker image without mounts, so nginx, prod.nginxconf, and the
static catalog are precisely the bytes that image ships. Offline checks run
the supplied package's launcher action, which verifies its provenance, runs
`nginx -t`, and then starts that package's nginx binary with its bundled
libraries and generated configuration.

At least one explicit artifact is required:

  validate-catalog-serving.py --production-image arkham-production:test
  validate-catalog-serving.py --offline-package offline/_dist/ArkhamHorror-...
"""

import gzip
import hashlib
import json
import os
import shutil
import subprocess
import sys
import time
import urllib.error
import urllib.request
import uuid
from pathlib import Path


ROOT = Path(os.environ.get("ARKHAM_LOCALE_CATALOG_REPOSITORY_ROOT", Path(__file__).resolve().parents[1]))
PRODUCTION_CONFIG = "/opt/arkham/src/backend/prod.nginxconf"
PRODUCTION_STATIC_ROOT = "/opt/arkham/src/frontend/dist"
REQUEST_TIMEOUT = 20
PORT: int | None = None

IMMUTABLE = "public, max-age=31536000, immutable"
REVALIDATE = "public, max-age=0, must-revalidate"
NO_STORE = "no-store"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog serving: {message}")


def run(command: list[str], *, capture_output: bool = True, **kwargs) -> subprocess.CompletedProcess:
    text = kwargs.pop("text", capture_output)
    return subprocess.run(command, capture_output=capture_output, text=text, check=False, **kwargs)


def tool(name: str) -> str:
    candidates = {
        "bash": ("/bin/bash",),
        "docker": (
            "/usr/bin/docker",
            "/Applications/Docker.app/Contents/Resources/bin/docker",
            "/usr/local/bin/docker",
        ),
    }.get(name)
    require(candidates is not None, f"{name} is not a declared serving-gate executable")
    for raw in candidates:
        path = Path(raw)
        if (
            raw == "/usr/local/bin/docker"
            and path.is_symlink()
            and path.resolve()
            == Path("/Applications/OrbStack.app/Contents/MacOS/xbin/docker-tools")
            and path.resolve().is_file()
            and path.resolve().stat().st_mode & 0o111
        ):
            return raw
        if not path.is_symlink() and path.is_file() and path.stat().st_mode & 0o111:
            return raw
    require(False, f"{name} is not installed at one of its declared absolute paths: {candidates}")
    raise AssertionError("unreachable")


def parse_args() -> tuple[str | None, Path | None]:
    production_image: str | None = None
    offline_package: Path | None = None
    arguments = iter(sys.argv[1:])
    for argument in arguments:
        if argument == "--production-image":
            require(production_image is None, "--production-image was supplied more than once")
            try:
                production_image = next(arguments)
            except StopIteration:
                require(False, "--production-image requires an image name")
        elif argument == "--offline-package":
            require(offline_package is None, "--offline-package was supplied more than once")
            try:
                offline_package = Path(next(arguments)).resolve()
            except StopIteration:
                require(False, "--offline-package requires a package path")
        else:
            require(False, f"unknown argument: {argument}")
    require(
        production_image is not None or offline_package is not None,
        "provide --production-image and/or --offline-package; surrogate nginx configs are not accepted",
    )
    return production_image, offline_package


def create_owned_work() -> tuple[Path, str]:
    parent = ROOT / "offline" / "_tmp"
    parent.mkdir(parents=True, exist_ok=True)
    token = uuid.uuid4().hex
    work = parent / f"catalog-serving-{token}"
    work.mkdir(mode=0o700)
    (work / "owner").write_text(token, encoding="ascii")
    return work, token


def release_owned_work(work: Path, token: str) -> None:
    owner = work / "owner"
    require(
        work.is_dir() and not work.is_symlink() and owner.is_file() and not owner.is_symlink(),
        f"serving workspace {work} no longer has this invocation's identity; refusing cleanup",
    )
    require(
        owner.read_text(encoding="ascii") == token,
        f"serving workspace {work} ownership token changed; refusing cleanup",
    )
    shutil.rmtree(work)


def request(path: str, *, method: str = "GET", headers: dict[str, str] | None = None):
    require(PORT is not None, "nginx request attempted outside an owned server session")
    url = f"http://127.0.0.1:{PORT}{path}"
    message = urllib.request.Request(url, method=method, headers=headers or {})
    try:
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


def require_gzip_static(version_output: str, label: str) -> None:
    require(
        "--with-http_gzip_static_module" in version_output,
        f"{label} nginx lacks --with-http_gzip_static_module required by gzip_static on",
    )


def verify_production_runtime_authority(docker: str, image: str) -> None:
    metadata = run(
        [
            docker,
            "image",
            "inspect",
            "--format",
            "{{.Architecture}}\t{{ index .Config.Labels \"org.opencontainers.image.nginx-runtime-reference\" }}",
            image,
        ]
    )
    require(metadata.returncode == 0, f"could not inspect production image authority: {metadata.stderr.strip()}")
    architecture, separator, reference = metadata.stdout.strip().partition("\t")
    require(separator == "\t", "production image does not expose nginx runtime authority metadata")
    platform = {"amd64": "linux-x86_64", "arm64": "linux-arm64"}.get(architecture)
    require(platform is not None, f"production image has unsupported architecture for nginx authority: {architecture}")
    lock = ROOT / "offline" / "toolchain.lock"
    index = locked_record(lock, "image", "nginx-runtime", "multiarch", "nginx:1.27.5")
    platform_manifest = locked_record(lock, "image", "nginx-runtime", platform, "nginx:1.27.5")
    require(index[4] == "exact" and valid_sha256(index[5]), "nginx runtime index authority is malformed")
    require(
        platform_manifest[4] == "exact" and valid_sha256(platform_manifest[5]),
        f"nginx runtime {platform} authority is malformed",
    )
    require(
        reference == f"nginx:1.27.5@sha256:{index[5]}",
        "production image nginx runtime label does not match the committed authority",
    )


def wait_for_server(label: str, diagnostic) -> None:
    for _ in range(80):
        try:
            request("/locale-catalog/manifest.json")
            return
        except (urllib.error.URLError, ConnectionError, TimeoutError):
            time.sleep(0.25)
    raise SystemExit(f"locale-catalog serving: {label} nginx did not become ready\n{diagnostic()}")


class ProductionImageNginx:
    """Runs the exact supplied final image with no config or static mounts."""

    def __init__(self, image: str, work: Path, token: str):
        self.image = image
        self.work = work
        self.container_name = f"arkham-production-catalog-{token}"
        self.container: str | None = None

    def diagnostics(self) -> str:
        if self.container is None:
            return "container did not start"
        result = run([tool("docker"), "logs", self.container])
        return f"{result.stdout}{result.stderr}"

    def __enter__(self):
        docker = tool("docker")
        inspected = run([docker, "image", "inspect", self.image])
        require(inspected.returncode == 0, f"production image is unavailable: {self.image}\n{inspected.stderr.strip()}")
        verify_production_runtime_authority(docker, self.image)
        command = (
            f"nginx -t -c {PRODUCTION_CONFIG} && "
            f"exec nginx -g 'daemon off;' -c {PRODUCTION_CONFIG}"
        )
        result = run(
            [
                docker,
                "run",
                "-d",
                "--name",
                self.container_name,
                "-p",
                f"127.0.0.1:{PORT}:3000",
                "--entrypoint",
                "/bin/sh",
                self.image,
                "-ec",
                command,
            ]
        )
        require(result.returncode == 0, f"could not start final production image: {result.stderr.strip()}")
        self.container = result.stdout.strip()
        version = run([docker, "exec", self.container, "nginx", "-V"])
        require(
            version.returncode == 0,
            f"could not inspect production nginx: {version.stdout}{version.stderr}",
        )
        require_gzip_static(f"{version.stdout}{version.stderr}", "production")
        wait_for_server("production", self.diagnostics)
        return self

    def copy_catalog(self) -> Path:
        require(self.container is not None, "production catalog requested outside a live container")
        destination = self.work / "production-static"
        destination.mkdir()
        result = run(
            [
                tool("docker"),
                "cp",
                f"{self.container}:{PRODUCTION_STATIC_ROOT}/locale-catalog",
                str(destination),
            ]
        )
        require(
            result.returncode == 0,
            f"could not copy the final image's catalog tree: {result.stderr.strip()}",
        )
        return destination

    def __exit__(self, *_):
        if self.container is not None:
            run([tool("docker"), "rm", "-f", self.container])
            self.container = None
        return False


def provenance_value(path: Path, key: str) -> str:
    require(path.is_file() and not path.is_symlink(), f"offline nginx provenance is missing or unsafe: {path}")
    matches = []
    for line in path.read_text(encoding="utf-8").splitlines():
        name, separator, value = line.partition("=")
        if separator and name == key:
            matches.append(value)
    require(len(matches) == 1 and matches[0] != "", f"offline nginx provenance has no unique {key} value")
    return matches[0]


def locked_record(lock: Path, record_type: str, component: str, platform: str, artifact: str) -> list[str]:
    require(lock.is_file() and not lock.is_symlink(), f"toolchain authority is missing or unsafe: {lock}")
    matches = []
    for line in lock.read_text(encoding="utf-8").splitlines():
        if not line or line.startswith("#"):
            continue
        fields = line.split("\t")
        if len(fields) == 7 and fields[:4] == [record_type, component, platform, artifact]:
            matches.append(fields)
    require(
        len(matches) == 1,
        f"toolchain authority has no unique {record_type} row for {component}/{platform}/{artifact}",
    )
    return matches[0]


def valid_sha256(value: str) -> bool:
    return len(value) == 64 and all(character in "0123456789abcdef" for character in value)


def verify_offline_provenance(package: Path) -> Path:
    game = package / "game"
    nginx_binary = game / "bin" / "nginx"
    provenance = game / "config" / "toolchain-provenance.env"
    packaged_lock = game / "config" / "toolchain.lock"
    repository_lock = ROOT / "offline" / "toolchain.lock"

    require(
        nginx_binary.is_file() and not nginx_binary.is_symlink() and nginx_binary.stat().st_mode & 0o111,
        f"offline package nginx is missing or unsafe: {nginx_binary}",
    )
    require(provenance_value(provenance, "schema") == "1", "offline nginx provenance uses an unsupported schema")
    platform = provenance_value(provenance, "platform")
    source_archive = provenance_value(provenance, "nginx_source_archive")
    source_sha256 = provenance_value(provenance, "nginx_source_sha256")
    build_identity = provenance_value(provenance, "nginx_build_identity")
    binary_sha256 = provenance_value(provenance, "nginx_binary_sha256")
    version = provenance_value(provenance, "nginx_version")
    required_option = provenance_value(provenance, "nginx_required_configure_option")

    for label, value in (
        ("nginx source SHA-256", source_sha256),
        ("nginx build identity", build_identity),
        ("nginx binary SHA-256", binary_sha256),
        ("toolchain lock SHA-256", provenance_value(provenance, "toolchain_lock_sha256")),
    ):
        require(valid_sha256(value), f"offline provenance has an invalid {label}")

    require(
        packaged_lock.is_file() and not packaged_lock.is_symlink(),
        f"offline package toolchain authority is missing or unsafe: {packaged_lock}",
    )
    require(
        hashlib.sha256(packaged_lock.read_bytes()).hexdigest() == provenance_value(provenance, "toolchain_lock_sha256"),
        "offline package provenance is not bound to its shipped toolchain authority",
    )
    require(
        repository_lock.is_file()
        and not repository_lock.is_symlink()
        and packaged_lock.read_bytes() == repository_lock.read_bytes(),
        "offline package toolchain authority differs from the committed release authority",
    )
    archive = locked_record(packaged_lock, "archive", "nginx", platform, source_archive)
    binary = locked_record(packaged_lock, "binary", "nginx", platform, "bin/nginx")
    require(archive[4] == "exact" and archive[5] == source_sha256, "offline nginx source digest differs from the lock")
    require(binary[4] == "derived" and binary[5] == build_identity, "offline nginx build identity differs from the lock")
    require(binary[6] == "native-build-v2-gzip-static", "offline nginx uses an unrecognized build recipe")
    require(version == "1.26.2", f"offline nginx provenance names an unexpected version: {version}")
    require(
        required_option == "--with-http_gzip_static_module",
        "offline nginx provenance does not require gzip_static",
    )
    require(
        hashlib.sha256(nginx_binary.read_bytes()).hexdigest() == binary_sha256,
        "offline nginx executable digest differs from its package provenance",
    )
    return game


class OfflinePackageNginx:
    """Runs the package launcher, not a mounted host/container substitute."""

    def __init__(self, package: Path):
        self.package = package
        self.game = verify_offline_provenance(package)
        self.start_script = self.game / "start.sh"
        self.process: subprocess.Popen | None = None

    def command(self, action: str) -> list[str]:
        return [
            "/usr/bin/env",
            f"ARKHAM_PORT={PORT}",
            "ARKHAM_API_PORT=39001",
            "ARKHAM_PG_PORT=39002",
            tool("bash"),
            str(self.start_script),
            action,
        ]

    def diagnostics(self) -> str:
        if self.process is None:
            return "offline package launcher did not start"
        if self.process.poll() is None:
            return "offline package launcher is still running but did not answer HTTP"
        stdout, stderr = self.process.communicate()
        return f"{stdout}{stderr}"

    def __enter__(self):
        require(
            self.start_script.is_file() and not self.start_script.is_symlink(),
            f"offline package launcher is missing or unsafe: {self.start_script}",
        )
        config_test = run(self.command("--validate-nginx-config"), cwd=self.game)
        require(
            config_test.returncode == 0,
            "offline package's actual nginx/config/bundled-library validation failed:\n"
            f"{config_test.stdout}{config_test.stderr}",
        )
        self.process = subprocess.Popen(
            self.command("--serve-nginx-for-validation"),
            cwd=self.game,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        wait_for_server("offline package", self.diagnostics)
        return self

    def __exit__(self, *_):
        if self.process is not None:
            if self.process.poll() is None:
                self.process.terminate()
                try:
                    self.process.communicate(timeout=10)
                except subprocess.TimeoutExpired:
                    self.process.kill()
                    self.process.communicate(timeout=10)
            else:
                self.process.communicate()
            self.process = None
        return False


def manifest_from_static_root(static_root: Path) -> dict:
    path = static_root / "locale-catalog" / "manifest.json"
    require(path.is_file(), f"catalog manifest is missing from the exact static tree: {path}")
    try:
        manifest = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        require(False, f"catalog manifest from exact static tree is invalid JSON: {error}")
    require(isinstance(manifest, dict), "catalog manifest from exact static tree is not an object")
    return manifest


def assert_headers(label: str, status: int, headers: Headers, *, expect_status: int, cache: str, json_type: bool = True):
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
    stored = static_root / path.lstrip("/")
    for accept, expected in NEGOTIATION_CASES:
        status, headers, body = request(path, headers={"Accept-Encoding": accept})
        shown = accept if accept else "<empty>"
        require(status == 200, f"{label} Accept-Encoding: {shown} returned {status}")
        encoding = headers.get("content-encoding")
        require(
            encoding == expected,
            f"{label} Accept-Encoding: {shown} was answered with {encoding or 'identity'}, "
            f"expected {expected or 'identity'}",
        )
        if expected == "br":
            require(
                body == stored.with_name(f"{stored.name}.br").read_bytes(),
                f"{label} Accept-Encoding: {shown} did not return the stored .br sibling",
            )
        elif expected == "gzip":
            require(
                gzip.decompress(body) == identity,
                f"{label} Accept-Encoding: {shown} returned a gzip body that is not the payload",
            )
        else:
            require(body == identity, f"{label} Accept-Encoding: {shown} did not return the identity payload")
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
            f"{label} Accept-Encoding: {shown} carries {headers.count('content-encoding')} Content-Encoding headers",
        )


def check_status_matrix(manifest: dict, label: str, static_root: Path) -> None:
    require(isinstance(manifest.get("locales"), list) and manifest["locales"], f"{label}: manifest has no locales")
    chunks = manifest["locales"][0].get("chunks")
    require(isinstance(chunks, list) and chunks, f"{label}: manifest has no chunks")
    chunk = chunks[0]
    path = chunk["path"]
    require(isinstance(path, str) and isinstance(chunk.get("sha256"), str), f"{label}: malformed chunk descriptor")

    status, headers, body = request(path)
    assert_headers(f"{label} chunk", status, headers, expect_status=200, cache=IMMUTABLE)
    require(hashlib.sha256(body).hexdigest() == chunk["sha256"], f"{label} chunk bytes do not match the manifest digest")
    require("accept-encoding" in headers.get("vary", "").lower(), f"{label} chunk does not vary on Accept-Encoding")
    etag = headers.get("etag")
    require(etag is not None, f"{label} chunk has no ETag")

    status, headers, _ = request(path, method="HEAD")
    assert_headers(f"{label} chunk HEAD", status, headers, expect_status=200, cache=IMMUTABLE)

    status, headers, ranged = request(path, headers={"Range": "bytes=0-31"})
    assert_headers(f"{label} chunk range", status, headers, expect_status=200, cache=IMMUTABLE)
    require(ranged == body, f"{label} range request did not return the whole file")
    require("content-range" not in headers, f"{label} range request produced a partial response")

    status, headers, gzipped = request(path, headers={"Accept-Encoding": "gzip"})
    require(status == 200, f"{label} gzip request returned {status}")
    require(headers.get("content-encoding") == "gzip", f"{label} did not serve gzip_static")
    require(gzip.decompress(gzipped) == body, f"{label} gzip payload differs from the identity body")
    assert_headers(f"{label} gzip", status, headers, expect_status=200, cache=IMMUTABLE)

    status, headers, brotli_body = request(path, headers={"Accept-Encoding": "br"})
    require(status == 200, f"{label} brotli request returned {status}")
    require(headers.get("content-encoding") == "br", f"{label} did not serve the stored brotli sibling")
    stored = static_root / path.lstrip("/")
    require(
        brotli_body == stored.with_name(f"{stored.name}.br").read_bytes(),
        f"{label} brotli response is not the stored .br sibling",
    )
    for header in ("cache-control", "vary", "x-content-type-options", "content-encoding"):
        require(
            headers.count(header) == 1,
            f"{label} brotli response carries {headers.count(header)} {header} headers",
        )

    check_encoding_negotiation(label, path, body, static_root)

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
        assert_headers(f"{label} missing {missing}", status, headers, expect_status=404, cache=NO_STORE, json_type=False)
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


def main() -> None:
    global PORT
    production_image, offline_package = parse_args()
    work, token = create_owned_work()
    PORT = 39000 + (int(token[:8], 16) % 1000)
    checked: list[str] = []
    try:
        if production_image is not None:
            with ProductionImageNginx(production_image, work, token) as production:
                production_static_root = production.copy_catalog()
                check_status_matrix(
                    manifest_from_static_root(production_static_root),
                    "production",
                    production_static_root,
                )
            checked.append("final production image")
        if offline_package is not None:
            with OfflinePackageNginx(offline_package):
                check_status_matrix(
                    manifest_from_static_root(offline_package / "game" / "frontend" / "dist"),
                    "offline package",
                    offline_package / "game" / "frontend" / "dist",
                )
            checked.append("offline packaged nginx")
    finally:
        release_owned_work(work, token)
        PORT = None
    print(
        "locale-catalog serving: live HTTP/status/cache/MIME/gzip/brotli matrix verified against "
        + " and ".join(checked)
    )


if __name__ == "__main__":
    sys.exit(main())
