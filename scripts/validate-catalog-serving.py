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
  validate-catalog-serving.py --offline-package offline/_dist/ArkhamHorror-... \
      --offline-authority-fd 9
"""

from __future__ import annotations

import gzip
import hashlib
import json
import os
import shutil
import stat
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
PRODUCTION_API = "/opt/arkham/bin/arkham-api"
REQUEST_TIMEOUT = 20
PORT = None

IMMUTABLE = "public, max-age=31536000, immutable"
REVALIDATE = "public, max-age=0, must-revalidate"
NO_STORE = "no-store"
AUTHORITY_CAPABILITY_ENV = (
    "ARKHAM_TOOLCHAIN_RECEIPT_FILE",
    "ARKHAM_TOOLCHAIN_RECEIPT_TOKEN",
    "GITHUB_ENV",
)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog serving: {message}")


def discard_authority_capabilities() -> None:
    """Never let a spawned package process inherit CI receipt capabilities."""

    for variable in AUTHORITY_CAPABILITY_ENV:
        os.environ.pop(variable, None)


def consume_offline_authority_frame(descriptor: int | None) -> tuple[Path | None, str | None]:
    if descriptor is None:
        return None, None
    require(3 <= descriptor <= 1024, "--offline-authority-fd must name a non-standard descriptor")
    try:
        with os.fdopen(descriptor, "rb", closefd=True) as source:
            frame = source.read(8193)
    except OSError as error:
        require(False, f"could not read offline authority descriptor: {error}")
    require(len(frame) <= 8192, "offline authority frame is too large")
    fields = frame.split(b"\n")
    require(
        len(fields) == 3 and fields[-1] == b"",
        "offline authority frame must contain exactly two newline-terminated fields",
    )
    authority_bytes, token_bytes, _ = fields
    require(
        authority_bytes
        and b"\0" not in authority_bytes
        and b"\r" not in authority_bytes,
        "offline authority frame contains an unsafe path",
    )
    try:
        authority = Path(authority_bytes.decode("utf-8")).resolve()
        token = token_bytes.decode("ascii")
    except (UnicodeDecodeError, OSError) as error:
        require(False, f"offline authority frame is invalid: {error}")
    require(valid_sha256(token), "offline authority frame contains an invalid token")
    return authority, token


def verify_consumed_authority_isolation() -> None:
    payload = r"""
import os
import re
import sys

if any("TOOLCHAIN_RECEIPT" in key or key == "GITHUB_ENV" for key in os.environ):
    raise SystemExit(91)
parent = os.getppid()
proc = f"/proc/{parent}"
if os.path.isdir(proc):
    with open(f"{proc}/cmdline", "rb") as source:
        arguments = source.read().split(b"\0")
    if any(
        b"receipt.tsv" in argument
        or re.fullmatch(rb"[0-9a-f]{64}", argument) is not None
        for argument in arguments
    ):
        raise SystemExit(92)
    with open(f"{proc}/environ", "rb") as source:
        environment = source.read().split(b"\0")
    if any(
        entry.startswith(
            (
                b"ARKHAM_TOOLCHAIN_RECEIPT_FILE=",
                b"ARKHAM_TOOLCHAIN_RECEIPT_TOKEN=",
                b"GITHUB_ENV=",
            )
        )
        for entry in environment
    ):
        raise SystemExit(93)
    for descriptor in os.listdir(f"{proc}/fd"):
        try:
            target = os.readlink(f"{proc}/fd/{descriptor}")
        except OSError:
            continue
        if "receipt.tsv" in target:
            raise SystemExit(94)
sys.exit(0)
"""
    result = subprocess.run(
        ["/usr/bin/python3", "-c", payload],
        check=False,
        env={},
        capture_output=True,
        text=True,
    )
    require(
        result.returncode == 0,
        "a serving-gate child recovered the consumed authority capability "
        f"(exit {result.returncode}): {result.stdout}{result.stderr}",
    )


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


def parse_args():
    production_image = None
    offline_package = None
    offline_authority_fd = None
    self_test = False
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
        elif argument == "--offline-authority-fd":
            require(offline_authority_fd is None, "--offline-authority-fd was supplied more than once")
            try:
                offline_authority_fd = int(next(arguments), 10)
            except (StopIteration, ValueError):
                require(False, "--offline-authority-fd requires a decimal descriptor")
        elif argument == "--self-test":
            require(not self_test, "--self-test was supplied more than once")
            self_test = True
        else:
            require(False, f"unknown argument: {argument}")
    if self_test:
        require(
            production_image is None
            and offline_package is None
            and (offline_authority_fd is None or offline_authority_fd >= 3),
            "--self-test cannot be combined with artifact arguments",
        )
        return None, None, offline_authority_fd, True
    require(
        production_image is not None or offline_package is not None,
        "provide --production-image and/or --offline-package; surrogate nginx configs are not accepted",
    )
    require(
        (offline_package is None) == (offline_authority_fd is None),
        "--offline-package requires --offline-authority-fd",
    )
    return production_image, offline_package, offline_authority_fd, False


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


def request(path: str, *, method: str = "GET", headers=None):
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


def production_runtime_closure_digest(docker: str, container: str) -> str:
    command = r"""
queue=/usr/sbin/nginx
seen=""
while [ -n "$queue" ]; do
  set -f
  set -- $queue
  set +f
  file=$1
  shift
  queue="$*"
  case " $seen " in *" $file "*) continue ;; esac
  seen="${seen} ${file}"
  test -f "$file"
  sha256sum "$file" | awk -v path="$file" '{print "file\t" path "\t" $1}'
  dependencies="$(ldd "$file" 2>/dev/null | awk '$2 == "=>" && $3 ~ /^\// {print $3} $1 ~ /^\// {print $1}' | tr '\n' ' ')"
  queue="${queue}${queue:+ }${dependencies}"
done
"""
    result = run([docker, "exec", container, "/bin/sh", "-ec", command])
    require(result.returncode == 0, f"could not inspect production nginx library closure: {result.stderr.strip()}")
    records = sorted(set(line for line in result.stdout.splitlines() if line.startswith("file\t")))
    require(records, "production nginx library closure is empty")
    for record in records:
        fields = record.split("\t")
        require(
            len(fields) == 3 and fields[1].startswith("/") and valid_sha256(fields[2]),
            f"production nginx library closure emitted an invalid record: {record!r}",
        )
    return hashlib.sha256(("\n".join(records) + "\n").encode("utf-8")).hexdigest()


def verify_production_loaded_closure(docker: str, container: str, architecture: str) -> None:
    platform = {"amd64": "linux-x86_64", "arm64": "linux-arm64"}.get(architecture)
    require(platform is not None, f"production image has unsupported architecture for nginx authority: {architecture}")
    expected = locked_record(
        ROOT / "offline" / "toolchain.lock",
        "image",
        "nginx-runtime-closure",
        platform,
        "nginx:1.27.5",
    )
    require(
        expected[4] == "exact" and valid_sha256(expected[5]) and expected[6] == "recursive-ldd-v1",
        f"production nginx {platform} closure authority is malformed",
    )
    require(
        production_runtime_closure_digest(docker, container) == expected[5],
        "production nginx executable or recursive loaded-library closure differs from committed authority",
    )


def verify_production_api_startup(docker: str, container: str) -> None:
    dependencies = run(
        [
            docker,
            "exec",
            container,
            "/bin/sh",
            "-ec",
            f"dependencies=\"$(ldd {PRODUCTION_API})\"; "
            "printf '%s\\n' \"$dependencies\"; "
            "! printf '%s\\n' \"$dependencies\" | grep -F 'not found'",
        ]
    )
    require(
        dependencies.returncode == 0,
        "final production image cannot resolve the arkham-api runtime closure:\n"
        f"{dependencies.stdout}{dependencies.stderr}",
    )
    api_log = "/tmp/arkham-api-serving-smoke.log"
    started = run(
        [
            docker,
            "exec",
            "-d",
            "-e",
            "DATABASE_URL=postgres://arkham:arkham@127.0.0.1:1/arkham",
            "-e",
            "PORT=3002",
            container,
            "/bin/sh",
            "-ec",
            f"exec /web-entrypoint.sh {PRODUCTION_API} >{api_log} 2>&1",
        ]
    )
    require(
        started.returncode == 0,
        f"could not launch arkham-api through the production entrypoint: {started.stderr.strip()}",
    )
    last_status = None
    last_body = b""
    for _ in range(80):
        try:
            last_status, _, last_body = request("/health")
        except (urllib.error.URLError, ConnectionError, TimeoutError):
            last_status = None
        if last_status == 200:
            return
        time.sleep(0.25)
    api_diagnostics = run(
        [
            docker,
            "exec",
            container,
            "/bin/sh",
            "-c",
            f"test ! -f {api_log} || cat {api_log}",
        ]
    )
    require(
        False,
        "arkham-api did not become healthy through the production nginx path "
        f"(last status {last_status}, body {last_body[:200]!r}):\n"
        f"{api_diagnostics.stdout}{api_diagnostics.stderr}",
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
        self.container = None

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
        container = result.stdout.strip()
        require(container != "", "final production image did not return a container identity")
        self.container = container
        try:
            version = run([docker, "exec", self.container, "nginx", "-V"])
            require(
                version.returncode == 0,
                f"could not inspect production nginx: {version.stdout}{version.stderr}",
            )
            require_gzip_static(f"{version.stdout}{version.stderr}", "production")
            architecture = run(
                [docker, "image", "inspect", "--format", "{{.Architecture}}", self.image]
            )
            require(architecture.returncode == 0, "could not inspect production image architecture")
            verify_production_loaded_closure(docker, self.container, architecture.stdout.strip())
            wait_for_server("production", self.diagnostics)
            verify_production_api_startup(docker, self.container)
            return self
        except BaseException:
            self.__exit__(None, None, None)
            raise

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


PACKAGE_NGINX_CLOSURE_PATHS = (
    "bin/nginx",
    "lib",
    "pgsql/lib",
    "start.sh",
    "config/mime.types",
    "config/toolchain.lock",
    "config/toolchain-provenance.env",
)


def closure_records(root: Path, selected: tuple[str, ...]) -> list[str]:
    require(root.is_dir() and not root.is_symlink(), f"authority root is missing or unsafe: {root}")
    root = root.resolve()
    records: dict[str, str] = {}

    def collect(path: Path, relative: str) -> None:
        if relative in records:
            return
        mode = path.lstat().st_mode
        if stat.S_ISLNK(mode):
            try:
                resolved = path.resolve(strict=True)
            except (OSError, RuntimeError) as error:
                require(False, f"authority closure has an unsafe symlink {path}: {error}")
            require(path_inside(resolved, root), f"authority closure symlink escapes its root: {path}")
            records[relative] = f"link\t{relative}\t{path.readlink()}"
            collect(resolved, resolved.relative_to(root).as_posix())
            return
        if stat.S_ISREG(mode):
            digest = hashlib.sha256(path.read_bytes()).hexdigest()
            permissions = format(stat.S_IMODE(mode), "o")
            records[relative] = f"file\t{relative}\t{permissions}\t{digest}"
            return
        if stat.S_ISDIR(mode):
            permissions = format(stat.S_IMODE(mode), "o")
            records[relative] = f"dir\t{relative}\t{permissions}"
            for child in sorted(path.iterdir(), key=lambda item: item.name):
                child_relative = f"{relative}/{child.name}" if relative else child.name
                collect(child, child_relative)
            return
        require(False, f"authority closure contains an unsupported file type: {path}")

    for relative in selected:
        require(
            relative
            and not relative.startswith("/")
            and ".." not in relative.split("/")
            and "." not in relative.split("/"),
            f"unsafe authority closure path: {relative}",
        )
        path = root / relative
        require(path.exists() or path.is_symlink(), f"required authority closure path is missing: {path}")
        collect(path, relative)
    return [record for _, record in sorted(records.items())]


def closure_digest(root: Path, selected: tuple[str, ...]) -> str:
    records = closure_records(root, selected)
    require(records, f"authority closure is empty: {root}")
    return hashlib.sha256(("\n".join(records) + "\n").encode("utf-8")).hexdigest()


def frontend_closure_digest(root: Path) -> str:
    """The shipped document root must contain only ordinary directories/files.
    Symlinks could redirect a static server after the package is attested."""

    require(root.is_dir() and not root.is_symlink(), f"frontend root is missing or unsafe: {root}")
    records: list[tuple[str, str]] = []

    def collect(path: Path, relative: str) -> None:
        mode = path.lstat().st_mode
        if stat.S_ISLNK(mode):
            require(False, f"shipped frontend contains a forbidden symlink: {relative}")
        if stat.S_ISREG(mode):
            digest = hashlib.sha256(path.read_bytes()).hexdigest()
            permissions = format(stat.S_IMODE(mode), "o")
            records.append((relative, f"file\t{relative}\t{permissions}\t{digest}"))
            return
        if stat.S_ISDIR(mode):
            permissions = format(stat.S_IMODE(mode), "o")
            records.append((relative, f"dir\t{relative}\t{permissions}"))
            for child in sorted(path.iterdir(), key=lambda item: item.name):
                child_relative = f"{relative}/{child.name}" if relative else child.name
                collect(child, child_relative)
            return
        require(False, f"shipped frontend contains an unsupported file type: {relative}")

    for child in sorted(root.iterdir(), key=lambda item: item.name):
        collect(child, child.name)
    require(records, f"frontend root is empty: {root}")
    return hashlib.sha256(
        ("\n".join(record for _, record in sorted(records)) + "\n").encode("utf-8")
    ).hexdigest()


def authority_record(authority: Path, token: str, component: str) -> tuple[str, str, str]:
    require(
        authority.is_file() and not authority.is_symlink(),
        f"external release authority is missing or unsafe: {authority}",
    )
    token_digest_matches: list[str] = []
    schema_matches: list[str] = []
    record_matches: list[list[str]] = []
    for line in authority.read_text(encoding="utf-8").splitlines():
        fields = line.split("\t")
        if fields[:1] == ["token_sha256"] and len(fields) == 2:
            token_digest_matches.append(fields[1])
        elif fields[:1] == ["schema"] and len(fields) == 2:
            schema_matches.append(fields[1])
        elif fields[:2] == ["record", component] and len(fields) == 6:
            record_matches.append(fields)
    require(schema_matches == ["2"], "external release authority has an unsupported schema")
    require(
        token_digest_matches == [hashlib.sha256(token.encode("ascii")).hexdigest()],
        "external release authority token does not match this invocation",
    )
    require(len(record_matches) == 1, f"external release authority has no unique {component} record")
    _, _, lock_sha256, identity, closure_sha256, authenticator = record_matches[0]
    require(
        valid_sha256(lock_sha256)
        and valid_sha256(identity)
        and valid_sha256(closure_sha256)
        and valid_sha256(authenticator),
        f"external release authority has malformed {component} digests",
    )
    expected_authenticator = hashlib.sha256(
        f"record\t{component}\t{lock_sha256}\t{identity}\t{closure_sha256}\t{token}".encode("ascii")
    ).hexdigest()
    require(
        authenticator == expected_authenticator,
        f"external release authority has an unauthenticated {component} record",
    )
    return lock_sha256, identity, closure_sha256


def path_inside(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
    except ValueError:
        return False
    return True


def package_library(game: Path, name: str) -> Path:
    candidates = [game / "lib" / name, game / "pgsql" / "lib" / name]
    present = [candidate for candidate in candidates if candidate.exists() or candidate.is_symlink()]
    require(len(present) == 1, f"nginx dependency {name!r} is not uniquely bundled in the package")
    require(
        present[0].is_file() or present[0].is_symlink(),
        f"nginx dependency {name!r} is not a regular bundled library",
    )
    require(path_inside(present[0], game), f"nginx dependency {name!r} escapes the package")
    return present[0]


def verify_macos_nginx_closure(game: Path) -> None:
    otool = Path("/usr/bin/otool")
    require(otool.is_file() and otool.stat().st_mode & 0o111, "macOS otool is required for package closure validation")
    queue = [game / "bin" / "nginx"]
    seen: set[Path] = set()
    while queue:
        binary = queue.pop()
        resolved_binary = binary.resolve()
        if resolved_binary in seen:
            continue
        seen.add(resolved_binary)
        result = run([str(otool), "-L", str(binary)])
        require(result.returncode == 0, f"could not inspect packaged dependency closure: {binary}\n{result.stderr}")
        for line in result.stdout.splitlines()[1:]:
            dependency = line.strip().split(" (", 1)[0]
            if dependency.startswith(("/usr/lib/", "/System/Library/")):
                continue
            if dependency.startswith("@rpath/"):
                child = package_library(game, Path(dependency).name)
            elif dependency.startswith("@loader_path/"):
                child = (binary.parent / dependency.removeprefix("@loader_path/")).resolve()
                require(path_inside(child, game), f"nginx loader-path dependency escapes the package: {dependency}")
            elif dependency.startswith("@executable_path/"):
                child = (game / "bin" / dependency.removeprefix("@executable_path/")).resolve()
                require(path_inside(child, game), f"nginx executable-path dependency escapes the package: {dependency}")
            elif dependency.startswith("/"):
                child = Path(dependency)
                require(path_inside(child, game), f"nginx has a non-allowlisted host dependency: {dependency}")
            else:
                require(False, f"nginx has an unsupported dependency reference: {dependency}")
            require(child.is_file(), f"nginx bundled dependency is missing: {child}")
            queue.append(child)


def verify_linux_nginx_closure(game: Path) -> None:
    readelf = Path("/usr/bin/readelf")
    if not readelf.is_file():
        readelf = Path("/bin/readelf")
    require(readelf.is_file() and readelf.stat().st_mode & 0o111, "readelf is required for package closure validation")
    system_libraries = {
        "linux-vdso.so.1",
        "libc.so.6",
        "libm.so.6",
        "libdl.so.2",
        "libpthread.so.0",
        "librt.so.1",
        "ld-linux-aarch64.so.1",
        "ld-linux-x86-64.so.2",
    }
    queue = [game / "bin" / "nginx"]
    seen: set[Path] = set()
    while queue:
        binary = queue.pop()
        resolved_binary = binary.resolve()
        if resolved_binary in seen:
            continue
        seen.add(resolved_binary)
        result = run([str(readelf), "-d", str(binary)])
        require(result.returncode == 0, f"could not inspect packaged dependency closure: {binary}\n{result.stderr}")
        for line in result.stdout.splitlines():
            marker = "Shared library: ["
            if marker not in line:
                continue
            soname = line.split(marker, 1)[1].split("]", 1)[0]
            if soname in system_libraries:
                continue
            queue.append(package_library(game, soname))


def verify_packaged_nginx_closure(game: Path, platform: str) -> None:
    if platform.startswith("macos-"):
        verify_macos_nginx_closure(game)
    elif platform.startswith("linux-"):
        verify_linux_nginx_closure(game)
    else:
        require(False, f"unsupported packaged nginx platform: {platform}")


def verify_offline_provenance(package: Path, authority: Path, token: str) -> Path:
    game = package / "game"
    nginx_binary = game / "bin" / "nginx"
    provenance = game / "config" / "toolchain-provenance.env"
    packaged_lock = game / "config" / "toolchain.lock"
    repository_lock = ROOT / "offline" / "toolchain.lock"

    require(
        nginx_binary.is_file() and not nginx_binary.is_symlink() and nginx_binary.stat().st_mode & 0o111,
        f"offline package nginx is missing or unsafe: {nginx_binary}",
    )
    try:
        authority.relative_to(package)
    except ValueError:
        pass
    else:
        require(False, "external release authority must not reside inside the package")
    require(provenance_value(provenance, "schema") == "1", "offline nginx provenance uses an unsupported schema")
    platform = provenance_value(provenance, "platform")
    source_archive = provenance_value(provenance, "nginx_source_archive")
    source_sha256 = provenance_value(provenance, "nginx_source_sha256")
    build_identity = provenance_value(provenance, "nginx_build_identity")
    binary_sha256 = provenance_value(provenance, "nginx_binary_sha256")
    runtime_closure_sha256 = provenance_value(provenance, "nginx_runtime_closure_sha256")
    version = provenance_value(provenance, "nginx_version")
    required_option = provenance_value(provenance, "nginx_required_configure_option")

    for label, value in (
        ("nginx source SHA-256", source_sha256),
        ("nginx build identity", build_identity),
        ("nginx binary SHA-256", binary_sha256),
        ("nginx runtime closure SHA-256", runtime_closure_sha256),
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
    authority_lock, authority_identity, authority_closure = authority_record(authority, token, "offline-nginx")
    require(
        authority_lock == hashlib.sha256(packaged_lock.read_bytes()).hexdigest()
        and authority_identity == build_identity
        and authority_closure == closure_digest(game, PACKAGE_NGINX_CLOSURE_PATHS),
        "offline nginx executable/provenance/library closure differs from the external release authority",
    )
    frontend_lock, _frontend_identity, frontend_closure = authority_record(
        authority, token, "offline-frontend"
    )
    require(
        frontend_lock == hashlib.sha256(packaged_lock.read_bytes()).hexdigest()
        and frontend_closure == frontend_closure_digest(game / "frontend" / "dist"),
        "final packaged frontend tree differs from its external authority",
    )
    verify_packaged_nginx_closure(game, platform)
    return game


class OfflinePackageNginx:
    """Runs the package launcher, not a mounted host/container substitute."""

    def __init__(
        self,
        package: Path,
        authority: Path,
        token: str,
        work: Path,
        process_factory=None,
    ):
        self.package = package
        self.authority = authority
        self.token = token
        self.work = work
        self.game = verify_offline_provenance(package, authority, token)
        self.start_script = self.game / "start.sh"
        self.process_factory = (
            process_factory if process_factory is not None else subprocess.Popen
        )
        self.process = None

    def command(self, action: str) -> list[str]:
        home = self.work / "offline-package-home"
        home.mkdir(exist_ok=True)
        return [
            "/usr/bin/env",
            "-i",
            f"HOME={home}",
            "PATH=/usr/bin:/bin",
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
        try:
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
            self.process = self.process_factory(
                self.command("--serve-nginx-for-validation"),
                cwd=self.game,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            )
            wait_for_server("offline package", self.diagnostics)
            return self
        except BaseException:
            self.__exit__(None, None, None)
            raise

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
        cleanup_offline_validation_artifacts(self.game)
        return False


def cleanup_offline_validation_artifacts(game: Path) -> None:
    """Remove only files generated by the serving gate, never shipped payloads."""
    for path in (
        game / "config" / "nginx.conf",
        game / "data" / "nginx.pid",
        game / "data" / "access.log",
        game / "data" / "error.log",
        game / "data" / "nginx_temp",
    ):
        if path.is_symlink():
            require(False, f"offline serving created an unsafe validation artifact: {path}")
        if path.is_dir():
            shutil.rmtree(path)
        elif path.exists():
            require(path.is_file(), f"offline serving created an unsupported validation artifact: {path}")
            path.unlink()


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


NEGOTIATION_CASES = (
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


def run_cleanup_self_tests() -> None:
    """Force post-start failures without launching Docker/nginx and prove their
    resources are released before the exception escapes."""

    global run, tool, verify_production_runtime_authority, verify_production_loaded_closure
    global verify_production_api_startup
    global production_runtime_closure_digest, verify_offline_provenance, wait_for_server, PORT
    original_run = run
    original_tool = tool
    original_production_authority = verify_production_runtime_authority
    original_production_closure_authority = verify_production_loaded_closure
    original_production_api_startup = verify_production_api_startup
    original_production_closure_digest = production_runtime_closure_digest
    original_offline_provenance = verify_offline_provenance
    original_wait = wait_for_server
    original_port = PORT
    work, token = create_owned_work()
    calls: list[list[str]] = []

    def fake_tool(name: str) -> str:
        return name

    def fake_run(command: list[str], **_) -> subprocess.CompletedProcess:
        calls.append(command)
        if len(command) > 1 and command[1] == "run":
            return subprocess.CompletedProcess(command, 0, "container-id\n", "")
        if len(command) > 1 and command[1] == "exec":
            return subprocess.CompletedProcess(command, 1, "", "forced nginx -V failure")
        return subprocess.CompletedProcess(command, 0, "", "")

    def forced_readiness_failure(*_) -> None:
        raise SystemExit("forced readiness failure")

    class FakePopen:
        instances: list["FakePopen"] = []

        def __init__(self, *_args, **_kwargs):
            self.terminated = False
            self.killed = False
            FakePopen.instances.append(self)

        def poll(self):
            return None

        def terminate(self):
            self.terminated = True

        def kill(self):
            self.killed = True

        def communicate(self, **_kwargs):
            return "", ""

    try:
        PORT = 39991
        os.environ["ARKHAM_TOOLCHAIN_RECEIPT_FILE"] = str(work / "receipt.tsv")
        os.environ["ARKHAM_TOOLCHAIN_RECEIPT_TOKEN"] = "a" * 64
        discard_authority_capabilities()
        require(
            all(variable not in os.environ for variable in AUTHORITY_CAPABILITY_ENV),
            "serving validator retained an authority capability in its child environment",
        )
        run = fake_run
        tool = fake_tool
        verify_production_runtime_authority = lambda *_: None
        verify_production_loaded_closure = lambda *_: None
        verify_production_api_startup = lambda *_: None

        try:
            with ProductionImageNginx("test-image", work, token):
                pass
        except SystemExit:
            pass
        else:
            require(False, "cleanup self-test did not force production nginx -V failure")
        require(
            any(len(command) > 1 and command[1] == "rm" for command in calls),
            "production nginx -V failure leaked its Docker container",
        )

        production_runtime_closure_digest = lambda *_: "0" * 64
        try:
            verify_production_loaded_closure = original_production_closure_authority
            verify_production_loaded_closure("docker", "container", "amd64")
        except SystemExit:
            pass
        else:
            require(False, "production nginx executable/library substitution was accepted")
        verify_production_loaded_closure = lambda *_: None

        calls.clear()
        def successful_version_run(command: list[str], **_kwargs) -> subprocess.CompletedProcess:
            calls.append(command)
            if len(command) > 1 and command[1] == "run":
                return subprocess.CompletedProcess(command, 0, "container-id\n", "")
            if len(command) > 1 and command[1] == "exec":
                return subprocess.CompletedProcess(command, 0, "", "--with-http_gzip_static_module")
            return subprocess.CompletedProcess(command, 0, "", "")

        run = successful_version_run
        wait_for_server = forced_readiness_failure
        try:
            with ProductionImageNginx("test-image", work, f"{token}ready"):
                pass
        except SystemExit:
            pass
        else:
            require(False, "cleanup self-test did not force production readiness failure")
        require(
            any(len(command) > 1 and command[1] == "rm" for command in calls),
            "production readiness failure leaked its Docker container",
        )

        calls.clear()
        wait_for_server = lambda *_: None
        verify_production_api_startup = forced_readiness_failure
        try:
            with ProductionImageNginx("test-image", work, f"{token}api"):
                pass
        except SystemExit:
            pass
        else:
            require(False, "cleanup self-test did not force production API startup failure")
        require(
            any(len(command) > 1 and command[1] == "rm" for command in calls),
            "production API startup failure leaked its Docker container",
        )
        verify_production_api_startup = lambda *_: None
        wait_for_server = forced_readiness_failure

        game = work / "offline-game"
        game.mkdir()
        (game / "config").mkdir()
        (game / "data" / "nginx_temp").mkdir(parents=True)
        for artifact in ("nginx.conf",):
            (game / "config" / artifact).write_text("validation artifact\n", encoding="utf-8")
        for artifact in ("nginx.pid", "access.log", "error.log"):
            (game / "data" / artifact).write_text("validation artifact\n", encoding="utf-8")
        (game / "start.sh").write_text("#!/usr/bin/env bash\n", encoding="utf-8")
        package = work / "offline-package"
        package.mkdir()
        authority = work / "external-receipt.tsv"
        authority.write_text("schema\t1\n", encoding="utf-8")
        verify_offline_provenance = lambda *_: game
        run = lambda command, **_kwargs: subprocess.CompletedProcess(command, 0, "", "")
        offline = OfflinePackageNginx(
            package,
            authority,
            "a" * 64,
            work,
            process_factory=FakePopen,
        )
        require(
            all("TOOLCHAIN_RECEIPT" not in value for value in offline.command("--validate-nginx-config")),
            "offline package command exposes an authority capability",
        )
        try:
            with offline:
                pass
        except SystemExit:
            pass
        else:
            require(False, "cleanup self-test did not force offline readiness failure")
        require(
            len(FakePopen.instances) == 1 and FakePopen.instances[0].terminated,
            "offline readiness failure leaked its nginx process",
        )
        require(
            not any(
                path.exists() or path.is_symlink()
                for path in (
                    game / "config" / "nginx.conf",
                    game / "data" / "nginx.pid",
                    game / "data" / "access.log",
                    game / "data" / "error.log",
                    game / "data" / "nginx_temp",
                )
            ),
            "offline serving validation left mutable package artifacts behind",
        )
    finally:
        run = original_run
        tool = original_tool
        verify_production_runtime_authority = original_production_authority
        verify_production_loaded_closure = original_production_closure_authority
        verify_production_api_startup = original_production_api_startup
        production_runtime_closure_digest = original_production_closure_digest
        verify_offline_provenance = original_offline_provenance
        wait_for_server = original_wait
        PORT = original_port
        release_owned_work(work, token)


def main() -> None:
    global PORT
    production_image, offline_package, offline_authority_fd, self_test = parse_args()
    offline_authority, offline_authority_token = consume_offline_authority_frame(
        offline_authority_fd
    )
    discard_authority_capabilities()
    if self_test:
        if offline_authority is not None:
            verify_consumed_authority_isolation()
        run_cleanup_self_tests()
        print("locale-catalog serving: context-manager cleanup self-tests passed")
        return
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
            require(offline_authority is not None and offline_authority_token is not None, "offline authority arguments are missing")
            with OfflinePackageNginx(offline_package, offline_authority, offline_authority_token, work):
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
