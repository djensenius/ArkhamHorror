#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
# ]
# ///

"""Generation, schema, and provenance gate for the public locale catalog.

The catalog (see docs/locale-catalog.md) is deployment-owned output, not a
committed artifact: `frontend/scripts/locale-catalog/generate.mjs` produces it
during the frontend build from the same `frontend/src/locales/**` snapshot Vue
bundles. This gate therefore checks the *generator and its deploy seam* rather
than checked-in bytes:

  * the generator is deterministic across separate processes;
  * a build whose *sources* are nothing but git-tracked files — with the
    lockfile-installed dependencies reused rather than reinstalled —
    reproduces the same revision byte for byte, so a clean checkout and the
    deployment agree;
  * the manifest and every chunk validate against the committed v1 schemas;
  * every chunk is pinned by size and SHA-256, content-addressed, immutable;
  * the manifest's provenance digests really are digests of the exact source
    bytes the catalog was generated from, and every one of those sources is
    tracked by git;
  * the required key set comes from the governed contract fixtures, resolves,
    and cannot silently regress: removing a required key from a scratch clone
    must fail generation;
  * `--check` detects stale output;
  * the production nginx/container/build wiring actually publishes it.

Nothing here is skipped when a tool is missing: an absent `node`, `npm`
install, or `git` is a failure, not a skip.
"""

import hashlib
import json
import re
import shutil
import subprocess
import sys
from pathlib import Path

from jsonschema.validators import validator_for

import strict_json

ROOT = Path(__file__).resolve().parents[1]
FRONTEND = ROOT / "frontend"
GENERATOR = FRONTEND / "scripts" / "locale-catalog" / "generate.mjs"
SCHEMA_DIR = FRONTEND / "schemas" / "locale-catalog" / "v1"
WORK_ROOT = FRONTEND / "node_modules" / ".locale-catalog-validate"

REQUIRED_KEY_FIXTURES = (
    "contracts/fixtures/question-read.json",
    "contracts/fixtures/question-read-with-cards.json",
)

# Files a clean-clone build needs. Everything else in the worktree (installed
# packages, generated output, card exports) is deliberately excluded, so the
# copy proves the generator depends only on committed sources.
CLEAN_CLONE_PREFIXES = (
    "frontend/src/",
    "frontend/homebrew/",
    "frontend/scripts/",
    "frontend/schemas/",
    "contracts/",
)
CLEAN_CLONE_FILES = ("frontend/package.json",)

# Production modules the normalizer mirrors (icon/literal classification and
# the asset-path variables). Their bytes change catalog semantics, so they must
# be part of its provenance.
SEMANTIC_SOURCES = (
    "frontend/src/arkham/helpers.ts",
    "frontend/src/arkham/homebrewAssets.ts",
    "frontend/src/arkham/components/FormattedEntry.vue",
)

MAX_CHUNK_BYTES = 8 * 1024 * 1024
MAX_CHUNKS_PER_LOCALE = 256


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog: {message}")


def sha256_hex(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def read_source_bytes(relative: str) -> bytes:
    """Reads a repository file for hashing, refusing symlinks and any path that
    resolves outside the repository."""
    path = (ROOT / relative).resolve()
    require(path.is_relative_to(ROOT), f"{relative} resolves outside the repository")
    require(not (ROOT / relative).is_symlink(), f"{relative} is a symlink")
    require(path.is_file(), f"{relative} is not a regular file")
    return path.read_bytes()


def canonical_bytes(value: object) -> bytes:
    """The generator's canonical JSON encoding: sorted keys, no insignificant
    whitespace, trailing newline. Used here to re-derive provenance digests
    independently of the JavaScript that produced them."""
    return (
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n"
    ).encode("utf-8")


def run_generator(args: list[str], *, frontend: Path = FRONTEND, expect_success: bool = True):
    node = shutil.which("node")
    require(node is not None, "node is required to validate the locale catalog")
    result = subprocess.run(
        [node, str(frontend / "scripts" / "locale-catalog" / "generate.mjs"), *args],
        cwd=frontend,
        capture_output=True,
        text=True,
        check=False,
    )
    if expect_success:
        require(
            result.returncode == 0,
            f"generator failed ({' '.join(args)}):\n{result.stdout}\n{result.stderr}",
        )
    return result


def git_tracked_files() -> set[str]:
    git = shutil.which("git")
    require(git is not None, "git is required to validate catalog provenance")
    result = subprocess.run(
        [git, "-C", str(ROOT), "ls-files", "-z"], capture_output=True, check=True
    )
    return {entry for entry in result.stdout.decode("utf-8").split("\0") if entry}


def load_schemas() -> dict[str, object]:
    schemas = {}
    for name in ("manifest", "chunk"):
        schema = strict_json.strict_json_load_path(SCHEMA_DIR / f"{name}.schema.json")
        validator_cls = validator_for(schema)
        validator_cls.check_schema(schema)
        require(
            schema.get("$id", "").endswith(f"/locale-catalog/v1/{name}.schema.json"),
            f"{name} schema is missing its versioned $id",
        )
        schemas[name] = validator_cls(schema)

    raw = {
        name: strict_json.strict_json_load_path(SCHEMA_DIR / f"{name}.schema.json")
        for name in ("manifest", "chunk")
    }
    constraints = {"type", "pattern", "maxLength"}
    require(
        {
            key: value
            for key, value in raw["manifest"]["$defs"]["messageKey"].items()
            if key in constraints
        }
        == {
            key: value
            for key, value in raw["chunk"]["$defs"]["messageKey"].items()
            if key in constraints
        },
        "the manifest and chunk schemas disagree on the message-key grammar",
    )
    require(
        raw["manifest"]["properties"]["provenance"]["properties"]["requiredKeys"]["items"]
        == {"$ref": "#/$defs/messageKey"},
        "required keys must be typed by the shared message-key grammar",
    )
    return schemas


def read_generated(directory: Path) -> dict[str, bytes]:
    files = {}
    for path in sorted(directory.rglob("*")):
        if path.is_dir():
            continue
        require(path.is_file() and not path.is_symlink(), f"{path} is not a regular file")
        files[path.relative_to(directory).as_posix()] = path.read_bytes()
    return files


def required_keys_from_fixture(value: object, into: set[str]) -> set[str]:
    """Independent re-implementation of the generator's required-key
    extraction, so the two must agree on what the contract demands."""
    if isinstance(value, list):
        for item in value:
            required_keys_from_fixture(item, into)
        return into
    if not isinstance(value, dict):
        return into

    if value.get("tag") == "I18nEntry" and isinstance(value.get("key"), str):
        into.add(value["key"])
    for field, child in value.items():
        if isinstance(child, str):
            if child.startswith("$") and field in {"title", "label", "text"}:
                key = child[1:].split(" ")[0]
                if key:
                    into.add(key)
            continue
        required_keys_from_fixture(child, into)
    return into


def validate_catalog(files: dict[str, bytes], schemas) -> dict:
    require("manifest.json" in files, "manifest.json was not generated")
    manifest = strict_json.strict_json_loads(files["manifest.json"], source="manifest.json")
    errors = sorted(schemas["manifest"].iter_errors(manifest), key=lambda error: str(list(error.path)))
    require(
        not errors,
        "manifest does not satisfy the v1 manifest schema: "
        + "; ".join(f"{list(error.path)}: {error.message}" for error in errors[:5]),
    )

    revision = manifest["catalogRevision"]
    require(
        revision == f"1.{manifest['provenance']['sha256'][:32]}",
        "catalogRevision is not derived from the provenance digest",
    )
    require(
        manifest["revisionManifestPath"] == f"{manifest['basePath']}/r/{revision}/manifest.json",
        "revisionManifestPath does not address the current revision",
    )
    # The stable URL a capability response will advertise; a client that
    # follows it must land on the manifest this build actually wrote.
    require(
        manifest["manifestPath"] == f"{manifest['basePath']}/manifest.json",
        "manifestPath is not the stable manifest URL under basePath",
    )
    require(
        manifest["manifestPath"][len(manifest["basePath"]) + 1 :] in files,
        "manifestPath does not address a generated file",
    )

    revision_manifest = f"r/{revision}/manifest.json"
    require(revision_manifest in files, "the immutable revision manifest was not generated")
    require(
        files[revision_manifest] == files["manifest.json"],
        "the stable and immutable manifests are not byte-identical",
    )

    seen_paths: set[str] = set()
    seen_packs: set[tuple[str, str]] = set()
    total_chunks = 0
    total_keys = 0
    total_unsupported = 0
    total_bytes = 0
    locales = {entry["locale"] for entry in manifest["locales"]}
    require(manifest["defaultLocale"] in locales, "the default locale is not published")

    for locale_entry in manifest["locales"]:
        locale = locale_entry["locale"]
        expected_fallback = None if locale == manifest["defaultLocale"] else manifest["defaultLocale"]
        require(
            locale_entry["fallback"] == expected_fallback,
            f"{locale} declares fallback {locale_entry['fallback']!r}",
        )
        require(
            len(locale_entry["chunks"]) <= MAX_CHUNKS_PER_LOCALE,
            f"{locale} publishes {len(locale_entry['chunks'])} chunks",
        )

        locale_keys = 0
        locale_bytes = 0
        for descriptor in locale_entry["chunks"]:
            prefix = f"{manifest['basePath']}/r/{revision}/{locale}/"
            require(
                descriptor["path"].startswith(prefix) and descriptor["path"].endswith(".json"),
                f"{descriptor['path']} is not an immutable revision path",
            )
            require(descriptor["path"] not in seen_paths, f"duplicate chunk path {descriptor['path']}")
            seen_paths.add(descriptor["path"])
            require(
                (locale, descriptor["pack"]) not in seen_packs,
                f"duplicate pack {locale}/{descriptor['pack']}",
            )
            seen_packs.add((locale, descriptor["pack"]))

            relative = descriptor["path"][len(manifest["basePath"]) + 1 :]
            require(relative in files, f"{descriptor['path']} was not generated")
            content = files[relative]
            require(len(content) == descriptor["bytes"], f"{descriptor['path']} size mismatch")
            require(len(content) <= MAX_CHUNK_BYTES, f"{descriptor['path']} exceeds the chunk size bound")
            digest = sha256_hex(content)
            require(digest == descriptor["sha256"], f"{descriptor['path']} digest mismatch")
            require(
                relative.endswith(f"{descriptor['pack']}.{digest[:16]}.json"),
                f"{descriptor['path']} is not content-addressed",
            )

            chunk = strict_json.strict_json_loads(content, source=descriptor["path"])
            chunk_errors = sorted(schemas["chunk"].iter_errors(chunk), key=lambda error: str(list(error.path)))
            require(
                not chunk_errors,
                f"{descriptor['path']} does not satisfy the v1 chunk schema: "
                + "; ".join(f"{list(error.path)}: {error.message}" for error in chunk_errors[:5]),
            )
            require(chunk["catalogRevision"] == revision, f"{descriptor['path']} revision mismatch")
            require(chunk["locale"] == locale, f"{descriptor['path']} locale mismatch")
            require(chunk["fallback"] == expected_fallback, f"{descriptor['path']} fallback mismatch")
            require(chunk["pack"] == descriptor["pack"], f"{descriptor['path']} pack mismatch")
            require(
                len(chunk["entries"]) == descriptor["keys"],
                f"{descriptor['path']} key count mismatch",
            )
            unsupported = sum(
                1 for entry in chunk["entries"].values() if entry["form"] == "unsupported"
            )
            require(
                unsupported == descriptor["unsupportedKeys"],
                f"{descriptor['path']} unsupported count mismatch",
            )

            total_chunks += 1
            total_keys += descriptor["keys"]
            total_unsupported += unsupported
            locale_keys += descriptor["keys"]
            locale_bytes += len(content)

        require(locale_entry["keys"] == locale_keys, f"{locale} key total mismatch")
        require(locale_entry["bytes"] == locale_bytes, f"{locale} byte total mismatch")
        total_bytes += locale_bytes

    require(manifest["totals"]["chunks"] == total_chunks, "manifest chunk total mismatch")
    require(manifest["totals"]["keys"] == total_keys, "manifest key total mismatch")
    require(manifest["totals"]["bytes"] == total_bytes, "manifest byte total mismatch")
    require(
        manifest["totals"]["unsupportedKeys"] == total_unsupported,
        "manifest unsupported total mismatch",
    )
    require(
        manifest["totals"]["locales"] == len(manifest["locales"]),
        "manifest locale total mismatch",
    )

    # Every generated file is accounted for by the manifest.
    accounted = {"manifest.json", revision_manifest} | {
        path[len(manifest["basePath"]) + 1 :] for path in seen_paths
    }
    require(
        set(files) == accounted,
        f"unaccounted generated files: {sorted(set(files) - accounted)[:5]}",
    )
    return manifest


def validate_required_keys(files: dict[str, bytes], manifest: dict) -> None:
    expected: set[str] = set()
    for fixture in REQUIRED_KEY_FIXTURES:
        required_keys_from_fixture(
            strict_json.strict_json_loads(
                strict_json.read_governed_worktree_bytes(ROOT, fixture), source=fixture
            ),
            expected,
        )
    require(expected, "the contract fixtures declared no required I18n keys")
    require(
        sorted(expected) == sorted(manifest["provenance"]["requiredKeys"]),
        "the manifest's required key set does not match the contract fixtures",
    )

    default_locale = manifest["defaultLocale"]
    chunks: dict[tuple[str, str], dict] = {}
    for locale_entry in manifest["locales"]:
        for descriptor in locale_entry["chunks"]:
            relative = descriptor["path"][len(manifest["basePath"]) + 1 :]
            chunks[(locale_entry["locale"], descriptor["pack"])] = strict_json.strict_json_loads(
                files[relative], source=descriptor["path"]
            )

    for key in sorted(expected):
        pack = key.split(".")[0] if "." in key else "core"
        for locale_entry in manifest["locales"]:
            locale = locale_entry["locale"]
            chunk = chunks.get((locale, pack))
            entry = None if chunk is None else chunk["entries"].get(key)
            if entry is None:
                require(
                    locale != default_locale,
                    f"required key {key} is missing from {default_locale}",
                )
                continue
            require(
                entry["form"] != "unsupported",
                f"required key {key} is unsupported in {locale}: {entry.get('reason')}",
            )

    # The rich required entry keeps its instructions: emphasis, the
    # encounter-set section, and semantic image references (never bytes/URLs).
    gather = chunks[("en", "nightOfTheZealot")]["entries"][
        "nightOfTheZealot.theGathering.setup.gatherSets"
    ]
    images = []
    stack = list(gather["nodes"])
    kinds = set()
    while stack:
        node = stack.pop()
        kinds.add(node["type"])
        if node["type"] == "image":
            images.append(node)
        stack.extend(node.get("children", []))
        for item in node.get("items", []):
            stack.extend(item["children"])
    require("emphasis" in kinds, "the required rich entry lost its emphasis")
    require(len(images) == 6, f"the required rich entry has {len(images)} encounter-set icons")
    for image in images:
        require(image["role"] == "encounterSet", "encounter-set icons lost their role")
        require(
            re.fullmatch(r"encounter-sets/[a-z0-9-]+\.png", image["assetPath"]) is not None,
            f"unexpected asset path {image['assetPath']}",
        )


def validate_provenance(provenance_path: Path, manifest: dict, tracked: set[str]) -> None:
    provenance = strict_json.strict_json_load_path(provenance_path)
    require(
        sha256_hex(canonical_bytes(provenance)) == manifest["provenance"]["sha256"],
        "the manifest's provenance digest does not match the generator's provenance record",
    )
    for field, value in (
        ("localeSourcesSha256", provenance["localeSources"]),
        ("schemasSha256", provenance["schemas"]),
        ("generatorSha256", provenance["generator"]),
    ):
        require(
            manifest["provenance"][field] == sha256_hex(canonical_bytes(value)),
            f"manifest provenance {field} does not match the hashed inputs",
        )
    require(
        manifest["provenance"]["localeSourceFiles"] == len(provenance["localeSources"]),
        "manifest provenance source count mismatch",
    )

    contract_manifest = strict_json.strict_json_loads(
        strict_json.read_governed_worktree_bytes(ROOT, "contracts/manifest.json"),
        source="contracts/manifest.json",
    )
    require(
        manifest["provenance"]["contractRevision"] == contract_manifest["schemaRevision"],
        "the catalog records a different contract revision than contracts/manifest.json",
    )

    hashed = []
    for group, prefix in (
        (provenance["localeSources"], "frontend/"),
        (provenance["schemas"], ""),
        (provenance["generator"]["sources"], ""),
        (provenance["contract"]["fixtures"], ""),
    ):
        for record in group:
            hashed.append((f"{prefix}{record['path']}", record["sha256"]))

    for relative, digest in hashed:
        require(relative in tracked, f"the catalog hashes untracked file {relative}")
        require(sha256_hex(read_source_bytes(relative)) == digest, f"provenance digest mismatch for {relative}")

    # Coverage: everything whose bytes can change the catalog must be hashed,
    # so a generator that quietly stopped hashing an input cannot pass.
    hashed_paths = {relative for relative, _ in hashed}
    covered = set(SEMANTIC_SOURCES) | set(REQUIRED_KEY_FIXTURES)
    for relative in tracked:
        if relative.startswith("frontend/src/locales/"):
            covered.add(relative)
        elif relative.startswith("frontend/homebrew/") and (
            ("/locales/" in relative and relative.endswith(".json"))
            or re.fullmatch(r"frontend/homebrew/[^/]+/icons\.json", relative) is not None
        ):
            covered.add(relative)
    for schema in SCHEMA_DIR.glob("*.schema.json"):
        covered.add(schema.relative_to(ROOT).as_posix())
    for generator in (FRONTEND / "scripts" / "locale-catalog").glob("*.mjs"):
        covered.add(generator.relative_to(ROOT).as_posix())

    for relative in sorted(covered):
        require(relative in tracked, f"{relative} is not tracked by git")
        require(relative in hashed_paths, f"{relative} is not covered by catalog provenance")


def validate_clean_clone(tracked: set[str], manifest: dict, files: dict[str, bytes]) -> Path:
    """Builds the catalog in a scratch tree containing only git-tracked source
    files (dependencies are reused from the already-installed
    frontend/node_modules rather than reinstalled) and requires it to
    reproduce the worktree build byte for byte."""
    clone = WORK_ROOT / "clean-clone"
    shutil.rmtree(clone, ignore_errors=True)
    for relative in sorted(tracked):
        if not (relative.startswith(CLEAN_CLONE_PREFIXES) or relative in CLEAN_CLONE_FILES):
            continue
        target = clone / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, target)

    (clone / "frontend" / "node_modules").symlink_to(FRONTEND / "node_modules")
    out = clone / "out"
    result = run_generator(["--out", str(out)], frontend=clone / "frontend")
    require("revision" in result.stdout, f"unexpected generator output: {result.stdout}")

    clone_files = read_generated(out)
    require(
        clone_files == files,
        "a build from git-tracked sources alone does not reproduce the worktree catalog",
    )
    require(
        strict_json.strict_json_loads(clone_files["manifest.json"], source="clean clone")[
            "catalogRevision"
        ]
        == manifest["catalogRevision"],
        "the clean-clone build produced a different revision",
    )
    return clone


def validate_fail_closed(clone: Path, manifest: dict) -> None:
    """Removing a required key from the clean clone must fail generation."""
    required = manifest["provenance"]["requiredKeys"]
    base = clone / "frontend" / "src" / "locales" / "en" / "base.json"
    messages = strict_json.strict_json_load_path(base)
    victim = next(key for key in required if key in messages)
    del messages[victim]
    base.write_text(json.dumps(messages, ensure_ascii=False), encoding="utf-8")

    result = run_generator(
        ["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False
    )
    require(
        result.returncode != 0 and f"required key {victim} is missing" in result.stderr,
        f"generation succeeded after removing required key {victim}",
    )

    # A malformed locale source must fail too, rather than emitting a partial
    # catalog.
    base.write_text("{ not json", encoding="utf-8")
    result = run_generator(
        ["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False
    )
    require(result.returncode != 0, "generation succeeded with malformed locale JSON")


def validate_stale_detection(directory: Path, manifest: dict) -> None:
    result = run_generator(["--check", "--out", str(directory)])
    require(manifest["catalogRevision"] in result.stdout, "--check did not confirm the revision")

    victim = next(
        path
        for path in sorted(directory.rglob("*.json"))
        if not path.name.startswith("manifest")
    )
    original = victim.read_bytes()
    victim.write_bytes(original + b" ")
    result = run_generator(["--check", "--out", str(directory)], expect_success=False)
    require(result.returncode != 0, "--check accepted a tampered chunk")
    require("stale generated file" in result.stderr, f"unexpected --check output: {result.stderr}")
    victim.write_bytes(original)

    victim.unlink()
    result = run_generator(["--check", "--out", str(directory)], expect_success=False)
    require(result.returncode != 0, "--check accepted a missing chunk")
    victim.write_bytes(original)


def validate_deployment_wiring(manifest: dict) -> None:
    base = manifest["basePath"]
    nginx = (ROOT / "prod.nginxconf").read_text(encoding="utf-8")
    require(
        f'"~^{base}/r/" "public, max-age=31536000, immutable"' in nginx,
        "prod.nginxconf does not cache revision-addressed catalog paths immutably",
    )
    # One `^~` prefix location, so an unknown catalog path 404s instead of
    # being answered with the SPA shell.
    marker = f"location ^~ {base}/ {{"
    require(marker in nginx, f"prod.nginxconf is missing the `{marker}` location")
    block = nginx.split(marker, 1)[1].split("\n    location ", 1)[0]
    require("default_type application/json;" in block, "the catalog location does not serve JSON")
    require(
        'add_header X-Content-Type-Options "nosniff" always;' in block,
        "the catalog location does not set nosniff",
    )
    require(
        "add_header Cache-Control $cache_control always;" in block,
        "the catalog location sets no cache policy",
    )
    require(
        'add_header Vary "Accept-Encoding" always;' in block,
        "the catalog location does not vary on Accept-Encoding when serving brotli",
    )
    # Every other encoding is covered by gzip_vary at the http level; without
    # it a shared cache could hand a gzip body to a client that cannot read it.
    require(
        re.search(r"^\s*gzip_vary\s+on;", nginx, re.MULTILINE) is not None,
        "prod.nginxconf must keep `gzip_vary on` so non-brotli responses also vary",
    )
    for directive in (
        "auth_basic",
        "auth_request",
        "auth_jwt",
        "auth_delay",
        "satisfy",
        "proxy_pass",
        "try_files",
        "return",
    ):
        require(
            re.search(rf"^\s*{directive}\b", block, re.MULTILINE) is None,
            f"the catalog location must not use `{directive}`; it is plain, unauthenticated static delivery",
        )
    require(
        nginx.count(f"location ^~ {base}") == 1 and f"location ~* ^{base}" not in nginx,
        "expected exactly one locale-catalog nginx location",
    )

    package = strict_json.strict_json_load_path(FRONTEND / "package.json")
    require(
        "npm run locale-catalog" in package["scripts"]["prebuild"],
        "the frontend build does not generate the catalog",
    )
    require(
        package["scripts"]["locale-catalog"].endswith("scripts/locale-catalog/generate.mjs"),
        "the locale-catalog npm script does not run the generator",
    )
    for dependency in ("parse5", "@intlify/message-compiler"):
        require(
            dependency in package["devDependencies"],
            f"{dependency} must be a declared dependency of the generator",
        )

    dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
    require(
        "COPY ./contracts /opt/arkham/src/contracts" in dockerfile,
        "the container build does not provide the contract fixtures the generator requires",
    )

    ignore = (FRONTEND / ".gitignore").read_text(encoding="utf-8")
    require("public/locale-catalog/" in ignore, "generated catalog output is not git-ignored")

    # Checked against paths *inside* the output directory: a trailing-slash
    # gitignore pattern only matches a directory that exists, so asking about
    # the bare directory would pass locally (where a build left it behind) and
    # fail on a clean checkout.
    git = shutil.which("git")
    for path in (
        "frontend/public/locale-catalog/manifest.json",
        "frontend/public/locale-catalog/r/1.0/en/core.0000000000000000.json",
    ):
        result = subprocess.run(
            [git, "-C", str(ROOT), "check-ignore", "-q", path], check=False
        )
        require(result.returncode == 0, f"{path} is not ignored by git")


def main() -> None:
    strict_json.run_self_tests()

    require(GENERATOR.is_file(), "the locale-catalog generator is missing")
    require(
        (FRONTEND / "node_modules" / "parse5").is_dir(),
        "frontend dependencies are not installed (run `npm ci` in frontend/)",
    )

    schemas = load_schemas()
    tracked = git_tracked_files()

    shutil.rmtree(WORK_ROOT, ignore_errors=True)
    WORK_ROOT.mkdir(parents=True)

    first = WORK_ROOT / "first"
    second = WORK_ROOT / "second"
    provenance_path = WORK_ROOT / "provenance.json"
    run_generator(["--out", str(first), "--provenance", str(provenance_path)])
    run_generator(["--out", str(second)])

    files = read_generated(first)
    require(
        files == read_generated(second),
        "two generator runs produced different bytes",
    )

    manifest = validate_catalog(files, schemas)
    validate_required_keys(files, manifest)
    validate_provenance(provenance_path, manifest, tracked)
    validate_stale_detection(first, manifest)
    clone = validate_clean_clone(tracked, manifest, files)
    validate_fail_closed(clone, manifest)
    validate_deployment_wiring(manifest)

    shutil.rmtree(WORK_ROOT, ignore_errors=True)

    print(
        f"locale-catalog: revision {manifest['catalogRevision']} verified — "
        f"{manifest['totals']['locales']} locales, {manifest['totals']['chunks']} files, "
        f"{manifest['totals']['keys']} keys "
        f"({manifest['totals']['unsupportedKeys']} explicitly unsupported), "
        f"{manifest['totals']['bytes'] / 1024 / 1024:.2f} MB"
    )


if __name__ == "__main__":
    sys.exit(main())
