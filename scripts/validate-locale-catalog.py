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
CLEAN_CLONE_FILES = (
    "frontend/package.json",
    "frontend/package-lock.json",
    "backend/arkham-api/i18n-emitted-keys.json",
)
BACKEND_KEYS = "backend/arkham-api/i18n-emitted-keys.json"
BACKEND_EXTRACTOR = "scripts/extract-backend-i18n-keys.py"

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


def run_generator(
    args: list[str],
    *,
    frontend: Path = FRONTEND,
    cwd: Path | None = None,
    expect_success: bool = True,
):
    node = shutil.which("node")
    require(node is not None, "node is required to validate the locale catalog")
    result = subprocess.run(
        [node, str(frontend / "scripts" / "locale-catalog" / "generate.mjs"), *args],
        cwd=cwd or frontend,
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
        raw["manifest"]["properties"]["provenance"]["properties"]["fixtureKeys"]["items"]
        == {"$ref": "#/$defs/messageKey"}
        and raw["manifest"]["properties"]["backend"]["properties"]["untranslatedKeys"]["items"]
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
    require(
        manifest["chunkPathPrefix"] == f"{manifest['basePath']}/c/",
        "chunkPathPrefix is not the content-addressed prefix",
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
            # Chunk URLs carry only the content digest, so an unchanged pack
            # keeps its URL across revisions and stays reachable from a replica
            # of either revision mid-rollout.
            require(
                descriptor["path"] == f"{manifest['chunkPathPrefix']}{descriptor['sha256']}.json",
                f"{descriptor['path']} is not addressed by its content digest",
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
            require(relative == f"c/{digest}.json", f"{descriptor['path']} is not content-addressed")

            chunk = strict_json.strict_json_loads(content, source=descriptor["path"])
            chunk_errors = sorted(schemas["chunk"].iter_errors(chunk), key=lambda error: str(list(error.path)))
            require(
                not chunk_errors,
                f"{descriptor['path']} does not satisfy the v1 chunk schema: "
                + "; ".join(f"{list(error.path)}: {error.message}" for error in chunk_errors[:5]),
            )
            require(
                "catalogRevision" not in chunk,
                f"{descriptor['path']} names a revision, which would break cross-revision reuse",
            )
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
        sorted(expected) == sorted(manifest["provenance"]["fixtureKeys"]),
        "the manifest's fixture key set does not match the contract fixtures",
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

    # At least one required entry must still carry the rich content this
    # catalog exists to deliver: emphasis plus semantic encounter-set image
    # references (never bytes, never URLs). The entry is discovered, not
    # hard-coded, so a fixture or default-locale change fails with a message
    # rather than a KeyError.
    rich_key = None
    for key in sorted(expected):
        pack = key.split(".")[0] if "." in key else "core"
        chunk = chunks.get((default_locale, pack))
        entry = None if chunk is None else chunk["entries"].get(key)
        if entry is None or entry["form"] != "message":
            continue
        kinds, images = collect_node_kinds(entry["nodes"])
        if "emphasis" in kinds and images:
            rich_key = key
            for image in images:
                require(
                    image["role"] == "encounterSet",
                    f"{key} image reference lost its role ({image['role']})",
                )
                require(
                    re.fullmatch(r"encounter-sets/[a-z0-9-]+\.png", image["assetPath"]) is not None,
                    f"{key} has unexpected asset path {image['assetPath']}",
                )
                require(
                    "src" not in image and "url" not in image,
                    f"{key} image is not a semantic reference",
                )
            break

    require(
        rich_key is not None,
        "no required key carries emphasis and encounter-set image references; "
        "the catalog would ship only plain text for the contract's rich content",
    )


def collect_node_kinds(nodes: list) -> tuple[set[str], list[dict]]:
    kinds: set[str] = set()
    images: list[dict] = []
    stack = list(nodes)
    while stack:
        node = stack.pop()
        kinds.add(node["type"])
        if node["type"] == "image":
            images.append(node)
        stack.extend(node.get("children", []))
        for item in node.get("items", []):
            stack.extend(item["children"])
    return kinds, images


def validate_backend_requirements(files: dict[str, bytes], manifest: dict) -> None:
    """The catalog must satisfy the backend's machine-derived emitted-key set.

    The registry is re-derived here (a drift check on the committed artifact),
    every emitted key the default locale translates must be published and
    renderable, and the gaps the generator reported must be exactly the gaps
    that actually exist — so a newly added backend key with no translation, or
    a message that needs a variable the backend does not send, cannot slip in
    unnoticed.
    """
    result = subprocess.run(
        [shutil.which("uv") or "uv", "run", str(ROOT / BACKEND_EXTRACTOR), "--check"],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    require(
        result.returncode == 0,
        f"the backend emitted-key artifact is stale: {result.stdout.strip()} {result.stderr.strip()}",
    )

    artifact = strict_json.strict_json_load_path(ROOT / BACKEND_KEYS)
    emitted = {entry["key"]: {v["name"] for v in entry["variables"]} for entry in artifact["keys"]}
    require(len(emitted) > 1000, f"the backend registry only lists {len(emitted)} keys")

    backend = manifest["backend"]
    require(
        backend["artifactSha256"] == sha256_hex(read_source_bytes(BACKEND_KEYS)),
        "the manifest records a different backend registry than the committed one",
    )
    require(
        backend["sourceSha256"] == artifact["source"]["sha256"],
        "the manifest records a different backend source digest than the registry",
    )
    require(backend["emittedKeys"] == len(emitted), "the manifest miscounts the backend's emitted keys")

    default_locale = manifest["defaultLocale"]
    entries: dict[str, dict] = {}
    for locale_entry in manifest["locales"]:
        if locale_entry["locale"] != default_locale:
            continue
        for descriptor in locale_entry["chunks"]:
            chunk = strict_json.strict_json_loads(
                files[descriptor["path"][len(manifest["basePath"]) + 1 :]], source=descriptor["path"]
            )
            entries.update(chunk["entries"])

    translated = {key for key in emitted if key in entries}
    untranslated = sorted(key for key in emitted if key not in entries)
    require(
        backend["requiredKeys"] == len({*manifest["provenance"]["fixtureKeys"], *translated}),
        "the manifest miscounts its required keys",
    )
    require(
        backend["untranslatedKeys"] == untranslated,
        "the manifest's untranslated-key list does not match the catalog",
    )

    for key in sorted(translated):
        entry = entries[key]
        require(
            entry["form"] != "unsupported",
            f"backend-emitted key {key} is unsupported ({entry.get('reason')}): it cannot be optional",
        )

    gaps = []
    for key in sorted(translated):
        entry = entries[key]
        needed = {
            variable["name"]
            for variable in entry["variables"]
            if variable["source"] == "named" and variable["role"] == "text"
        }
        missing = sorted(needed - emitted[key])
        if missing:
            gaps.append({"key": key, "missing": missing, "resolved": bool(emitted[key])})
    require(
        gaps == manifest["backend"]["variableGaps"],
        "the manifest's variable-gap report does not match the catalog "
        f"({len(gaps)} computed, {len(manifest['backend']['variableGaps'])} reported)",
    )


def validate_provenance(provenance_path: Path, manifest: dict, tracked: set[str]) -> None:
    record = strict_json.strict_json_load_path(provenance_path)
    require(
        sha256_hex(canonical_bytes(record)) == manifest["provenance"]["sha256"],
        "the manifest's provenance digest does not match the generator's provenance record",
    )
    require(
        record["output"]["sha256"] == manifest["provenance"]["outputSha256"],
        "the manifest's output digest does not match the generator's record",
    )
    require(
        manifest["catalogRevision"] == f"1.{manifest['provenance']['sha256'][:32]}",
        "the revision is not derived from the provenance digest",
    )
    provenance = record["provenance"]
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

    require(
        provenance["backend"]["sha256"] == manifest["backend"]["artifactSha256"],
        "the provenance record and the manifest disagree about the backend registry",
    )
    require(
        len(provenance["lockfile"]) == 1
        and provenance["lockfile"][0]["path"] == "frontend/package-lock.json",
        "the resolved dependency closure is not part of the catalog provenance",
    )
    require(
        provenance["toolchain"]["node"] == str(sys.version_info[0] * 0 + int(provenance["toolchain"]["node"])),
        "the provenance record does not pin a Node major version",
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
        (provenance["lockfile"], ""),
        ([{"path": provenance["backend"]["path"], "sha256": provenance["backend"]["sha256"]}], ""),
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
    run_generator(["--out", str(out)], frontend=clone / "frontend")
    require((out / "manifest.json").is_file(), "the clean-clone build wrote no manifest")

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
    """Every gate must have teeth: each mutation below has to stop the build."""
    original = {}

    def remember(path: Path) -> Path:
        original[path] = path.read_bytes()
        return path

    def restore() -> None:
        for path, content in original.items():
            path.write_bytes(content)
        original.clear()

    base = clone / "frontend" / "src" / "locales" / "en" / "base.json"

    # 1. A duplicate key inside one raw locale file (JSON.parse would silently
    #    keep the last one).
    remember(base)
    text = base.read_text(encoding="utf-8")
    injected = text.replace("{", '{\n  "setup": "shadowed",', 1)
    base.write_text(injected, encoding="utf-8")
    result = run_generator(["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False)
    restore()
    require(
        result.returncode != 0 and "duplicate key" in result.stderr,
        f"a duplicate locale key did not fail generation:\n{result.stdout}\n{result.stderr}",
    )

    # 2. Markup a native client cannot render, on a key the backend emits.
    emitted_key = manifest["backend"]["untranslatedKeys"]
    gathering = clone / "frontend" / "src" / "locales" / "en" / "nightOfTheZealot" / "scenarios" / "theGathering.json"
    remember(gathering)
    messages = strict_json.strict_json_load_path(gathering)
    messages["setup"]["gatherSets"] = '<a href="https://example.test">gather</a>'
    gathering.write_text(json.dumps(messages, ensure_ascii=False, indent=2), encoding="utf-8")
    result = run_generator(["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False)
    restore()
    require(
        result.returncode != 0 and "is unsupported" in result.stderr,
        f"unsupported markup on a required key did not fail generation:\n{result.stderr}",
    )

    # 3. A link cycle must be refused rather than published.
    remember(base)
    messages = strict_json.strict_json_load_path(base)
    messages["setup"] = "@:continue"
    messages["continue"] = "@:setup"
    base.write_text(json.dumps(messages, ensure_ascii=False, indent=2), encoding="utf-8")
    result = run_generator(["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False)
    restore()
    require(
        result.returncode != 0 and ("link-cycle" in result.stderr or "is unsupported" in result.stderr),
        f"a link cycle did not fail generation:\n{result.stderr}",
    )

    # 4. A contract-required key removed entirely. `gatherSets` is declared in
    #    exactly one file, so deleting it really does remove the key.
    remember(gathering)
    messages = strict_json.strict_json_load_path(gathering)
    victim = "gatherSets"
    require(victim in messages["setup"], "the fail-closed probe needs a new victim key")
    del messages["setup"][victim]
    gathering.write_text(json.dumps(messages, ensure_ascii=False, indent=2), encoding="utf-8")
    result = run_generator(
        ["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False
    )
    restore()
    require(
        result.returncode != 0 and "is missing from en" in result.stderr,
        f"generation succeeded after removing required key {victim}:\n{result.stderr}",
    )

    # 5. A malformed locale source must fail too, rather than emitting a
    #    partial catalog.
    remember(base)
    base.write_text("{ not json", encoding="utf-8")
    result = run_generator(
        ["--out", str(clone / "out")], frontend=clone / "frontend", expect_success=False
    )
    restore()
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


def validate_revision_sensitivity(clone: Path, manifest: dict) -> None:
    """The revision must move when any input that can change the bytes moves:
    the locale content itself, the resolved dependency closure, and the
    generator's own output."""
    out = clone / "out-revision"
    baseline = strict_json.strict_json_loads(
        run_and_read(clone, out), source="clean clone"
    )["catalogRevision"]
    require(baseline == manifest["catalogRevision"], "the clean clone drifted from the worktree build")

    lockfile = clone / "frontend" / "package-lock.json"
    original_lock = lockfile.read_bytes()
    mutated = json.loads(original_lock)
    mutated["packages"][""]["devDependencies"]["parse5"] = "^8.0.2"
    lockfile.write_text(json.dumps(mutated, indent=2) + "\n", encoding="utf-8")
    lock_revision = strict_json.strict_json_loads(run_and_read(clone, out), source="lock mutation")[
        "catalogRevision"
    ]
    lockfile.write_bytes(original_lock)
    require(
        lock_revision != baseline,
        "changing the resolved dependency closure did not change the catalog revision",
    )

    base = clone / "frontend" / "src" / "locales" / "en" / "base.json"
    original_messages = base.read_bytes()
    messages = strict_json.strict_json_load_path(base)
    key = next(key for key, value in messages.items() if isinstance(value, str))
    messages[key] = f"{messages[key]} (revision probe)"
    base.write_text(json.dumps(messages, ensure_ascii=False, indent=2), encoding="utf-8")
    content_revision = strict_json.strict_json_loads(
        run_and_read(clone, out), source="content mutation"
    )["catalogRevision"]
    base.write_bytes(original_messages)
    require(
        content_revision != baseline,
        "changing published content did not change the catalog revision",
    )

    require(
        strict_json.strict_json_loads(run_and_read(clone, out), source="restored")["catalogRevision"]
        == baseline,
        "restoring the sources did not restore the revision",
    )
    shutil.rmtree(out, ignore_errors=True)


def run_and_read(clone: Path, out: Path) -> bytes:
    run_generator(["--out", str(out)], frontend=clone / "frontend")
    return (out / "manifest.json").read_bytes()


def validate_deployment_wiring(manifest: dict) -> None:
    base = manifest["basePath"]
    nginx = (ROOT / "prod.nginxconf").read_text(encoding="utf-8")
    for prefix in ("c", "r"):
        require(
            re.search(
                rf'"~\^{re.escape(base)}/{prefix}/"\s+"public,\s*max-age=31536000,\s*immutable"\s*;?',
                nginx,
            )
            is not None,
            f"prod.nginxconf does not cache {base}/{prefix}/ immutably",
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
        "add_header Cache-Control $catalog_cache_control always;" in block,
        "the catalog location does not use the status-aware cache policy",
    )
    # The status map is what keeps an immutable year off a 404/405/416.
    require(
        re.search(r"map\s+\$status\s+\$catalog_cache_control\s*\{", nginx) is not None,
        "prod.nginxconf does not select the catalog cache policy by response status",
    )
    status_map = nginx.split("map $status $catalog_cache_control {", 1)[1].split("}", 1)[0]
    require(
        re.search(r'default\s+"no-store"', status_map) is not None,
        "prod.nginxconf does not make catalog error responses non-storable",
    )
    for status in ("200", "206", "304"):
        require(
            re.search(rf"^\s*{status}\s+\$cache_control;", status_map, re.MULTILINE) is not None,
            f"prod.nginxconf does not cache {status} catalog responses by path policy",
        )
    require(
        "error_page 416 = @locale_catalog_error;" in nginx,
        "prod.nginxconf does not re-header a 416, whose status nginx finalizes after add_header",
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
    declared = {**package.get("dependencies", {}), **package.get("devDependencies", {})}
    for dependency in ("parse5", "@intlify/message-compiler"):
        require(
            dependency in declared,
            f"{dependency} must be a declared dependency of the generator",
        )

    offline_nginx = (ROOT / "offline" / "scripts" / "05-package.sh").read_text(encoding="utf-8")
    require(
        f"location ^~ {base}/ {{" in offline_nginx,
        "the offline package does not publish the catalog on its own route",
    )
    for needle in (
        "map \\$status \\$catalog_cache_control",
        'default "no-store"',
        'add_header X-Content-Type-Options "nosniff" always;',
        "default_type application/json;",
    ):
        require(needle in offline_nginx, f"the offline nginx config is missing {needle!r}")

    offline_build = (ROOT / "offline" / "scripts" / "03-build-frontend.sh").read_text(encoding="utf-8")
    for needle in ("scripts/locale-catalog", "homebrew", "i18n-emitted-keys.json", "node --version", "verify-dist.mjs"):
        require(
            needle in offline_build,
            f"the offline frontend build does not account for {needle!r} in its cache key or verification",
        )

    offline_deps = (ROOT / "offline" / "scripts" / "01-check-project-deps.sh").read_text(encoding="utf-8")
    mise = (ROOT / "mise.toml").read_text(encoding="utf-8")
    node_major = strict_json.strict_json_load_path(FRONTEND / "package.json")["engines"]["node"]
    major = re.search(r"(\d+)", node_major).group(1)
    require(f'node = "{major}.' in mise, f"mise.toml does not pin Node {major}.x")
    require(f'node_ver="{major}.' in offline_deps, f"the offline build does not install Node {major}.x")
    require(
        f"FROM node:{major}." in (ROOT / "Dockerfile").read_text(encoding="utf-8"),
        f"the container build does not use Node {major}.x",
    )

    dockerfile = (ROOT / "Dockerfile").read_text(encoding="utf-8")
    require(
        re.search(
            r"^\s*COPY\s+\.?/?contracts\s+/opt/arkham/src/contracts\s*$",
            dockerfile,
            re.MULTILINE,
        )
        is not None,
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

    # The deployment runs the generator from frontend/ (npm prebuild); a
    # developer or script may run it from the repository root. Both must
    # produce the same bytes.
    from_repo_root = WORK_ROOT / "from-repo-root"
    run_generator(["--out", str(from_repo_root)], cwd=ROOT)
    require(
        files == read_generated(from_repo_root),
        "generation depends on the working directory it is invoked from",
    )

    manifest = validate_catalog(files, schemas)
    validate_required_keys(files, manifest)
    validate_backend_requirements(files, manifest)
    validate_provenance(provenance_path, manifest, tracked)
    validate_stale_detection(first, manifest)
    clone = validate_clean_clone(tracked, manifest, files)
    validate_revision_sensitivity(clone, manifest)
    validate_fail_closed(clone, manifest)
    validate_deployment_wiring(manifest)

    shutil.rmtree(WORK_ROOT, ignore_errors=True)

    print(
        f"locale-catalog: revision {manifest['catalogRevision']} verified — "
        f"{manifest['totals']['locales']} locales, {manifest['totals']['chunks']} files, "
        f"{manifest['totals']['keys']} keys "
        f"({manifest['totals']['unsupportedKeys']} explicitly unsupported), "
        f"{manifest['backend']['requiredKeys']} backend-required "
        f"({len(manifest['backend']['untranslatedKeys'])} emitted keys untranslated, "
        f"{len(manifest['backend']['variableGaps'])} variable gaps), "
        f"{manifest['totals']['bytes'] / 1024 / 1024:.2f} MB"
    )


if __name__ == "__main__":
    sys.exit(main())
