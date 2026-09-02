#!/usr/bin/env python3
"""Build (or check) the synthetic locale-catalog fixture set.

`contracts/fixtures/capabilities-locale-catalog.json` has to show a real,
verifiable catalog pointer, but it must not carry a *production* revision or
digest: the real catalog is regenerated from `frontend/src/locales/**` on every
content change, so pinning its output would force a contract revision bump
every time a translator fixed a typo, while a copied constant would have the
fixture self-attest a catalog no artifact in this repository has.

So the pointer is derived from a synthetic catalog whose every claim is backed
by a committed, narrative-free authority file under
`contracts/fixtures/locale-catalog-*`:

    locale-catalog-source-<locale>.json  miniature locale sources
    locale-catalog-backend-registry.json miniature emitted-key registry
    locale-catalog-chunk-<sha256>.json   the rendered chunks, content-addressed
    locale-catalog-manifest.json         the v1 manifest, derived from the above

`generatorSha256` and `schemasSha256` are the digests of the *real* inputs --
every repository-local module this generator executes (its own bytes plus the
`scripts/` import closure, `strict_json` included) and the published v1 schemas
-- rather than of a committed description of them, so editing any of them
really does move the catalog revision, as it does in production.

Every digest, count, path, `outputSha256` and provenance value in that manifest
is *computed here* from those bytes -- nothing is asserted by hand. `--check`
recomputes the whole set and requires the committed bytes to match exactly, so
a single edited byte anywhere in the chain fails, and `--self-test` proves that
by mutating each claimed field in turn.

The chain deliberately contains no production input, so publishing new catalog
content never touches it. It does depend on `contracts/manifest.json`'s
`schemaRevision` (the real generator records the same binding), so a contract
revision bump is regenerated with `mise run contracts:catalog-fixture`.
"""

import argparse
import copy
import hashlib
import json
from pathlib import Path

import json_schema_subset
import locale_catalog_python_boundary
import strict_json

ROOT = Path(__file__).resolve().parents[1]
FIXTURE_DIR = ROOT / "contracts" / "fixtures"
# `strict_json` deliberately refuses any governed path below a subdirectory of
# contracts/fixtures/, so the synthetic catalog's authority files are flat and
# share one prefix instead of living in a folder.
FIXTURE_PREFIX = "locale-catalog-"
CONTRACT_MANIFEST = "contracts/manifest.json"
CATALOG_SCHEMA_DIR = ROOT / "frontend" / "schemas" / "locale-catalog" / "v1"

# The generator's own sources and the schema set it renders against, hashed as
# themselves rather than described by a committed stand-in: `generatorSha256`
# is the digest of every repository-local module this script executes (see
# `generator_sources()`), `schemasSha256` the digest of the published v1
# schemas. Editing either changes the catalog revision, which is exactly what
# the production generator's provenance means -- and is why a change to this
# script is followed by `mise run contracts:catalog-fixture-write`.
GENERATOR_ENTRY = "scripts/build-locale-catalog-fixture.py"
RUNTIME_PROFILE = "scripts/locale_catalog_python_runtime.json"
GENERATOR_EXECUTION_SOURCES = (
    "mise.toml",
    "pyproject.toml",
    "uv.lock",
    "scripts/run-locale-catalog-python.sh",
    RUNTIME_PROFILE,
)
SCHEMA_SOURCES = (
    "frontend/schemas/locale-catalog/v1/manifest.schema.json",
    "frontend/schemas/locale-catalog/v1/chunk.schema.json",
)

# The one value this script contributes itself. Everything else -- the schema
# version, the published routes, the digest algorithm, the generator name -- is
# read out of the v1 schemas' own `const` declarations by `catalog_constants()`,
# so no literal is spelled twice.
GENERATOR_VERSION = "1.0.0"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog fixture: {message}")


def canonical_bytes(value: object) -> bytes:
    """One spelling for every JSON artifact this script writes, so a rebuilt
    fixture is byte-comparable with the committed one."""
    return (json.dumps(value, indent=2, ensure_ascii=False, sort_keys=False) + "\n").encode("utf-8")


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def fileset_digest(entries: list[tuple[str, bytes]]) -> str:
    """Digest of a *named* set of files: each entry contributes its path and
    its content digest, in sorted path order, so renaming a file or reordering
    the set changes the result just as much as editing one does.
    """
    accumulator = hashlib.sha256()
    for path, data in sorted(entries):
        accumulator.update(path.encode("utf-8"))
        accumulator.update(b"\0")
        accumulator.update(sha256_hex(data).encode("ascii"))
        accumulator.update(b"\n")
    return accumulator.hexdigest()


def runtime_identity() -> dict[str, object]:
    profile = strict_json.strict_json_load_path(ROOT / RUNTIME_PROFILE)
    require(
        isinstance(profile, dict)
        and set(profile) == {"implementation", "version", "cacheTag", "stdlibIdentity"}
        and all(isinstance(value, str) for value in profile.values()),
        f"{RUNTIME_PROFILE} is not the complete pinned runtime identity",
    )
    return {
        "python": profile,
        "tools": {
            "miseAction": "2026.8.14",
            "uv": "0.12.6",
        },
    }


# ---------------------------------------------------------------------------
# The synthetic catalog's inputs. Identifier-shaped values only: this fixture
# carries no prose, and nothing here is derived from frontend/src/locales/**.
# ---------------------------------------------------------------------------

LOCALES = ("de", "en", "pt-BR")
DEFAULT_LOCALE = "en"

SOURCE_MESSAGES: dict[str, dict[str, dict[str, str]]] = {
    "en": {
        "core": {"continue": "core.continue.en", "setup": "core.setup.en"},
        "nightOfTheZealot": {
            "nightOfTheZealot.theGathering.setup.gatherSets": "notz.gatherSets.en",
            "nightOfTheZealot.theGathering.setup.placeLocations": "notz.placeLocations.en",
        },
    },
    "de": {"core": {"continue": "core.continue.de"}},
    "pt-BR": {"core": {"continue": "core.continue.pt-BR"}},
}

# A key the registry says the backend emits that no locale translates, so the
# catalog publishes a real, non-empty `backend.untranslatedKeys` gap.
UNTRANSLATED_KEY = "nightOfTheZealot.theGathering.setup.setOutOfPlay"

# A key published as explicitly unavailable, so `unsupportedKeys` is non-zero.
UNSUPPORTED_KEY = "shuffleRemainder"

LANGUAGE_RESOLUTION = (
    ("de", "de"),
    ("en", "en"),
    ("pt", "pt-BR"),
    ("pt-BR", "pt-BR"),
)


def build_sources() -> dict[str, bytes]:
    files: dict[str, bytes] = {}
    for locale, packs in SOURCE_MESSAGES.items():
        merged: dict[str, str] = {}
        for pack_messages in packs.values():
            merged.update(pack_messages)
        files[f"{FIXTURE_PREFIX}source-{locale}.json"] = canonical_bytes(dict(sorted(merged.items())))
    return files


def build_backend_registry() -> bytes:
    emitted = sorted(
        {key for packs in SOURCE_MESSAGES["en"].values() for key in packs}
        | {UNTRANSLATED_KEY, UNSUPPORTED_KEY}
    )
    registry = {
        "artifactVersion": GENERATOR_VERSION,
        "source": {
            "files": len(SOURCE_MESSAGES),
            "sha256": fileset_digest(list(build_sources().items())),
        },
        "dynamicSites": 0,
        "emittedKeys": emitted,
        "requiredKeys": emitted,
        "variableGaps": [],
        "unknownVariableTypes": [],
    }
    return canonical_bytes(registry)


def scan_generator_sources(read_source=locale_catalog_python_boundary.read_source) -> tuple[str, ...]:
    """Every repository-local module this generator actually executes.

    Hashing only the entry module would leave behavior-bearing source out of
    the provenance: the derivation runs `strict_json`'s readers and parsers too,
    and a future helper would join them silently. So the set is the import
    closure of the entry module, resolved statically against the repository and
    confined to a closed implementation boundary -- the `scripts/` tree, plain
    files only, no symlinks, no relative imports, no packages outside it, no
    dynamic import or `sys.path` machinery, and no import the closure cannot
    classify as generator-local, standard-library, or a pinned dependency.

    `read_source` is injectable so the self-tests can put each bypass into the
    real entry module's own AST and require a refusal.
    """
    return locale_catalog_python_boundary.scan_python_closure(
        GENERATOR_ENTRY, source_reader=read_source
    )


def generator_sources() -> tuple[str, ...]:
    try:
        return tuple(
            sorted(
                set(locale_catalog_python_boundary.all_executable_sources())
                | set(GENERATOR_EXECUTION_SOURCES)
            )
        )
    except locale_catalog_python_boundary.SourceBoundaryError as error:
        raise SystemExit(f"locale-catalog fixture: {error}") from None


def read_repository_files(relative_paths: tuple[str, ...]) -> list[tuple[str, bytes]]:
    """Read real repository files as provenance inputs, by their repo-relative
    path, so a rename counts as a change just as much as an edit does.
    """
    entries: list[tuple[str, bytes]] = []
    for relative_path in relative_paths:
        path = ROOT / relative_path
        require(path.is_file(), f"missing provenance input {relative_path}")
        entries.append((relative_path, path.read_bytes()))
    return entries


def catalog_constants() -> dict[str, str]:
    """Every fixed value the v1 schemas already pin, read from their own `const`
    declarations instead of being re-typed here. A schema that moved the
    published route or the digest algorithm would move this fixture with it.
    """
    manifest_schema = strict_json.strict_json_load_path(CATALOG_SCHEMA_DIR / "manifest.schema.json")
    chunk_schema = strict_json.strict_json_load_path(CATALOG_SCHEMA_DIR / "chunk.schema.json")

    def const(schema: dict, *pointer: str) -> str:
        node: object = schema
        for segment in pointer:
            require(
                isinstance(node, dict) and segment in node,
                f"the v1 schema has no {'/'.join(pointer)} to read a constant from",
            )
            node = node[segment]
        require(
            isinstance(node, dict) and isinstance(node.get("const"), str),
            f"{'/'.join(pointer)} is not a string const in the v1 schema",
        )
        return node["const"]

    schema_version = const(manifest_schema, "properties", "schemaVersion")
    chunk_schema_version = const(chunk_schema, "$defs", "schemaVersion")
    require(
        schema_version == chunk_schema_version,
        "the v1 manifest and chunk schemas disagree about schemaVersion "
        f"({schema_version!r} vs {chunk_schema_version!r})",
    )
    return {
        "schemaVersion": schema_version,
        "basePath": const(manifest_schema, "properties", "basePath"),
        "manifestPath": const(manifest_schema, "properties", "manifestPath"),
        "chunkPathPrefix": const(manifest_schema, "properties", "chunkPathPrefix"),
        "digestAlgorithm": const(manifest_schema, "properties", "digestAlgorithm"),
        "generatorName": const(
            manifest_schema, "properties", "provenance", "properties", "generator", "properties", "name"
        ),
    }


def build_chunks(constants: dict[str, str]) -> tuple[list[dict], dict[str, bytes]]:
    """Render one chunk per (locale, pack), content-addressed by its own bytes."""
    chunk_records: list[dict] = []
    chunk_files: dict[str, bytes] = {}

    for locale in LOCALES:
        for pack, messages in sorted(SOURCE_MESSAGES[locale].items()):
            entries: dict[str, dict] = {}
            for key, value in sorted(messages.items()):
                entries[key] = {
                    "form": "message",
                    "nodes": [{"type": "text", "value": value}],
                    "variables": [],
                }
            if locale == DEFAULT_LOCALE and pack == "core":
                entries[UNSUPPORTED_KEY] = {
                    "form": "unsupported",
                    "reason": "unusable-variable-type",
                    "detail": "synthetic fixture: published as unavailable on purpose",
                }
            chunk = {
                "schemaVersion": constants["schemaVersion"],
                "locale": locale,
                "fallback": None if locale == DEFAULT_LOCALE else DEFAULT_LOCALE,
                "pack": pack,
                "entries": dict(sorted(entries.items())),
            }
            data = canonical_bytes(chunk)
            digest = sha256_hex(data)
            chunk_files[f"{FIXTURE_PREFIX}chunk-{digest}.json"] = data
            chunk_records.append(
                {
                    "locale": locale,
                    "pack": pack,
                    "path": f"{constants['chunkPathPrefix']}{digest}.json",
                    "bytes": len(data),
                    "sha256": digest,
                    "keys": len(entries),
                    "unsupportedKeys": sum(
                        1 for entry in entries.values() if entry["form"] == "unsupported"
                    ),
                }
            )
    return chunk_records, chunk_files


def derive_provenance_sha256(
    *,
    output_sha256: str,
    locale_sources_sha256: str,
    schemas_sha256: str,
    generator_sha256: str,
    artifact_sha256: str,
    backend_source_sha256: str,
    contract_revision: str,
    generator_name: str,
    generator_version: str,
    fixture_keys: list[str],
) -> str:
    """Bind the rendered output to every input that produced it, the way the
    production generator's two-phase derivation does. Kept pure so
    `run_provenance_input_self_tests` can prove each input is load-bearing.
    """
    return sha256_hex(
        "\n".join(
            [
                f"outputSha256={output_sha256}",
                f"localeSourcesSha256={locale_sources_sha256}",
                f"schemasSha256={schemas_sha256}",
                f"generatorSha256={generator_sha256}",
                f"backendArtifactSha256={artifact_sha256}",
                f"backendSourceSha256={backend_source_sha256}",
                f"contractRevision={contract_revision}",
                f"generator={generator_name}@{generator_version}",
                "fixtureKeys=" + ",".join(fixture_keys),
            ]
        ).encode("utf-8")
    )


def build_all() -> dict[str, bytes]:
    contract_manifest = strict_json.strict_json_loads(
        strict_json.read_governed_worktree_bytes(ROOT, CONTRACT_MANIFEST),
        source=CONTRACT_MANIFEST,
    )
    require(isinstance(contract_manifest, dict), "contracts/manifest.json is not an object")
    contract_revision = contract_manifest["schemaRevision"]

    constants = catalog_constants()

    files: dict[str, bytes] = {}
    files.update(build_sources())
    files[f"{FIXTURE_PREFIX}backend-registry.json"] = build_backend_registry()

    chunk_records, chunk_files = build_chunks(constants)
    files.update(chunk_files)

    registry = json.loads(files[f"{FIXTURE_PREFIX}backend-registry.json"])
    translated = {
        key
        for record in chunk_records
        if record["locale"] == DEFAULT_LOCALE
        for key in json.loads(chunk_files[f"{FIXTURE_PREFIX}chunk-{record['sha256']}.json"])["entries"]
    }
    untranslated = sorted(key for key in registry["emittedKeys"] if key not in translated)

    locale_records = []
    for locale in LOCALES:
        chunks = [
            {key: record[key] for key in ("pack", "path", "bytes", "sha256", "keys", "unsupportedKeys")}
            for record in chunk_records
            if record["locale"] == locale
        ]
        locale_records.append(
            {
                "locale": locale,
                "fallback": None if locale == DEFAULT_LOCALE else DEFAULT_LOCALE,
                "keys": sum(chunk["keys"] for chunk in chunks),
                "bytes": sum(chunk["bytes"] for chunk in chunks),
                "chunks": chunks,
            }
        )

    source_entries = [(path, data) for path, data in files.items() if path.startswith(f"{FIXTURE_PREFIX}source-")]
    output_sha256 = fileset_digest(list(chunk_files.items()))
    locale_sources_sha256 = fileset_digest(source_entries)
    generator_sha256 = fileset_digest(read_repository_files(generator_sources()))
    schemas_sha256 = fileset_digest(read_repository_files(SCHEMA_SOURCES))
    artifact_sha256 = sha256_hex(files[f"{FIXTURE_PREFIX}backend-registry.json"])

    fixture_keys = sorted(registry["requiredKeys"])

    provenance_sha256 = derive_provenance_sha256(
        output_sha256=output_sha256,
        locale_sources_sha256=locale_sources_sha256,
        schemas_sha256=schemas_sha256,
        generator_sha256=generator_sha256,
        artifact_sha256=artifact_sha256,
        backend_source_sha256=registry["source"]["sha256"],
        contract_revision=contract_revision,
        generator_name=constants["generatorName"],
        generator_version=GENERATOR_VERSION,
        fixture_keys=fixture_keys,
    )
    catalog_revision = f"1.{provenance_sha256[:32]}"

    manifest = {
        "schemaVersion": constants["schemaVersion"],
        "catalogRevision": catalog_revision,
        "basePath": constants["basePath"],
        "manifestPath": constants["manifestPath"],
        "revisionManifestPath": f"{constants['basePath']}/r/{catalog_revision}/manifest.json",
        "chunkPathPrefix": constants["chunkPathPrefix"],
        "digestAlgorithm": constants["digestAlgorithm"],
        "defaultLocale": DEFAULT_LOCALE,
        "languageResolution": [{"tag": tag, "locale": locale} for tag, locale in LANGUAGE_RESOLUTION],
        "locales": locale_records,
        "totals": {
            "locales": len(locale_records),
            "chunks": sum(len(record["chunks"]) for record in locale_records),
            "bytes": sum(record["bytes"] for record in locale_records),
            "keys": sum(record["keys"] for record in locale_records),
            "unsupportedKeys": sum(
                chunk["unsupportedKeys"] for record in locale_records for chunk in record["chunks"]
            ),
        },
        "backend": {
            "artifactPath": f"contracts/fixtures/{FIXTURE_PREFIX}backend-registry.json",
            "artifactSha256": artifact_sha256,
            "sourceSha256": registry["source"]["sha256"],
            "emittedKeys": len(registry["emittedKeys"]),
            "requiredKeys": len(registry["requiredKeys"]),
            "untranslatedKeys": untranslated,
            "variableGaps": registry["variableGaps"],
            "dynamicSites": registry["dynamicSites"],
            "unknownVariableTypes": registry["unknownVariableTypes"],
        },
        "provenance": {
            "sha256": provenance_sha256,
            "outputSha256": output_sha256,
            "generator": {"name": constants["generatorName"], "version": GENERATOR_VERSION},
            "contractRevision": contract_revision,
            "fixtureKeys": fixture_keys,
            "localeSourceFiles": len(source_entries),
            "localeSourcesSha256": locale_sources_sha256,
            "schemasSha256": schemas_sha256,
            "generatorSha256": generator_sha256,
        },
    }
    files[f"{FIXTURE_PREFIX}manifest.json"] = canonical_bytes(manifest)
    return files


def validate_against_published_schemas(files: dict[str, bytes]) -> None:
    manifest_schema = strict_json.strict_json_load_path(CATALOG_SCHEMA_DIR / "manifest.schema.json")
    chunk_schema = strict_json.strict_json_load_path(CATALOG_SCHEMA_DIR / "chunk.schema.json")

    for schema, instances in (
        (manifest_schema, [(f"{FIXTURE_PREFIX}manifest.json", files[f"{FIXTURE_PREFIX}manifest.json"])]),
        (
            chunk_schema,
            [
                (path, data)
                for path, data in files.items()
                if path.startswith(f"{FIXTURE_PREFIX}chunk-")
            ],
        ),
    ):
        json_schema_subset.check_schema(schema, source="published locale-catalog schema")
        for path, data in instances:
            errors = json_schema_subset.iter_errors(schema, json.loads(data), source=path)
            require(
                not errors,
                f"{path} does not validate against the published v1 schema: "
                f"{errors}",
            )


def committed_files() -> dict[str, bytes]:
    require(FIXTURE_DIR.is_dir(), f"missing fixture directory {FIXTURE_DIR}")
    found: dict[str, bytes] = {}
    for path in sorted(FIXTURE_DIR.glob(f"{FIXTURE_PREFIX}*.json")):
        name = path.name
        found[name] = strict_json.read_governed_worktree_bytes(ROOT, f"contracts/fixtures/{name}")
    return found


def check_failure(files: dict[str, bytes], found: dict[str, bytes]) -> str | None:
    """Compare a committed set against the derived one, returning a failure
    message rather than raising, so `run_self_tests` can drive this exact
    comparison over mutated inputs and prove it bites.
    """
    expected_names = set(files)
    found_names = set(found)
    if expected_names != found_names:
        return (
            "the committed synthetic catalog fixture set does not match the derived one; "
            f"missing {sorted(expected_names - found_names)}, "
            f"unexpected {sorted(found_names - expected_names)}"
        )
    for name in sorted(expected_names):
        if found[name] != files[name]:
            return (
                f"contracts/fixtures/{name} is not what its inputs derive; every digest, count "
                "and path in this fixture set is computed, never asserted"
            )
    return None


def check(files: dict[str, bytes]) -> None:
    failure = check_failure(files, committed_files())
    require(failure is None, f"{failure} -- run `mise run contracts:catalog-fixture`")


def write(files: dict[str, bytes]) -> None:
    for path in sorted(FIXTURE_DIR.glob(f"{FIXTURE_PREFIX}*.json")):
        path.unlink()
    for name, data in sorted(files.items()):
        (FIXTURE_DIR / name).write_bytes(data)


def run_self_tests(files: dict[str, bytes]) -> None:
    """Prove the derivation is load-bearing by running the *real* comparison
    (`check_failure`) against mutated committed sets: every claimed field of
    the manifest, and every byte of every authority file, must make it fail.
    """
    committed = committed_files()
    require(
        check_failure(files, committed) is None,
        "self-test precondition: the committed fixture set must match the derived one before "
        "mutations can prove anything",
    )

    manifest_name = f"{FIXTURE_PREFIX}manifest.json"
    manifest = json.loads(files[manifest_name])

    field_mutations: list[tuple[str, object]] = [
        ("catalogRevision", "1." + "0" * 32),
        ("revisionManifestPath", "/locale-catalog/r/1." + "0" * 32 + "/manifest.json"),
        ("schemaVersion", "1.0.1"),
        ("defaultLocale", "de"),
        ("manifestPath", "/locale-catalog/index.json"),
        ("chunkPathPrefix", "/locale-catalog/chunk/"),
        ("languageResolution", manifest["languageResolution"][:1]),
        ("locales", manifest["locales"][:1]),
        ("totals", {**manifest["totals"], "keys": manifest["totals"]["keys"] + 1}),
        ("totals", {**manifest["totals"], "chunks": manifest["totals"]["chunks"] + 1}),
        ("totals", {**manifest["totals"], "bytes": manifest["totals"]["bytes"] + 1}),
        ("totals", {**manifest["totals"], "unsupportedKeys": 0}),
        ("backend", {**manifest["backend"], "artifactSha256": "0" * 64}),
        ("backend", {**manifest["backend"], "sourceSha256": "0" * 64}),
        ("backend", {**manifest["backend"], "emittedKeys": manifest["backend"]["emittedKeys"] + 1}),
        ("backend", {**manifest["backend"], "requiredKeys": manifest["backend"]["requiredKeys"] + 1}),
        ("backend", {**manifest["backend"], "untranslatedKeys": []}),
        ("backend", {**manifest["backend"], "dynamicSites": 1}),
        ("backend", {**manifest["backend"], "artifactPath": "contracts/fixtures/other.json"}),
        ("provenance", {**manifest["provenance"], "sha256": "0" * 64}),
        ("provenance", {**manifest["provenance"], "outputSha256": "0" * 64}),
        ("provenance", {**manifest["provenance"], "localeSourcesSha256": "0" * 64}),
        ("provenance", {**manifest["provenance"], "schemasSha256": "0" * 64}),
        ("provenance", {**manifest["provenance"], "generatorSha256": "0" * 64}),
        ("provenance", {**manifest["provenance"], "localeSourceFiles": 99}),
        ("provenance", {**manifest["provenance"], "contractRevision": "0.0.1"}),
        ("provenance", {**manifest["provenance"], "fixtureKeys": ["setup"]}),
        (
            "provenance",
            {
                **manifest["provenance"],
                "generator": {**manifest["provenance"]["generator"], "version": "9.9.9"},
            },
        ),
    ]

    for field, value in field_mutations:
        mutated_manifest = copy.deepcopy(manifest)
        mutated_manifest[field] = value
        mutated = dict(committed)
        mutated[manifest_name] = canonical_bytes(mutated_manifest)
        require(
            check_failure(files, mutated) is not None,
            f"Self-test failure: a mutated manifest {field} ({value!r}) survived the derivation "
            "check, so that field is an unverified claim",
        )

    # A chunk record is nested, so mutate one in place too: its digest, size and
    # key count each have to be load-bearing.
    for chunk_field, chunk_value in (("sha256", "0" * 64), ("bytes", 1), ("keys", 99), ("path", "/x.json")):
        mutated_manifest = copy.deepcopy(manifest)
        mutated_manifest["locales"][0]["chunks"][0][chunk_field] = chunk_value
        mutated = dict(committed)
        mutated[manifest_name] = canonical_bytes(mutated_manifest)
        require(
            check_failure(files, mutated) is not None,
            f"Self-test failure: a mutated chunk {chunk_field} survived the derivation check",
        )

    # Every authority file: one appended byte must be caught, because the
    # manifest's digests and counts are taken over exactly these bytes.
    for name in sorted(files):
        mutated = dict(committed)
        mutated[name] = committed[name] + b" "
        require(
            check_failure(files, mutated) is not None,
            f"Self-test failure: an edited {name} survived the derivation check",
        )

    # A missing or extra authority must fail too, so the set itself is pinned.
    for name in sorted(files):
        mutated = {key: value for key, value in committed.items() if key != name}
        require(
            check_failure(files, mutated) is not None,
            f"Self-test failure: a deleted {name} survived the derivation check",
        )
    require(
        check_failure(files, {**committed, f"{FIXTURE_PREFIX}extra.json": b"{}\n"}) is not None,
        "Self-test failure: an unexpected extra authority file survived the derivation check",
    )

    run_provenance_input_self_tests(manifest)
    run_generator_boundary_self_tests()


def run_generator_boundary_self_tests() -> None:
    """Exercise aliases and indirect loader paths through the shared resolver."""
    entry_source = locale_catalog_python_boundary.read_source(GENERATOR_ENTRY)
    helper = "scripts/strict_json.py"
    helper_source = locale_catalog_python_boundary.read_source(helper)

    def reader_with(overrides: dict[str, bytes]):
        def read(relative_path: str) -> bytes:
            return overrides.get(relative_path, locale_catalog_python_boundary.read_source(relative_path))

        return read

    def require_refusal(label: str, overrides: dict[str, bytes]) -> None:
        try:
            scan_generator_sources(reader_with(overrides))
        except locale_catalog_python_boundary.SourceBoundaryError:
            return
        raise SystemExit(
            f"locale-catalog fixture: Self-test failure: {label} was accepted by the generator "
            "source closure, so the boundary is not fail-closed"
        )

    bypasses = {
        "an aliased sys.path mutation": b'import sys as runtime_sys\nruntime_sys.path.append("elsewhere")\n',
        "an imported sys.modules alias": b"from sys import modules\nmodules.clear()\n",
        "an aliased builtins exec": b"import builtins as runtime_builtins\nruntime_builtins.exec('VALUE = 1')\n",
        "an imported builtins exec": b"from builtins import exec as runtime_exec\nruntime_exec('VALUE = 1')\n",
        "an aliased builtins import": b"from builtins import __import__ as runtime_import\nruntime_import('os')\n",
        "an indirect getattr path access": b'import sys\ngetattr(sys, "path")\n',
        "a loader dunder access": b'__loader__.load_module("os")\n',
        "a zipimport loader": b'import zipimport\nzipimport.zipimporter("x").load_module("os")\n',
        "an importlib loader": b"import importlib.util\n",
        "a relative import": b"from . import strict_json\n",
        "an undeclared import": b"import totally_unknown_third_party\n",
    }
    for label, injected in bypasses.items():
        require_refusal(f"{label} in the entry module", {GENERATOR_ENTRY: injected + entry_source})
        require_refusal(f"{label} in a generator helper", {helper: injected + helper_source})

    require(
        GENERATOR_ENTRY in generator_sources()
        and "scripts/strict_json.py" in generator_sources()
        and "scripts/json_schema_subset.py" in generator_sources()
        and "scripts/locale_catalog_runtime.py" in generator_sources(),
        "Self-test failure: generator provenance omitted a declared executable source or runtime bootstrap",
    )


def run_provenance_input_self_tests(manifest: dict) -> None:
    """`generatorSha256` and `schemasSha256` are digests of real repository
    bytes, so prove they move: appending one byte to this script or to either
    published v1 schema, renaming an input, or changing only the generator
    version must each produce a different revision.
    """
    sources = generator_sources()
    generator_inputs = read_repository_files(sources)
    schema_inputs = read_repository_files(SCHEMA_SOURCES)
    constants = catalog_constants()
    provenance = manifest["provenance"]
    runtime = runtime_identity()

    require(
        provenance["generatorSha256"] == fileset_digest(generator_inputs),
        "Self-test failure: generatorSha256 is not the digest of this generator's own sources",
    )
    require(
        GENERATOR_ENTRY in sources and "scripts/strict_json.py" in sources,
        "Self-test failure: the generator source closure must contain this module and every local "
        f"helper it executes; got {sources}",
    )
    for source in sources:
        require(
            (ROOT / source).is_file(),
            f"Self-test failure: the generator source closure names a missing file {source}",
        )
    require(
        provenance["schemasSha256"] == fileset_digest(schema_inputs),
        "Self-test failure: schemasSha256 is not the digest of the published v1 schemas",
    )
    require(
        RUNTIME_PROFILE in sources and runtime["python"]["version"] == "3.14.7",
        "Self-test failure: generator provenance omitted the validated runtime identity",
    )

    baseline = derive_provenance_sha256(
        output_sha256=provenance["outputSha256"],
        locale_sources_sha256=provenance["localeSourcesSha256"],
        schemas_sha256=provenance["schemasSha256"],
        generator_sha256=provenance["generatorSha256"],
        artifact_sha256=manifest["backend"]["artifactSha256"],
        backend_source_sha256=manifest["backend"]["sourceSha256"],
        contract_revision=provenance["contractRevision"],
        generator_name=constants["generatorName"],
        generator_version=provenance["generator"]["version"],
        fixture_keys=provenance["fixtureKeys"],
    )
    require(
        baseline == provenance["sha256"],
        "Self-test failure: the committed provenance digest is not what its own inputs derive",
    )

    def perturbed(**overrides: object) -> str:
        arguments: dict[str, object] = {
            "output_sha256": provenance["outputSha256"],
            "locale_sources_sha256": provenance["localeSourcesSha256"],
            "schemas_sha256": provenance["schemasSha256"],
            "generator_sha256": provenance["generatorSha256"],
            "artifact_sha256": manifest["backend"]["artifactSha256"],
            "backend_source_sha256": manifest["backend"]["sourceSha256"],
            "contract_revision": provenance["contractRevision"],
            "generator_name": constants["generatorName"],
            "generator_version": provenance["generator"]["version"],
            "fixture_keys": provenance["fixtureKeys"],
        }
        arguments.update(overrides)
        return derive_provenance_sha256(**arguments)

    edited_generator = fileset_digest([(path, data + b"\n") for path, data in generator_inputs])
    edited_helper = fileset_digest(
        [
            (path, data + b"\n" if path != GENERATOR_ENTRY else data)
            for path, data in generator_inputs
        ]
    )
    added_local_import = fileset_digest(
        generator_inputs + [("scripts/new_helper.py", b"# a new local helper\n")]
    )
    dropped_helper = fileset_digest(
        [(path, data) for path, data in generator_inputs if path == GENERATOR_ENTRY]
    )
    renamed_helper = fileset_digest(
        [
            (path if path == GENERATOR_ENTRY else path + ".moved", data)
            for path, data in generator_inputs
        ]
    )
    edited_schemas = fileset_digest([(path, data + b"\n") for path, data in schema_inputs])
    renamed_schemas = fileset_digest(
        [(path + ".moved", data) for path, data in schema_inputs]
    )
    partial_schemas = fileset_digest(schema_inputs[:1])

    for label, digest in (
        ("an edited generator source", perturbed(generator_sha256=edited_generator)),
        ("an edited generator helper", perturbed(generator_sha256=edited_helper)),
        ("a newly imported local helper", perturbed(generator_sha256=added_local_import)),
        ("a dropped generator helper", perturbed(generator_sha256=dropped_helper)),
        ("a renamed generator helper", perturbed(generator_sha256=renamed_helper)),
        ("an edited v1 schema", perturbed(schemas_sha256=edited_schemas)),
        ("a renamed v1 schema", perturbed(schemas_sha256=renamed_schemas)),
        ("a dropped v1 schema", perturbed(schemas_sha256=partial_schemas)),
        ("a different generator version", perturbed(generator_version="9.9.9")),
        ("a different generator name", perturbed(generator_name="something-else")),
        ("a different contract revision", perturbed(contract_revision="0.0.1")),
    ):
        require(
            digest != baseline,
            f"Self-test failure: {label} left the provenance digest unchanged, so it is not "
            "actually bound into the catalog revision",
        )

    for label, digest in (
        ("an edited generator source", edited_generator),
        ("an edited generator helper", edited_helper),
        ("a newly imported local helper", added_local_import),
        ("a dropped generator helper", dropped_helper),
        ("a renamed generator helper", renamed_helper),
    ):
        require(
            digest != provenance["generatorSha256"],
            f"Self-test failure: {label} produced the same generator fileset digest",
        )
    require(
        edited_schemas != provenance["schemasSha256"]
        and renamed_schemas != provenance["schemasSha256"],
        "Self-test failure: a mutated schema input produced the same fileset digest",
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="verify without writing")
    arguments = parser.parse_args()

    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    files = build_all()
    validate_against_published_schemas(files)

    if arguments.check:
        check(files)
        run_self_tests(files)
        print(
            f"locale-catalog fixture: {len(files)} committed artifacts re-derived and verified "
            f"(revision {json.loads(files[f'{FIXTURE_PREFIX}manifest.json'])['catalogRevision']})."
        )
        return

    write(files)
    print(
        f"locale-catalog fixture: wrote {len(files)} artifacts "
        f"(revision {json.loads(files[f'{FIXTURE_PREFIX}manifest.json'])['catalogRevision']}) to "
        f"{FIXTURE_DIR.relative_to(ROOT)}/{FIXTURE_PREFIX}*."
    )


if __name__ == "__main__":
    main()
