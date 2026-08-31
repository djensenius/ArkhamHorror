#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
# ]
# ///
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
    locale-catalog-generator.json        the generator identity it was built by
    locale-catalog-schemas.json          the schema set it was rendered against
    locale-catalog-chunk-<sha256>.json   the rendered chunks, content-addressed
    locale-catalog-manifest.json         the v1 manifest, derived from the above

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
import sys
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for

import strict_json

ROOT = Path(__file__).resolve().parents[1]
FIXTURE_DIR = ROOT / "contracts" / "fixtures"
# `strict_json` deliberately refuses any governed path below a subdirectory of
# contracts/fixtures/, so the synthetic catalog's authority files are flat and
# share one prefix instead of living in a folder.
FIXTURE_PREFIX = "locale-catalog-"
CONTRACT_MANIFEST = "contracts/manifest.json"
CATALOG_SCHEMA_DIR = ROOT / "frontend" / "schemas" / "locale-catalog" / "v1"

BASE_PATH = "/locale-catalog"
CHUNK_PATH_PREFIX = f"{BASE_PATH}/c/"
GENERATOR_NAME = "arkham-locale-catalog"


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
        "artifactVersion": "1.0.0",
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


def build_generator_descriptor() -> bytes:
    return canonical_bytes(
        {
            "name": GENERATOR_NAME,
            "version": "1.0.0",
            "description": (
                "Identity of the generator this synthetic catalog was built by. "
                "scripts/build-locale-catalog-fixture.py is that generator; this file is the "
                "committed authority its provenance.generatorSha256 is taken over."
            ),
        }
    )


def build_schema_descriptor() -> bytes:
    return canonical_bytes(
        {
            "schemaVersion": "1.0.0",
            "documents": [
                "frontend/schemas/locale-catalog/v1/manifest.schema.json",
                "frontend/schemas/locale-catalog/v1/chunk.schema.json",
            ],
            "description": (
                "The schema set this synthetic catalog is rendered against, named rather than "
                "hashed: the v1 schemas are frontend-owned, and hashing their bytes here would "
                "let an unrelated frontend edit churn a governed contract fixture."
            ),
        }
    )


def build_chunks() -> tuple[list[dict], dict[str, bytes]]:
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
                "schemaVersion": "1.0.0",
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
                    "path": f"{CHUNK_PATH_PREFIX}{digest}.json",
                    "bytes": len(data),
                    "sha256": digest,
                    "keys": len(entries),
                    "unsupportedKeys": sum(
                        1 for entry in entries.values() if entry["form"] == "unsupported"
                    ),
                }
            )
    return chunk_records, chunk_files


def build_all() -> dict[str, bytes]:
    contract_manifest = strict_json.strict_json_loads(
        strict_json.read_governed_worktree_bytes(ROOT, CONTRACT_MANIFEST),
        source=CONTRACT_MANIFEST,
    )
    require(isinstance(contract_manifest, dict), "contracts/manifest.json is not an object")
    contract_revision = contract_manifest["schemaRevision"]

    files: dict[str, bytes] = {}
    files.update(build_sources())
    files[f"{FIXTURE_PREFIX}backend-registry.json"] = build_backend_registry()
    files[f"{FIXTURE_PREFIX}generator.json"] = build_generator_descriptor()
    files[f"{FIXTURE_PREFIX}schemas.json"] = build_schema_descriptor()

    chunk_records, chunk_files = build_chunks()
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
    generator_sha256 = fileset_digest(
        [(f"{FIXTURE_PREFIX}generator.json", files[f"{FIXTURE_PREFIX}generator.json"])]
    )
    schemas_sha256 = fileset_digest(
        [(f"{FIXTURE_PREFIX}schemas.json", files[f"{FIXTURE_PREFIX}schemas.json"])]
    )
    artifact_sha256 = sha256_hex(files[f"{FIXTURE_PREFIX}backend-registry.json"])

    fixture_keys = sorted(registry["requiredKeys"])

    # The provenance digest binds the rendered output to every input that
    # produced it, exactly like the real generator's two-phase derivation.
    provenance_input = "\n".join(
        [
            f"outputSha256={output_sha256}",
            f"localeSourcesSha256={locale_sources_sha256}",
            f"schemasSha256={schemas_sha256}",
            f"generatorSha256={generator_sha256}",
            f"backendArtifactSha256={artifact_sha256}",
            f"backendSourceSha256={registry['source']['sha256']}",
            f"contractRevision={contract_revision}",
            f"generator={GENERATOR_NAME}@1.0.0",
            "fixtureKeys=" + ",".join(fixture_keys),
        ]
    )
    provenance_sha256 = sha256_hex(provenance_input.encode("utf-8"))
    catalog_revision = f"1.{provenance_sha256[:32]}"

    manifest = {
        "schemaVersion": "1.0.0",
        "catalogRevision": catalog_revision,
        "basePath": BASE_PATH,
        "manifestPath": f"{BASE_PATH}/manifest.json",
        "revisionManifestPath": f"{BASE_PATH}/r/{catalog_revision}/manifest.json",
        "chunkPathPrefix": CHUNK_PATH_PREFIX,
        "digestAlgorithm": "sha256",
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
            "generator": {"name": GENERATOR_NAME, "version": "1.0.0"},
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
        validator_class = validator_for(schema)
        validator_class.check_schema(schema)
        validator = validator_class(schema, format_checker=FormatChecker())
        for path, data in instances:
            errors = list(validator.iter_errors(json.loads(data)))
            require(
                not errors,
                f"{path} does not validate against the published v1 schema: "
                f"{[error.message for error in errors]}",
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
            {**manifest["provenance"], "generator": {"name": GENERATOR_NAME, "version": "9.9.9"}},
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
