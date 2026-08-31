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
import ast
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

# The generator's own sources and the schema set it renders against, hashed as
# themselves rather than described by a committed stand-in: `generatorSha256`
# is the digest of every repository-local module this script executes (see
# `generator_sources()`), `schemasSha256` the digest of the published v1
# schemas. Editing either changes the catalog revision, which is exactly what
# the production generator's provenance means -- and is why a change to this
# script is followed by `mise run contracts:catalog-fixture-write`.
GENERATOR_ENTRY = "scripts/build-locale-catalog-fixture.py"
GENERATOR_MODULE_DIR = "scripts"

# Third-party distributions this generator is allowed to import, pinned here
# and in the PEP 723 block above. Everything else must be either a module in
# the generator tree or a standard-library module: an unrecognised import is a
# refusal, never a silent "probably external" fallback, because an import the
# closure cannot classify is an input it cannot hash.
PINNED_EXTERNAL_MODULES = frozenset({"jsonschema", "referencing"})

# Names and attributes that would let a module reach code the AST closure
# cannot see. Fail-closed: the generator has no need for any of them, so their
# mere presence in a source is a refusal rather than something to analyse.
BANNED_IMPORT_ROOTS = frozenset({"importlib", "imp", "runpy", "pkgutil", "pkg_resources"})
BANNED_NAMES = frozenset({"__import__", "eval", "exec", "compile", "globals", "locals", "vars"})
BANNED_SYS_ATTRIBUTES = frozenset({"path", "meta_path", "path_hooks", "path_importer_cache", "modules"})
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


class GeneratorBoundaryError(Exception):
    """A generator source reached outside the closed implementation boundary."""


def canonical_generator_tree() -> Path:
    tree = (ROOT / GENERATOR_MODULE_DIR).resolve(strict=True)
    require(tree.is_dir(), f"{GENERATOR_MODULE_DIR} is not a directory")
    return tree


def check_generator_path(relative_path: str) -> Path:
    """Every generator source must be a plain file inside the canonical
    generator tree.

    A symlink is refused outright rather than followed: following one would let
    the bytes that are hashed live somewhere the boundary does not cover, and a
    retarget would change the generator's behavior without changing anything
    this closure reads. The path is canonicalized and required to stay inside
    the tree, so `scripts/../something.py` cannot slip out either.
    """
    tree = canonical_generator_tree()
    path = ROOT / relative_path

    walked = ROOT
    for part in Path(relative_path).parts:
        walked = walked / part
        if walked.is_symlink():
            raise GeneratorBoundaryError(
                f"{relative_path} is reached through the symlink {walked.relative_to(ROOT)}; "
                "generator sources must be plain files inside the generator tree"
            )

    if not path.is_file():
        raise GeneratorBoundaryError(f"missing generator source {relative_path}")

    resolved = path.resolve(strict=True)
    if resolved != tree and tree not in resolved.parents:
        raise GeneratorBoundaryError(
            f"{relative_path} resolves to {resolved}, outside the generator tree {tree}"
        )
    return path


def read_generator_source(relative_path: str) -> bytes:
    return check_generator_path(relative_path).read_bytes()


def local_module_candidates(module_name: str) -> list[str]:
    """Repository paths a module name could resolve to.

    Both roots the generator actually runs with are considered: the generator
    tree (which is on `sys.path` because the entry module lives there) and the
    repository root. Packages are resolved through their `__init__.py`, and
    every parent package on the way is a candidate too, so a package's
    initialisation code is hashed alongside the module that was imported.
    """
    segments = module_name.split(".")
    candidates: list[str] = []
    for root in (GENERATOR_MODULE_DIR, ""):
        prefix = f"{root}/" if root else ""
        for depth in range(1, len(segments) + 1):
            head = "/".join(segments[:depth])
            candidates.append(f"{prefix}{head}/__init__.py")
            if depth == len(segments):
                candidates.append(f"{prefix}{head}.py")
    return candidates


def imported_module_names(tree: ast.AST, relative_path: str) -> list[str]:
    """Every module name a source imports, with anything the closure cannot
    follow statically refused rather than skipped.
    """
    names: list[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names.extend(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom):
            if node.level:
                raise GeneratorBoundaryError(
                    f"{relative_path} uses a relative import; generator modules are resolved by "
                    "absolute name so the closure can bind each one to a repository path"
                )
            if node.module is None:
                raise GeneratorBoundaryError(f"{relative_path} has an import with no module name")
            names.append(node.module)
            # `from package import submodule` imports the submodule too.
            names.extend(f"{node.module}.{alias.name}" for alias in node.names if alias.name != "*")
        elif isinstance(node, ast.Name) and node.id in BANNED_NAMES:
            raise GeneratorBoundaryError(
                f"{relative_path} references {node.id!r}, which can execute or import code this "
                "closure cannot see; the generator's inputs must all be statically resolvable"
            )
        elif isinstance(node, ast.Attribute):
            value = node.value
            if isinstance(value, ast.Name) and value.id == "sys" and node.attr in BANNED_SYS_ATTRIBUTES:
                raise GeneratorBoundaryError(
                    f"{relative_path} touches sys.{node.attr}, which can change where imports "
                    "resolve from; the generator's module search must stay fixed"
                )
    return names


def classify_import(module_name: str, relative_path: str) -> list[str]:
    """Return every repository path a module resolves to -- the module itself
    and each parent package's `__init__.py`, so a package's initialisation code
    is hashed alongside it -- or an empty list when it is an allowed external
    module. Anything else raises.
    """
    root_name = module_name.split(".")[0]
    if root_name == "__future__":
        return []

    inside: list[str] = []
    outside: list[str] = []
    for candidate in local_module_candidates(module_name):
        if not (ROOT / candidate).exists():
            continue
        (inside if candidate.startswith(f"{GENERATOR_MODULE_DIR}/") else outside).append(candidate)

    if outside:
        raise GeneratorBoundaryError(
            f"{relative_path} imports {module_name!r}, which resolves to the repository-local "
            f"{outside[0]} outside the generator tree; move it into {GENERATOR_MODULE_DIR}/ or "
            "stop importing it"
        )
    if inside:
        return inside

    if root_name in PINNED_EXTERNAL_MODULES or root_name in sys.stdlib_module_names:
        return []

    raise GeneratorBoundaryError(
        f"{relative_path} imports {module_name!r}, which is neither a generator module, a "
        "standard-library module, nor one of this generator's pinned dependencies "
        f"({sorted(PINNED_EXTERNAL_MODULES)}); the closure refuses imports it cannot classify"
    )


def scan_generator_sources(read_source=read_generator_source) -> tuple[str, ...]:
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
    pending = [GENERATOR_ENTRY]
    closure: set[str] = set()

    while pending:
        relative_path = pending.pop()
        if relative_path in closure:
            continue
        source = read_source(relative_path)
        closure.add(relative_path)

        tree = ast.parse(source, filename=relative_path)
        for module_name in imported_module_names(tree, relative_path):
            if module_name.split(".")[0] in BANNED_IMPORT_ROOTS:
                raise GeneratorBoundaryError(
                    f"{relative_path} imports {module_name!r}, which can load code this closure "
                    "cannot see; the generator's inputs must all be statically resolvable"
                )
            pending.extend(classify_import(module_name, relative_path))

    if GENERATOR_ENTRY not in closure:
        raise GeneratorBoundaryError("the generator source closure lost its own entry module")
    return tuple(sorted(closure))


def generator_sources() -> tuple[str, ...]:
    try:
        return scan_generator_sources()
    except GeneratorBoundaryError as error:
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
    """Put each bypass into the *real* generator sources' own AST and require a
    refusal, rather than asserting the boundary in prose.

    The AST cases run through an injected reader, so nothing is written to
    disk; the path cases need real filesystem state and clean it up.
    """
    entry_source = read_generator_source(GENERATOR_ENTRY)
    helper = f"{GENERATOR_MODULE_DIR}/strict_json.py"
    helper_source = read_generator_source(helper)

    def reader_with(overrides: dict[str, bytes]):
        def read(relative_path: str) -> bytes:
            if relative_path in overrides:
                return overrides[relative_path]
            return read_generator_source(relative_path)

        return read

    def require_refusal(label: str, overrides: dict[str, bytes]) -> None:
        try:
            scan_generator_sources(reader_with(overrides))
        except GeneratorBoundaryError:
            return
        raise SystemExit(
            f"locale-catalog fixture: Self-test failure: {label} was accepted by the generator "
            "source closure, so the boundary is not fail-closed"
        )

    bypasses = {
        "an importlib import": b"import importlib\n",
        "an importlib.util import": b"from importlib import util\n",
        "a runpy import": b"import runpy\n",
        "a pkgutil import": b"import pkgutil\n",
        "a __import__ call": b'__import__("os")\n',
        "an exec call": b'exec("VALUE = 1")\n',
        "an eval call": b'eval("1")\n',
        "a compile call": b'compile("1", "<s>", "eval")\n',
        "a sys.path mutation": b'import sys\nsys.path.insert(0, "elsewhere")\n',
        "a sys.meta_path mutation": b"import sys\nsys.meta_path.clear()\n",
        "a sys.path_hooks mutation": b"import sys\nsys.path_hooks.clear()\n",
        "a sys.modules lookup": b'import sys\nVALUE = sys.modules.get("os")\n',
        "a relative import": b"from . import strict_json\n",
        "an unclassifiable import": b"import totally_unknown_third_party\n",
    }
    for label, injected in bypasses.items():
        require_refusal(f"{label} in the entry module", {GENERATOR_ENTRY: injected + entry_source})
        require_refusal(f"{label} in a generator helper", {helper: injected + helper_source})

    # A repository-local module *outside* the generator tree must be refused
    # rather than quietly treated as external and left unhashed.
    outside = ROOT / "locale_catalog_boundary_probe.py"
    package_root = ROOT / GENERATOR_MODULE_DIR / "locale_catalog_boundary_pkg"
    outside_package = ROOT / "locale_catalog_boundary_pkg_outside"
    link = ROOT / GENERATOR_MODULE_DIR / "locale_catalog_boundary_link.py"
    try:
        outside.write_bytes(b"VALUE = 1\n")
        require_refusal(
            "a repository-local import outside the generator tree",
            {GENERATOR_ENTRY: b"import locale_catalog_boundary_probe\n" + entry_source},
        )

        outside_package.mkdir()
        (outside_package / "__init__.py").write_bytes(b"VALUE = 1\n")
        require_refusal(
            "a repository-local package outside the generator tree",
            {GENERATOR_ENTRY: b"import locale_catalog_boundary_pkg_outside\n" + entry_source},
        )

        # A package *inside* the tree must resolve, and its __init__ must join
        # the closure -- otherwise its initialisation code would go unhashed.
        package_root.mkdir()
        (package_root / "__init__.py").write_bytes(b"VALUE = 1\n")
        (package_root / "leaf.py").write_bytes(b"VALUE = 2\n")
        with_package = scan_generator_sources(
            reader_with(
                {
                    GENERATOR_ENTRY: b"from locale_catalog_boundary_pkg import leaf\n" + entry_source
                }
            )
        )
        for expected in (
            f"{GENERATOR_MODULE_DIR}/locale_catalog_boundary_pkg/__init__.py",
            f"{GENERATOR_MODULE_DIR}/locale_catalog_boundary_pkg/leaf.py",
        ):
            require(
                expected in with_package,
                f"Self-test failure: an in-tree package import left {expected} out of the closure",
            )

        # A symlinked source must be refused rather than followed.
        link.symlink_to(ROOT / GENERATOR_MODULE_DIR / "strict_json.py")
        require_refusal(
            "a symlinked generator module",
            {GENERATOR_ENTRY: b"import locale_catalog_boundary_link\n" + entry_source},
        )
    finally:
        for path in (link, package_root / "leaf.py", package_root / "__init__.py"):
            if path.is_symlink() or path.exists():
                path.unlink()
        for directory in (package_root, outside_package):
            if directory.exists():
                for child in sorted(directory.iterdir()):
                    child.unlink()
                directory.rmdir()
        if outside.exists():
            outside.unlink()

    # Finally, the real closure must still be exactly what the manifest hashed.
    require(
        scan_generator_sources() == generator_sources(),
        "Self-test failure: the injectable reader changed the real generator source closure",
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

    require(
        provenance["generatorSha256"] == fileset_digest(generator_inputs),
        "Self-test failure: generatorSha256 is not the digest of this generator's own sources",
    )
    require(
        GENERATOR_ENTRY in sources and f"{GENERATOR_MODULE_DIR}/strict_json.py" in sources,
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
        generator_inputs + [(f"{GENERATOR_MODULE_DIR}/new_helper.py", b"# a new local helper\n")]
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
