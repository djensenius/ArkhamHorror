#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "jsonschema==4.26.0",
# ]
# ///
"""Deployment seam: can a *real* generated catalog manifest configure the
capability the backend advertises?

`contracts/fixtures/capabilities-locale-catalog.json` is bound to a committed,
synthetic manifest (`contracts/fixtures/locale-catalog-manifest.json`), which
is what keeps publishing new catalog content from churning a governed
contract. The risk that design creates is the opposite one: the synthetic
fixture could drift into a shape no real deployment can produce, and nobody
would notice until a native client asked a production server for a catalog.

So this check derives the six `ARKHAM_LOCALE_CATALOG_*` settings from the
manifest the frontend build actually generated -- exactly the derivation
`docs/locale-catalog.md` documents -- and requires the resulting
`localeCatalog` object, and the whole capabilities response it would appear
in, to satisfy the governed schema the backend's encoder is bound to.

With `--probe`, it stops modelling the backend and *runs* it: the probe
(`backend/arkham-api/app-capabilities-probe`) loads settings through the same
`loadYamlSettings` call `Application.appMain` uses, builds the body through the
handler's own `capabilitiesResponse`, and prints `Data.Aeson.encode`'s bytes --
the production `toEncoding` path. Those bytes are validated against the
governed schema and against the generated manifest's exact metadata, and then
every setting is corrupted in turn to prove the server refuses to start rather
than advertising a broken catalog.

Nothing generated is hashed, committed, or compared against contract bytes:
this script asserts the *shape* is producible, and separately asserts the
generated catalog is free to differ from the synthetic fixture.
"""

import argparse
import hashlib
import json
import os
import shlex
import subprocess
import sys
from pathlib import Path

from jsonschema import FormatChecker
from jsonschema.validators import validator_for

import strict_json

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = ROOT / "frontend" / "public" / "locale-catalog" / "manifest.json"
CATALOG_SCHEMA = ROOT / "frontend" / "schemas" / "locale-catalog" / "v1" / "manifest.schema.json"
CAPABILITIES_SCHEMA = "contracts/schemas/capabilities.schema.json"
SYNTHETIC_MANIFEST = "contracts/fixtures/locale-catalog-manifest.json"
CONTRACT_MANIFEST = "contracts/manifest.json"
ADVERTISED_FIXTURE = "contracts/fixtures/capabilities-locale-catalog.json"
LOCALE_CATALOG_CAPABILITY = "i18n.locale-catalog.v1"

SETTINGS = (
    "ARKHAM_LOCALE_CATALOG_MANIFEST_URL",
    "ARKHAM_LOCALE_CATALOG_REVISION",
    "ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION",
    "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE",
    "ARKHAM_LOCALE_CATALOG_LOCALES",
    "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256",
)


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(f"locale-catalog capability settings: {message}")


def load_governed(relative_path: str) -> object:
    content = strict_json.read_governed_worktree_bytes(ROOT, relative_path)
    return strict_json.strict_json_loads(content, source=f"<worktree>:{relative_path}")


def derive_settings(catalog: dict, catalog_bytes: bytes) -> dict[str, str]:
    """The derivation documented for operators, in one place.

    `manifestPath` is the catalog's own stable, same-origin route, so a hosted
    and a self-hosted deployment configure the identical value and no hostname
    is ever baked in.
    """
    return {
        "ARKHAM_LOCALE_CATALOG_MANIFEST_URL": catalog["manifestPath"],
        "ARKHAM_LOCALE_CATALOG_REVISION": catalog["catalogRevision"],
        "ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION": catalog["schemaVersion"],
        "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE": catalog["defaultLocale"],
        "ARKHAM_LOCALE_CATALOG_LOCALES": ",".join(
            record["locale"] for record in catalog["locales"]
        ),
        "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256": hashlib.sha256(catalog_bytes).hexdigest(),
    }


def advertised_from_settings(settings: dict[str, str]) -> dict:
    """The `localeCatalog` object the backend builds from those settings.

    `Base.Api.Types.LocaleCatalog.parseSupportedLocales` canonicalizes each tag
    and then publishes them in ascending order. Canonicalization is *not*
    re-implemented here: `require_canonical_locales` below asserts the
    generated manifest's tags are already canonical, which is what makes plain
    sorting the whole of the remaining transformation — and is itself worth
    knowing, since a generator that started emitting `zh-hant` would make the
    advertised list stop matching the manifest's own tags.
    """
    return {
        "manifestUrl": settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"],
        "catalogRevision": settings["ARKHAM_LOCALE_CATALOG_REVISION"],
        "schemaVersion": settings["ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION"],
        "defaultLocale": settings["ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE"],
        "supportedLocales": sorted(settings["ARKHAM_LOCALE_CATALOG_LOCALES"].split(",")),
        "manifestSha256": settings["ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"],
    }


def require_canonical_locales(capabilities_schema: dict, settings: dict[str, str]) -> None:
    """Every locale the generated catalog publishes must already be in the
    canonical BCP-47 spelling `contracts/schemas/capabilities.schema.json`'s
    `$defs.locale` encodes, so the backend's canonicalization is the identity
    on real generated input.
    """
    validator = make_validator(capabilities_schema, capabilities_schema["$defs"]["locale"])
    tags = settings["ARKHAM_LOCALE_CATALOG_LOCALES"].split(",") + [
        settings["ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE"]
    ]
    for tag in tags:
        errors = list(validator.iter_errors(tag))
        require(
            not errors,
            f"the generated catalog publishes locale {tag!r}, which is not the canonical spelling "
            "the server would advertise; the advertised list would then differ from the "
            f"manifest's own tags: {[error.message for error in errors]}",
        )


def make_validator(schema: dict, sub_schema: dict | None = None):
    target = dict(sub_schema) if sub_schema is not None else dict(schema)
    if sub_schema is not None:
        target.setdefault("$schema", schema.get("$schema"))
        if "$defs" in schema:
            target["$defs"] = schema["$defs"]
    validator_class = validator_for(target)
    validator_class.check_schema(target)
    return validator_class(target, format_checker=FormatChecker())


def run_probe(
    command: list[str],
    environment: dict[str, str],
    settings_files: list[Path] | None = None,
) -> subprocess.CompletedProcess:
    """Run the production probe with exactly `environment` added to the current
    one, so an inherited ARKHAM_LOCALE_CATALOG_* value cannot mask a failure.

    `settings_files` are passed as command-line arguments, which is how
    `Application.loadAppSettingsArgs` takes runtime config files: they are
    merged over the compile-time `config/settings.yml` value key by key, and the
    environment reaches whatever `_env:` markers survive that merge. Output is
    captured as raw bytes — the probe writes the
    production encoder's output and nothing else, so a caller can assert those
    bytes exactly.
    """
    child_environment = {
        key: value for key, value in os.environ.items() if not key.startswith("ARKHAM_LOCALE_CATALOG_")
    }
    child_environment.update(environment)
    arguments = [str(path) for path in settings_files or []]
    return subprocess.run(
        command + arguments,
        env=child_environment,
        capture_output=True,
        cwd=ROOT,
        check=False,
    )


def encoded_capabilities(response: dict) -> bytes:
    """The exact bytes aeson's generic `toEncoding` produces for
    `ServerCapabilities`: declaration field order, no separator whitespace, no
    trailing newline. Spelled out here so the probe's stdout can be asserted as
    *bytes*, rather than being decoded and compared as a normalized value --
    which is precisely the check that would miss a toEncoding/toJSON drift.
    """
    order = [
        "schemaRevision",
        "status",
        "apiBasePath",
        "nativeClientMinimumRevision",
        "capabilities",
        "localeCatalog",
    ]
    catalog_order = [
        "manifestUrl",
        "catalogRevision",
        "schemaVersion",
        "defaultLocale",
        "supportedLocales",
        "manifestSha256",
    ]
    ordered: dict[str, object] = {}
    for key in order:
        if key not in response:
            continue
        if key == "localeCatalog":
            ordered[key] = {name: response[key][name] for name in catalog_order}
        else:
            ordered[key] = response[key]
    require(
        set(ordered) == set(response),
        f"the response carries fields this encoder model does not order: "
        f"{sorted(set(response) - set(ordered))}",
    )
    return json.dumps(ordered, separators=(",", ":"), ensure_ascii=False).encode("utf-8")


def check_with_probe(
    command: list[str],
    capabilities_schema: dict,
    settings: dict[str, str],
    advertised: dict,
    legacy_baseline: dict,
    contract_revision: str,
) -> None:
    scratch = ROOT / "scratch-capability-probe"
    scratch.mkdir(exist_ok=True)
    try:
        _check_with_probe(
            command, capabilities_schema, settings, advertised, legacy_baseline,
            contract_revision, scratch,
        )
    finally:
        for path in sorted(scratch.glob("*")):
            path.unlink()
        scratch.rmdir()


def write_settings_file(scratch: Path, name: str, values: dict[str, str]) -> Path:
    """A runtime settings file in the production key spelling, passed to the
    probe on the command line exactly as a deployment would."""
    path = scratch / name
    path.write_text(
        "".join(f"{key}: {json.dumps(value)}\n" for key, value in sorted(values.items())),
        encoding="utf-8",
    )
    return path


SETTINGS_FILE_KEYS = {
    "ARKHAM_LOCALE_CATALOG_MANIFEST_URL": "locale-catalog-manifest-url",
    "ARKHAM_LOCALE_CATALOG_REVISION": "locale-catalog-revision",
    "ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION": "locale-catalog-schema-version",
    "ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE": "locale-catalog-default-locale",
    "ARKHAM_LOCALE_CATALOG_LOCALES": "locale-catalog-locales",
    "ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256": "locale-catalog-manifest-sha256",
}


def _check_with_probe(
    command: list[str],
    capabilities_schema: dict,
    settings: dict[str, str],
    advertised: dict,
    legacy_baseline: dict,
    contract_revision: str,
    scratch: Path,
) -> None:
    printed = run_probe(command, settings)
    require(
        printed.returncode == 0,
        "the production probe refused the settings derived from the generated catalog: "
        f"{printed.stderr.decode('utf-8', 'replace').strip()}",
    )

    # Exact bytes first: the wire is toEncoding, and a decode-then-compare would
    # not notice field reordering, added whitespace or a stray newline.
    expected_advertised_bytes = encoded_capabilities(
        {
            "schemaRevision": contract_revision,
            "status": legacy_baseline["status"],
            "apiBasePath": legacy_baseline["apiBasePath"],
            "nativeClientMinimumRevision": legacy_baseline["nativeClientMinimumRevision"],
            "capabilities": sorted(
                legacy_baseline["capabilities"] + [LOCALE_CATALOG_CAPABILITY]
            ),
            "localeCatalog": advertised,
        }
    )
    require(
        printed.stdout == expected_advertised_bytes,
        "the production encoder's bytes are not what this contract describes.\n"
        f"  emitted:  {printed.stdout!r}\n"
        f"  expected: {expected_advertised_bytes!r}",
    )

    response = strict_json.strict_json_loads(printed.stdout, source="<probe>")
    errors = list(make_validator(capabilities_schema).iter_errors(response))
    require(
        not errors,
        "the bytes the production encoder emitted for the generated catalog do not satisfy "
        f"{CAPABILITIES_SCHEMA}: {[error.message for error in errors]}",
    )
    require(
        response["localeCatalog"] == advertised,
        "the production encoder did not advertise the generated catalog's own metadata: "
        f"{response['localeCatalog']} != {advertised}",
    )

    disabled = run_probe(command, {})
    require(
        disabled.returncode == 0,
        f"the probe failed with no catalog configured at all: {disabled.stderr.decode()}",
    )
    expected_disabled_bytes = encoded_capabilities({**legacy_baseline, "schemaRevision": contract_revision})
    require(
        disabled.stdout == expected_disabled_bytes,
        "an unconfigured deployment's bytes are not the legacy shape at this revision.\n"
        f"  emitted:  {disabled.stdout!r}\n"
        f"  expected: {expected_disabled_bytes!r}",
    )

    # The same configuration, supplied as a runtime settings file on the command
    # line rather than through the environment, must produce identical bytes.
    file_settings = write_settings_file(
        scratch,
        "settings-valid.yml",
        {SETTINGS_FILE_KEYS[key]: value for key, value in settings.items()},
    )
    from_file = run_probe(command, {}, [file_settings])
    require(
        from_file.returncode == 0,
        f"the probe refused a runtime settings file: {from_file.stderr.decode('utf-8', 'replace')}",
    )
    require(
        from_file.stdout == printed.stdout,
        "a command-line settings file and the environment produced different bytes",
    )

    # Precedence, as `loadAppSettingsArgs` actually defines it: a runtime
    # settings file is merged *over* the compile-time value key by key, and the
    # environment reaches a setting only through an `_env:` marker. So a key the
    # file spells literally wins over the environment (the file replaced the
    # marker the variable would have reached), while a key the file omits keeps
    # its compile-time marker and stays environment-configurable. Both
    # directions are exercised below. The literal-wins direction is the safe
    # one: an operator cannot be surprised by an inherited variable, and a bad
    # settings file cannot be silently rescued by one.
    literal_override = run_probe(
        command,
        {"ARKHAM_LOCALE_CATALOG_MANIFEST_URL": "https://static.example.org/l10n/manifest.json"},
        [file_settings],
    )
    require(literal_override.returncode == 0, "the settings file was refused")
    literal_response = strict_json.strict_json_loads(literal_override.stdout, source="<probe>")
    require(
        literal_response["localeCatalog"]["manifestUrl"]
        == settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"],
        "a literal value in a command-line settings file must win over the environment",
    )

    # An `_env:` marker in that same file is how a deployment opts a setting
    # back into the environment, with the file supplying the fallback.
    env_marked_file = write_settings_file(
        scratch,
        "settings-env-marked.yml",
        {
            **{SETTINGS_FILE_KEYS[key]: value for key, value in settings.items()},
            "locale-catalog-manifest-url": (
                "_env:ARKHAM_LOCALE_CATALOG_MANIFEST_URL:"
                + settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"]
            ),
        },
    )
    marked_default = run_probe(command, {}, [env_marked_file])
    require(
        marked_default.returncode == 0
        and strict_json.strict_json_loads(marked_default.stdout, source="<probe>")["localeCatalog"][
            "manifestUrl"
        ]
        == settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"],
        "an unset _env: marker must fall back to the value the settings file supplies",
    )
    marked_override = run_probe(
        command,
        {"ARKHAM_LOCALE_CATALOG_MANIFEST_URL": "https://static.example.org/l10n/manifest.json"},
        [env_marked_file],
    )
    require(marked_override.returncode == 0, "the _env: override was refused")
    require(
        strict_json.strict_json_loads(marked_override.stdout, source="<probe>")["localeCatalog"][
            "manifestUrl"
        ]
        == "https://static.example.org/l10n/manifest.json",
        "an environment variable must override the _env: marker's fallback",
    )

    # A settings file that is only partially filled in, with nothing else to
    # complete it, must fail startup rather than advertise half a pointer.
    partial_file = write_settings_file(
        scratch,
        "settings-partial.yml",
        {"locale-catalog-manifest-url": settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"]},
    )
    partial = run_probe(command, {}, [partial_file])
    require(
        partial.returncode != 0 and b"localeCatalog" not in partial.stdout,
        "a partially configured settings file must fail startup",
    )
    # A runtime settings file is *merged* over the compile-time value rather
    # than replacing it, so the keys it does not mention keep their `_env:`
    # markers and the environment can still complete the configuration.
    partial_with_env = run_probe(command, settings, [partial_file])
    require(
        partial_with_env.returncode == 0,
        "a settings file is merged over the compile-time value, so the environment must still "
        f"complete the keys it omits: {partial_with_env.stderr.decode('utf-8', 'replace')}",
    )
    require(
        partial_with_env.stdout == printed.stdout,
        "completing a partial settings file from the environment produced different bytes",
    )

    invalid_file = write_settings_file(
        scratch,
        "settings-invalid.yml",
        {
            **{SETTINGS_FILE_KEYS[key]: value for key, value in settings.items()},
            "locale-catalog-manifest-url": "http://cdn.example.com/manifest.json",
        },
    )
    invalid = run_probe(command, {}, [invalid_file])
    require(
        invalid.returncode != 0 and b"localeCatalog" not in invalid.stdout,
        "an insecure manifest URL in a settings file must fail startup",
    )
    not_rescued = run_probe(
        command,
        {"ARKHAM_LOCALE_CATALOG_MANIFEST_URL": settings["ARKHAM_LOCALE_CATALOG_MANIFEST_URL"]},
        [invalid_file],
    )
    require(
        not_rescued.returncode != 0 and b"localeCatalog" not in not_rescued.stdout,
        "an environment variable must not silently rescue an insecure literal in a settings file",
    )

    # Every setting, corrupted in turn: the server must refuse to start rather
    # than advertise a catalog a client cannot verify.
    corruptions: list[tuple[str, str]] = [
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "http://cdn.example.com/manifest.json"),
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", "https://2130706433/manifest.json"),
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_URL", ""),
        ("ARKHAM_LOCALE_CATALOG_REVISION", "1.0.0"),
        ("ARKHAM_LOCALE_CATALOG_REVISION", "2" + settings["ARKHAM_LOCALE_CATALOG_REVISION"][1:]),
        ("ARKHAM_LOCALE_CATALOG_REVISION", "0" + settings["ARKHAM_LOCALE_CATALOG_REVISION"]),
        ("ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION", "2.0.0"),
        ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "xx-not-a-locale-tag"),
        ("ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE", "sv"),
        ("ARKHAM_LOCALE_CATALOG_LOCALES", settings["ARKHAM_LOCALE_CATALOG_LOCALES"] + ",english"),
        (
            "ARKHAM_LOCALE_CATALOG_LOCALES",
            settings["ARKHAM_LOCALE_CATALOG_LOCALES"]
            + ","
            + settings["ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE"].upper(),
        ),
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", "deadbeef"),
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", settings["ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"].upper()),
        # YAML re-parses substituted values, so an all-digit digest is a Number
        # by the time the settings parser sees it: that must be a named refusal,
        # not an uncaught type error.
        ("ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256", "0" * 64),
        ("ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION", "1.0"),
    ]
    for name, value in corruptions:
        corrupted = dict(settings)
        corrupted[name] = value
        result = run_probe(command, corrupted)
        require(
            result.returncode != 0,
            f"the server started with {name}={value!r}; a supplied-but-invalid catalog "
            "configuration must fail startup, never be silently dropped",
        )
        require(
            b"localeCatalog" not in result.stdout,
            f"the server printed a catalog pointer while failing on {name}={value!r}",
        )

    # A digest that is well-formed but not this manifest's is the one corruption
    # the server cannot detect: it is the client that verifies the bytes. Prove
    # the server still advertises exactly what it was told, so the mismatch is
    # visible to the client rather than papered over here.
    mismatched = dict(settings)
    mismatched["ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"] = "a" * 64
    result = run_probe(command, mismatched)
    require(result.returncode == 0, "a well-formed digest must be accepted verbatim")
    mismatched_response = strict_json.strict_json_loads(result.stdout, source="<probe>")
    require(
        mismatched_response["localeCatalog"]["manifestSha256"] == "a" * 64,
        "the server must advertise the digest it was configured with, so a client verifying the "
        "manifest sees the mismatch",
    )
    print(
        f"locale-catalog capability probe: exact production toEncoding bytes for the advertised "
        f"and disabled responses, command-line settings-file and environment precedence, and "
        f"{len(corruptions)} corrupted settings each refused at startup."
    )


def main() -> None:
    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    parser = argparse.ArgumentParser(description="Locale-catalog capability deployment seam")
    parser.add_argument("manifest", nargs="?", default=str(DEFAULT_MANIFEST))
    parser.add_argument(
        "--probe",
        help="command that runs the production capabilities probe, e.g. "
        "'stack exec --system-ghc arkham-capabilities-probe --'",
    )
    arguments = parser.parse_args()
    manifest_path = Path(arguments.manifest).resolve()
    require(
        manifest_path.is_file(),
        f"no generated catalog manifest at {manifest_path}; run "
        "`npm --prefix frontend run locale-catalog` (or `npm run build`) first -- this check is "
        "about the real generated artifact, so it must not silently skip",
    )

    generated_bytes = manifest_path.read_bytes()
    generated = strict_json.strict_json_loads(generated_bytes, source=str(manifest_path))
    require(isinstance(generated, dict), f"{manifest_path} is not a JSON object")

    catalog_schema = strict_json.strict_json_load_path(CATALOG_SCHEMA)
    schema_errors = list(make_validator(catalog_schema).iter_errors(generated))
    require(
        not schema_errors,
        f"{manifest_path} does not validate against {CATALOG_SCHEMA.relative_to(ROOT)}: "
        f"{[error.message for error in schema_errors]}",
    )

    settings = derive_settings(generated, generated_bytes)
    for name in SETTINGS:
        value = settings[name]
        require(
            isinstance(value, str) and value and value.strip() == value,
            f"{name} derived from the generated manifest is empty or padded: {value!r}",
        )

    capabilities_schema = load_governed(CAPABILITIES_SCHEMA)
    require(isinstance(capabilities_schema, dict), f"{CAPABILITIES_SCHEMA} is not a JSON object")
    locale_catalog_schema = capabilities_schema["$defs"]["localeCatalog"]

    require_canonical_locales(capabilities_schema, settings)
    advertised = advertised_from_settings(settings)
    advertised_errors = list(
        make_validator(capabilities_schema, locale_catalog_schema).iter_errors(advertised)
    )
    require(
        not advertised_errors,
        "the localeCatalog object a deployment would advertise for the generated catalog does not "
        f"satisfy {CAPABILITIES_SCHEMA}: {[error.message for error in advertised_errors]}",
    )

    # The whole response, not just the fragment: a real deployment's body must
    # satisfy the closed schema including the capability/object pairing.
    governed_response = load_governed(ADVERTISED_FIXTURE)
    require(isinstance(governed_response, dict), f"{ADVERTISED_FIXTURE} is not a JSON object")
    response = dict(governed_response)
    response["localeCatalog"] = advertised
    require(
        LOCALE_CATALOG_CAPABILITY in response.get("capabilities", []),
        f"{ADVERTISED_FIXTURE} must advertise {LOCALE_CATALOG_CAPABILITY}",
    )
    response_errors = list(make_validator(capabilities_schema).iter_errors(response))
    require(
        not response_errors,
        "the capabilities response a deployment would serve for the generated catalog does not "
        f"satisfy {CAPABILITIES_SCHEMA}: {[error.message for error in response_errors]}",
    )

    # The point of the synthetic fixture: production content moves without the
    # governed contract moving. Assert the two really are independent rather
    # than accidentally identical, which would hide exactly that coupling.
    synthetic_bytes = strict_json.read_governed_worktree_bytes(ROOT, SYNTHETIC_MANIFEST)
    require(
        hashlib.sha256(synthetic_bytes).hexdigest()
        != settings["ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256"],
        f"the generated catalog manifest is byte-identical to {SYNTHETIC_MANIFEST}; the synthetic "
        "fixture must stay a fixed test artifact, never a copy of generated output",
    )
    require(
        governed_response["localeCatalog"]["catalogRevision"] != advertised["catalogRevision"],
        f"{ADVERTISED_FIXTURE} advertises the generated catalog's revision "
        f"({advertised['catalogRevision']}); the governed fixture must describe the synthetic "
        "manifest, or every production content change would force a contract revision bump",
    )

    if arguments.probe:
        contract_manifest = load_governed(CONTRACT_MANIFEST)
        require(isinstance(contract_manifest, dict), f"{CONTRACT_MANIFEST} is not a JSON object")
        legacy = contract_manifest["legacyCompatibilityChecks"]
        check_with_probe(
            shlex.split(arguments.probe),
            capabilities_schema,
            settings,
            advertised,
            legacy["baselineResponse"],
            contract_manifest["schemaRevision"],
        )

    print(
        "locale-catalog capability settings: the generated catalog "
        f"({settings['ARKHAM_LOCALE_CATALOG_REVISION']}, "
        f"{len(advertised['supportedLocales'])} locales) populates all "
        f"{len(SETTINGS)} ARKHAM_LOCALE_CATALOG_* settings and its advertised response validates "
        f"against {CAPABILITIES_SCHEMA}, independently of the governed synthetic fixture "
        f"({governed_response['localeCatalog']['catalogRevision']})."
    )


if __name__ == "__main__":
    main()
