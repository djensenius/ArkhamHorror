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

Nothing generated is hashed, committed, or compared against contract bytes:
this script asserts the *shape* is producible, and separately asserts the
generated catalog is free to differ from the synthetic fixture.
"""

import hashlib
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


def main() -> None:
    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)

    manifest_path = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else DEFAULT_MANIFEST
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
