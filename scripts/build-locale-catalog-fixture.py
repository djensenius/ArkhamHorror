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

`schemasSha256` is the digest of the *real* published v1 schemas, so editing
them really does move the catalog revision. `generatorSha256` is derived from
the generator's identity alone -- its schema-pinned name and
`GENERATOR_VERSION` -- so editing tooling (this script, its `scripts/` import
closure, the launchers, mise.toml, the workflows, the Dockerfile, ...) never
reaches governed bytes and never forces a contract revision bump. Those tooling
bytes are pinned instead by the non-governed, committed
`scripts/locale_catalog_tooling_lock.json`, which `--check` requires to match
exactly (see `tooling_lock_bytes()`).

Every digest, count, path, `outputSha256` and provenance value in that manifest
is *computed here* from those bytes -- nothing is asserted by hand. `--check`
recomputes the whole set and requires the committed bytes to match exactly, so
a single edited byte anywhere in the chain fails, and `--self-test` proves that
by mutating each claimed field in turn.

The chain deliberately reads no production input, so publishing new catalog
content never touches it. The one production-shaped part, the `cardNames` pack,
is a hand-pinned copy of the Night of the Zealot card names (en and de, plus
the card-only `pl` locale) that
`frontend/tests/localeCatalogCardNames.test.mjs` proves equal to what the real
generator resolves from `frontend/public/cards_<locale>.json`. The chain does
depend on `contracts/manifest.json`'s `schemaRevision` (the real generator
records the same binding), so a contract revision bump is regenerated with
`mise run contracts:catalog-fixture`.
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
FIXTURE_OWNERSHIP_FILE = f"{FIXTURE_PREFIX}owned-files.json"
CONTRACT_MANIFEST = "contracts/manifest.json"
CAPABILITIES_FIXTURES = (
    "capabilities.json",
    "capabilities-locale-catalog.json",
)
CATALOG_SCHEMA_DIR = ROOT / "frontend" / "schemas" / "locale-catalog" / "v1"

# The generator's own sources and execution environment (see
# `generator_sources()`) are tooling: their per-file digests live in the
# non-governed TOOLING_LOCK, checked by `--check`, so a tooling edit moves that
# reviewed lock rather than a governed contract hash. `schemasSha256` stays the
# digest of the published v1 schemas, so a schema edit still changes the
# catalog revision. `generatorSha256` is derived from the generator name and
# GENERATOR_VERSION only (see `derive_generator_sha256()`); bump
# GENERATOR_VERSION when a generator change alters what the fixture means.
GENERATOR_ENTRY = "scripts/build-locale-catalog-fixture.py"
RUNTIME_PROFILE = "scripts/locale_catalog_python_runtime.json"
TOOLING_LOCK = "scripts/locale_catalog_tooling_lock.json"
TOOLING_LOCK_FORMAT_VERSION = 1
GENERATOR_EXECUTION_SOURCES = (
    "mise.toml",
    "pyproject.toml",
    "uv.lock",
    "frontend/package.json",
    "Dockerfile",
    ".github/workflows/contracts.yml",
    ".github/workflows/locale-catalog.yml",
    ".github/workflows/haskell.yml",
    "scripts/run-locale-catalog-python.sh",
    "scripts/locale-catalog-python-sealed.sh",
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


RUNTIME_IDENTITY_KEYS = ("implementation", "version", "cacheTag", "stdlibIdentity")
RUNTIME_PLATFORMS = frozenset({"darwin-arm64", "linux-x86_64"})


def has_digest_candidates(value: object) -> bool:
    return (
        isinstance(value, list)
        and bool(value)
        and all(
            isinstance(digest, str)
            and len(digest) == 64
            and all(character in "0123456789abcdef" for character in digest)
            for digest in value
        )
        and len(value) == len(set(value))
    )


def has_binary_identity(value: object, fields: set[str]) -> bool:
    if not isinstance(value, dict) or set(value) != fields | {"binarySha256"}:
        return False
    digests = value.get("binarySha256")
    return (
        isinstance(digests, dict)
        and set(digests) == RUNTIME_PLATFORMS
        and all(has_digest_candidates(candidates) for candidates in digests.values())
    )


def has_path_identity(value: object, fields: set[str]) -> bool:
    return isinstance(value, dict) and set(value) == fields and all(
        isinstance(value[field], str) and value[field] for field in fields
    )


def runtime_identity() -> dict[str, object]:
    """The *committed* toolchain pin, never a measurement of this host.

    Read only by the self-tests, which require `RUNTIME_PROFILE` to be complete
    and to pin the exact bytes of the trusted computing base. The profile's own
    bytes are pinned by the non-governed TOOLING_LOCK. Neither it nor anything
    the sealed runtime attests at execution time (the resolved interpreter
    path, the toolchain root, the hashes it recomputes) reaches fixture
    provenance, so the derived revision is identical on every machine.
    """
    profile = strict_json.strict_json_load_path(ROOT / RUNTIME_PROFILE)
    require(
        isinstance(profile, dict)
        and set(profile)
        == {
            *RUNTIME_IDENTITY_KEYS,
            "stdlibSourceTreeSha256",
            "trustedSources",
            "interpreter",
            "uv",
            "externalTools",
            "distribution",
            "platformVariantModules",
            "stdlibImportClosureMinimumSources",
            "stdlibModuleCount",
            "stdlibModules",
        },
        f"{RUNTIME_PROFILE} is not the complete pinned runtime identity",
    )
    require(
        all(isinstance(profile[key], str) and profile[key] for key in RUNTIME_IDENTITY_KEYS),
        f"{RUNTIME_PROFILE} interpreter identity values must be non-empty strings",
    )
    require(
        isinstance(profile["stdlibSourceTreeSha256"], str)
        and len(profile["stdlibSourceTreeSha256"]) == 64
        and all(character in "0123456789abcdef" for character in profile["stdlibSourceTreeSha256"]),
        f"{RUNTIME_PROFILE} does not pin the complete stdlib source tree digest",
    )
    require(
        has_path_identity(
            profile["interpreter"],
            {"installRelativePath", "binaryRelativePath", "stdlibRelativePath"},
        )
        and has_binary_identity(profile["uv"], {"version", "installRelativePath"})
        and profile["uv"]["version"] == "0.12.6"
        and isinstance(profile["externalTools"], dict)
        and set(profile["externalTools"]) == {"node"}
        and has_binary_identity(
            profile["externalTools"]["node"], {"version", "binaryRelativePath"}
        )
        and profile["externalTools"]["node"]["version"] == "26.7.0",
        f"{RUNTIME_PROFILE} does not pin complete Python path plus uv and Node binary identities",
    )
    require(
        isinstance(profile["trustedSources"], dict)
        and set(profile["trustedSources"]) == set(locale_catalog_python_boundary.TRUSTED_SOURCES)
        and all(
            isinstance(digest, str)
            and len(digest) == 64
            and all(character in "0123456789abcdef" for character in digest)
            for digest in profile["trustedSources"].values()
        )
        and all(
            sha256_hex((ROOT / relative).read_bytes()) == digest
            for relative, digest in profile["trustedSources"].items()
        ),
        f"{RUNTIME_PROFILE} does not pin the exact bytes of the trusted computing base",
    )
    require(
        isinstance(profile["stdlibModules"], dict)
        and isinstance(profile["stdlibModuleCount"], int)
        and profile["stdlibModuleCount"] > 0
        and len(profile["stdlibModules"]) == profile["stdlibModuleCount"],
        f"{RUNTIME_PROFILE} does not pin a complete stdlib digest table",
    )
    return {
        "python": {key: profile[key] for key in RUNTIME_IDENTITY_KEYS},
        "tools": {
            "miseAction": "2026.8.14",
            "uv": profile["uv"]["version"],
            "node": profile["externalTools"]["node"]["version"],
        },
    }


# ---------------------------------------------------------------------------
# The synthetic catalog's inputs. Identifier-shaped values only: this fixture
# carries no prose, and nothing here is derived from frontend/src/locales/**.
# The card names below are the one exception: card data, not prose.
# ---------------------------------------------------------------------------

LOCALES = ("de", "en", "pl", "pt-BR")
DEFAULT_LOCALE = "en"

SOURCE_MESSAGES: dict[str, dict[str, dict[str, str]]] = {
    "en": {
        "core": {
            "addToken": "core.addToken.en",
            "continue": "core.continue.en",
            "label.doneWithMulligan": "core.doneWithMulligan.en",
            "setup": "core.setup.en",
        },
        "nightOfTheZealot": {
            "nightOfTheZealot.theGathering.intro.body": "notz.intro.body.en",
            "nightOfTheZealot.theGathering.intro.title": "notz.intro.title.en",
            "nightOfTheZealot.theGathering.setup.gatherSets": "notz.gatherSets.en",
            "nightOfTheZealot.theGathering.setup.placeLocations": "notz.placeLocations.en",
        },
    },
    "de": {"core": {"continue": "core.continue.de"}},
    "pt-BR": {"core": {"continue": "core.continue.pt-BR"}},
}

# The `cardNames` pack: what `cardNames.<code>` answers in en and de for the
# Night of the Zealot investigators (01001-01005), the basic-weakness enemies a
# DrewPlayerEnemy label names (01101-01103), and every location, enemy, story
# asset and treachery in the core-box encounter sets (discoverClueAt, enemy
# logs, add-Lita-to-deck, drew/heal labels). 01121 shows a bare code answered
# by its `a` record, 01121b a real `b` record, and 01001b the lent `b` alias.
# pt-BR has no card pack, so it falls back to en; the card-only pl locale
# below has nothing else. These are card data, never
# backend-emitted keys, so the backend registry leaves them out.
#
# The table is pinned by hand so card-data refreshes do not move the fixture.
# frontend/tests/localeCatalogCardNames.test.mjs requires it to be exactly this
# code set, with the names the real generator resolves from
# frontend/public/cards_en.json and cards_de.json. If an upstream sync renames
# a pinned card there, that test fails: update the names here, rewrite the
# fixtures (`mise run contracts:catalog-fixture-write`, then
# `mise run contracts:manifest-hashes`) and bump `schemaRevision`.
CARD_NAMES_PACK = "cardNames"
CARD_NAMES: dict[str, tuple[str, str]] = {
    "01001": ("Roland Banks", "Roland Banks"),
    "01001b": ("Roland Banks", "Roland Banks"),
    "01002": ("Daisy Walker", "Daisy Walker"),
    "01003": ("\"Skids\" O'Toole", "\u201eSkids\u201c O\u2019Toole"),
    "01004": ("Agnes Baker", "Agnes Baker"),
    "01005": ("Wendy Adams", "Wendy Adams"),
    "01101": ("Mob Enforcer", "Mobvollstrecker"),
    "01102": ("Silver Twilight Acolyte", "Akolyth der Silberloge der D\u00e4mmerung"),
    "01103": ("Stubborn Detective", "Sturer Detective"),
    "01111": ("Study", "Arbeitszimmer"),
    "01112": ("Hallway", "Flur"),
    "01113": ("Attic", "Dachboden"),
    "01114": ("Cellar", "Keller"),
    "01115": ("Parlor", "Wohnzimmer"),
    "01116": ("Ghoul Priest", "Ghulpriester"),
    "01117": ("Lita Chantler", "Lita Chantler"),
    "01118": ("Flesh-Eater", "Fleischfresser"),
    "01119": ("Icy Ghoul", "Eisghul"),
    "01121": ("Predator or Prey?", "J\u00e4ger oder Beute?"),
    "01121b": ("The Masked Hunter", "Der Maskierte J\u00e4ger"),
    "01124": ("Your House", "Dein Haus"),
    "01125": ("Rivertown", "Hafenviertel"),
    "01126": ("Southside", "S\u00fcdstadt"),
    "01127": ("Southside", "S\u00fcdstadt"),
    "01128": ("St. Mary's Hospital", "St. Mary's Hospital"),
    "01129": ("Miskatonic University", "Miskatonic Universit\u00e4t"),
    "01130": ("Downtown", "Innenstadt"),
    "01131": ("Downtown", "Innenstadt"),
    "01132": ("Easttown", "Oststadt"),
    "01133": ("Graveyard", "Friedhof"),
    "01134": ("Northside", "Nordstadt"),
    "01135": ("Hunting Shadow", "Hetzender Schatten"),
    "01136": ("False Lead", "Falsche F\u00e4hrte"),
    "01137": ("\"Wolf-Man\" Drew", "Drew, der \u201eWolfsmensch\u201d"),
    "01138": ("Herman Collins", "Herman Collins"),
    "01139": ("Peter Warren", "Peter Warren"),
    "01140": ("Victoria Devereux", "Victoria Devereux"),
    "01141": ("Ruth Turner", "Ruth Turner"),
    "01149": ("Main Path", "Hauptweg"),
    "01150": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01151": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01152": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01153": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01154": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01155": ("Arkham Woods", "W\u00e4lder von Arkham"),
    "01156": ("Ritual Site", "Ritualort"),
    "01157": ("Um\u00f4rdhoth", "Um\u00f4rdhoth"),
    "01158": ("Um\u00f4rdhoth's Wrath", "Um\u00f4rdhoths Zorn"),
    "01159": ("Swarm of Rats", "Rattenschwarm"),
    "01160": ("Ghoul Minion", "Ghuldiener"),
    "01161": ("Ravenous Ghoul", "Gefr\u00e4\u00dfiger Ghul"),
    "01162": ("Grasping Hands", "Packende H\u00e4nde"),
    "01163": ("Rotting Remains", "Verrottende \u00dcberreste"),
    "01164": ("Frozen in Fear", "Starr vor Angst"),
    "01165": ("Dissonant Voices", "Missklingende Stimmen"),
    "01166": ("Ancient Evils", "B\u00f6ses aus uralter Zeit"),
    "01167": ("Crypt Chill", "Grabesk\u00e4lte"),
    "01168": ("Obscuring Fog", "Verh\u00fcllender Nebel"),
    "01169": ("Acolyte", "Akolyth"),
    "01170": ("Wizard of the Order", "Hexenmeister des Ordens"),
    "01171": ("Mysterious Chanting", "Geheimnisvolle Ges\u00e4nge"),
    "01172": ("Hunting Nightgaunt", "Jagender Dunkeld\u00fcrrer"),
    "01173": ("On Wings of Darkness", "Auf den Schwingen der Dunkelheit"),
    "01174": ("Locked Door", "Verschlossene T\u00fcr"),
    "01175": ("Screeching Byakhee", "Kreischendes Byakhee"),
    "01176": ("The Yellow Sign", "Das Gelbe Zeichen"),
    "01177": ("Yithian Observer", "Yith-Beobachter"),
    "01178": ("Offer of Power", "Verlockende Macht"),
    "01179": ("Relentless Dark Young", "Unbarmherziges Dunkles Junges"),
    "01180": ("Goat Spawn", "Ausgeburt der Ziege"),
    "01181": ("Young Deep One", "Junges Tiefes Wesen"),
    "01182": ("Dreams of R'lyeh", "Tr\u00e4ume von R'lyeh"),
}
SOURCE_MESSAGES["en"][CARD_NAMES_PACK] = {f"{CARD_NAMES_PACK}.{code}": en for code, (en, _) in CARD_NAMES.items()}
SOURCE_MESSAGES["de"][CARD_NAMES_PACK] = {f"{CARD_NAMES_PACK}.{code}": de for code, (_, de) in CARD_NAMES.items()}

# `pl` is a card-only locale, as the real catalog publishes pl, pt, ru and uk:
# its only keys are the same Night of the Zealot card names, resolved from
# frontend/public/cards_pl.json, and every other key falls back to en. Pinned
# and proven exactly like the en and de names above.
CARD_ONLY_CARD_NAMES: dict[str, dict[str, str]] = {
    "pl": {
        "01001": "Roland Banks",
        "01001b": "Roland Banks",
        "01002": "Daisy Walker",
        "01003": "\"\u015alizg\" O'Toole",
        "01004": "Agnes Baker",
        "01005": "Wendy Adams",
        "01101": "Mafijny egzekutor d\u0142ug\u00f3w",
        "01102": "Akolita Srebrnego \u015awitu",
        "01103": "Uparty detektyw",
        "01111": "Gabinet",
        "01112": "Hol",
        "01113": "Strych",
        "01114": "Piwnica",
        "01115": "Salon",
        "01116": "Kap\u0142an ghouli",
        "01117": "Lita Chantler",
        "01118": "Po\u017ceracz cia\u0142",
        "01119": "Lodowy Ghoul",
        "01121": "My\u015bliwy czy zwierzyna?",
        "01121b": "Zamaskowany \u0142owca",
        "01124": "Tw\u00f3j dom",
        "01125": "Rivertown",
        "01126": "Southside",
        "01127": "Southside",
        "01128": "Szpital \u015bw Marii",
        "01129": "Uniwersytet Miskatonic",
        "01130": "Downtown",
        "01131": "Downtown",
        "01132": "Easttown",
        "01133": "Cmentarz",
        "01134": "Northside",
        "01135": "Prze\u015bladuj\u0105cy cie\u0144",
        "01136": "Fa\u0142szywy trop",
        "01137": "Drew \"Cz\u0142owiek-wilk\"",
        "01138": "Herman Collins",
        "01139": "Peter Warren",
        "01140": "Victoria Devereux",
        "01141": "Ruth Turner",
        "01149": "G\u0142\u00f3wna \u015bcie\u017cka",
        "01150": "Lasy Arkham",
        "01151": "Lasy Arkham",
        "01152": "Lasy Arkham",
        "01153": "Lasy Arkham",
        "01154": "Lasy Arkham",
        "01155": "Lasy Arkham",
        "01156": "Miejsce rytua\u0142u",
        "01157": "Um\u00f4rdhoth",
        "01158": "Gniew Um\u00f4rdhotha",
        "01159": "R\u00f3j szczur\u00f3w",
        "01160": "Ghoul s\u0142ugus",
        "01161": "Wyg\u0142odnia\u0142y ghoul",
        "01162": "Chwytaj\u0105ce d\u0142onie",
        "01163": "Gnij\u0105ce szcz\u0105tki",
        "01164": "Sparali\u017cowany ze strachu",
        "01165": "Kakofonia g\u0142os\u00f3w",
        "01166": "Pradawne z\u0142o",
        "01167": "Grobowy ch\u0142\u00f3d",
        "01168": "G\u0119sta mg\u0142a",
        "01169": "Akolita",
        "01170": "Czarownik bractwa",
        "01171": "Tajemnicze pie\u015bni",
        "01172": "Poluj\u0105ce Skrzydlate Dzieci\u0119 Nocy",
        "01173": "Na skrzyd\u0142ach ciemno\u015bci",
        "01174": "Zamkni\u0119te drzwi",
        "01175": "Wrzeszcz\u0105cy Byakhee",
        "01176": "\u017b\u00f3\u0142ty Znak",
        "01177": "Yithian obserwator",
        "01178": "Oferta pot\u0119gi",
        "01179": "Nieust\u0119pliwe Mroczne M\u0142ode",
        "01180": "Kozi pomiot",
        "01181": "M\u0142oda Istota z G\u0142\u0119bin",
        "01182": "Sny o R'lyeh",
    },
}
for _locale, _names in CARD_ONLY_CARD_NAMES.items():
    require(set(_names) == set(CARD_NAMES), f"the {_locale} card-only fixture names a different code set")
    SOURCE_MESSAGES[_locale] = {CARD_NAMES_PACK: {f"{CARD_NAMES_PACK}.{code}": name for code, name in _names.items()}}

# A key the registry says the backend emits that no locale translates, so the
# catalog publishes a real, non-empty `backend.untranslatedKeys` gap.
UNTRANSLATED_KEY = "nightOfTheZealot.theGathering.setup.setOutOfPlay"

# A key with a proven backend icon variable, so native-client fixtures exercise
# value-based icon rendering instead of only text substitutions.
ICON_VARIABLE_KEY = "addToken"
ICON_VARIABLE_NAME = "token"
ICON_VARIABLE_TYPE = "chaosTokenFace"
ICON_VARIABLE_SAMPLE_VALUE = "elderThing"
CHAOS_TOKEN_FACE_VALUES = [
    "skull",
    "cultist",
    "tablet",
    "elderThing",
    "autoFail",
    "elderSign",
    "curse",
    "bless",
    "frost",
    "blood",
]

# A key published as explicitly unavailable, so `unsupportedKeys` is non-zero.
UNSUPPORTED_KEY = "shuffleRemainder"

LANGUAGE_RESOLUTION = (
    ("de", "de"),
    ("en", "en"),
    ("pl", "pl"),
    ("pl-PL", "pl"),
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
        {
            key
            for pack, messages in SOURCE_MESSAGES["en"].items()
            if pack != CARD_NAMES_PACK
            for key in messages
        }
        | {UNTRANSLATED_KEY, UNSUPPORTED_KEY}
    )
    registry = {
        "artifactVersion": GENERATOR_VERSION,
        "source": {
            "files": len(SOURCE_MESSAGES),
            "sha256": fileset_digest(list(build_sources().items())),
        },
        "variableTypes": {
            ICON_VARIABLE_TYPE: {
                "kind": "enum",
                "values": CHAOS_TOKEN_FACE_VALUES,
                "source": "Arkham/ChaosToken/Types.hs",
            }
        },
        "keys": [
            {
                "key": key,
                "variables": [
                    {"name": ICON_VARIABLE_NAME, "type": ICON_VARIABLE_TYPE}
                ]
                if key == ICON_VARIABLE_KEY
                else [],
                "emitters": ["syntheticCatalogFixture"],
                "site": "contracts/fixtures/locale-catalog-source-en.json:1",
                "sites": 1,
            }
            for key in emitted
        ],
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
    the tooling lock: the derivation runs `strict_json`'s readers and parsers too,
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
    except locale_catalog_python_boundary.CapabilityLintError as error:
        raise SystemExit(f"locale-catalog fixture: {error}") from None


def read_repository_files(relative_paths: tuple[str, ...]) -> list[tuple[str, bytes]]:
    """Read real repository files by their repo-relative path, so a rename
    counts as a change just as much as an edit does.

    Callers pass either the generator sources, whose bytes feed the tooling
    lock, or the v1 schemas, whose bytes feed provenance `schemasSha256`.
    """
    entries: list[tuple[str, bytes]] = []
    for relative_path in relative_paths:
        path = ROOT / relative_path
        require(path.is_file(), f"missing provenance input {relative_path}")
        entries.append((relative_path, path.read_bytes()))
    return entries


def derive_generator_sha256(generator_name: str, generator_version: str) -> str:
    """The fixture's `generatorSha256`: a reproducible digest of the generator's
    identity, spelled as `derive_provenance_sha256` spells it. No tooling file
    contents reach it, so a tooling edit cannot move governed bytes; the
    tooling is pinned by TOOLING_LOCK instead.
    """
    return sha256_hex(f"{generator_name}@{generator_version}".encode("utf-8"))


def tooling_lock_bytes(entries: list[tuple[str, bytes]]) -> bytes:
    """The exact committed bytes TOOLING_LOCK must hold for these tooling
    files: a sorted path -> sha256 map plus a format version, in this
    script's one canonical JSON spelling with a trailing newline.
    """
    paths = [path for path, _ in entries]
    require(len(paths) == len(set(paths)), f"{TOOLING_LOCK} inputs name a path twice")
    return canonical_bytes(
        {
            "formatVersion": TOOLING_LOCK_FORMAT_VERSION,
            "files": {path: sha256_hex(data) for path, data in sorted(entries)},
        }
    )


def tooling_lock_failure(expected: bytes, found: bytes | None) -> str | None:
    """Compare the committed lock against the tooling's actual digests,
    returning a failure message that names TOOLING_LOCK (and prints the
    expected bytes, for a reviewed hand update) rather than raising, so the
    self-tests can drive this exact comparison over mutated locks.
    """
    if found == expected:
        return None
    detail = "the file is missing or not a regular file"
    if found is not None:
        try:
            committed = strict_json.strict_json_loads(found, source=TOOLING_LOCK)
        except strict_json.StrictJSONError as error:
            committed = None
            detail = f"it is not strict JSON ({error})"
        if committed is not None:
            wanted = json.loads(expected)
            if not (
                isinstance(committed, dict)
                and set(committed) == {"formatVersion", "files"}
                and committed["formatVersion"] == wanted["formatVersion"]
                and isinstance(committed["files"], dict)
            ):
                detail = f"it is not a format-version {wanted['formatVersion']} tooling lock"
            else:
                actual, recorded = wanted["files"], committed["files"]
                changed = sorted(path for path in set(actual) & set(recorded) if actual[path] != recorded[path])
                missing = sorted(set(actual) - set(recorded))
                unexpected = sorted(set(recorded) - set(actual))
                if changed or missing or unexpected:
                    detail = (
                        f"changed {changed}, missing {missing}, unexpected {unexpected}"
                    )
                else:
                    detail = "its digests match but its bytes are not the canonical spelling"
    return (
        f"{TOOLING_LOCK} does not match the locale-catalog tooling digests: {detail}. "
        "This lock is not a governed contract artifact, so no schemaRevision bump is needed; "
        f"review the tooling change, then replace {TOOLING_LOCK} with exactly:\n"
        + expected.decode("utf-8")
    )


def read_tooling_lock() -> bytes | None:
    path = ROOT / TOOLING_LOCK
    if path.is_symlink() or not path.is_file():
        return None
    return path.read_bytes()


def check_tooling_lock() -> None:
    failure = tooling_lock_failure(
        tooling_lock_bytes(read_repository_files(generator_sources())), read_tooling_lock()
    )
    require(failure is None, failure or "")


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


def build_message_entry(key: str, value: str) -> dict:
    if key == ICON_VARIABLE_KEY:
        icon_variable = {
            "name": ICON_VARIABLE_NAME,
            "source": "named",
            "role": "iconVariable",
        }
        return {
            "form": "message",
            "nodes": [
                {"type": "text", "value": f"{value}."},
                {"type": "var", **icon_variable},
            ],
            "variables": [icon_variable],
        }
    return {
        "form": "message",
        "nodes": [{"type": "text", "value": value}],
        "variables": [],
    }


def build_chunks(constants: dict[str, str]) -> tuple[list[dict], dict[str, bytes]]:
    """Render one chunk per (locale, pack), content-addressed by its own bytes."""
    chunk_records: list[dict] = []
    chunk_files: dict[str, bytes] = {}

    for locale in LOCALES:
        for pack, messages in sorted(SOURCE_MESSAGES[locale].items()):
            entries: dict[str, dict] = {}
            for key, value in sorted(messages.items()):
                entries[key] = build_message_entry(key, value)
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
    generator_sha256 = derive_generator_sha256(constants["generatorName"], GENERATOR_VERSION)
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
    legacy = contract_manifest["legacyCompatibilityChecks"]
    baseline = legacy["baselineResponse"]
    require(
        isinstance(legacy, dict)
        and isinstance(baseline, dict)
        and isinstance(legacy.get("addedCapability"), str)
        and isinstance(legacy.get("globalCapabilities"), list)
        and all(
            isinstance(capability, str)
            for capability in legacy["globalCapabilities"]
        ),
        "contracts/manifest.json has no complete capabilities-fixture authority",
    )
    common_capabilities = {
        "schemaRevision": contract_revision,
        "status": contract_manifest["status"],
        "apiBasePath": contract_manifest["apiBasePath"],
        "nativeClientMinimumRevision": contract_manifest["compatibility"][
            "nativeClientMinimumRevision"
        ],
        "capabilities": sorted(
            [*baseline["capabilities"], *legacy["globalCapabilities"]]
        ),
        "campaignCatalog": {
            "endpoint": "/api/v1/arkham/campaign-catalog",
            "catalogRevision": "1.0123456789abcdef0123456789abcdef",
            "schemaVersion": "1.0.0",
            "digestAlgorithm": "sha256",
        },
    }
    files["capabilities.json"] = canonical_bytes(common_capabilities)
    files["capabilities-locale-catalog.json"] = canonical_bytes(
        {
            **common_capabilities,
            "capabilities": sorted(
                [*common_capabilities["capabilities"], legacy["addedCapability"]]
            ),
            "localeCatalog": {
                "manifestUrl": manifest["manifestPath"],
                "catalogRevision": manifest["catalogRevision"],
                "schemaVersion": manifest["schemaVersion"],
                "defaultLocale": manifest["defaultLocale"],
                "supportedLocales": sorted(record["locale"] for record in manifest["locales"]),
                "manifestSha256": sha256_hex(files[f"{FIXTURE_PREFIX}manifest.json"]),
            },
        }
    )
    files[FIXTURE_OWNERSHIP_FILE] = canonical_bytes(
        {"files": sorted(name for name in files if name != FIXTURE_OWNERSHIP_FILE)}
    )
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
    paths = [
        *sorted(FIXTURE_DIR.glob(f"{FIXTURE_PREFIX}*.json")),
        *(FIXTURE_DIR / name for name in CAPABILITIES_FIXTURES),
    ]
    for path in paths:
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


def validate_icon_variable_fixture(files: dict[str, bytes]) -> None:
    """The native-client fixture must exercise value-based icon rendering."""
    registry = json.loads(files[f"{FIXTURE_PREFIX}backend-registry.json"])
    variable_type = registry.get("variableTypes", {}).get(ICON_VARIABLE_TYPE)
    require(
        variable_type is not None and ICON_VARIABLE_SAMPLE_VALUE in variable_type.get("values", []),
        f"{ICON_VARIABLE_TYPE} must advertise sample value {ICON_VARIABLE_SAMPLE_VALUE!r}",
    )
    matching_keys = [entry for entry in registry.get("keys", []) if entry.get("key") == ICON_VARIABLE_KEY]
    require(len(matching_keys) == 1, f"backend registry must describe {ICON_VARIABLE_KEY}")
    require(
        matching_keys[0].get("variables")
        == [{"name": ICON_VARIABLE_NAME, "type": ICON_VARIABLE_TYPE}],
        f"backend registry must type {ICON_VARIABLE_KEY}.{ICON_VARIABLE_NAME} as {ICON_VARIABLE_TYPE}",
    )

    icon_variable = {
        "name": ICON_VARIABLE_NAME,
        "source": "named",
        "role": "iconVariable",
    }
    found_entry = None
    for name, data in files.items():
        if not name.startswith(f"{FIXTURE_PREFIX}chunk-"):
            continue
        entry = json.loads(data).get("entries", {}).get(ICON_VARIABLE_KEY)
        if entry is not None:
            found_entry = entry
            break
    require(found_entry is not None, f"catalog chunks must include {ICON_VARIABLE_KEY}")
    require(
        icon_variable in found_entry.get("variables", []),
        f"{ICON_VARIABLE_KEY} must declare {ICON_VARIABLE_NAME} as an iconVariable",
    )
    require(
        {"type": "var", **icon_variable} in found_entry.get("nodes", []),
        f"{ICON_VARIABLE_KEY} must contain a var node for {ICON_VARIABLE_NAME}",
    )


def check(files: dict[str, bytes]) -> None:
    failure = check_failure(files, committed_files())
    require(failure is None, f"{failure} -- run `mise run contracts:catalog-fixture`")


def write(files: dict[str, bytes]) -> None:
    existing = {path.name for path in FIXTURE_DIR.glob(f"{FIXTURE_PREFIX}*.json")}
    ownership_path = FIXTURE_DIR / FIXTURE_OWNERSHIP_FILE
    if ownership_path.exists():
        ownership = strict_json.strict_json_loads(
            strict_json.read_governed_worktree_bytes(
                ROOT, f"contracts/fixtures/{FIXTURE_OWNERSHIP_FILE}"
            ),
            source=f"contracts/fixtures/{FIXTURE_OWNERSHIP_FILE}",
        )
        owned = ownership.get("files") if isinstance(ownership, dict) else None
        require(
            isinstance(owned, list)
            and all(
                isinstance(name, str)
                and (name.startswith(FIXTURE_PREFIX) or name in CAPABILITIES_FIXTURES)
                for name in owned
            )
            and len(owned) == len(set(owned)),
            f"contracts/fixtures/{FIXTURE_OWNERSHIP_FILE} is not a valid generated ownership record",
        )
        owned_names = set(owned) | {FIXTURE_OWNERSHIP_FILE}
    else:
        # The first governed rewrite introduces the ownership record.  The
        # checked-in pre-record set is still recognized narrowly from its
        # manifest and known source names; any unrelated matching sentinel is
        # a refusal, never a candidate for deletion.
        previous_manifest = strict_json.strict_json_loads(
            strict_json.read_governed_worktree_bytes(
                ROOT, f"contracts/fixtures/{FIXTURE_PREFIX}manifest.json"
            ),
            source=f"contracts/fixtures/{FIXTURE_PREFIX}manifest.json",
        )
        previous_chunks = {
            f"{FIXTURE_PREFIX}chunk-{chunk['sha256']}.json"
            for locale in previous_manifest["locales"]
            for chunk in locale["chunks"]
        }
        owned_names = {
            f"{FIXTURE_PREFIX}manifest.json",
            f"{FIXTURE_PREFIX}backend-registry.json",
            *build_sources(),
            *previous_chunks,
        }
    unowned = sorted(existing - owned_names)
    require(
        not unowned,
        "refusing to delete pre-existing locale-catalog fixture paths without this invocation's "
        f"ownership record: {unowned}",
    )
    for name in sorted(existing & owned_names - set(files)):
        path = FIXTURE_DIR / name
        strict_json.read_governed_worktree_bytes(ROOT, f"contracts/fixtures/{name}")
        path.unlink()
    for name, data in sorted(files.items()):
        strict_json.write_governed_worktree_bytes(ROOT, f"contracts/fixtures/{name}", data)


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
        except locale_catalog_python_boundary.CapabilityLintError:
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
    """`schemasSha256` is a digest of real repository bytes and
    `generatorSha256` a digest of the generator's identity, so prove each
    moves the revision: appending one byte to either published v1 schema,
    renaming or dropping one, changing the generator name or version, a locale
    source or the contract revision must each produce a different revision.
    The generator's tooling bytes are the lock's job, proven separately by
    `run_tooling_lock_self_tests`.
    """
    sources = generator_sources()
    generator_inputs = read_repository_files(sources)
    schema_inputs = read_repository_files(SCHEMA_SOURCES)
    constants = catalog_constants()
    provenance = manifest["provenance"]
    runtime = runtime_identity()

    require(
        provenance["generatorSha256"]
        == derive_generator_sha256(constants["generatorName"], provenance["generator"]["version"]),
        "Self-test failure: generatorSha256 is not derived from the generator name and version",
    )
    require(
        GENERATOR_ENTRY in sources and "scripts/strict_json.py" in sources,
        "Self-test failure: the tooling lock's source closure must contain this module and every "
        f"local helper it executes; got {sources}",
    )
    for source in sources:
        require(
            (ROOT / source).is_file(),
            f"Self-test failure: the tooling lock's source closure names a missing file {source}",
        )
    require(
        provenance["schemasSha256"] == fileset_digest(schema_inputs),
        "Self-test failure: schemasSha256 is not the digest of the published v1 schemas",
    )
    require(
        RUNTIME_PROFILE in sources and runtime["python"]["version"] == "3.14.7",
        "Self-test failure: the tooling lock omitted the validated runtime identity",
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

    edited_schemas = fileset_digest([(path, data + b"\n") for path, data in schema_inputs])
    renamed_schemas = fileset_digest(
        [(path + ".moved", data) for path, data in schema_inputs]
    )
    partial_schemas = fileset_digest(schema_inputs[:1])
    bumped_generator = derive_generator_sha256(constants["generatorName"], "9.9.9")
    renamed_generator = derive_generator_sha256("something-else", provenance["generator"]["version"])
    edited_sources = fileset_digest(
        [
            (name, data + b" ")
            for name, data in build_sources().items()
        ]
    )

    for label, digest in (
        ("an edited v1 schema", perturbed(schemas_sha256=edited_schemas)),
        ("a renamed v1 schema", perturbed(schemas_sha256=renamed_schemas)),
        ("a dropped v1 schema", perturbed(schemas_sha256=partial_schemas)),
        ("an edited locale source", perturbed(locale_sources_sha256=edited_sources)),
        (
            "a different generator version",
            perturbed(generator_version="9.9.9", generator_sha256=bumped_generator),
        ),
        (
            "a different generator name",
            perturbed(generator_name="something-else", generator_sha256=renamed_generator),
        ),
        ("a different generator digest", perturbed(generator_sha256=bumped_generator)),
        ("a different contract revision", perturbed(contract_revision="0.0.1")),
    ):
        require(
            digest != baseline,
            f"Self-test failure: {label} left the provenance digest unchanged, so it is not "
            "actually bound into the catalog revision",
        )

    require(
        bumped_generator != provenance["generatorSha256"]
        and renamed_generator != provenance["generatorSha256"],
        "Self-test failure: a different generator name or version produced the same generatorSha256",
    )
    require(
        edited_schemas != provenance["schemasSha256"]
        and renamed_schemas != provenance["schemasSha256"],
        "Self-test failure: a mutated schema input produced the same fileset digest",
    )
    require(
        edited_sources != provenance["localeSourcesSha256"],
        "Self-test failure: an edited locale source produced the same fileset digest",
    )

    run_tooling_lock_self_tests(generator_inputs)


def run_tooling_lock_self_tests(generator_inputs: list[tuple[str, bytes]]) -> None:
    """The tooling is pinned by TOOLING_LOCK, not by governed bytes. Prove both
    halves: every way of perturbing a tooling input (an edited workflow,
    launcher, generator source or helper, an added, dropped or renamed file)
    fails the lock check with a message that names the lock, while the governed
    fixture derivation -- which reads no tooling bytes -- is byte-identical.
    """
    expected = tooling_lock_bytes(generator_inputs)
    committed = read_tooling_lock()
    require(
        tooling_lock_failure(expected, committed) is None,
        "self-test precondition: the committed tooling lock must match before mutations can "
        "prove anything",
    )
    paths = [path for path, _ in generator_inputs]
    workflow = ".github/workflows/contracts.yml"
    require(
        workflow in paths and GENERATOR_ENTRY in paths and RUNTIME_PROFILE in paths,
        "Self-test failure: the tooling lock does not cover the workflows, this generator and the "
        "runtime profile",
    )

    def edited(target: str) -> list[tuple[str, bytes]]:
        return [(path, data + b"\n" if path == target else data) for path, data in generator_inputs]

    perturbations: list[tuple[str, list[tuple[str, bytes]]]] = [
        (f"an edited {path}", edited(path))
        for path in (
            workflow,
            "mise.toml",
            "Dockerfile",
            "scripts/run-locale-catalog-python.sh",
            GENERATOR_ENTRY,
            "scripts/strict_json.py",
        )
    ]
    perturbations += [
        ("an added tooling file", generator_inputs + [("scripts/new_helper.py", b"VALUE = 1\n")]),
        ("a dropped tooling file", [entry for entry in generator_inputs if entry[0] != workflow]),
        (
            "a renamed tooling file",
            [(path + ".moved" if path == workflow else path, data) for path, data in generator_inputs],
        ),
    ]
    for label, inputs in perturbations:
        failure = tooling_lock_failure(tooling_lock_bytes(inputs), committed)
        require(
            failure is not None and TOOLING_LOCK in failure,
            f"Self-test failure: {label} was not caught by the tooling lock check, or the "
            "failure did not name the lock",
        )

    lock = json.loads(expected)
    first = sorted(lock["files"])[0]
    lock_mutations: list[tuple[str, bytes | None]] = [
        ("a missing lock", None),
        ("an appended byte", expected + b" "),
        ("a non-canonical spelling", json.dumps(lock, sort_keys=True).encode("utf-8") + b"\n"),
        ("a duplicate key", expected.replace(b'"formatVersion"', b'"formatVersion": 1, "formatVersion"', 1)),
        ("a different format version", canonical_bytes({**lock, "formatVersion": 2})),
        ("an extra field", canonical_bytes({**lock, "generatorSha256": "0" * 64})),
        ("a mutated digest", canonical_bytes({**lock, "files": {**lock["files"], first: "0" * 64}})),
        (
            "a dropped entry",
            canonical_bytes({**lock, "files": {k: v for k, v in lock["files"].items() if k != first}}),
        ),
        ("an extra entry", canonical_bytes({**lock, "files": {**lock["files"], "zz/extra": "0" * 64}})),
    ]
    for label, found in lock_mutations:
        failure = tooling_lock_failure(expected, found)
        require(
            failure is not None and TOOLING_LOCK in failure,
            f"Self-test failure: {label} in {TOOLING_LOCK} survived the tooling lock check",
        )

    # The governed half: tooling bytes never reach the derivation, so the
    # generator digest is a function of name and version alone. The
    # end-to-end proof -- an edited workflow in a probe tree fails this check
    # naming the lock while the rewritten governed fixtures stay byte-identical
    # -- is `test_tooling_lock_end_to_end` in
    # scripts/test_locale_catalog_python_boundary.py.
    constants = catalog_constants()
    require(
        derive_generator_sha256(constants["generatorName"], GENERATOR_VERSION)
        == json.loads(build_all()[f"{FIXTURE_PREFIX}manifest.json"])["provenance"]["generatorSha256"],
        "Self-test failure: the fixture's generatorSha256 is not derived from the generator "
        "identity alone",
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="verify without writing")
    arguments = parser.parse_args()

    strict_json.run_self_tests()
    strict_json.run_governed_bytes_self_tests()
    strict_json.run_governed_path_self_tests(ROOT)
    # The published v1 schemas are validated by the in-repository subset
    # validator rather than an installed one, so its fail-closed proofs -- an
    # unimplemented keyword is a refusal, and a Python-only regular-expression
    # construct is never silently given Python's meaning -- run on the same
    # governed route that depends on them.
    json_schema_subset.run_self_tests()

    files = build_all()
    validate_against_published_schemas(files)
    validate_icon_variable_fixture(files)

    if arguments.check:
        check(files)
        check_tooling_lock()
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
        f"{FIXTURE_DIR.relative_to(ROOT)}."
    )


if __name__ == "__main__":
    main()
