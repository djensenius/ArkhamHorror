#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "tree-sitter==0.25.2",
#   "tree-sitter-haskell==0.23.1",
# ]
# ///

"""Enumerate the localization keys the Haskell backend emits to clients.

The backend sends story content as opaque `I18nEntry` keys plus typed
variables (`backend/arkham-api/library/Arkham/I18n.hs`). Native clients can
only render that content if the published locale catalog actually resolves
those keys, so the catalog build needs a machine-derived statement of what the
backend emits — not a hand-maintained list, and not an inference from a couple
of contract fixtures.

This extractor parses every backend module with tree-sitter's Haskell grammar
(a real CST, not regular expressions or indentation heuristics) and resolves,
for each key-emitting call site:

  * the lexical scope stack built by `scope`/`unscoped`/`popScope`/`cardI18n`
    and by per-campaign/scenario aliases such as `campaignI18n`,
    `scenarioI18n`, `standaloneI18n` (resolved across modules through their
    imports), and
  * the variables attached by `countVar`/`withXp`/`nameVar`/... at that site.

Sites whose key or scope is not a literal (built from an enum, a counter, or a
runtime value) are never guessed at: they are recorded as `dynamic` with their
location and reason, so the artifact states exactly how much of the surface it
covers.

The result is written to `backend/arkham-api/i18n-emitted-keys.json` and is
consumed by the locale-catalog generator, which requires every emitted key that
the default locale translates to render safely. `--check` re-derives the
artifact and fails if it drifts, so adding a new backend `ikey` fails CI until
the catalog is regenerated against it.
"""

import argparse
import hashlib
import json
import re
import sys
from pathlib import Path

import tree_sitter_haskell
from tree_sitter import Language, Parser

ROOT = Path(__file__).resolve().parents[1]
LIBRARY = ROOT / "backend" / "arkham-api" / "library"
ARTIFACT = ROOT / "backend" / "arkham-api" / "i18n-emitted-keys.json"
ARTIFACT_VERSION = "1.0.0"

# Modules that *define* the i18n DSL. Their bodies emit keys on behalf of a
# caller whose scope is unknown at the definition site, so their call sites are
# modelled through WRAPPERS below instead of being scanned for emissions.
DSL_MODULES = {
    "Arkham.I18n",
    "Arkham.Text",
    "Arkham.FlavorText",
    "Arkham.Helpers.FlavorText",
    "Arkham.Message.Lifted.Choose",
    "Arkham.Message.Lifted.Scenario",
    "Arkham.Message.Lifted.Prompt",
}

# Scope combinators from Arkham/I18n.hs. `reset` clears the stack first.
# How many arguments each scope primitive actually takes. Anything passed
# beyond that is applied to the *result*, so it is outside the scope:
# `unscoped (countVar 1 $ labeled' "x") do …` labels inside the reset but runs
# the block in the caller's scope.
SCOPE_ARITY = {
    "scope": 2,
    "unscoped": 1,
    "withI18n": 1,
    "cardI18n": 1,
    "standaloneI18n": 2,
    "popScope": 1,
}

SCOPE_PRIMITIVES = {
    "scope": {"reset": False, "literal_arg": 0},
    "cardI18n": {"reset": True, "segments": ["cards"]},
    "withI18n": {"reset": True, "segments": []},
    "unscoped": {"reset": True, "segments": []},
    "popScope": {"pop": 1},
    "standaloneI18n": {"reset": True, "segments": ["standalone"], "literal_arg": 0},
}

# Helpers that both push a scope and emit keys of their own
# (Arkham/Helpers/FlavorText.hs).
WRAPPERS = {
    # setup body = scope "setup" $ flavor (unscoped (setTitle "setup") >> body)
    "setup": {"push": ["setup"], "emits": [{"key": "setup", "unscoped": True}]},
    # setup' body = scope "setup" $ flavor (setTitle "title" >> body)
    "setup'": {"push": ["setup"], "emits": [{"key": "title", "scoped": True}]},
    # additionalRules s =
    #   scope "rules" $ scope s $ flavor (setTitle "title" >> compose (h3 "title" >> p "body"))
    "additionalRules": {
        "push": ["rules"],
        "push_literal_arg": 0,
        "emits": [{"key": "title", "scoped": True}, {"key": "body", "scoped": True}],
    },
}

# Key-emitting helpers: which argument holds the key, and which suffixes the
# helper appends. `label` marks the labelKey rewrite from Arkham/I18n.hs.
EMITTERS = {
    "ikey": {"arg": 0, "suffixes": [""]},
    "ikey'": {"arg": 0, "suffixes": [""]},
    "toI18n": {"arg": 0, "suffixes": [""]},
    "i18n": {"arg": 0, "suffixes": [""]},
    "i18nEntry": {"arg": 0, "suffixes": [""]},
    "storyI": {"arg": 0, "suffixes": [""]},
    "i18nWithTitle": {"arg": 0, "suffixes": [".title", ".body"]},
    "resolution": {"arg": 0, "suffixes": [".title", ".body"]},
    "resolutionWithXp": {"arg": 0, "suffixes": [".title", ".body"], "vars": {"xp": "integer"}},
    "p": {"arg": 0, "suffixes": [""]},
    "li": {"arg": 0, "suffixes": [""]},
    "h": {"arg": 0, "suffixes": [""]},
    "h1": {"arg": 0, "suffixes": [""]},
    "h3": {"arg": 0, "suffixes": [""]},
    "h_": {"arg": 0, "suffixes": [""]},
    "setTitle": {"arg": 0, "suffixes": [""]},
    "labeled'": {"arg": 0, "suffixes": [""], "label": True},
    "labeledI": {"arg": 0, "suffixes": [""], "label": True, "reset": True},
    "labeledI18n": {"arg": 0, "suffixes": [""], "label": True},
    "invalidLabeled'": {"arg": 0, "suffixes": [""], "label": True},
    "questionLabeled'": {"arg": 0, "suffixes": [""], "label": True},
    "questionLabeledI": {"arg": 0, "suffixes": [""], "label": True, "reset": True},
    "skip_": {"literal": "skip", "label": True},
}

# `p.green`, `li.nested`, `h.blue`: presentation modifiers on the flavor-text
# DSL. They change styling, never the key.
# The value is how many arguments the modifier inserts before the key, so
# `li.validate cond "key"` still resolves to "key".
PRESENTATION_MODIFIERS = {
    "green": 0,
    "blue": 0,
    "red": 0,
    "nested": 0,
    "codex": 0,
    "right": 0,
    "center": 0,
    "small": 0,
    "italic": 0,
    "bold": 0,
    "validate": 1,
}


def emitter_for(name: str) -> dict | None:
    """The emitter for a call head, seeing through presentation modifiers."""
    emitter = EMITTERS.get(name)
    if emitter is not None:
        return emitter
    head, _, modifier = name.partition(".")
    if not modifier or modifier not in PRESENTATION_MODIFIERS:
        return None
    base = EMITTERS.get(head)
    if base is None or "arg" not in base:
        return base
    shifted = dict(base)
    shifted["arg"] = base["arg"] + PRESENTATION_MODIFIERS[modifier]
    return shifted


# Amount prompts send bare `$name` labels that the client resolves under
# `choice.` (frontend/src/arkham/components/Question.vue paymentChoiceLabel).
AMOUNT_LABEL_FUNCTIONS = {
    "chooseAmount",
    "chooseAmount'",
    "chooseAmounts",
    "chooseAmounts'",
    "chooseAmountM",
}
# Readers and updaters of a previously chosen amount address it by the same
# `$name` string the prompt used. The prompt already registered the key; these
# are lookups, and counting them again invents a root-level key that no locale
# has (`ammo`, `charge`, `secret`) and that nothing ever renders.
AMOUNT_READER_PATTERN = re.compile(r"(?:^|[a-z])[Aa]mounts?$")


def is_amount_reader(name: str | None) -> bool:
    return (
        name is not None
        and name not in AMOUNT_LABEL_FUNCTIONS
        and AMOUNT_READER_PATTERN.search(name) is not None
    )


# Variable attachments resolvable at a call site (Arkham/I18n.hs).
VARIABLE_BINDERS = {
    "countVar": [("count", "integer")],
    "withXp": [("xp", "integer")],
    "nameVar": [("name", "text")],
    "cardNameVar": [("name", "text"), ("__name", "text")],
    "investigatorNameVar": [("iname", "text"), ("__iname", "text")],
    "tokenVar": [("token", "text")],
    "skillVar": [("skill", "text")],
    "skillIconVar": [("skillIcon", "text")],
}
# numberVar/keyVar/withVar take the name as a literal first argument.
NAMED_VARIABLE_BINDERS = {
    "numberVar": "integer",
    "keyVar": "text",
}

# A literal "$key" in backend source is a wire token the client resolves
# verbatim (see handleI18n in frontend/src/arkham/i18n.ts), e.g.
# `Label "$continue"` in Arkham/Message.hs and `prompt iid "$label.continue"`.
DOLLAR_KEY = re.compile(r"^\$([A-Za-z0-9_][A-Za-z0-9_.'\[\]-]*)$")

# A key must have no empty segment. A literal like `"customizations."` is the
# static half of a key whose remainder is computed at runtime: that is a
# dynamic site, not a key.
KEY_PATTERN = re.compile(r"^[^\s.]+(\.[^\s.]+)*$")

PARSER = Parser(Language(tree_sitter_haskell.language()))

# tree-sitter-haskell (0.23.1, its latest release) does not accept two GHC
# extensions this codebase uses: `OverloadedLabels` written with a string
# literal (`#"+1"`, chaos token labels), a type application of a string literal
# (`@"circled"`), a pattern type signature inside a do-bind
# (`tag :: Text <- ...`), and operators containing `?`. None of them can ever be
# an i18n key, so each is replaced
# with an opaque token of exactly the same length, which keeps every byte
# offset and line number intact. This is the only preprocessing performed, and
# after it *any* remaining parse error is a hard failure.
OVERLOADED_LABEL_STRING = re.compile(rb'#"(?:[^"\\\n]|\\.)*"')
TYPE_LEVEL_STRING = re.compile(rb'@"(?:[^"\\\n]|\\.)*"')
# `tag :: Text <- o .: "tag"` — a pattern type signature inside a do-bind.
DO_BIND_SIGNATURE = re.compile(rb"(?m)^([ \t]*[a-z][A-Za-z0-9_']*[ \t]*)::[^\n<]*(?=<-)")
# Operators containing `?` (`.:?`, `!!?`, `^?`, …) are read as implicit-parameter
# syntax by the grammar; the `?` is swapped for another symbol character of the
# same width, which cannot change where a key literal sits.
OPERATOR_QUESTION_MARK = re.compile(rb"(?<=[.!:<>=+*/&|^~-])\?")


def preprocess(source: bytes) -> bytes:
    source = OVERLOADED_LABEL_STRING.sub(
        lambda match: b"(" + b"l" * (len(match.group(0)) - 2) + b")", source
    )
    source = TYPE_LEVEL_STRING.sub(lambda match: b"@L" + b"l" * (len(match.group(0)) - 2), source)
    source = DO_BIND_SIGNATURE.sub(
        lambda match: match.group(1) + b" " * (len(match.group(0)) - len(match.group(1))), source
    )
    return OPERATOR_QUESTION_MARK.sub(b"!", source)


# `rec` is a keyword to the grammar (RecursiveDo) but an ordinary identifier to
# GHC unless that extension is on, and this codebase uses it as one.
SOFT_KEYWORDS = (b"rec", b"proc", b"mdo")
# `[n| (…) <- xs]`: the grammar wants whitespace before the comprehension bar.
COMPREHENSION_BAR = re.compile(rb"(?<=[A-Za-z0-9_)\]])\| ")


def code_mask(source: bytes) -> bytearray:
    """Marks the bytes that are code, so repairs never touch text.

    A rewrite inside a string literal could change an emitted key, and one
    inside a comment is pointless; both are excluded by a single pass over the
    module that tracks `--`/`{- -}` comments and string and character
    literals.
    """
    mask = bytearray(b"\x01" * len(source))
    index = 0
    length = len(source)
    while index < length:
        byte = source[index : index + 1]
        pair = source[index : index + 2]
        if pair == b"{-":
            depth = 1
            index += 2
            while index < length and depth:
                if source[index : index + 2] == b"{-":
                    depth += 1
                    index += 2
                elif source[index : index + 2] == b"-}":
                    depth -= 1
                    index += 2
                else:
                    index += 1
            mask[min(index, length) - 1 : index] = b"\x00" * 0
            continue
        if pair == b"--":
            while index < length and source[index : index + 1] != b"\n":
                mask[index] = 0
                index += 1
            continue
        if byte == b'"':
            mask[index] = 0
            index += 1
            while index < length:
                mask[index] = 0
                if source[index : index + 1] == b"\\":
                    mask[index + 1 : index + 2] = b"\x00"
                    index += 2
                    continue
                if source[index : index + 1] == b'"':
                    index += 1
                    break
                index += 1
            continue
        if byte == b"'" and source[index : index + 3] in (b"'\\n'", b"' '") or (
            byte == b"'" and index + 2 < length and source[index + 2 : index + 3] == b"'"
            and source[index - 1 : index] not in (b"'",) and not source[index - 1 : index].isalnum()
        ):
            end = index + 3 if source[index + 2 : index + 3] == b"'" else index + 4
            for position in range(index, min(end, length)):
                mask[position] = 0
            index = end
            continue
        index += 1
    # Comment bodies are marked by the loop above; block comments are handled
    # by masking their whole span here.
    for match in re.finditer(rb"\{-.*?-\}", source, re.S):
        for position in range(match.start(), match.end()):
            mask[position] = 0
    return mask


def repair(source: bytes) -> bytes:
    """Length-preserving rewrites for constructs the grammar mis-reads.

    Applied only to a module that failed to parse, and only to code bytes, so
    no string literal — and therefore no key — can be altered. Every rewrite
    keeps byte offsets, so reported line numbers stay true.
    """
    mask = code_mask(source)
    patched = bytearray(source)

    for keyword in SOFT_KEYWORDS:
        for match in re.finditer(rb"(?<![A-Za-z0-9_'])" + keyword + rb"(?![A-Za-z0-9_'])", source):
            if not all(mask[position] for position in range(match.start(), match.end())):
                continue
            replacement = _safe_identifier(source, keyword)
            patched[match.start() : match.end()] = replacement

    for match in COMPREHENSION_BAR.finditer(bytes(patched)):
        if all(mask[position] for position in range(match.start(), match.end())):
            patched[match.start() : match.end()] = b" |"

    return bytes(patched)


def _safe_identifier(source: bytes, keyword: bytes) -> bytes:
    """A same-length identifier the module does not already use."""
    for digit in b"0123456789":
        candidate = keyword[:1] + bytes([digit]) + keyword[2:]
        if not re.search(rb"(?<![A-Za-z0-9_'])" + candidate + rb"(?![A-Za-z0-9_'])", source):
            return candidate
    raise SystemExit(f"backend-i18n: cannot rename {keyword!r} without colliding")


def _relative_path(path: Path, library: Path | None) -> str:
    """Repo-relative path, or library-relative when a synthetic library is used."""
    base = ROOT if library in (None, LIBRARY) else library
    try:
        return path.relative_to(base).as_posix()
    except ValueError:
        return path.as_posix()


def parse_module(path: Path, source: bytes, library: Path | None = None):
    """Parses one module. A module that will not parse fails the run.

    tree-sitter's Haskell grammar does not cover every GHC extension this
    codebase uses. `preprocess` neutralizes the ones that appear everywhere and
    `repair` handles the rest, on code bytes only, without moving a single
    offset. What is left is a module the extractor cannot read — and there is
    no safe way to skip one: a waiver rests on a lexical guess about what an
    unreadable file contains, which is exactly the reasoning this registry
    exists to replace.
    """
    tree = PARSER.parse(preprocess(source))
    if not tree.root_node.has_error:
        return tree

    repaired = PARSER.parse(repair(preprocess(source)))
    if not repaired.root_node.has_error:
        return repaired

    relative = _relative_path(path, library)
    raise SystemExit(
        f"backend-i18n: {relative} does not parse (line {first_error(repaired.root_node)}) — "
        "extend `preprocess`/`repair` or the grammar; the extractor will not skip a module it "
        "cannot read"
    )


def first_error(node) -> int | None:
    if node.type == "ERROR" or node.is_missing:
        return node.start_point[0] + 1
    for child in node.children:
        found = first_error(child)
        if found is not None:
            return found
    return None


def text_of(node, source: bytes) -> str:
    return source[node.start_byte : node.end_byte].decode("utf-8", errors="replace")


def string_literal(node, source: bytes) -> str | None:
    """Returns the value of a Haskell string literal node, or None."""
    if node is None:
        return None
    if node.type in {"literal", "exp_literal"} and node.child_count == 1:
        return string_literal(node.children[0], source)
    if node.type != "string":
        return None
    raw = text_of(node, source)
    if not (raw.startswith('"') and raw.endswith('"')):
        return None
    body = raw[1:-1]
    if "\\" in body:
        try:
            return json.loads(f'"{body}"')
        except json.JSONDecodeError:
            return None
    return body


def significant_children(node):
    return [child for child in node.children if child.is_named and child.type != "comment"]


def flatten_application(node, source: bytes):
    """Returns (head_name, [argument nodes]) for an application, else None."""
    if node.type != "apply":
        return None
    children = significant_children(node)
    if not children:
        return None
    head, args = children[0], children[1:]
    while head.type == "apply":
        inner = flatten_application(head, source)
        if inner is None:
            return None
        head_name, inner_args = inner
        return head_name, inner_args + args
    if head.type in {"variable", "constructor", "operator", "qualified", "projection"}:
        return text_of(head, source), args
    return None


def infix_parts(node, source: bytes):
    """Returns (left, operator, right) for `a $ b` style infix nodes."""
    children = significant_children(node)
    if len(children) != 3:
        return None
    left, operator, right = children
    return left, text_of(operator, source), right


class ModuleIndex:
    """Modules, their imports, and their local i18n scope aliases."""

    def __init__(self):
        self.by_module: dict[str, dict] = {}
        self.aliases: dict[tuple[str, str], dict] = {}
        self._closures: dict[tuple[str, str, str], list[str]] = {}

    def add(self, module: str, record: dict) -> None:
        self.by_module[module] = record

    def alias_parameters(self, module: str, name: str) -> list[str]:
        """Ordered parameter names of the alias `name` as seen from `module`."""
        record = self.by_module.get(module)
        if record is None:
            return []
        definition = record["aliases"].get(name)
        if definition is None:
            for provider in self._providers_of(module, name):
                definition = self.by_module[provider]["aliases"].get(name)
                if definition is not None:
                    break
        return list(definition.get("parameters", [])) if definition else []

    def _providers_of(self, module: str, name: str, table: str = "aliases") -> list[str]:
        record = self.by_module.get(module)
        if record is None:
            return []
        providers: list[str] = []
        for entry in record["imports"]:
            if entry["qualified"]:
                continue
            if entry["only"] is not None and name not in entry["only"]:
                continue
            if entry["hiding"] is not None and name in entry["hiding"]:
                continue
            providers.extend(self._exporters_of(entry["module"], name, table))
        return list(dict.fromkeys(providers))

    def defining_modules(self, module: str, name: str) -> list[str]:
        """Where an unqualified top-level name used in `module` is defined.

        Two scenarios each define their own `scenarioFlavorText`; resolving one
        helper's callers by bare name would hand every scenario's entries to
        every other. Scoping answers which definition a call site means.
        """
        record = self.by_module.get(module)
        if record is None:
            return []
        if name in record["definitions"]:
            return [module]
        return self._providers_of(module, name, "definitions")

    def resolve_alias(
        self,
        module: str,
        name: str,
        seen: frozenset = frozenset(),
        arguments: dict[str, str] | None = None,
    ) -> dict | None:
        """Resolves a scope alias (campaignI18n, scenarioI18n, ...) to its effect.

        Haskell scoping decides this, not proximity: a local definition wins,
        otherwise the name must arrive through an import that actually carries
        it. Campaign helpers reach their aliases through `module` re-exports,
        so re-exports are followed, but only re-exports — a definition sitting
        in some module that merely happens to be reachable is not in scope and
        must not turn the site ambiguous.
        """
        if (module, name) in seen:
            return {"dynamic": "recursive alias"}
        record = self.by_module.get(module)
        if record is None:
            return None

        definition = record["aliases"].get(name)
        if definition is not None:
            return self._effect_of(module, definition, seen | {(module, name)}, arguments)

        effects = []
        for provider in self._providers_of(module, name):
            other = self.by_module.get(provider)
            if other is None or name not in other["aliases"]:
                continue
            effect = self._effect_of(
                provider, other["aliases"][name], seen | {(module, name)}, arguments
            )
            if effect not in effects:
                effects.append(effect)

        if not effects:
            return None
        if len(effects) > 1:
            concrete = [effect for effect in effects if not effect.get("dynamic")]
            distinct = {tuple(effect.get("segments", [])) for effect in concrete}
            if len(distinct) != 1:
                return {"dynamic": f"ambiguous alias {name}"}
            return concrete[0]
        return effects[0]

    def _exporters_of(self, module: str, name: str, table: str = "aliases") -> list[str]:
        """Modules that supply `name` when `module` is imported unqualified.

        Re-export chains are followed to their end. Cycles are broken with a
        visited set rather than a depth cap, because a cap silently answers
        "nothing exports this" for deep hubs and strands every key behind them.
        """
        cached = self._closures.get((module, name, table))
        if cached is not None:
            return cached
        found = self._walk_exports(module, name, set(), table)
        self._closures[(module, name, table)] = found
        return found

    def _walk_exports(self, module: str, name: str, visited: set[str], table: str = "aliases") -> list[str]:
        if module in visited:
            return []
        visited.add(module)
        record = self.by_module.get(module)
        if record is None:
            return []

        exports = record["exports"]
        found: list[str] = []
        targets: list[str] = []
        exports_own_definitions = exports is None
        if exports is not None:
            for reexported in exports["modules"]:
                for target in self._reexport_targets(record, reexported, name):
                    if target == module:
                        # `module Arkham.X (module Arkham.X, ...) where`: the
                        # idiomatic way to re-export everything defined here.
                        exports_own_definitions = True
                    else:
                        targets.append(target)

        if name in record[table] and (
            exports_own_definitions or (exports is not None and name in exports["names"])
        ):
            found.append(module)
        for target in targets:
            found.extend(self._walk_exports(target, name, visited, table))
        return list(dict.fromkeys(found))

    @staticmethod
    def _reexport_targets(record: dict, exported: str, name: str) -> list[str]:
        """Real modules behind a `module X` export, for one name.

        `module Arkham.Scenarios...Helpers (module X) where` paired with
        `import Arkham.Campaigns...Helpers as X` re-exports the *aliased*
        module, so an export name is matched against import aliases before it
        is treated as a module name of its own. A `module X` export only
        carries what the matching import brought in, so an import list on that
        import narrows the re-export too — otherwise a helper that re-exports
        two unrelated names from another scenario looks like a second, rival
        definition of `scenarioI18n`.
        """
        targets = []
        for entry in record["imports"]:
            if entry["qualified"] or entry["alias"] != exported:
                continue
            if entry["only"] is not None and name not in entry["only"]:
                continue
            if entry["hiding"] is not None and name in entry["hiding"]:
                continue
            targets.append(entry["module"])
        if targets:
            return targets
        return [] if any(entry["alias"] == exported for entry in record["imports"]) else [exported]

    def _effect_of(
        self,
        module: str,
        definition: dict,
        seen: frozenset,
        arguments: dict[str, str] | None = None,
    ) -> dict:
        effect = {"reset": False, "segments": [], "dynamic": None}
        for step in definition["steps"]:
            kind = step["kind"]
            if kind == "literal_scope":
                effect["segments"].append(step["value"])
            elif kind == "template_scope":
                rendered = render_template(step["parts"], arguments or {})
                if rendered is None:
                    return {"dynamic": "scope argument not a literal"}
                effect["segments"].append(rendered)
            elif kind == "alternatives_scope":
                effect["segments"].append(tuple(step["values"]))
            elif kind == "pop":
                for _ in range(step["count"]):
                    if effect["segments"]:
                        effect["segments"].pop()
                    else:
                        return {"dynamic": "popScope below the resolved scope"}
            elif kind == "reset":
                effect["reset"] = True
                effect["segments"] = list(step.get("segments", []))
            elif kind == "alias":
                nested = self.resolve_alias(module, step["name"], seen, arguments)
                if nested is None or nested.get("dynamic"):
                    return {"dynamic": f"alias {step['name']}"}
                if nested.get("reset"):
                    effect["reset"] = True
                    effect["segments"] = list(nested["segments"]) + effect["segments"]
                else:
                    effect["segments"] = list(nested["segments"]) + effect["segments"]
            elif kind == "dynamic":
                return {"dynamic": step["value"]}
        return effect


def scope_steps_of_expression(node, source: bytes, parameters: set[str]) -> list[dict]:
    """Describes the scope effect of an alias definition body, outermost first."""
    steps: list[dict] = []
    current = node
    guard = 0
    while current is not None and guard < 64:
        guard += 1
        if current.type in {"exp", "parens"} and len(significant_children(current)) == 1:
            current = significant_children(current)[0]
            continue
        if current.type == "infix":
            parts = infix_parts(current, source)
            if parts is None:
                break
            left, operator, right = parts
            if operator not in {"$", "."}:
                break
            steps.extend(scope_steps_of_expression(left, source, parameters))
            current = right
            continue
        application = flatten_application(current, source)
        if application is not None:
            name, args = application
            steps.extend(_steps_for_call(name, args, source, parameters))
            current = args[-1] if args else None
            continue
        if current.type == "variable":
            name = text_of(current, source)
            if name in parameters:
                break
            if name in SCOPE_PRIMITIVES or name.endswith("I18n"):
                steps.extend(_steps_for_call(name, [], source, parameters))
            break
        break
    return steps


MAX_SCOPE_COMBINATIONS = 64


def scope_alternatives(node, source: bytes, parameters: set[str], depth: int = 0, context=None, site=None):
    """Every literal a scope expression can evaluate to, or None.

    `scope (if headedWest then "west" else "east")` and `scope version`, where
    `version` is a local binding over literals, both name a small closed set of
    real keys. Enumerating that set keeps the requirement honest — the catalog
    must carry every branch — instead of writing the site off as dynamic.
    """
    if depth > 8:
        return None
    site = site if site is not None else node
    if node.type in {"exp", "parens"} and len(significant_children(node)) == 1:
        return scope_alternatives(significant_children(node)[0], source, parameters, depth + 1, context, site)

    literal = string_literal(node, source)
    if literal is not None:
        return [literal]

    if node.type == "conditional":
        decided = decide_conditional(node, site, source) or decide_by_matching_condition(
            node, site, source
        )
        if decided is not None:
            return scope_alternatives(decided, source, parameters, depth + 1, context, site)
        branches = [child for child in significant_children(node) if child.type not in {"if", "then", "else"}]
        values: list[str] = []
        for branch in branches[1:]:
            resolved = scope_alternatives(branch, source, parameters, depth + 1, context, site)
            if resolved is None:
                return None
            values.extend(resolved)
        return values or None

    if node.type == "case":
        values = []
        for alternative in node.children:
            if alternative.type != "alternatives":
                continue
            for entry in alternative.children:
                if entry.type != "alternative":
                    continue
                body = significant_children(entry)[-1] if significant_children(entry) else None
                if body is None:
                    return None
                if body.type == "match":
                    inner = significant_children(body)
                    body = inner[-1] if inner else None
                resolved = scope_alternatives(body, source, parameters, depth + 1, context, site) if body else None
                if resolved is None:
                    return None
                values.extend(resolved)
        return values or None

    if node.type == "variable":
        name = text_of(node, source)
        if name in parameters:
            return None
        binding = local_binding(node, name, source)
        if binding is not None:
            return scope_alternatives(binding, source, parameters, depth + 1, context, site)
        return parameter_alternatives(node, name, source, parameters, depth, context)

    return None


class ParameterContext:
    """Literal arguments a function's callers supply, per parameter position.

    A local `let interlude k = ...` is answered from the block that binds it —
    two blocks in the same module may bind the same name and mean different
    things. A top-level helper is answered from every module, because its
    callers usually live elsewhere; those requests are collected in one pass
    and resolved in the next.
    """

    def __init__(self, values: dict | None = None):
        self.values = values or {}
        self.requests: set[tuple] = set()
        self.requested_modules: set[str] = set()
        self.module: str | None = None
        self._variables: dict[str, str] = {}

    def offer_variables(self, variables: dict[str, str]) -> None:
        """Variables the answering call sites had in force."""
        self._variables.update(variables)

    def take_variables(self) -> dict[str, str]:
        variables, self._variables = self._variables, {}
        return variables

    def request(self, function_name: str, position: int) -> None:
        self.requests.add((self.module, function_name, position))
        if self.module is not None:
            self.requested_modules.add(self.module)


def parameter_alternatives(node, name: str, source: bytes, parameters: set[str], depth: int, context):
    """Literals a function parameter can hold, taken from its call sites.

    `let interlude k = storyBuild $ setTitle "title" >> p k` is the campaign
    idiom for a handful of interludes that share a layout: the keys are real
    and static, they just live one call away. Which call sites count is a
    scoping question, not a name lookup — `TheDunwichLegacy` binds `interlude`
    twice, once under `scope "interlude1"` and once under `scope "interlude2"`,
    and reading both sets into both scopes invents keys that do not exist. So a
    local binding is answered only from the block it is bound in, and a
    top-level definition only from a cross-module pass. If a single call site
    is unresolvable the whole parameter is, because a partial answer would
    understate what the backend emits.
    """
    holder = node.parent
    while holder is not None and holder.type != "function":
        holder = holder.parent
    if holder is None:
        return None
    children = significant_children(holder)
    if not children or children[0].type != "variable":
        return None
    function_name = text_of(children[0], source)
    positions: list[str] = []
    for child in children[1:-1]:
        if child.type == "variable":
            positions.append(text_of(child, source))
        elif child.type == "patterns":
            positions.extend(text_of(pattern, source) for pattern in significant_children(child))
        else:
            positions.append("")
    if name not in positions:
        return None
    position = positions.index(name)

    scope_root = lexical_scope_root(holder)
    if scope_root is None:
        # Top level: the callers are spread across the codebase, so this is
        # answered by the cross-module pass rather than guessed at from the
        # defining module alone.
        if context is None:
            return None
        answer = context.values.get((context.module, function_name, position))
        if answer is None:
            context.request(function_name, position)
            return None
        # `sendI18n s = send $ ikey' s` is called as
        # `cardNameVar a $ sendI18n "log.retaliate"`: the key and the variables
        # both live at the call site, so both come back from it.
        context.offer_variables(answer["variables"])
        return list(answer["values"])

    values: list[str] = []
    found_call = False
    for _, arguments in _call_sites(scope_root, function_name, source):
        if len(arguments) <= position:
            return None
        resolved = scope_alternatives(arguments[position], source, parameters, depth + 1, context)
        if resolved is None:
            return None
        found_call = True
        values.extend(resolved)
    return sorted(set(values)) if found_call else None


def lexical_scope_root(definition):
    """The expression a local binding is visible in, or None when top level.

    `let`/`where` bindings scope over the block that introduces them; a
    definition reached only through `declarations` is top level.
    """
    BINDING_GROUPS = {"let", "local_binds", "where", "binds", "let_in", "declarations"}
    ancestor = definition.parent
    while ancestor is not None:
        if ancestor.type == "declarations":
            return None
        if ancestor.type in BINDING_GROUPS:
            # Climb out of the whole binding group (`function` -> `local_binds`
            # -> `let`) to the block the bindings are visible in; the group
            # itself holds only the definitions.
            block = ancestor
            while block.parent is not None and block.parent.type in BINDING_GROUPS:
                block = block.parent
            return block.parent if block.parent is not None else block
        ancestor = ancestor.parent
    return None


def _call_sites(root, function_name: str, source: bytes):
    """Every `function_name arg…` application under `root`."""
    calls = []

    def visit(node):
        if node.type == "apply":
            application = flatten_application(node, source)
            if application is not None and application[0] == function_name:
                calls.append((node, application[1]))
        for child in node.children:
            visit(child)

    visit(root)
    return calls


def decide_conditional(node, site, source: bytes):
    """Picks a branch when an enclosing `case` already fixed the condition.

    `let version = if n == 1 then "version1" else "version2"` inside
    `case n of 1 -> …; 2 -> …` is not two possibilities per branch: it is one.
    Fanning out anyway would demand keys for a version that branch never
    reaches.
    """
    children = [child for child in significant_children(node) if child.type not in {"if", "then", "else"}]
    if len(children) != 3:
        return None
    condition, then_branch, else_branch = children
    if condition.type != "infix":
        return None
    parts = infix_parts(condition, source)
    if parts is None or parts[1] not in {"==", "/="}:
        return None
    left, operator, right = parts
    left_text = text_of(left, source).strip()
    right_text = text_of(right, source).strip()
    if INTEGER_ARGUMENT.fullmatch(right_text):
        scrutinee, literal = left_text, right_text
    elif INTEGER_ARGUMENT.fullmatch(left_text):
        scrutinee, literal = right_text, left_text
    else:
        return None

    for case_scrutinee, pattern in _case_bindings(site, source):
        if case_scrutinee != scrutinee or not INTEGER_ARGUMENT.fullmatch(pattern):
            continue
        equal = pattern == literal
        holds = equal if operator == "==" else not equal
        return then_branch if holds else else_branch
    return None


def decide_by_matching_condition(node, site, source: bytes):
    """Picks the branch an enclosing `if` on the same condition already chose.

    `scope (if headedWest then "west" else "east")` wrapping
    `if headedWest then li "a" else li "b"` names two entries, not four.
    """
    children = [child for child in significant_children(node) if child.type not in {"if", "then", "else"}]
    if len(children) != 3:
        return None
    condition, then_branch, else_branch = children
    wanted = _normalized(text_of(condition, source))
    for enclosing, branch in _conditional_branches(site, source):
        if _normalized(enclosing) == wanted:
            return then_branch if branch == "then" else else_branch
    return None


def _normalized(text: str) -> str:
    return " ".join(text.split())


def _conditional_branches(node, source: bytes):
    """(condition text, "then"|"else") for every `if` around `node`."""
    child = node
    ancestor = node.parent
    while ancestor is not None:
        if ancestor.type == "conditional":
            parts = [c for c in significant_children(ancestor) if c.type not in {"if", "then", "else"}]
            if len(parts) == 3 and child is not None:
                if parts[1].id == child.id:
                    yield text_of(parts[0], source), "then"
                elif parts[2].id == child.id:
                    yield text_of(parts[0], source), "else"
        child = ancestor
        ancestor = ancestor.parent


def _case_bindings(node, source: bytes):
    """(scrutinee, pattern) for every `case … of` alternative around `node`."""
    child = node
    ancestor = node.parent
    while ancestor is not None:
        if ancestor.type == "alternative":
            patterns = significant_children(ancestor)
            case_node = ancestor.parent
            while case_node is not None and case_node.type != "case":
                case_node = case_node.parent
            if case_node is not None and patterns:
                scrutinee = significant_children(case_node)
                if scrutinee:
                    yield text_of(scrutinee[0], source).strip(), text_of(patterns[0], source).strip()
        child = ancestor
        ancestor = ancestor.parent


def local_binding(node, name: str, source: bytes):
    """The body of the nearest enclosing `let`/`where` binding of `name`."""
    ancestor = node.parent
    while ancestor is not None:
        for candidate in _descendant_binds(ancestor, name, source):
            children = significant_children(candidate)
            body = children[-1] if children else None
            if body is not None and body.type == "match":
                inner = significant_children(body)
                body = inner[-1] if inner else None
            return body
        ancestor = ancestor.parent
    return None


def _descendant_binds(node, name: str, source: bytes, depth: int = 0):
    if depth > 3:
        return
    for child in node.children:
        if child.type in {"bind", "function"}:
            children = significant_children(child)
            if children and children[0].type == "variable" and text_of(children[0], source) == name:
                yield child
        elif child.type in {"local_binds", "let", "declarations", "where", "binds"}:
            yield from _descendant_binds(child, name, source, depth + 1)


def expand_scope(stack) -> list[list[str]] | None:
    """Cross-product of a scope stack that may hold alternative segments."""
    expanded: list[list[str]] = [[]]
    for segment in stack:
        options = [segment] if isinstance(segment, str) else list(segment)
        if len(expanded) * len(options) > MAX_SCOPE_COMBINATIONS:
            return None
        expanded = [prefix + [option] for prefix in expanded for option in options]
    return expanded


def render_template(parts: list[dict], arguments: dict[str, str]) -> str | None:
    """Substitutes call-site arguments into a scope template, or None."""
    rendered = []
    for part in parts:
        if "literal" in part:
            rendered.append(part["literal"])
            continue
        value = arguments.get(part["param"])
        if value is None:
            return None
        rendered.append(value)
    return "".join(rendered)


def scope_template(node, source: bytes, parameters: set[str]) -> list[dict] | None:
    """Compiles `"iceAndDeath.part" <> tshow n` into literal/parameter parts.

    Aliases such as `scenarioI18n n a = campaignI18n $ scope (... <> tshow n) a`
    are only dynamic until the call site supplies `n`, so the shape is kept
    rather than discarded. Anything richer than string concatenation of
    literals and plain parameters stays dynamic.
    """
    if node.type in {"exp", "parens"} and len(significant_children(node)) == 1:
        return scope_template(significant_children(node)[0], source, parameters)

    literal = string_literal(node, source)
    if literal is not None:
        return [{"literal": literal}]

    if node.type == "infix":
        parts = infix_parts(node, source)
        if parts is None:
            return None
        left, operator, right = parts
        if operator != "<>":
            return None
        head = scope_template(left, source, parameters)
        tail = scope_template(right, source, parameters)
        if head is None or tail is None:
            return None
        return head + tail

    if node.type == "variable" and text_of(node, source) in parameters:
        return [{"param": text_of(node, source)}]

    application = flatten_application(node, source)
    if application is not None:
        name, args = application
        if name in {"tshow", "show", "T.pack"} and len(args) == 1:
            return scope_template(args[0], source, parameters)
    return None


def wrapper_prefix(wrapper: dict, args, source: bytes) -> list[str] | None:
    """The scope segments a wrapper pushes around its own keys."""
    prefix = list(wrapper.get("push", []))
    if "push_literal_arg" in wrapper:
        index = wrapper["push_literal_arg"]
        if len(args) <= index:
            return None
        value = string_literal(args[index], source)
        if value is None:
            return None
        prefix.append(value)
    return prefix


def _steps_for_call(name: str, args, source: bytes, parameters: set[str], context=None, site=None) -> list[dict]:
    primitive = SCOPE_PRIMITIVES.get(name)
    if primitive is not None:
        steps: list[dict] = []
        if primitive.get("reset"):
            steps.append({"kind": "reset", "segments": primitive.get("segments", [])})
        if "literal_arg" in primitive and len(args) > primitive["literal_arg"]:
            argument = args[primitive["literal_arg"]]
            value = string_literal(argument, source)
            if value is None:
                template = scope_template(argument, source, parameters)
                if template is not None:
                    steps.append({"kind": "template_scope", "parts": template})
                else:
                    alternatives = scope_alternatives(argument, source, parameters, 0, context, site)
                    if alternatives is None:
                        return [{"kind": "dynamic", "value": f"{name} non-literal scope"}]
                    steps.append(
                        {"kind": "alternatives_scope", "values": sorted(set(alternatives))}
                    )
            else:
                steps.append({"kind": "literal_scope", "value": value})
        elif "literal_arg" in primitive:
            return [{"kind": "dynamic", "value": f"{name} without literal scope"}]
        if primitive.get("pop"):
            steps.append({"kind": "pop", "count": primitive["pop"]})
        return steps

    wrapper = WRAPPERS.get(name)
    if wrapper is not None:
        steps = [{"kind": "literal_scope", "value": segment} for segment in wrapper.get("push", [])]
        if "push_literal_arg" in wrapper:
            value = string_literal(args[wrapper["push_literal_arg"]], source) if args else None
            if value is None:
                return [{"kind": "dynamic", "value": f"{name} non-literal scope"}]
            steps.append({"kind": "literal_scope", "value": value})
        return steps

    if name.endswith("I18n") and name not in parameters:
        return [{"kind": "alias", "name": name, "arguments": args}]
    return []


def collect_aliases(tree, source: bytes) -> dict[str, dict]:
    """Finds `<name>I18n <params> = <scope expression>` definitions.

    Both shapes matter: `scenarioI18n a = campaignI18n $ scope "x" a` parses as
    a `function`, while the far more common point-free
    `campaignI18n = standaloneI18n "x"` parses as a `bind`. Missing the second
    shape silently strands every key in the campaign that defines it.
    """
    aliases: dict[str, dict] = {}

    def visit(node):
        if node.type in {"function", "bind"}:
            children = significant_children(node)
            if children and children[0].type == "variable":
                name = text_of(children[0], source)
                if name.endswith("I18n"):
                    ordered: list[str] = []
                    for child in children[1:-1]:
                        if child.type == "variable":
                            ordered.append(text_of(child, source))
                        elif child.type == "patterns":
                            ordered.extend(
                                text_of(pattern, source)
                                for pattern in significant_children(child)
                            )
                    parameters = set(ordered)
                    body = next(
                        (child for child in children if child.type in {"match", "exp", "infix", "apply"}),
                        None,
                    )
                    if body is not None and body.type == "match":
                        inner = significant_children(body)
                        body = inner[-1] if inner else None
                    if body is not None:
                        aliases[name] = {
                            "steps": scope_steps_of_expression(body, source, parameters),
                            "parameters": ordered,
                            "line": node.start_point[0] + 1,
                        }
        for child in node.children:
            visit(child)

    visit(tree.root_node)
    return aliases


NUMERIC_RESULT = re.compile(r"\b(Int|Integer|Double|Natural)\b")
TEXT_RESULT = re.compile(r"\b(Text|String)\b")


def top_level_signatures(tree, source: bytes) -> dict[str, str]:
    """Top-level type signatures, so a bound value can be typed from source.

    `withVars ["shelterValue" .= sv]` says nothing about `sv`; the signature of
    whatever produced it does (`shelterValue :: … -> m (Maybe Int)`).
    """
    signatures: dict[str, str] = {}
    for child in tree.root_node.children:
        if child.type != "declarations":
            continue
        for declaration in child.children:
            if declaration.type != "signature":
                continue
            parts = significant_children(declaration)
            if len(parts) >= 2 and parts[0].type == "variable":
                signatures[text_of(parts[0], source)] = text_of(parts[-1], source)
    return signatures


def top_level_definitions(tree, source: bytes) -> set[str]:
    """Names this module defines at the top level."""
    names: set[str] = set()
    for child in tree.root_node.children:
        if child.type != "declarations":
            continue
        for declaration in child.children:
            if declaration.type not in {"function", "bind"}:
                continue
            head = significant_children(declaration)
            if head and head[0].type == "variable":
                names.add(text_of(head[0], source))
    return names


def module_name_of(tree, source: bytes) -> str | None:
    for child in tree.root_node.children:
        if child.type == "header":
            for grandchild in child.children:
                # `is_named` matters: the `module` keyword token has the same
                # type name as the module-identifier node.
                if grandchild.is_named and grandchild.type == "module":
                    return text_of(grandchild, source)
    return None


def imports_of(tree, source: bytes) -> list[dict]:
    """Structured import declarations: what a module can actually see.

    Qualified-only imports, explicit import lists and `hiding` clauses all
    change whether an unqualified `campaignI18n` at a use site refers to the
    imported module, so all three are captured rather than reducing an import
    to a module name.
    """
    entries: list[dict] = []
    for child in tree.root_node.children:
        if child.type != "imports":
            continue
        for entry in child.children:
            if entry.type != "import":
                continue
            record = {"module": None, "alias": None, "qualified": False, "only": None, "hiding": None}
            seen_module = False
            for grandchild in entry.children:
                kind = grandchild.type
                if kind == "qualified":
                    record["qualified"] = True
                elif kind == "module" and grandchild.is_named and not seen_module:
                    record["module"] = text_of(grandchild, source)
                    seen_module = True
                elif kind == "module" and grandchild.is_named:
                    record["alias"] = text_of(grandchild, source)
                elif kind == "hiding":
                    record["hiding"] = []
                elif kind == "import_list":
                    names = sorted(
                        {
                            text_of(name, source)
                            for name in grandchild.children
                            if name.type == "import_name"
                        }
                    )
                    if record["hiding"] is None:
                        record["only"] = names
                    else:
                        record["hiding"] = names
            if record["module"] is not None:
                entries.append(record)
    return entries


def exports_of(tree, source: bytes) -> dict | None:
    """The module's export list, or None when it exports everything it defines.

    Only two things matter here: which local names escape, and which modules
    are re-exported wholesale (`module Arkham.Import.Lifted`), because the
    campaign helpers reach their scope aliases through exactly those
    re-exports.
    """
    for child in tree.root_node.children:
        if child.type != "header":
            continue
        for grandchild in child.children:
            if grandchild.type != "exports":
                continue
            names: set[str] = set()
            modules: set[str] = set()
            for entry in grandchild.children:
                if entry.type == "export":
                    names.add(text_of(entry, source).split("(")[0].strip())
                elif entry.type == "module_export":
                    for part in entry.children:
                        if part.type == "module" and part.is_named:
                            modules.add(text_of(part, source))
            return {"names": sorted(names), "modules": sorted(modules)}
    return None


# Constructs that only ever appear around localized text. Their presence is
# what separates the flavor-text DSL from ordinary functions that happen to be
# called `p`, `h` or `li`.
I18N_CONTEXTS = {
    "flavor",
    "flavorText",
    "story",
    "storyBuild",
    "storyWithChooseOneM'",
    "storyWithContinue",
    "investigatorStoryWithChooseOneM'",
    "ul",
    "blueFlavor",
    "greenFlavor",
    "victoryFlavor",
    "compose",
}


def i18n_context(node, source: bytes) -> bool:
    """True when the node sits inside a recognized i18n construct."""
    ancestor = node.parent
    while ancestor is not None:
        if ancestor.type in {"apply", "infix"}:
            application = flatten_application(
                ancestor if ancestor.type == "apply" else ancestor, source
            )
            names = []
            if application is not None:
                names.append(application[0])
            for child in significant_children(ancestor):
                if child.type in {"variable", "qualified_variable", "field"}:
                    names.append(text_of(child, source))
            for name in names:
                head = name.split(".")[0]
                if (
                    name in I18N_CONTEXTS
                    or head in I18N_CONTEXTS
                    or name in SCOPE_PRIMITIVES
                    or name in WRAPPERS
                    or name in EMITTERS
                    or name.endswith("I18n")
                ):
                    return True
        ancestor = ancestor.parent
    return False


def _enclosing_call(node, source: bytes) -> str | None:
    """Head of the nearest application this node is an argument of."""
    ancestor = node.parent
    while ancestor is not None:
        if ancestor.type == "apply":
            application = flatten_application(ancestor, source)
            if application is not None:
                return application[0]
        # Amount prompts pass their labels inside tuples, lists and list
        # comprehensions, so those wrappers are transparent here.
        if ancestor.type not in {
            "apply",
            "parens",
            "exp",
            "literal",
            "tuple",
            "list",
            "list_comprehension",
            "infix",
            "generator",
            "qualifiers",
        }:
            return None
        ancestor = ancestor.parent
    return None


def local_helper(node, source: bytes):
    """The local `let`/`where` helper this emitter is defined inside, if any.

    `let gideonEntry k = setTitle "title" >> compose.green (p "header" >> p k)`
    emits nothing where it is written: the scope — and often the key — belong
    to `scope "gideonMizrah" $ flavor $ gideonEntry "gideon1"`. Reading the
    definition's own scope would file the entry one level too high.
    """
    holder = node.parent
    while holder is not None:
        if holder.type in {"function", "bind"}:
            root = lexical_scope_root(holder)
            children = significant_children(holder)
            if root is not None and children and children[0].type == "variable":
                parameters: list[str] = []
                for child in children[1:-1]:
                    if child.type == "variable":
                        parameters.append(text_of(child, source))
                    elif child.type == "patterns":
                        parameters.extend(
                            text_of(pattern, source) for pattern in significant_children(child)
                        )
                return {
                    "name": text_of(children[0], source),
                    "holder": holder,
                    "root": root,
                    "parameters": parameters,
                }
            return None
        holder = holder.parent
    return None


def bind_alias_arguments(parameters: list[str], arguments, source: bytes) -> dict[str, str]:
    """Binds literal call-site arguments to an alias's parameters.

    `scenarioI18n 1 $ ...` supplies the `n` of
    `scenarioI18n n a = campaignI18n $ scope ("iceAndDeath.part" <> tshow n) a`.
    Non-literal arguments are simply left unbound; the scope that needs them
    then reports itself dynamic rather than inventing a segment.
    """
    bound: dict[str, str] = {}
    for parameter, argument in zip(parameters, arguments):
        literal = string_literal(argument, source)
        if literal is None:
            text = text_of(argument, source).strip()
            literal = text if INTEGER_ARGUMENT.fullmatch(text) else None
        if literal is not None:
            bound[parameter] = literal
    return bound


INTEGER_ARGUMENT = re.compile(r"-?\d+")


def enclosing_scope(node, source: bytes, index: ModuleIndex, module: str, context=None, stop=None):
    """Walks ancestors, collecting the scope stack in force at `node`."""
    effects: list[dict] = []
    variables: dict[str, str] = {}
    saw_reset = False
    child = node
    parent = node.parent
    while parent is not None:
        if stop is not None and parent.id == stop.id:
            break
        if parent.type == "apply":
            application = flatten_application(parent, source)
            if application is not None:
                name, args = application
                arity = SCOPE_ARITY.get(name)
                scoped = args[arity - 1] if arity is not None and len(args) >= arity else (
                    args[-1] if args else None
                )
                if scoped is not None and scoped.id != child.id and not _contains(scoped, node):
                    pass
                else:
                    effects.append({"name": name, "args": args})
                    _collect_variables(name, args, source, variables, index, module)
        elif parent.type == "infix":
            parts = infix_parts(parent, source)
            if parts is not None:
                left, operator, right = parts
                if operator in {"$", ">>", ">>=", "*>", "<*", "."} and _contains(right, node):
                    if operator == "$":
                        application = flatten_application(left, source)
                        if application is not None:
                            name, args = application
                            effects.append({"name": name, "args": args})
                            _collect_variables(name, args, source, variables, index, module)
                        elif left.type == "variable":
                            name = text_of(left, source)
                            effects.append({"name": name, "args": []})
                            _collect_variables(name, [], source, variables, index, module)
        child = parent
        parent = parent.parent

    stack: list[str] = []
    dynamic: str | None = None
    saw_reset = False
    for effect in reversed(effects):
        # `node` is the emitting call: an enclosing `case` around it can decide
        # a conditional scope that wraps the whole case.
        steps = _steps_for_call(effect["name"], effect["args"], source, set(), context, node)
        if not steps and effect["name"].endswith("I18n"):
            steps = [{"kind": "alias", "name": effect["name"], "arguments": effect["args"]}]
        for step in steps:
            kind = step["kind"]
            if kind == "literal_scope":
                stack.append(step["value"])
            elif kind == "alternatives_scope":
                stack.append(tuple(step["values"]))
            elif kind == "pop":
                for _ in range(step["count"]):
                    if stack:
                        stack.pop()
                    else:
                        dynamic = "popScope below the resolved scope"
            elif kind == "reset":
                saw_reset = True
                stack = list(step.get("segments", []))
            elif kind == "alias":
                bound = bind_alias_arguments(
                    index.alias_parameters(module, step["name"]),
                    step.get("arguments", []),
                    source,
                )
                resolved = index.resolve_alias(module, step["name"], frozenset(), bound)
                if resolved is None or resolved.get("dynamic"):
                    dynamic = resolved.get("dynamic") if resolved else f"unknown alias {step['name']}"
                    continue
                if resolved.get("reset"):
                    saw_reset = True
                    stack = list(resolved["segments"])
                else:
                    stack.extend(resolved["segments"])
            elif kind == "dynamic":
                dynamic = step["value"]
    return stack, variables, dynamic, saw_reset


def _contains(candidate, node) -> bool:
    return candidate.start_byte <= node.start_byte and candidate.end_byte >= node.end_byte


def _collect_variables(name: str, args, source: bytes, variables: dict[str, str], index=None, module: str | None = None) -> None:
    # `withVars ["xp" .= xp, "shelterValue" .= n]` and `withVar "name" value`
    # bind arbitrary names (Arkham/I18n.hs), and they are how most resolutions
    # supply their numbers. Without them every one of those keys looks like it
    # renders a variable the backend never sends.
    if name == "withVars" and args:
        for pair_name, value in _pair_bindings(args[0], source):
            variables.setdefault(pair_name, value_type(value, source, index, module))
        return
    if name == "withVar" and len(args) >= 1:
        variable = string_literal(args[0], source)
        if variable is not None:
            variables.setdefault(
                variable,
                value_type(args[1], source, index, module) if len(args) > 1 else "unknown",
            )
        return

    binder = VARIABLE_BINDERS.get(name)
    if binder is not None:
        for variable, kind in binder:
            variables[variable] = kind
        return
    named = NAMED_VARIABLE_BINDERS.get(name)
    if named is not None and args:
        variable = string_literal(args[0], source)
        if variable is not None:
            variables[variable] = named


def _pair_bindings(node, source: bytes) -> list[tuple[str, object]]:
    """Name/value pairs of an Aeson-style `["a" .= x, "b" .= y]` list."""
    bindings = []

    def visit(current):
        if current.type == "infix":
            parts = infix_parts(current, source)
            if parts is not None and parts[1] == ".=":
                literal = string_literal(parts[0], source)
                if literal is not None:
                    bindings.append((literal, parts[2]))
                return
        for child in current.children:
            visit(child)

    visit(node)
    return bindings


# Shapes that betray a number without a type checker. Everything else is
# reported as `unknown`: `withVars ["shelterValue" .= n]` binds an `Int`, and
# calling that `text` because the binder is generic is a false statement about
# the wire.
NUMERIC_CALLS = {"length", "count", "sum", "toInteger", "fromIntegral", "genericLength"}
NUMERIC_OPERATORS = {"+", "-", "*", "`div`", "`max`", "`min`"}


def value_type(node, source: bytes, index=None, module: str | None = None) -> str:
    """The type of a bound value where the source proves one.

    Syntax first (a literal, arithmetic, `length`), then the binding the value
    came from: a `<-`/`let` bound name is traced to the function that produced
    it and typed from that function's signature.
    """
    if node is None:
        return "unknown"
    if node.type in {"exp", "parens"} and len(significant_children(node)) == 1:
        return value_type(significant_children(node)[0], source)
    if string_literal(node, source) is not None:
        return "text"
    text = text_of(node, source).strip()
    if INTEGER_ARGUMENT.fullmatch(text):
        return "integer"
    if node.type == "infix":
        parts = infix_parts(node, source)
        if parts is not None and parts[1] in NUMERIC_OPERATORS:
            return "integer"
        # `fromMaybe 0 <$> getCurrentShelterValue`, `f $ x`: the value's type is
        # the type of what the left-hand side produces.
        if parts is not None and parts[1] in {"<$>", "<&>", "$", "=<<", "<*>"}:
            left = value_type(parts[0], source, index, module)
            if left != "unknown":
                return left
            return value_type(parts[2], source, index, module)
    application = flatten_application(node, source)
    if application is not None and application[0] in NUMERIC_CALLS:
        return "integer"
    # `fromMaybe 0 …` and `fromMaybe "" …` carry their own default's type.
    if application is not None and application[0] in {"fromMaybe", "maybe"} and application[1]:
        default = value_type(application[1][0], source, index, module)
        if default != "unknown":
            return default
    if application is not None and index is not None and module is not None:
        from_signature = _signature_type(application[0], index, module)
        if from_signature != "unknown":
            return from_signature
    if node.type == "variable" and index is not None and module is not None:
        name = text_of(node, source)
        producer = _binding_producer(node, name, source)
        if producer is not None:
            return value_type(producer, source, index, module)
        parameter = _parameter_type(node, name, source, index, module)
        if parameter != "unknown":
            return parameter
    return "unknown"


def _parameter_type(node, name: str, source: bytes, index, module: str) -> str:
    """Types a parameter from its own function's signature.

    `resolutionWithXp' s xp = … withVars ["xp" .= xp] …` types `xp` from
    `resolutionWithXp' :: … -> Int -> …`.
    """
    holder = node.parent
    while holder is not None and holder.type != "function":
        holder = holder.parent
    if holder is None:
        return "unknown"
    children = significant_children(holder)
    if not children or children[0].type != "variable":
        return "unknown"
    positions: list[str] = []
    for child in children[1:-1]:
        if child.type == "variable":
            positions.append(text_of(child, source))
        elif child.type == "patterns":
            positions.extend(text_of(pattern, source) for pattern in significant_children(child))
        else:
            positions.append("")
    if name not in positions:
        return "unknown"
    signature = index.by_module.get(module, {}).get("signatures", {}).get(
        text_of(children[0], source)
    )
    if signature is None:
        return "unknown"
    arguments = [part.strip() for part in re.split(r"->(?![^(]*\))", signature)]
    position = positions.index(name)
    if position >= len(arguments) - 1:
        return "unknown"
    argument = arguments[position]
    if NUMERIC_RESULT.search(argument) and not TEXT_RESULT.search(argument):
        return "integer"
    if TEXT_RESULT.search(argument) and not NUMERIC_RESULT.search(argument):
        return "text"
    return "unknown"


def _signature_type(name: str, index, module: str) -> str:
    """Types a call from the signature of the function it invokes."""
    for origin in index.defining_modules(module, name):
        signature = index.by_module.get(origin, {}).get("signatures", {}).get(name)
        if signature is None:
            continue
        if NUMERIC_RESULT.search(signature) and not TEXT_RESULT.search(signature):
            return "integer"
        if TEXT_RESULT.search(signature) and not NUMERIC_RESULT.search(signature):
            return "text"
    return "unknown"


def _binding_producer(node, name: str, source: bytes):
    """The expression a `<-` or `let` in scope bound `name` to."""
    ancestor = node.parent
    while ancestor is not None:
        for candidate in ancestor.children:
            if candidate.type == "bind":
                children = significant_children(candidate)
                if len(children) >= 2 and children[0].type == "variable":
                    if text_of(children[0], source) == name:
                        body = children[-1]
                        if body.type == "match":
                            inner = significant_children(body)
                            body = inner[-1] if inner else None
                        return body
            if candidate.type in {"let", "local_binds"}:
                found = _binding_producer_in(candidate, name, source)
                if found is not None:
                    return found
        ancestor = ancestor.parent
    return None


def _binding_producer_in(node, name: str, source: bytes):
    for child in node.children:
        if child.type in {"bind", "function"}:
            children = significant_children(child)
            if children and children[0].type == "variable" and text_of(children[0], source) == name:
                body = children[-1]
                if body.type == "match":
                    inner = significant_children(body)
                    body = inner[-1] if inner else None
                return body
        elif child.type in {"local_binds", "declarations"}:
            found = _binding_producer_in(child, name, source)
            if found is not None:
                return found
    return None


def label_key(stack: list[str], key: str) -> list[str]:
    """`labelKey` from Arkham/I18n.hs."""
    if stack and stack[0] == "cards":
        return ["cards", "label", *stack[1:], key]
    return [*stack, "label", key]


def enclosing_definition(node, source: bytes) -> str | None:
    """Name of the top-level function a node belongs to."""
    current = node
    name = None
    while current is not None:
        if current.type == "function":
            children = significant_children(current)
            if children and children[0].type == "variable":
                name = text_of(children[0], source)
        current = current.parent
    return name


def extract_module(
    path: Path,
    source: bytes,
    tree,
    index: ModuleIndex,
    module: str,
    library: Path | None = None,
    context=None,
):
    emitted: dict[str, dict] = {}
    dynamic: list[dict] = []
    pending: list[dict] = []
    relative = _relative_path(path, library)

    def record_literal(node, key: str) -> None:
        entry = emitted.setdefault(key, {"key": key, "variables": {}, "sites": []})
        entry["sites"].append(
            {"file": relative, "line": node.start_point[0] + 1, "emitter": "$literal"}
        )

    def record_dynamic(node, reason: str, emitter: str) -> None:
        dynamic.append(
            {
                "file": relative,
                "line": node.start_point[0] + 1,
                "emitter": emitter,
                "reason": reason,
            }
        )

    def visit(node):
        if node.type == "apply":
            application = flatten_application(node, source)
            if application is not None:
                name, args = application
                emitter = emitter_for(name)
                if emitter is not None:
                    handle(node, name, emitter, args)
                wrapper = WRAPPERS.get(name)
                if wrapper is not None:
                    # A wrapper's own keys sit under the scope the wrapper
                    # pushes, not the caller's: `additionalRules "stepsOfSlumber"`
                    # writes `rules.stepsOfSlumber.title`, not `title`.
                    prefix = wrapper_prefix(wrapper, args, source)
                    if prefix is None:
                        for emit in wrapper.get("emits", []):
                            record_dynamic(node, f"{name} non-literal scope", name)
                            break
                    else:
                        for emit in wrapper.get("emits", []):
                            leaf = (
                                ".".join([*prefix, emit["key"]])
                                if emit.get("scoped")
                                else emit["key"]
                            )
                            handle(
                                node,
                                name,
                                {
                                    "literal": leaf,
                                    "suffixes": [""],
                                    "reset": emit.get("unscoped", False),
                                },
                                args,
                            )
        elif node.type == "string":
            literal = string_literal(node, source)
            match = DOLLAR_KEY.fullmatch(literal) if literal else None
            if match is not None:
                enclosing = _enclosing_call(node, source)
                if is_amount_reader(enclosing):
                    return
                key = match.group(1)
                if enclosing in AMOUNT_LABEL_FUNCTIONS:
                    key = f"choice.{key}"
                if KEY_PATTERN.fullmatch(key) is None:
                    # e.g. `"$xp." <> suffix`: the static half of a key whose
                    # remainder is computed at runtime.
                    record_dynamic(node, "partial key (runtime remainder)", "$literal")
                else:
                    record_literal(node, key)
        elif node.type in {"variable", "projection"} and emitter_for(text_of(node, source)):
            name = text_of(node, source)
            emitter = emitter_for(name)
            if "literal" in emitter:
                handle(node, name, emitter, [])
        for child in node.children:
            visit(child)

    def emit_through_helper(node, name, emitter, helper, keys, call_site_variables, key_node):
        """Emits a local helper's keys once per call site, in that site's scope."""
        inner_stack, inner_variables, inner_dynamic, inner_reset = enclosing_scope(
            node, source, index, module, context, helper["holder"]
        )
        if inner_dynamic is not None:
            record_dynamic(node, inner_dynamic, name)
            return

        # When the key is the helper's own parameter, each call site supplies
        # its own key, so keys and scopes are paired per call rather than
        # multiplied together.
        parameter_index = None
        if key_node is not None and key_node.type == "variable":
            parameter = text_of(key_node, source)
            if parameter in helper["parameters"]:
                parameter_index = helper["parameters"].index(parameter)
        if parameter_index is None and keys is None:
            record_dynamic(node, "non-literal key", name)
            return

        # A helper that anchors its own scope (`showOutcome key = scenarioI18n
        # $ ...`) does not need its callers for the scope — only, possibly, for
        # the key.
        if inner_reset and parameter_index is None:
            variables = dict(inner_variables)
            variables.update(call_site_variables)
            for variable, kind in emitter.get("vars", {}).items():
                variables[variable] = kind
            emit(node, name, emitter, inner_stack, keys, variables)
            return

        calls = _call_sites(helper["root"], helper["name"], source)
        if not calls:
            # Defined but never called in its own block: nothing is emitted.
            return

        for call_node, call_arguments in calls:
            call_stack, call_variables, call_dynamic, call_reset = enclosing_scope(
                call_node, source, index, module, context
            )
            if call_dynamic is not None:
                record_dynamic(call_node, call_dynamic, name)
                return

            if parameter_index is None:
                call_keys = list(keys)
            else:
                if len(call_arguments) <= parameter_index:
                    continue
                resolved = scope_alternatives(
                    call_arguments[parameter_index], source, set(), 0, context, call_node
                )
                if resolved is None:
                    record_dynamic(call_arguments[parameter_index], "non-literal key", name)
                    return
                call_keys = resolved

            variables = dict(inner_variables)
            variables.update(call_variables)
            variables.update(call_site_variables)
            for variable, kind in emitter.get("vars", {}).items():
                variables[variable] = kind

            if inner_reset:
                # The helper anchors its own scope; the call site only supplied
                # the key.
                emit(node, name, emitter, inner_stack, call_keys, variables)
                continue

            if not call_reset:
                # The call site's own scope is relative too; hand the whole
                # thing to the caller-scope pass rather than anchoring it here.
                pending.append(
                    {
                        "module": module,
                        "function": enclosing_definition(call_node, source),
                        "file": relative,
                        "line": node.start_point[0] + 1,
                        "emitter": name,
                        "keys": list(call_keys),
                        "relativeScope": [*call_stack, *inner_stack],
                        "label": bool(emitter.get("label")),
                        "suffixes": list(emitter.get("suffixes", [""])),
                        "variables": variables,
                    }
                )
                continue

            emit(node, name, emitter, [*call_stack, *inner_stack], call_keys, variables)

    def emit(node, name, emitter, stack, keys, variables):
        combinations = expand_scope(stack)
        if combinations is None:
            record_dynamic(node, "too many scope alternatives", name)
            return
        for resolved_stack in combinations:
            for key in keys:
                for suffix in emitter.get("suffixes", [""]):
                    leaf = f"{key}{suffix}"
                    segments = (
                        label_key(resolved_stack, leaf)
                        if emitter.get("label")
                        else [*resolved_stack, leaf]
                    )
                    if not all(segments):
                        record_dynamic(node, "empty scope segment", name)
                        continue
                    full = ".".join(segments)
                    if KEY_PATTERN.fullmatch(full) is None:
                        record_dynamic(node, "partial key (runtime remainder)", name)
                        continue
                    entry = emitted.setdefault(full, {"key": full, "variables": {}, "sites": []})
                    entry["variables"].update(variables)
                    entry["sites"].append(
                        {"file": relative, "line": node.start_point[0] + 1, "emitter": name}
                    )

    def handle(node, name, emitter, args):
        key_node = None
        call_site_variables = {}
        if "literal" in emitter:
            keys = [emitter["literal"]]
        else:
            index_of_arg = emitter["arg"]
            if len(args) <= index_of_arg:
                return
            key_node = args[index_of_arg]
            key = string_literal(args[index_of_arg], source)
            helper_here = local_helper(node, source)
            if (
                key is None
                and helper_here is not None
                and key_node.type == "variable"
                and text_of(key_node, source) in helper_here["parameters"]
            ):
                keys = None
            else:
                keys = (
                    [key]
                    if key is not None
                    else scope_alternatives(args[index_of_arg], source, set(), 0, context, node)
                )
            call_site_variables = context.take_variables() if context is not None else {}
            if keys is None and helper_here is None:
                if not i18n_context(node, source):
                    # `p x` in Arkham.Prelude is function application, not the
                    # flavor-text DSL. Without a single i18n construct anywhere
                    # above it, this is a name collision, not an emitter.
                    return
                record_dynamic(args[index_of_arg], "non-literal key", name)
                return

        helper = local_helper(node, source) if "literal" not in emitter else None
        if helper is not None:
            emit_through_helper(node, name, emitter, helper, keys, call_site_variables, key_node)
            return

        stack, variables, dynamic_reason, saw_reset = enclosing_scope(
            node, source, index, module, context
        )
        if dynamic_reason is not None:
            record_dynamic(node, dynamic_reason, name)
            return
        if emitter.get("reset"):
            stack = []
            saw_reset = True
        variables = dict(variables)
        if "literal" not in emitter:
            variables.update(call_site_variables)
        for variable, kind in emitter.get("vars", {}).items():
            variables[variable] = kind

        # Unless the scope chain began with a reset (withI18n, cardI18n, or a
        # campaign/scenario alias), what we resolved is only the tail of the
        # key: the head comes from whoever calls this function. Emitting it as
        # an absolute key would invent a requirement the backend never emits,
        # so it is deferred to the caller-scope pass and, failing that,
        # reported as dynamic.
        if not saw_reset:
            pending.append(
                {
                    "module": module,
                    "function": enclosing_definition(node, source),
                    "file": relative,
                    "line": node.start_point[0] + 1,
                    "emitter": name,
                    "keys": list(keys),
                    "relativeScope": list(stack),
                    "label": bool(emitter.get("label")),
                    "suffixes": list(emitter.get("suffixes", [""])),
                    "variables": variables,
                }
            )
            return

        emit(node, name, emitter, stack, keys, variables)

    visit(tree.root_node)
    return emitted, dynamic, pending


def _is_top_level(index, key: tuple[str, str]) -> bool:
    record = index.by_module.get(key[0])
    return record is not None and key[1] in record["definitions"]


def caller_scopes(parsed, index, wanted: set[tuple[str, str]]) -> dict[tuple[str, str], set[tuple]]:
    """Scopes in force at every call site of the given helper.

    Scenario and campaign modules routinely factor emission into a helper with
    a `HasI18n` constraint (`setupTheGathering`, ...) whose scope is supplied
    by its caller. Which helper a call site means is a scoping question — two
    modules may define the same helper name — so a call only answers for the
    module the name resolves to there. When every call site agrees on one
    scope, that scope is the helper's scope; when they disagree, the site stays
    dynamic.
    """
    found: dict[tuple[str, str], set[tuple]] = {key: set() for key in wanted}
    if not wanted:
        return found
    names = {name for _, name in wanted}

    for path, source, tree, module in parsed:
        def visit(node):
            if node.type == "variable":
                name = text_of(node, source)
                if name in names:
                    parent = node.parent
                    is_definition = (
                        parent is not None
                        and parent.type == "function"
                        and significant_children(parent)
                        and significant_children(parent)[0].id == node.id
                    )
                    if not is_definition:
                        origins = index.defining_modules(module, name)
                        targets = [
                            key
                            for key in wanted
                            if key[1] == name
                            and (
                                key[0] in origins
                                # A `let`/`where` helper is not exported at all:
                                # only its own module can be talking about it.
                                or (key[0] == module and not _is_top_level(index, key))
                            )
                        ]
                        if targets:
                            stack, _, dynamic_reason, saw_reset = enclosing_scope(
                                node, source, index, module
                            )
                            # Only an anchored scope (one that began with a
                            # reset such as withI18n/campaignI18n) can define a
                            # helper's scope; a relative one would just defer
                            # the question to its own caller.
                            if dynamic_reason is None and saw_reset:
                                for key in targets:
                                    found[key].add(tuple(stack))
            for child in node.children:
                visit(child)

        visit(tree.root_node)
    return found


def collect_parameter_values(parsed, requests, index: ModuleIndex) -> dict:
    """Literal arguments every call site in the codebase passes, per request.

    Top-level helpers such as `campaignFlavorText entry = ... scope entry ...`
    are called from the campaign and scenario modules, not from the module that
    defines them, so their keys only exist once every caller has been read.
    Which definition a call site means is a scoping question: two scenarios
    each define `scenarioFlavorText`, and a request is only answered by callers
    for whom the name resolves to the module that made the request. A request
    whose call sites are not all literal stays unresolved rather than being
    answered from the subset that happened to be readable.
    """
    wanted: dict[str, list[tuple]] = {}
    for request in requests:
        wanted.setdefault(request[1], []).append(request)
    collected: dict[tuple, list[str] | None] = {request: [] for request in requests}
    variables: dict[tuple, dict[str, str]] = {request: {} for request in requests}

    for _, source, tree, module in parsed:

        def visit(node):
            if node.type == "apply":
                application = flatten_application(node, source)
                if application is not None and application[0] in wanted:
                    name, arguments = application
                    origins = index.defining_modules(module, name)
                    for request in wanted[name]:
                        defining_module, _, position = request
                        if defining_module not in origins:
                            continue
                        if len(origins) > 1:
                            # The name is ambiguous at this call site; answering
                            # from it could attribute one helper's keys to
                            # another.
                            collected[request] = None
                            continue
                        if len(arguments) <= position:
                            # A partial application; the real call supplies the
                            # argument further out, and that outer `apply` is
                            # visited too.
                            continue
                        resolved = scope_alternatives(arguments[position], source, set())
                        if resolved is None:
                            collected[request] = None
                        elif collected[request] is not None:
                            collected[request].extend(resolved)
                            _, site_variables, dynamic_reason, _ = enclosing_scope(
                                node, source, index, module
                            )
                            if dynamic_reason is None:
                                variables[request].update(site_variables)
            for child in node.children:
                visit(child)

        visit(tree.root_node)

    return {
        request: {"values": sorted(set(values)), "variables": variables[request]}
        for request, values in collected.items()
        if values
    }


def build_artifact(dynamic_report: Path | None = None, library: Path | None = None) -> dict:
    """Reads every module under `library` (the backend by default) and returns
    the registry. Tests pass a synthetic library to exercise one rule at a
    time; production always reads the real thing."""
    library = library or LIBRARY
    files = sorted(library.rglob("*.hs"))
    index = ModuleIndex()
    parsed = []

    for path in files:
        source = path.read_bytes()
        tree = parse_module(path, source, library)
        module = module_name_of(tree, source)
        if module is None:
            continue
        index.add(
            module,
            {
                "imports": imports_of(tree, source),
                "exports": exports_of(tree, source),
                "aliases": collect_aliases(tree, source),
                "definitions": top_level_definitions(tree, source),
                "signatures": top_level_signatures(tree, source),
            },
        )
        parsed.append((path, source, tree, module))

    digest = hashlib.sha256()
    for path, source, tree, module in parsed:
        digest.update(_relative_path(path, library).encode("utf-8"))
        digest.update(hashlib.sha256(source).digest())

    # Pass one resolves everything that can be answered from a single module
    # and records which top-level helper parameters need their callers.
    context = ParameterContext()
    results: dict[str, tuple] = {}
    for path, source, tree, module in parsed:
        if module in DSL_MODULES:
            continue
        context.module = module
        results[module] = extract_module(path, source, tree, index, module, library, context)

    # Pass two answers those requests from every call site in the codebase and
    # re-reads only the modules that asked.
    if context.requests:
        values = collect_parameter_values(parsed, context.requests, index)
        answered = ParameterContext(values)
        for path, source, tree, module in parsed:
            if module not in context.requested_modules or module in DSL_MODULES:
                continue
            answered.module = module
            results[module] = extract_module(path, source, tree, index, module, library, answered)

    emitted: dict[str, dict] = {}
    dynamic: list[dict] = []
    pending: list[dict] = []
    for module in sorted(results):
        module_keys, module_dynamic, module_pending = results[module]
        dynamic.extend(module_dynamic)
        pending.extend(module_pending)
        for key, entry in module_keys.items():
            existing = emitted.setdefault(key, {"key": key, "variables": {}, "sites": []})
            existing["variables"].update(entry["variables"])
            existing["sites"].extend(entry["sites"])

    # Resolve helper functions whose scope comes from their call sites.
    wanted = {(site["module"], site["function"]) for site in pending if site["function"]}
    scopes = caller_scopes(parsed, index, wanted)
    for site in pending:
        candidates = scopes.get((site["module"], site["function"]), set())
        if len(candidates) != 1:
            dynamic.append(
                {
                    "file": site["file"],
                    "line": site["line"],
                    "emitter": site["emitter"],
                    "reason": (
                        "scope supplied by caller"
                        if not candidates
                        else f"caller scope ambiguous ({len(candidates)} scopes)"
                    ),
                }
            )
            continue
        stack = list(next(iter(candidates))) + list(site["relativeScope"])
        combinations = expand_scope(stack)
        if combinations is None:
            dynamic.append(
                {
                    "file": site["file"],
                    "line": site["line"],
                    "emitter": site["emitter"],
                    "reason": "too many scope alternatives",
                }
            )
            continue
        for resolved_stack in combinations:
          for key in site["keys"]:
            for suffix in site["suffixes"]:
                leaf = f"{key}{suffix}"
                segments = (
                    label_key(resolved_stack, leaf) if site["label"] else [*resolved_stack, leaf]
                )
                if not all(segments):
                    continue
                full = ".".join(segments)
                if KEY_PATTERN.fullmatch(full) is None:
                    dynamic.append(
                        {
                            "file": site["file"],
                            "line": site["line"],
                            "emitter": site["emitter"],
                            "reason": "partial key (runtime remainder)",
                        }
                    )
                    continue
                entry = emitted.setdefault(full, {"key": full, "variables": {}, "sites": []})
                entry["variables"].update(site["variables"])
                entry["sites"].append(
                    {"file": site["file"], "line": site["line"], "emitter": site["emitter"]}
                )

    keys = []
    for key in sorted(emitted):
        entry = emitted[key]
        sites = sorted(
            {(site["file"], site["line"], site["emitter"]) for site in entry["sites"]}
        )
        # One site per key: enough to trace a requirement back to the code
        # that emits it, without turning this artifact into a line-number
        # database that churns on every unrelated edit.
        file, line, emitter = sites[0]
        keys.append(
            {
                "key": key,
                "variables": [
                    {"name": name, "type": entry["variables"][name]}
                    for name in sorted(entry["variables"])
                ],
                "emitters": sorted({emitter for _, _, emitter in sites}),
                "site": f"{file}:{line}",
                "sites": len(sites),
            }
        )

    if dynamic_report is not None:
        dynamic_report.write_text(
            "\n".join(
                f"{site['file']}:{site['line']}\t{site['emitter']}\t{site['reason']}"
                for site in sorted(dynamic, key=lambda site: (site["reason"], site["file"], site["line"]))
            )
            + "\n",
            encoding="utf-8",
        )

    dynamic_summary: dict[str, int] = {}
    class_summary: dict[str, int] = {}
    for site in dynamic:
        dynamic_summary[site["emitter"]] = dynamic_summary.get(site["emitter"], 0) + 1
        classification = classify_dynamic(site["reason"])
        class_summary[classification] = class_summary.get(classification, 0) + 1

    return {
        "artifactVersion": ARTIFACT_VERSION,
        "generator": "scripts/extract-backend-i18n-keys.py",
        "source": {
            "root": _relative_path(library, library.parent if library != LIBRARY else ROOT),
            "files": len(parsed),
            "sha256": digest.hexdigest(),
        },
        "keys": keys,
        "dynamicSites": {
            "total": len(dynamic),
            "byEmitter": dict(sorted(dynamic_summary.items())),
            "byReason": dict(
                sorted(
                    {
                        reason: sum(1 for site in dynamic if site["reason"] == reason)
                        for reason in {site["reason"] for site in dynamic}
                    }.items()
                )
            ),
            "byClass": dict(sorted(class_summary.items())),
            # The complete inventory, not a sample: a site the extractor cannot
            # resolve is a hole in the requirement set, and the only honest way
            # to carry one is to name it, commit it, and let `--check` fail the
            # moment the set moves.
            "sites": [
                {
                    "site": f"{site['file']}:{site['line']}",
                    "emitter": site["emitter"],
                    "reason": site["reason"],
                    "class": classify_dynamic(site["reason"]),
                }
                for site in sorted(
                    dynamic, key=lambda site: (site["file"], site["line"], site["emitter"], site["reason"])
                )
            ],
        },
    }


# Every reason the extractor is allowed to give up for. A site it cannot place
# in one of these classes is a gap in the model, not an acceptable unknown, and
# fails the run.
DYNAMIC_CLASSES = {
    "runtime-key": (
        "the key itself is computed at runtime (an enum, a partner name, a "
        "player choice) and has no static spelling"
    ),
    "runtime-scope": "the scope segment is computed at runtime",
    "caller-scope": (
        "the emitter lives in a helper whose scope its callers supply, and the "
        "call sites do not agree on one scope"
    ),
    "partial-key": "the key is a static prefix with a runtime remainder",
    "scope-underflow": "popScope unwinds past the scope the extractor resolved",
}

DYNAMIC_REASON_CLASSES = {
    "non-literal key": "runtime-key",
    "too many scope alternatives": "runtime-scope",
    "empty scope segment": "runtime-scope",
    "partial key (runtime remainder)": "partial-key",
    "popScope below the resolved scope": "scope-underflow",
    "scope supplied by caller": "caller-scope",
    "recursive alias": "runtime-scope",
    "scope argument not a literal": "runtime-scope",
}

DYNAMIC_REASON_PREFIXES = {
    "caller scope ambiguous": "caller-scope",
    "unknown alias": "caller-scope",
    "ambiguous alias": "caller-scope",
}

DYNAMIC_REASON_SUFFIXES = {
    "non-literal scope": "runtime-scope",
    "without literal scope": "runtime-scope",
}


def classify_dynamic(reason: str) -> str:
    """Places an unresolved site in the closed vocabulary, or fails."""
    classification = DYNAMIC_REASON_CLASSES.get(reason)
    if classification is None:
        for prefix, candidate in DYNAMIC_REASON_PREFIXES.items():
            if reason.startswith(prefix):
                classification = candidate
                break
    if classification is None:
        for suffix, candidate in DYNAMIC_REASON_SUFFIXES.items():
            if reason.endswith(suffix):
                classification = candidate
                break
    if classification is None:
        raise SystemExit(
            f"backend-i18n: unclassified unresolved site reason {reason!r} — every reason must "
            f"map to one of {sorted(DYNAMIC_CLASSES)}"
        )
    return classification


def canonical_bytes(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n").encode(
        "utf-8"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="fail if the artifact is out of date")
    parser.add_argument(
        "--dynamic-report",
        type=Path,
        help="write every unresolved site to this file (triage aid, never committed)",
    )
    arguments = parser.parse_args()

    artifact = build_artifact(arguments.dynamic_report)
    payload = canonical_bytes(artifact)

    if arguments.check:
        if not ARTIFACT.is_file():
            print(f"backend-i18n: {ARTIFACT.relative_to(ROOT)} is missing", file=sys.stderr)
            return 1
        if ARTIFACT.read_bytes() != payload:
            print(
                f"backend-i18n: {ARTIFACT.relative_to(ROOT)} is stale — "
                "re-run `mise run locale-catalog:backend-keys`",
                file=sys.stderr,
            )
            return 1
        print(
            f"backend-i18n: {len(artifact['keys'])} emitted keys verified from "
            f"{artifact['source']['files']} modules"
        )
        return 0

    ARTIFACT.write_bytes(payload)
    print(
        f"backend-i18n: wrote {len(artifact['keys'])} emitted keys "
        f"({artifact['dynamicSites']['total']} dynamic sites) to {ARTIFACT.relative_to(ROOT)}"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
