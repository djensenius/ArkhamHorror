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
    "setup'": {"push": ["setup"], "emits": [{"key": "title"}]},
    # additionalRules s body = scope "rules" $ scope s $ flavor (setTitle "title" >> body)
    "additionalRules": {"push": ["rules"], "push_literal_arg": 0, "emits": [{"key": "title"}]},
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


# Tokens that can start an emitted key. A module that does not parse is only
# ever set aside if it provably contains none of them.
EMITTER_TOKENS = re.compile(
    rb"\b(?:ikey'?|i18n|i18nWithTitle|i18nEntry|storyI|setTitle|labeled'|labeledI|labeledI18n"
    rb"|questionLabeled'|questionLabeledI|invalidLabeled'|skip_|resolution|resolutionWithXp"
    # A trailing `\b` would never match after the apostrophe in `labeled'`.
    rb"|toI18n|withVar|withVars|scope)(?![A-Za-z0-9_])|\bp\s+\"|\bli\s+\"|\bh[13]?\s+\"|\"\$"
)


def _relative_path(path: Path, library: Path | None) -> str:
    """Repo-relative path, or library-relative when a synthetic library is used."""
    base = ROOT if library in (None, LIBRARY) else library
    try:
        return path.relative_to(base).as_posix()
    except ValueError:
        return path.as_posix()


def parse_module(path: Path, source: bytes, library: Path | None = None):
    """Parses one module, or proves it carries no i18n surface.

    tree-sitter's Haskell grammar does not cover every GHC extension this
    codebase uses. `preprocess` neutralizes the ones that appear; anything left
    is a module the extractor cannot read, and skipping it silently is exactly
    how a registry goes stale. So an unparsed module is a hard failure unless a
    lexical scan proves it contains no key-emitting token at all — and then it
    is recorded, with its digest, in the artifact, so the waiver dies the
    moment the file changes.
    """
    tree = PARSER.parse(preprocess(source))
    if not tree.root_node.has_error:
        return tree, None

    relative = _relative_path(path, library)
    hits = sorted({match.group(0).decode("utf-8", "replace") for match in EMITTER_TOKENS.finditer(source)})
    if hits:
        raise SystemExit(
            f"backend-i18n: {relative} does not parse (line {first_error(tree.root_node)}) and "
            f"contains i18n tokens {hits[:5]} — extend `preprocess` or the grammar; the extractor "
            "refuses to guess at a module it cannot read"
        )
    return None, {
        "path": relative,
        "sha256": hashlib.sha256(source).hexdigest(),
        "reason": f"tree-sitter cannot parse this module (line {first_error(tree.root_node)})",
        "emitterTokens": [],
    }


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
        self._closures: dict[tuple[str, str], list[str]] = {}

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

    def _providers_of(self, module: str, name: str) -> list[str]:
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
            providers.extend(self._exporters_of(entry["module"], name))
        return list(dict.fromkeys(providers))

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

    def _exporters_of(self, module: str, name: str) -> list[str]:
        """Modules that supply `name` when `module` is imported unqualified.

        Re-export chains are followed to their end. Cycles are broken with a
        visited set rather than a depth cap, because a cap silently answers
        "nothing exports this" for deep hubs and strands every key behind them.
        """
        cached = self._closures.get((module, name))
        if cached is not None:
            return cached
        found = self._walk_exports(module, name, set())
        self._closures[(module, name)] = found
        return found

    def _walk_exports(self, module: str, name: str, visited: set[str]) -> list[str]:
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

        if name in record["aliases"] and (
            exports_own_definitions or (exports is not None and name in exports["names"])
        ):
            found.append(module)
        for target in targets:
            found.extend(self._walk_exports(target, name, visited))
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


def scope_alternatives(node, source: bytes, parameters: set[str], depth: int = 0):
    """Every literal a scope expression can evaluate to, or None.

    `scope (if headedWest then "west" else "east")` and `scope version`, where
    `version` is a local binding over literals, both name a small closed set of
    real keys. Enumerating that set keeps the requirement honest — the catalog
    must carry every branch — instead of writing the site off as dynamic.
    """
    if depth > 8:
        return None
    if node.type in {"exp", "parens"} and len(significant_children(node)) == 1:
        return scope_alternatives(significant_children(node)[0], source, parameters, depth + 1)

    literal = string_literal(node, source)
    if literal is not None:
        return [literal]

    if node.type == "conditional":
        branches = [child for child in significant_children(node) if child.type not in {"if", "then", "else"}]
        values: list[str] = []
        for branch in branches[1:]:
            resolved = scope_alternatives(branch, source, parameters, depth + 1)
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
                resolved = scope_alternatives(body, source, parameters, depth + 1) if body else None
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
            return scope_alternatives(binding, source, parameters, depth + 1)
        return parameter_alternatives(node, name, source, parameters, depth)

    return None


def parameter_alternatives(node, name: str, source: bytes, parameters: set[str], depth: int):
    """Literals a function parameter can hold, taken from its call sites.

    `let interlude k = storyBuild $ setTitle "title" >> p k` is the campaign
    idiom for a handful of interludes that share a layout. The keys are real
    and static; they simply live one call away, so the call sites in the same
    module are read and every literal argument in that position becomes a
    requirement. If a single call site is unresolvable the whole parameter is,
    because a partial answer would understate what the backend emits.
    """
    holder = node.parent
    while holder is not None and holder.type not in {"function"}:
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

    root = holder
    while root.parent is not None:
        root = root.parent

    values: list[str] = []
    found_call = False
    for call in _call_sites(root, function_name, source):
        arguments = call[1]
        if len(arguments) <= position:
            return None
        resolved = scope_alternatives(arguments[position], source, parameters, depth + 1)
        if resolved is None:
            return None
        found_call = True
        values.extend(resolved)
    return sorted(set(values)) if found_call else None


def _call_sites(root, function_name: str, source: bytes):
    """Every `function_name arg…` application under `root`."""
    calls = []

    def visit(node):
        if node.type == "apply":
            application = flatten_application(node, source)
            if application is not None and application[0] == function_name:
                calls.append(application)
        for child in node.children:
            visit(child)

    visit(root)
    return calls


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


def _steps_for_call(name: str, args, source: bytes, parameters: set[str]) -> list[dict]:
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
                    alternatives = scope_alternatives(argument, source, parameters)
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


def enclosing_scope(node, source: bytes, index: ModuleIndex, module: str):
    """Walks ancestors, collecting the scope stack in force at `node`."""
    effects: list[dict] = []
    variables: dict[str, str] = {}
    saw_reset = False
    child = node
    parent = node.parent
    while parent is not None:
        if parent.type == "apply":
            application = flatten_application(parent, source)
            if application is not None:
                name, args = application
                if args and args[-1].id != child.id and not _contains(args[-1], node):
                    pass
                else:
                    effects.append({"name": name, "args": args})
                    _collect_variables(name, args, source, variables)
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
                            _collect_variables(name, args, source, variables)
                        elif left.type == "variable":
                            name = text_of(left, source)
                            effects.append({"name": name, "args": []})
                            _collect_variables(name, [], source, variables)
        child = parent
        parent = parent.parent

    stack: list[str] = []
    dynamic: str | None = None
    saw_reset = False
    for effect in reversed(effects):
        steps = _steps_for_call(effect["name"], effect["args"], source, set())
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


def _collect_variables(name: str, args, source: bytes, variables: dict[str, str]) -> None:
    # `withVars ["xp" .= xp, "shelterValue" .= n]` and `withVar "name" value`
    # bind arbitrary names (Arkham/I18n.hs), and they are how most resolutions
    # supply their numbers. Without them every one of those keys looks like it
    # renders a variable the backend never sends.
    if name == "withVars" and args:
        for pair_name in _pair_names(args[0], source):
            variables.setdefault(pair_name, "text")
        return
    if name == "withVar" and args:
        variable = string_literal(args[0], source)
        if variable is not None:
            variables.setdefault(variable, "text")
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


def _pair_names(node, source: bytes) -> list[str]:
    """Names bound by an Aeson-style `["a" .= x, "b" .= y]` list."""
    names = []

    def visit(current):
        if current.type == "infix":
            parts = infix_parts(current, source)
            if parts is not None and parts[1] == ".=":
                literal = string_literal(parts[0], source)
                if literal is not None:
                    names.append(literal)
                return
        for child in current.children:
            visit(child)

    visit(node)
    return names


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


def extract_module(path: Path, source: bytes, tree, index: ModuleIndex, module: str, library: Path | None = None):
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
                    for emit in wrapper.get("emits", []):
                        handle(
                            node,
                            name,
                            {
                                "literal": emit["key"],
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

    def handle(node, name, emitter, args):
        if "literal" in emitter:
            keys = [emitter["literal"]]
        else:
            index_of_arg = emitter["arg"]
            if len(args) <= index_of_arg:
                return
            key = string_literal(args[index_of_arg], source)
            keys = [key] if key is not None else scope_alternatives(args[index_of_arg], source, set())
            if keys is None:
                if not i18n_context(node, source):
                    # `p x` in Arkham.Prelude is function application, not the
                    # flavor-text DSL. Without a single i18n construct anywhere
                    # above it, this is a name collision, not an emitter.
                    return
                record_dynamic(args[index_of_arg], "non-literal key", name)
                return

        stack, variables, dynamic_reason, saw_reset = enclosing_scope(node, source, index, module)
        if dynamic_reason is not None:
            record_dynamic(node, dynamic_reason, name)
            return
        if emitter.get("reset"):
            stack = []
            saw_reset = True
        variables = dict(variables)
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
                entry = emitted.setdefault(
                    full, {"key": full, "variables": {}, "sites": []}
                )
                entry["variables"].update(variables)
                entry["sites"].append(
                    {"file": relative, "line": node.start_point[0] + 1, "emitter": name}
                )

    visit(tree.root_node)
    return emitted, dynamic, pending


def caller_scopes(parsed, index, wanted: set[str]) -> dict[str, set[tuple[str, ...]]]:
    """Scopes in force at every call site of the given function names.

    Scenario and campaign modules routinely factor emission into a helper with
    a `HasI18n` constraint (`setupTheGathering`, ...) whose scope is supplied
    by its caller. When every call site agrees on one scope, that scope is the
    helper's scope; when they disagree, the site stays dynamic.
    """
    found: dict[str, set[tuple[str, ...]]] = {name: set() for name in wanted}
    if not wanted:
        return found

    for path, source, tree, module in parsed:
        def visit(node):
            if node.type == "variable":
                name = text_of(node, source)
                if name in wanted:
                    parent = node.parent
                    is_definition = (
                        parent is not None
                        and parent.type == "function"
                        and significant_children(parent)
                        and significant_children(parent)[0].id == node.id
                    )
                    if not is_definition:
                        stack, _, dynamic_reason, saw_reset = enclosing_scope(
                            node, source, index, module
                        )
                        # Only an anchored scope (one that began with a
                        # reset such as withI18n/campaignI18n) can define a
                        # helper's scope; a relative one would just defer the
                        # question to its own caller.
                        if dynamic_reason is None and saw_reset:
                            found[name].add(tuple(stack))
            for child in node.children:
                visit(child)

        visit(tree.root_node)
    return found


def build_artifact(dynamic_report: Path | None = None, library: Path | None = None) -> dict:
    """Reads every module under `library` (the backend by default) and returns
    the registry. Tests pass a synthetic library to exercise one rule at a
    time; production always reads the real thing."""
    library = library or LIBRARY
    files = sorted(library.rglob("*.hs"))
    index = ModuleIndex()
    parsed = []

    unparsed = []
    for path in files:
        source = path.read_bytes()
        tree, waiver = parse_module(path, source, library)
        if waiver is not None:
            unparsed.append(waiver)
            continue
        module = module_name_of(tree, source)
        if module is None:
            continue
        index.add(
            module,
            {
                "imports": imports_of(tree, source),
                "exports": exports_of(tree, source),
                "aliases": collect_aliases(tree, source),
            },
        )
        parsed.append((path, source, tree, module))

    emitted: dict[str, dict] = {}
    dynamic: list[dict] = []
    pending: list[dict] = []
    digest = hashlib.sha256()
    for path, source, tree, module in parsed:
        digest.update(_relative_path(path, library).encode("utf-8"))
        digest.update(hashlib.sha256(source).digest())
        if module in DSL_MODULES:
            continue
        module_keys, module_dynamic, module_pending = extract_module(
            path, source, tree, index, module, library
        )
        dynamic.extend(module_dynamic)
        pending.extend(module_pending)
        for key, entry in module_keys.items():
            existing = emitted.setdefault(key, {"key": key, "variables": {}, "sites": []})
            existing["variables"].update(entry["variables"])
            existing["sites"].extend(entry["sites"])

    # Resolve helper functions whose scope comes from their call sites.
    wanted = {site["function"] for site in pending if site["function"]}
    scopes = caller_scopes(parsed, index, wanted)
    for site in pending:
        candidates = scopes.get(site["function"] or "", set())
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
        "unparsedModules": sorted(unparsed, key=lambda entry: entry["path"]),
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
