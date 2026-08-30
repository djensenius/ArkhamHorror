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
    if head.type in {"variable", "constructor", "operator", "qualified"}:
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

    def add(self, module: str, record: dict) -> None:
        self.by_module[module] = record

    def resolve_alias(self, module: str, name: str, seen: frozenset = frozenset()) -> dict | None:
        """Resolves a scope alias (campaignI18n, scenarioI18n, ...) to its
        effect, following the defining module's own aliases through imports."""
        if (module, name) in seen:
            return {"dynamic": "recursive alias"}
        record = self.by_module.get(module)
        if record is None:
            return None

        definition = record["aliases"].get(name)
        if definition is not None:
            return self._effect_of(module, definition, seen | {(module, name)})

        for imported in record["imports"]:
            other = self.by_module.get(imported)
            if other is not None and name in other["aliases"]:
                return self.resolve_alias(imported, name, seen | {(module, name)})
        return None

    def _effect_of(self, module: str, definition: dict, seen: frozenset) -> dict:
        effect = {"reset": False, "segments": [], "dynamic": None}
        for step in definition["steps"]:
            kind = step["kind"]
            if kind == "literal_scope":
                effect["segments"].append(step["value"])
            elif kind == "reset":
                effect["reset"] = True
                effect["segments"] = list(step.get("segments", []))
            elif kind == "alias":
                nested = self.resolve_alias(module, step["name"], seen)
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


def _steps_for_call(name: str, args, source: bytes, parameters: set[str]) -> list[dict]:
    primitive = SCOPE_PRIMITIVES.get(name)
    if primitive is not None:
        steps: list[dict] = []
        if primitive.get("reset"):
            steps.append({"kind": "reset", "segments": primitive.get("segments", [])})
        if "literal_arg" in primitive and len(args) > primitive["literal_arg"]:
            value = string_literal(args[primitive["literal_arg"]], source)
            if value is None:
                return [{"kind": "dynamic", "value": f"{name} non-literal scope"}]
            steps.append({"kind": "literal_scope", "value": value})
        elif "literal_arg" in primitive:
            return [{"kind": "dynamic", "value": f"{name} without literal scope"}]
        if primitive.get("pop"):
            steps.append({"kind": "dynamic", "value": "popScope"})
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
        return [{"kind": "alias", "name": name}]
    return []


def collect_aliases(tree, source: bytes) -> dict[str, dict]:
    """Finds `<name>I18n <params> = <scope expression>` definitions."""
    aliases: dict[str, dict] = {}

    def visit(node):
        if node.type == "function":
            children = significant_children(node)
            if children and children[0].type == "variable":
                name = text_of(children[0], source)
                if name.endswith("I18n"):
                    parameters = {
                        text_of(child, source)
                        for child in children[1:-1]
                        if child.type in {"variable", "patterns"}
                    }
                    body = next(
                        (child for child in children if child.type in {"match", "exp", "infix", "apply"}),
                        None,
                    )
                    if body is not None:
                        if body.type == "match":
                            inner = significant_children(body)
                            body = inner[-1] if inner else None
                    if body is not None:
                        aliases[name] = {
                            "steps": scope_steps_of_expression(body, source, parameters),
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


def imports_of(tree, source: bytes) -> list[str]:
    modules = []
    for child in tree.root_node.children:
        if child.type == "imports":
            for entry in child.children:
                if entry.type != "import":
                    continue
                for grandchild in entry.children:
                    if grandchild.is_named and grandchild.type == "module":
                        modules.append(text_of(grandchild, source))
                        break
    return modules


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
            steps = [{"kind": "alias", "name": effect["name"]}]
        for step in steps:
            kind = step["kind"]
            if kind == "literal_scope":
                stack.append(step["value"])
            elif kind == "reset":
                saw_reset = True
                stack = list(step.get("segments", []))
            elif kind == "alias":
                resolved = index.resolve_alias(module, step["name"])
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


def extract_module(path: Path, source: bytes, tree, index: ModuleIndex, module: str):
    emitted: dict[str, dict] = {}
    dynamic: list[dict] = []
    pending: list[dict] = []
    relative = path.relative_to(ROOT).as_posix()

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
                emitter = EMITTERS.get(name)
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
                key = match.group(1)
                if KEY_PATTERN.fullmatch(key) is None:
                    # e.g. `"$xp." <> suffix`: the static half of a key whose
                    # remainder is computed at runtime.
                    record_dynamic(node, "partial key (runtime remainder)", "$literal")
                else:
                    record_literal(node, key)
        elif node.type == "variable" and text_of(node, source) in EMITTERS:
            name = text_of(node, source)
            emitter = EMITTERS[name]
            if "literal" in emitter:
                handle(node, name, emitter, [])
        for child in node.children:
            visit(child)

    def handle(node, name, emitter, args):
        if "literal" in emitter:
            key = emitter["literal"]
        else:
            index_of_arg = emitter["arg"]
            if len(args) <= index_of_arg:
                return
            key = string_literal(args[index_of_arg], source)
            if key is None:
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
                    "key": key,
                    "relativeScope": list(stack),
                    "label": bool(emitter.get("label")),
                    "suffixes": list(emitter.get("suffixes", [""])),
                    "variables": variables,
                }
            )
            return

        for suffix in emitter.get("suffixes", [""]):
            leaf = f"{key}{suffix}"
            segments = label_key(stack, leaf) if emitter.get("label") else [*stack, leaf]
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
            entry["sites"].append({"file": relative, "line": node.start_point[0] + 1, "emitter": name})

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


def build_artifact() -> dict:
    files = sorted(LIBRARY.rglob("*.hs"))
    index = ModuleIndex()
    parsed = []

    for path in files:
        source = path.read_bytes()
        tree = PARSER.parse(source)
        module = module_name_of(tree, source)
        if module is None:
            continue
        index.add(
            module,
            {"imports": imports_of(tree, source), "aliases": collect_aliases(tree, source)},
        )
        parsed.append((path, source, tree, module))

    emitted: dict[str, dict] = {}
    dynamic: list[dict] = []
    pending: list[dict] = []
    digest = hashlib.sha256()
    for path, source, tree, module in parsed:
        digest.update(path.relative_to(ROOT).as_posix().encode("utf-8"))
        digest.update(hashlib.sha256(source).digest())
        if module in DSL_MODULES:
            continue
        module_keys, module_dynamic, module_pending = extract_module(
            path, source, tree, index, module
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
        for suffix in site["suffixes"]:
            leaf = f"{site['key']}{suffix}"
            segments = label_key(stack, leaf) if site["label"] else [*stack, leaf]
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

    dynamic_summary: dict[str, int] = {}
    for site in dynamic:
        dynamic_summary[site["emitter"]] = dynamic_summary.get(site["emitter"], 0) + 1

    return {
        "artifactVersion": ARTIFACT_VERSION,
        "generator": "scripts/extract-backend-i18n-keys.py",
        "source": {
            "root": LIBRARY.relative_to(ROOT).as_posix(),
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
            "examples": [
                f"{site['file']}:{site['line']} {site['emitter']} ({site['reason']})"
                for site in sorted(dynamic, key=lambda site: (site["file"], site["line"]))[:20]
            ],
        },
    }


def canonical_bytes(value: object) -> bytes:
    return (json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n").encode(
        "utf-8"
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="fail if the artifact is out of date")
    arguments = parser.parse_args()

    artifact = build_artifact()
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
