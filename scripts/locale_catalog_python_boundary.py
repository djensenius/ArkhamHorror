"""Capability lint for the locale-catalog Python tooling.

This is a **review aid and defence in depth, not a security boundary**. All
committed repository code in this project is trusted: it is reviewed through
pull request, and a malicious commit, reviewer or maintainer is out of scope.
This lint exists so that an *accidental* widening -- a new import, a dynamic
loader, an unsafe deserializer, a capability quietly reached through another
module's namespace -- shows up as a failing check and a review conversation
instead of merging silently. It does not sandbox anything, it cannot stop code
that runs, and its completeness is explicitly not a release invariant.

What it does check. Every declared repository Python source is parsed before it
is imported. Imports resolve only to declared stdlib, locked dependency, or
in-tree modules, and every dotted reference that reaches a sensitive root is
checked against a per-source grant naming the exact capability **and the exact
shape it may be used in** -- called, read as a value, assigned to, or consumed
as a call result. Granting `subprocess.run` for a call therefore does not grant
`subprocess`, `subprocess.Popen`, or storing `subprocess.run` itself. On top of
that, conservative taint refuses to let a value derived from a propagating
capability (an importer, loader, `runpy`, `ctypes`, or a serialization loader)
escape through assignment, containers, attributes, returns, yields, parameters
or bindings, and a module identity is tracked through containers, `match`
captures, branches and loops so that reaching a sensitive module indirectly is
reported too.

The grant tables live in this file, which is itself a declared source: it is
linted like every other source, and its bytes are folded into the
locale-catalog fixture provenance digest through `all_executable_sources()`, so
a grant cannot be widened without moving a governed contract hash and showing
up in review. Its digest is also recorded in
`scripts/locale_catalog_python_runtime.json` and checked by the pinned runner
before the lint is imported; that is drift detection over a reviewed file, not
an authorization root.

KNOWN_INCOMPLETE
----------------

This lint is a conservative abstract interpretation, not a semantic proof, and
the list below is not exhaustive:

* Values are tracked as *may-be* identities over a bounded lattice. Container
  nesting deeper than `MAX_VALUE_DEPTH`, dotted chains longer than
  `MAX_CAPABILITY_DEPTH`, and loops or module/function summaries that do not
  converge within `MAX_FIXPOINT_ROUNDS` are refused rather than approximated --
  which is fail-closed, but it means legitimate code can be refused too.
* Inter-procedural flow is not modelled. A function's *return* value is checked
  where it is written, not threaded to its call sites, and a value that passes
  through a call is not tracked out the other side.
* Attribute-level state of objects and class instances is not modelled; storing
  an identity on an attribute is refused instead of tracked.
* Reflection over the interpreter (`getattr`, `globals`, `vars`, dunder
  traversal) is refused outright rather than analyzed, so any construct that
  depends on it is rejected even when it is harmless.
* `eval`/`exec`/dynamic import are refused; nothing here can reason about code
  that does not exist at lint time.
* An AST node type this lint does not model is refused where it is seen
  (`EVALUATORS`/`STATEMENTS`/`bind_pattern` all fail closed on an unknown
  node), but "fails closed where detected" is a weaker statement than "detects
  everything": a future language construct could be modelled incorrectly rather
  than not at all.

Treat a refusal as "explain this in review", and treat a pass as "no known
widening was detected", not as proof that none exists.
"""

from __future__ import annotations

import ast
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"

EXECUTABLE_SOURCES = frozenset(
    {
        "scripts/build-locale-catalog-fixture.py",
        "scripts/check-locale-catalog-settings.py",
        "scripts/check-schema-revision-drift.py",
        "scripts/extract-backend-i18n-keys.py",
        "scripts/extract_backend_i18n_keys.py",
        "scripts/generate-locale-catalog.py",
        "scripts/json_schema_subset.py",
        "scripts/locale_catalog_python_boundary.py",
        "scripts/locale_catalog_runtime.py",
        "scripts/strict_json.py",
        "scripts/test_extract_backend_i18n_keys.py",
        "scripts/test_locale_catalog_python_boundary.py",
        "scripts/update-manifest-hashes.py",
        "scripts/validate-catalog-serving.py",
        "scripts/validate-contract-fixtures.py",
        "scripts/validate-locale-catalog.py",
        "scripts/validate-route-inventory.py",
    }
)

ENTRY_POINTS = frozenset(
    {
        "scripts/build-locale-catalog-fixture.py",
        "scripts/check-locale-catalog-settings.py",
        "scripts/check-schema-revision-drift.py",
        "scripts/extract-backend-i18n-keys.py",
        "scripts/generate-locale-catalog.py",
        "scripts/test_extract_backend_i18n_keys.py",
        "scripts/test_locale_catalog_python_boundary.py",
        "scripts/update-manifest-hashes.py",
        "scripts/validate-catalog-serving.py",
        "scripts/validate-contract-fixtures.py",
        "scripts/validate-locale-catalog.py",
        "scripts/validate-route-inventory.py",
    }
)

# The trusted computing base on the repository side: sources whose bytes decide
# whether other sources may run at all. Each is pinned by exact SHA-256 in
# `scripts/locale_catalog_python_runtime.json` and verified by the sealed shell
# before the interpreter is pointed at any of them. They stay governed sources
# too -- capability-scanned and folded into provenance -- but authentication
# comes first, because a scanner that has already executed cannot vouch for
# itself.
TRUSTED_SOURCES = frozenset(
    {
        "frontend/scripts/locale-catalog/generator-launcher.mjs",
        "scripts/locale-catalog-python-sealed.sh",
        "scripts/locale_catalog_python_boundary.py",
        "scripts/locale_catalog_runtime.py",
        "scripts/run-locale-catalog-python.sh",
    }
)

ALLOWED_IMPORTS = {
    "__future__",
    "argparse",
    "ast",
    "base64",
    "builtins",
    "copy",
    "csv",
    "dataclasses",
    "decimal",
    "hashlib",
    "gzip",
    "json",
    "math",
    "re",
    "shlex",
    "shutil",
    "stat",
    "tempfile",
    "time",
    "tomllib",
    "uuid",
    "urllib.error",
    "urllib.parse",
    "urllib.request",
    "pathlib",
    "collections",
    "jsonschema",
    "jsonschema.validators",
    "referencing",
    "tree_sitter",
    "tree_sitter_haskell",
    "extract_backend_i18n_keys",
    "strict_json",
    "json_schema_subset",
    "locale_catalog_python_boundary",
}

# Modules a *specific* source may import in addition to the shared set above.
# `os`, `subprocess`, `sys`, `runpy` and `importlib.metadata` are never in the
# shared set: a source that needs one has to name it here, and then still has
# to declare every individual capability it uses below.
SOURCE_SENSITIVE_IMPORTS = {
    "scripts/check-locale-catalog-settings.py": frozenset({"os", "shutil", "subprocess", "sys"}),
    "scripts/check-schema-revision-drift.py": frozenset({"os", "shutil", "subprocess", "sys"}),
    "scripts/extract_backend_i18n_keys.py": frozenset({"sys"}),
    "scripts/generate-locale-catalog.py": frozenset({"subprocess", "sys"}),
    "scripts/locale_catalog_runtime.py": frozenset({"os", "sys"}),
    "scripts/strict_json.py": frozenset({"os", "shutil", "subprocess", "sys"}),
    "scripts/test_extract_backend_i18n_keys.py": frozenset({"sys"}),
    # `io`/`zipfile` are scoped to the adversarial suite so it can *build* the
    # zip import root it plants; no governed source may reach either, and the
    # boundary still refuses a zip root wherever it appears.
    "scripts/test_locale_catalog_python_boundary.py": frozenset(
        {"io", "os", "subprocess", "yaml", "zipfile"}
    ),
    "scripts/validate-catalog-serving.py": frozenset({"os", "shutil", "subprocess", "sys", "urllib.request"}),
    "scripts/validate-route-inventory.py": frozenset({"yaml"}),
    "scripts/validate-locale-catalog.py": frozenset({"os", "shutil", "subprocess", "sys"}),
}

# Use shapes a grant can authorise. A grant lists exactly the shapes its
# capability may appear in; every other appearance of that capability, and
# every capability without a grant, fails closed.
USE_CALL = "call"
USE_VALUE = "value"
USE_ASSIGN = "assign"
USE_RESULT = "result"
USE_SHAPES = frozenset({USE_CALL, USE_VALUE, USE_ASSIGN, USE_RESULT})

CALL = frozenset({USE_CALL})
VALUE = frozenset({USE_VALUE})
CALL_VALUE = frozenset({USE_CALL, USE_VALUE})
CALL_RESULT = frozenset({USE_CALL, USE_RESULT})

# Exact capability -> permitted use shapes, per source file. `os.environ` read
# as a value does not permit `os.environ.get`; that is a separate entry.
SOURCE_SENSITIVE_CAPABILITIES: dict[str, dict[str, frozenset[str]]] = {
    "scripts/check-locale-catalog-settings.py": {
        "os.environ.items": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.run": CALL,
        "sys.executable": VALUE,
    },
    "scripts/check-schema-revision-drift.py": {
        "os.environ": frozenset({USE_VALUE, USE_ASSIGN}),
        "os.environ.get": CALL,
        "os.environ.pop": CALL,
        "shutil.copyfile": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.run": CALL,
        "sys.argv": VALUE,
        "sys.executable": VALUE,
    },
    "scripts/extract_backend_i18n_keys.py": {
        "sys.exit": CALL,
        "sys.stderr": VALUE,
    },
    "scripts/generate-locale-catalog.py": {
        "subprocess.run": CALL,
        "sys.argv": VALUE,
    },
    "scripts/locale_catalog_runtime.py": {
        "os.environ.get": CALL,
        "os.uname": CALL,
        "sys.argv": frozenset({USE_VALUE, USE_ASSIGN}),
        "sys.base_exec_prefix": VALUE,
        "sys.base_prefix": VALUE,
        "sys.executable": VALUE,
        "sys.flags.dont_write_bytecode": VALUE,
        "sys.flags.ignore_environment": VALUE,
        "sys.flags.isolated": VALUE,
        "sys.flags.no_site": VALUE,
        "sys.flags.safe_path": VALUE,
        "sys.implementation.cache_tag": VALUE,
        "sys.implementation.name": VALUE,
        "sys.pycache_prefix": VALUE,
        "sys.path": VALUE,
        "sys.path.insert": CALL,
        "sys.modules.items": CALL,
        "sys.prefix": VALUE,
        "sys.stderr": VALUE,
        "sys.platform": VALUE,
        "sys.version_info": VALUE,
        "sys._base_executable": VALUE,
    },
    "scripts/strict_json.py": {
        "os.chmod": CALL,
        "os.environ": frozenset({USE_VALUE, USE_ASSIGN}),
        "os.environ.get": CALL,
        "os.environ.pop": CALL,
        "os.fsync": CALL,
        "os.replace": CALL,
        "shutil.rmtree": CALL,
        "subprocess.run": CALL,
        "sys.float_info.max": VALUE,
        "sys.stderr": VALUE,
    },
    "scripts/test_extract_backend_i18n_keys.py": {
        "sys.exit": CALL,
        "sys.stderr": VALUE,
    },
    "scripts/test_locale_catalog_python_boundary.py": {
        "os.environ": VALUE,
        "os.environ.get": CALL,
        "os.getpid": CALL,
        "os.pathsep": VALUE,
        "shutil.copy2": CALL,
        "shutil.copytree": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.PIPE": VALUE,
        "subprocess.Popen": CALL,
        "subprocess.TimeoutExpired": VALUE,
        "subprocess.run": CALL,
        # Structured workflow parsing for the CI privilege policy check: the
        # only YAML entry point that cannot construct arbitrary objects.
        "yaml.safe_load": CALL_RESULT,
    },
    "scripts/validate-catalog-serving.py": {
        "os.environ.get": CALL,
        "os.environ.get": CALL,
        "shutil.copyfile": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.PIPE": VALUE,
        "subprocess.run": CALL,
        "sys.exit": CALL,
        "urllib.error.HTTPError": VALUE,
        "urllib.error.URLError": VALUE,
        "urllib.request.Request": CALL,
        "urllib.request.urlopen": CALL,
    },
    "scripts/validate-locale-catalog.py": {
        "os.environ.get": CALL,
        "shutil.copyfile": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.run": CALL,
        "sys.exit": CALL,
    },
    "scripts/validate-route-inventory.py": {
        "urllib.parse.urlparse": CALL_VALUE,
        # The only YAML entry point that cannot construct arbitrary Python
        # objects. `yaml` is a propagating root, so its results are tainted
        # unless a grant says otherwise; this one declares that exact
        # exemption, and `yaml.load`, `yaml.unsafe_load`, `yaml.full_load` and
        # every loader class stay forbidden outright.
        "yaml.safe_load": CALL_RESULT,
    },
}

ALLOWED_FROM_IMPORTS = {
    "__future__": frozenset({"annotations"}),
    "collections": frozenset({"Counter"}),
    "builtins": frozenset(
        {
            "__import__",
            "breakpoint",
            "compile",
            "delattr",
            "eval",
            "exec",
            "getattr",
            "globals",
            "locals",
            "setattr",
            "vars",
        }
    ),
    "dataclasses": frozenset({"dataclass"}),
    "decimal": frozenset({"Decimal"}),
    "gzip": frozenset({"gzip"}),
    "jsonschema": frozenset({"FormatChecker"}),
    "jsonschema.validators": frozenset({"validator_for"}),
    "pathlib": frozenset({"Path"}),
    "referencing": frozenset({"Registry", "Resource"}),
    "tree_sitter": frozenset({"Language", "Parser"}),
    "urllib.parse": frozenset({"urlparse"}),
    "sys": frozenset({"meta_path", "modules", "path", "path_hooks", "path_importer_cache"}),
    "locale_catalog_python_boundary": frozenset(
        {
            "ALLOWED_IMPORTS",
            "ENTRY_POINTS",
            "LOCAL_MODULE_SOURCES",
            "SOURCE_SENSITIVE_IMPORTS",
            "TRUSTED_SOURCES",
            "CapabilityLintError",
            "SourceBoundaryError",
            "all_executable_sources",
            "scan_python_closure",
        }
    ),
    "extract_backend_i18n_keys": frozenset({"main"}),
}

LOCAL_MODULE_SOURCES = {
    "extract_backend_i18n_keys": "scripts/extract_backend_i18n_keys.py",
    "strict_json": "scripts/strict_json.py",
    "json_schema_subset": "scripts/json_schema_subset.py",
    "locale_catalog_python_boundary": "scripts/locale_catalog_python_boundary.py",
}

ALLOWED_DUNDER_NAMES = frozenset(
    {
        "__doc__",
        "__exit__",
        "__file__",
        "__future__",
        "__init__",
        "__name__",
        "__slots__",
    }
)

# Roots whose every member is sensitive: reaching one at all requires a grant.
SENSITIVE_ROOTS = frozenset(
    {
        "builtins",
        "ctypes",
        "yaml",
        "importlib",
        "marshal",
        "os",
        "pickle",
        "runpy",
        "shelve",
        "shutil",
        "subprocess",
        "sys",
        "urllib",
        "zipimport",
    }
)

# Roots whose call results carry taint: anything derived from an importer,
# loader, code executor, or serialization loader is treated as executable
# until an explicit `result` grant says otherwise.
PROPAGATING_ROOTS = frozenset(
    {
        "builtins",
        "ctypes",
        "yaml",
        "importlib",
        "marshal",
        "pickle",
        "runpy",
        "shelve",
        "zipimport",
    }
)

FORBIDDEN_CAPABILITIES = frozenset(
    {
        "builtins.__import__",
        # `breakpoint()` hands the process to `sys.breakpointhook`, which
        # `PYTHONBREAKPOINT` selects by dotted name -- and Python 3.14's
        # `breakpoint(commands=...)` additionally hands pdb a command script.
        "builtins.breakpoint",
        "builtins.compile",
        "builtins.delattr",
        "builtins.eval",
        "builtins.exec",
        "builtins.getattr",
        "builtins.globals",
        "builtins.locals",
        "builtins.setattr",
        "builtins.vars",
        "os.execv",
        "os.execve",
        "os.execvp",
        "os.execvpe",
        "os.fork",
        "os.popen",
        "os.system",
        # Every YAML entry point that can construct arbitrary Python objects.
        # Only `yaml.safe_load` is grantable, and only where it is declared.
        "yaml.CFullLoader",
        "yaml.CLoader",
        "yaml.CUnsafeLoader",
        "yaml.FullLoader",
        "yaml.Loader",
        "yaml.UnsafeLoader",
        "yaml.add_constructor",
        "yaml.add_multi_constructor",
        "yaml.full_load",
        "yaml.full_load_all",
        "yaml.load",
        "yaml.load_all",
        "yaml.unsafe_load",
        "yaml.unsafe_load_all",
    }
)

BUILTIN_CAPABILITY_NAMES = frozenset(
    {
        "__import__",
        "breakpoint",
        "compile",
        "delattr",
        "eval",
        "exec",
        "getattr",
        "globals",
        "locals",
        "setattr",
        "vars",
    }
)

# Never permitted as an attribute name, no matter what it is reached through.
FORBIDDEN_ATTRIBUTE_NAMES = frozenset(
    {
        "create_module",
        "exec_module",
        "find_module",
        "find_spec",
        "load_module",
        "module_from_spec",
        "spec_from_file_location",
        "spec_from_loader",
        "zipimporter",
    }
)

# Permitted on a resolved, granted chain, but never on an expression the
# analyzer could not resolve -- that is the `entry_points()[0].load()` shape.
FORBIDDEN_UNRESOLVED_ATTRIBUTES = frozenset(
    {
        "load",
        "loader",
        "meta_path",
        "modules",
        "path_hooks",
        "path_importer_cache",
        "run_module",
        "run_path",
        "system",
    }
)

FORBIDDEN_SYS_ATTRIBUTES = frozenset(
    {
        "meta_path",
        "modules",
        "path",
        "path_hooks",
        "path_importer_cache",
    }
)


# Every module name a governed source may bind through an import, plus every
# sensitive root. A dotted reference whose head is one of these *is* another
# namespace, so the analyzer keeps that identity alive through containers,
# branches, loops and `match` captures instead of losing it at the first
# indirection.
MODULE_NAMES = frozenset(
    ALLOWED_IMPORTS
    | {name for names in SOURCE_SENSITIVE_IMPORTS.values() for name in names}
    | set(LOCAL_MODULE_SOURCES)
    | SENSITIVE_ROOTS
)
MODULE_ROOTS = frozenset(name.split(".", 1)[0] for name in MODULE_NAMES)


class CapabilityLintError(ValueError):
    """A construct this lint refuses. Not a security verdict -- a review flag."""


# Retained so a governed caller reads naturally; the lint is the same object.
SourceBoundaryError = CapabilityLintError


def _source_path(relative_path: str) -> Path:
    path = ROOT / relative_path
    if relative_path not in EXECUTABLE_SOURCES:
        raise CapabilityLintError(f"{relative_path} is not a declared executable source")
    if path.is_symlink() or not path.is_file():
        raise CapabilityLintError(f"{relative_path} must be a regular source file")
    if path.resolve(strict=True) != path:
        raise CapabilityLintError(f"{relative_path} resolves through a symlink")
    return path


def read_source(relative_path: str) -> bytes:
    return _source_path(relative_path).read_bytes()


def _local_source(module: str) -> str | None:
    candidate = LOCAL_MODULE_SOURCES.get(module)
    if candidate is None:
        return None
    _source_path(candidate)
    return candidate


class Value:
    """What one expression may be, over-approximated on purpose.

    `caps` is the set of dotted capability/module identities the value may
    *be*; `items` is what subscripting, iterating, unpacking or calling a bound
    method of it may yield; `tainted` marks a value derived from a propagating
    capability. Every combining operation unions, so a value that may be
    `strict_json` down one path and `None` down another is still `strict_json`
    for checking purposes.
    """

    __slots__ = ("caps", "items", "tainted")

    def __init__(
        self,
        caps: frozenset[str] = frozenset(),
        items: "Value | None" = None,
        tainted: bool = False,
    ) -> None:
        self.caps = caps
        self.items = items
        self.tainted = tainted

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Value):
            return NotImplemented
        return (self.caps, self.items, self.tainted) == (other.caps, other.items, other.tainted)

    def __hash__(self) -> int:
        return hash((self.caps, self.items, self.tainted))

    def carries(self) -> bool:
        """True when this value may still hold an identity of any kind."""
        if self.caps or self.tainted:
            return True
        return self.items is not None and self.items.carries()

    def namespaces(self, depth: int = 0) -> set[str]:
        """Every module namespace this value may be, or may hand out."""
        found = {capability for capability in self.caps if capability in MODULE_NAMES}
        if self.items is not None and depth < MAX_VALUE_DEPTH:
            found |= self.items.namespaces(depth + 1)
        return found

    def collapsed(self) -> "Value":
        """Everything this value may be *or* contain, as one flat value."""
        caps = set(self.caps)
        items = self.items
        depth = 0
        while items is not None and depth < MAX_VALUE_DEPTH:
            caps |= items.caps
            items = items.items
            depth += 1
        return Value(frozenset(caps), self.items, self.tainted)


EMPTY = Value()
# How deep container nesting is tracked exactly, and how long a dotted chain
# may grow. Both bounds exist so the analyzer's lattice has finite height and
# a loop fixpoint is reached rather than truncated; exceeding either is a
# refusal, never a silent approximation.
MAX_VALUE_DEPTH = 8
MAX_CAPABILITY_DEPTH = 12
# A loop is iterated until its state stops changing. The bound below only
# exists so a bug cannot hang the analyzer; reaching it is a refusal.
MAX_FIXPOINT_ROUNDS = 64


def join_values(left: Value | None, right: Value | None, depth: int = 0) -> Value | None:
    if left is None:
        return right
    if right is None:
        return left
    if depth >= MAX_VALUE_DEPTH:
        return Value(
            left.caps | right.caps | left.namespaces() | right.namespaces(),
            None,
            left.tainted or right.tainted,
        )
    return Value(
        left.caps | right.caps,
        join_values(left.items, right.items, depth + 1),
        left.tainted or right.tainted,
    )


def contained(values: list[Value]) -> Value:
    """The value of a container literal built from `values`."""
    items: Value | None = None
    tainted = False
    for value in values:
        items = join_values(items, value)
        tainted = tainted or value.tainted
    return Value(frozenset(), items, tainted)


State = dict[str, Value]


def join_states(states: list[State]) -> State:
    """Conservative join: a name is everything it may be on any live path.

    A name bound on one path and absent on another keeps the binding, because
    the *possible* identity is what matters. That is what makes a false `if`, a
    zero-iteration loop, an untaken `except` and a `break` unable to erase a
    module identity the way a last-writer-wins walk did.
    """
    joined: State = {}
    for state in states:
        for name, value in state.items():
            joined[name] = join_values(joined.get(name), value) or EMPTY
    return joined


def local_binding_names(body: list[ast.stmt], arguments: ast.arguments | None) -> set[str]:
    """Every name a function body binds locally, without descending into
    nested function or class bodies (which have their own scopes).

    Python decides a name is local for the *whole* body if it is assigned
    anywhere in it, so these names must not be seeded from the module-level
    summary -- doing so would report an identity the body never sees.
    """
    names: set[str] = set()
    # A comprehension has its own scope: `[value for value in items]` inside a
    # function does not make `value` a local of that function. A walrus inside
    # one does, so those names are kept.
    comprehension_targets: set[str] = set()
    walrus_targets: set[str] = set()
    if arguments is not None:
        for argument in [
            *arguments.posonlyargs,
            *arguments.args,
            *arguments.kwonlyargs,
            *(item for item in (arguments.vararg, arguments.kwarg) if item is not None),
        ]:
            names.add(argument.arg)
    pending: list[ast.AST] = list(body)
    while pending:
        node = pending.pop()
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            names.add(node.name)
            continue
        if isinstance(node, ast.Lambda):
            continue
        if isinstance(node, ast.Global) or isinstance(node, ast.Nonlocal):
            names.difference_update(node.names)
            continue
        if isinstance(node, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)):
            for generator in node.generators:
                for inner in ast.walk(generator.target):
                    if isinstance(inner, ast.Name):
                        comprehension_targets.add(inner.id)
        if isinstance(node, ast.NamedExpr) and isinstance(node.target, ast.Name):
            walrus_targets.add(node.target.id)
        if isinstance(node, ast.Name) and isinstance(node.ctx, (ast.Store, ast.Del)):
            names.add(node.id)
        if isinstance(node, (ast.Import, ast.ImportFrom)):
            for alias in node.names:
                names.add(alias.asname or alias.name.split(".", 1)[0])
            continue
        if isinstance(node, ast.ExceptHandler) and node.name is not None:
            names.add(node.name)
        if isinstance(node, ast.MatchAs) and node.name is not None:
            names.add(node.name)
        if isinstance(node, ast.MatchStar) and node.name is not None:
            names.add(node.name)
        if isinstance(node, ast.MatchMapping) and node.rest is not None:
            names.add(node.rest)
        pending.extend(ast.iter_child_nodes(node))
    return names - (comprehension_targets - walrus_targets)


class LoopFrame:
    """Where `break` and `continue` send the state they were reached with."""

    __slots__ = ("breaks", "continues")

    def __init__(self) -> None:
        self.breaks: list[State] = []
        self.continues: list[State] = []


class Scope:
    """One lexical scope's bookkeeping.

    `kind` matters because Python's scoping is not the same as its nesting: a
    function defined in a class body does *not* see the class body's names, so
    a class frame must be skipped when a nested function resolves a free name.

    `escaping` holds names a `global`/`nonlocal` declaration has connected to an
    outer scope; binding an identity-bearing value to one of those fails closed
    rather than being tracked across an unknown call order.
    """

    __slots__ = ("kind", "escaping", "walrus", "state", "summary")

    def __init__(self, kind: str, state: State) -> None:
        self.kind = kind
        self.state = state
        # Everything this scope ever binds, joined. A nested `def` runs when it
        # is called, so its free names must see the whole enclosing scope, not
        # just the part written above the `def`.
        self.summary: State = {}
        self.escaping: set[str] = set()
        self.walrus: list[str] | None = None


class CapabilityVisitor:
    """Walk one source, resolving aliases and propagating identity and taint.

    Expression handling is explicit rather than generic: `evaluate` returns what
    an expression may be *without* checking how it is used, and `expr` adds the
    use-shape check. That keeps a dotted chain checked once, at its outermost
    node, while still tracking the same chain when it flows through a list, a
    subscript, a branch, a loop, a `match` capture or a container method.

    Every value-bearing and binding AST node must be named in the dispatch
    tables below; anything this analyzer does not model is refused rather than
    walked generically, so a new language construct cannot silently become a
    hole.
    """

    def __init__(self, relative_path: str) -> None:
        self.relative_path = relative_path
        self.grants = SOURCE_SENSITIVE_CAPABILITIES.get(relative_path, {})
        self.scopes: list[Scope] = [Scope("module", {})]
        self.imports: set[str] = set()
        self.terminated = False
        self.loops: list[LoopFrame] = []
        self.bindings: list[set[str]] = []
        self.raise_points: list[State] | None = None
        # Every value a module-level name may hold anywhere in the module. A
        # function body resolves its free names against this, because a
        # function runs when it is *called*, not where it is written -- the
        # import that gives a global its module identity may come after the
        # `def` that uses it.
        self.globals: State = {}
        self.line = 0

    # -- diagnostics ----------------------------------------------------

    def fail(self, message: str) -> None:
        raise CapabilityLintError(f"{self.relative_path}: line {self.line}: {message}")

    def at(self, node: ast.AST) -> None:
        """Remember where a refusal happened; a fail-closed analyzer that
        cannot say *where* is far harder to act on."""
        if isinstance(node, (ast.expr, ast.stmt, ast.pattern, ast.excepthandler)):
            self.line = node.lineno

    @property
    def scope(self) -> Scope:
        return self.scopes[-1]

    @property
    def state(self) -> State:
        return self.scopes[-1].state

    @state.setter
    def state(self, value: State) -> None:
        self.scopes[-1].state = value

    def enclosing_value_state(self) -> State:
        """The state a nested function's free names really resolve against.

        Class bodies are skipped: `class C: x = None` does not give a method
        inside `C` a local `x`, nor does it give a comprehension in the class
        body one, so both must still see the enclosing binding.
        """
        for scope in reversed(self.scopes):
            if scope.kind != "class":
                return join_states([scope.state, scope.summary])
        return {}

    def record_global(self, name: str, value: Value) -> None:
        merged = join_values(self.globals.get(name), value) or EMPTY
        self.globals[name] = merged

    # -- capability checking --------------------------------------------

    def check_capability(self, capability: str, use: str) -> None:
        if capability in FORBIDDEN_CAPABILITIES:
            self.fail(f"uses forbidden dynamic capability {capability}")
        if capability.startswith("importlib.") and not capability.startswith("importlib.metadata"):
            self.fail(f"uses forbidden import-loader capability {capability}")
        parts = capability.split(".")
        if len(parts) > MAX_CAPABILITY_DEPTH:
            self.fail(
                f"builds the dotted reference {capability} deeper than the analyzer bounds; "
                "an unbounded chain is refused rather than approximated"
            )
        if parts[-1] in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader capability {capability}")
        if parts[0] == "sys" and len(parts) >= 2 and parts[1] in FORBIDDEN_SYS_ATTRIBUTES:
            if capability not in self.grants:
                self.fail(f"uses forbidden import-state capability {capability}")
        if parts[0] == "os" and len(parts) >= 2 and (
            parts[1].startswith("exec") or parts[1].startswith("spawn") or parts[1] in {"fork", "popen", "system"}
        ):
            self.fail(f"uses forbidden process capability {capability}")
        if parts[0] not in SENSITIVE_ROOTS:
            # `strict_json.subprocess.run(...)` reaches a sensitive module
            # through a module that is not itself sensitive. Every import of a
            # sensitive module is already declared per source, so re-entering
            # one through another module's namespace is never legitimate -- and
            # neither is doing it through a list, a branch, a loop variable or a
            # `match` capture that may still hold that module.
            smuggled = [part for part in parts[1:] if part in SENSITIVE_ROOTS]
            if smuggled:
                self.fail(
                    f"reaches sensitive module {smuggled[0]!r} through another module's "
                    f"namespace ({capability})"
                )
            return
        granted = self.grants.get(capability)
        if granted is None:
            self.fail(f"uses undeclared source-local capability {capability} (as a {use})")
        if use not in granted:
            self.fail(
                f"uses capability {capability} as a {use}, but its source-local grant only "
                f"allows {sorted(granted)}"
            )

    def check_value(self, value: Value, use: str) -> None:
        for capability in sorted(value.caps):
            self.check_capability(capability, use)

    def reject_taint(self, value: Value, context: str) -> None:
        if value.tainted:
            self.fail(f"{context} a value derived from a propagating capability")

    def reject_escaping_module(self, value: Value, context: str) -> None:
        """A module namespace may be named, but never smuggled out of reach."""
        escaping = sorted(value.namespaces())
        if escaping:
            self.fail(
                f"{context} module namespace {escaping[0]!r}; a module identity may not leave "
                "the reference the boundary can resolve exactly"
            )

    def reject_carrier(self, value: Value, context: str) -> None:
        """Fail closed on any operation this analyzer does not model exactly."""
        self.reject_taint(value, context)
        if value.carries():
            self.fail(
                f"{context} a value that may still hold a module or capability identity through "
                "an operation the boundary does not model exactly"
            )

    def reject_rebind(self, name: str, context: str, replacement: str | None = None) -> None:
        value = self.state.get(name)
        if value is None:
            return
        if replacement is not None and value.caps == frozenset({replacement}):
            return
        sensitive = sorted(
            capability
            for capability in value.caps
            if capability.split(".", 1)[0] in SENSITIVE_ROOTS
        )
        if sensitive:
            self.fail(
                f"{context} sensitive capability alias {name!r} ({sensitive[0]}); lexical "
                "shadowing and control-flow rebinding fail closed"
            )
        if value.tainted:
            self.fail(f"{context} tainted capability alias {name!r}")

    # -- expression evaluation ------------------------------------------

    def evaluate(self, node: ast.AST, use: str = USE_VALUE) -> Value:
        """What `node` may be. Sub-expressions are fully checked here; the
        returned value's own use shape is checked by `expr`."""
        self.at(node)
        handler = EVALUATORS.get(type(node))
        if handler is None:
            self.fail(
                f"uses the unmodelled expression node {type(node).__name__}; the capability "
                "boundary refuses what it cannot analyze exactly"
            )
        return handler(self, node, use)

    def expr(self, node: ast.AST, use: str = USE_VALUE) -> Value:
        value = self.evaluate(node, use)
        self.check_value(value, use)
        if self.raise_points is not None:
            # Any sub-expression can raise, so a handler must see the state a
            # *partly* evaluated statement left behind -- a walrus in the first
            # tuple element is visible even if a later element raises.
            self.raise_points.append(dict(self.state))
        return value

    def _evaluate_Constant(self, node: ast.Constant, use: str) -> Value:
        return EMPTY

    def _evaluate_Name(self, node: ast.Name, use: str) -> Value:
        if "__" in node.id and node.id not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder name {node.id!r}")
        value = self.state.get(node.id)
        if value is not None:
            if value.tainted:
                self.fail(
                    f"uses {node.id!r}, which holds a value derived from a propagating capability"
                )
            return value
        if node.id in BUILTIN_CAPABILITY_NAMES:
            return Value(frozenset({f"builtins.{node.id}"}))
        return EMPTY

    def _evaluate_Attribute(self, node: ast.Attribute, use: str) -> Value:
        if "__" in node.attr and node.attr not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder attribute {node.attr!r}")
        if node.attr in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader attribute {node.attr!r}")
        base = self.evaluate(node.value)
        self.reject_taint(base, "reads an attribute of")
        if base.caps:
            return Value(frozenset(f"{capability}.{node.attr}" for capability in base.caps))
        if node.attr in FORBIDDEN_UNRESOLVED_ATTRIBUTES:
            self.fail(
                f"reads attribute {node.attr!r} through a chain the boundary cannot resolve"
            )
        if base.items is not None:
            # A bound method of a module-bearing container -- `holder.pop`,
            # `holder.get`, `holder.copy` -- may hand back what the container
            # holds, so the identity survives the method call too.
            return Value(frozenset(), base.items, base.tainted)
        return EMPTY

    def _evaluate_Subscript(self, node: ast.Subscript, use: str) -> Value:
        base = self.evaluate(node.value)
        if base.caps:
            self.check_value(base, USE_ASSIGN if use is USE_ASSIGN else USE_VALUE)
        self.reject_taint(base, "subscripts")
        self.expr(node.slice)
        if isinstance(node.slice, ast.Slice):
            # A slice of a container is still that container.
            return Value(frozenset(), base.items, base.tainted)
        # A value can be both a resolved capability (`os.environ`) and a
        # container of identities (a copied list of modules); subscripting must
        # keep whatever it holds either way.
        return base.items if base.items is not None else EMPTY

    def _evaluate_Slice(self, node: ast.Slice, use: str) -> Value:
        for part in (node.lower, node.upper, node.step):
            if part is not None:
                self.reject_carrier(self.expr(part), "builds a slice bound from")
        return EMPTY

    def _evaluate_Call(self, node: ast.Call, use: str) -> Value:
        if isinstance(node.func, (ast.Call, ast.Subscript)):
            self.fail("calls a value derived from a call result or subscript, which is not statically resolvable")
        function = self.evaluate(node.func, USE_CALL)
        self.check_value(function, USE_CALL)
        self.reject_taint(function, "calls")
        for argument in node.args:
            value = self.expr(argument)
            self.reject_taint(value, "passes as an argument")
            self.reject_escaping_module(value, "passes as a call argument")
        for keyword in node.keywords:
            value = self.expr(keyword.value)
            self.reject_taint(value, "passes as a keyword argument")
            self.reject_escaping_module(value, "passes as a keyword call argument")
        tainted = any(
            capability.split(".", 1)[0] in PROPAGATING_ROOTS
            and USE_RESULT not in self.grants.get(capability, frozenset())
            for capability in function.caps
        )
        if function.items is not None:
            # Calling a module-bearing container's bound method may return an
            # element (`pop`) or another container (`copy`); assume both.
            carried = function.items
            return Value(carried.caps, carried, tainted or function.tainted)
        return Value(frozenset(), None, tainted)

    def _evaluate_container(self, elements: list[ast.expr | None]) -> Value:
        values: list[Value] = []
        for element in elements:
            if element is None:
                continue
            value = self.expr(element)
            self.reject_taint(value, "collects")
            values.append(value)
        return contained(values)

    def _evaluate_List(self, node: ast.List, use: str) -> Value:
        return self._evaluate_container(list(node.elts))

    def _evaluate_Tuple(self, node: ast.Tuple, use: str) -> Value:
        return self._evaluate_container(list(node.elts))

    def _evaluate_Set(self, node: ast.Set, use: str) -> Value:
        return self._evaluate_container(list(node.elts))

    def _evaluate_Dict(self, node: ast.Dict, use: str) -> Value:
        return self._evaluate_container([*node.keys, *node.values])

    def _evaluate_Starred(self, node: ast.Starred, use: str) -> Value:
        value = self.expr(node.value)
        self.reject_taint(value, "unpacks")
        return value.items if value.items is not None else EMPTY

    def _evaluate_BinOp(self, node: ast.BinOp, use: str) -> Value:
        left = self.expr(node.left)
        right = self.expr(node.right)
        # `list[ast.expr | None]` is a binary operator over ordinary dotted
        # references, so only a *module namespace* is refused here.
        self.reject_escaping_module(left, "combines with a binary operator")
        self.reject_escaping_module(right, "combines with a binary operator")
        # Concatenating or merging containers keeps whatever they may hold.
        return Value(
            frozenset(),
            join_values(left.items, right.items),
            left.tainted or right.tainted,
        )

    def _evaluate_UnaryOp(self, node: ast.UnaryOp, use: str) -> Value:
        # `not x`, `-x`, `~x` and `+x` all build a new object; no module or
        # capability identity survives one, so the operand is checked as an
        # ordinary value and the result carries nothing.
        self.expr(node.operand)
        return EMPTY

    def _evaluate_Compare(self, node: ast.Compare, use: str) -> Value:
        # `a < b < c` short-circuits: `c` is only evaluated when `a < b` holds.
        self.expr(node.left)
        if node.comparators:
            self.expr(node.comparators[0])
            self._evaluate_branchpoint(list(node.comparators[1:]), USE_VALUE)
        return EMPTY

    def _evaluate_Await(self, node: ast.Await, use: str) -> Value:
        self.reject_carrier(self.expr(node.value), "awaits")
        return EMPTY

    def _evaluate_branchpoint(self, arms: list[ast.expr], use: str) -> Value:
        """Evaluate arms that may or may not run, then join their effects.

        A conditional expression evaluates exactly one arm and `and`/`or`
        short-circuit, so a walrus in a later arm may never happen. Each arm is
        therefore evaluated from a snapshot and the states are joined, which is
        what stops `True or (holder := None)` from erasing an identity.
        """
        entry = dict(self.state)
        result: Value | None = None
        outcomes: list[State] = [entry]
        # Ordered: arm *i* can only run if every arm before it ran, so it is
        # evaluated from the join of the entry state and those arms' effects.
        reached = entry
        for arm in arms:
            self.state = dict(reached)
            result = join_values(result, self.expr(arm, use))
            outcomes.append(self.state)
            reached = join_states([reached, self.state])
        self.state = join_states(outcomes)
        return result or EMPTY

    def _evaluate_IfExp(self, node: ast.IfExp, use: str) -> Value:
        self.expr(node.test)
        # Exactly one arm runs, and either may be the value.
        return self._evaluate_branchpoint([node.body, node.orelse], use)

    def _evaluate_BoolOp(self, node: ast.BoolOp, use: str) -> Value:
        # The first operand always runs; the rest are conditional.
        first = self.expr(node.values[0], use)
        rest = self._evaluate_branchpoint(list(node.values[1:]), use)
        return join_values(first, rest) or EMPTY

    def _evaluate_NamedExpr(self, node: ast.NamedExpr, use: str) -> Value:
        value = self.expr(node.value)
        self.bind(node.target, value)
        if self.scope.walrus is not None and isinstance(node.target, ast.Name):
            # `:=` inside a comprehension binds in the *enclosing* scope, so the
            # identity must not be discarded with the comprehension's own frame.
            self.scope.walrus.append(node.target.id)
        return value

    def _evaluate_JoinedStr(self, node: ast.JoinedStr, use: str) -> Value:
        for part in node.values:
            self.expr(part)
        return EMPTY

    def _evaluate_FormattedValue(self, node: ast.FormattedValue, use: str) -> Value:
        self.reject_escaping_module(self.expr(node.value), "formats")
        if node.format_spec is not None:
            self.expr(node.format_spec)
        return EMPTY

    def _evaluate_TemplateStr(self, node: ast.TemplateStr, use: str) -> Value:
        for part in node.values:
            self.expr(part)
        return EMPTY

    def _evaluate_Interpolation(self, node: ast.Interpolation, use: str) -> Value:
        # A t-string keeps the interpolated *object*, not its text, so an
        # identity would survive into whatever consumes the template.
        self.reject_carrier(self.expr(node.value), "interpolates")
        if node.format_spec is not None:
            self.expr(node.format_spec)
        return EMPTY

    def _evaluate_Lambda(self, node: ast.Lambda, use: str) -> Value:
        for default in [
            *node.args.defaults,
            *(default for default in node.args.kw_defaults if default is not None),
        ]:
            value = self.expr(default)
            self.reject_taint(value, "uses as a default")
            self.reject_escaping_module(value, "uses as a lambda default")
        loops = self.push_scope("function", self.function_entry_state([], node.args))
        try:
            self._bind_parameters(node.args)
            body = self.expr(node.body)
            self.reject_taint(body, "captures in a lambda")
            self.reject_escaping_module(body, "captures in a lambda")
        finally:
            self.pop_scope(loops)
        return EMPTY

    def _evaluate_Yield(self, node: ast.Yield, use: str) -> Value:
        if node.value is not None:
            value = self.expr(node.value)
            self.reject_taint(value, "yields")
            self.reject_escaping_module(value, "yields")
        return EMPTY

    def _evaluate_YieldFrom(self, node: ast.YieldFrom, use: str) -> Value:
        value = self.expr(node.value)
        self.reject_taint(value, "yields")
        self.reject_escaping_module(value, "yields")
        return EMPTY

    def _evaluate_comprehension(
        self, generators: list[ast.comprehension], elements: list[ast.expr]
    ) -> Value:
        # A comprehension is its own function scope, so inside a class body its
        # free names resolve to the *enclosing* scope, not to the class
        # namespace. Its first iterable is the exception: that one is evaluated
        # in the enclosing scope before the comprehension scope exists.
        leading = self.expr(generators[0].iter) if generators else EMPTY
        self.reject_taint(leading, "iterates")
        entry = join_states([self.enclosing_value_state(), dict(self.globals)])
        loops = self.push_scope("function", entry)
        self.scope.walrus = []
        scope = self.scope
        try:
            for index, generator in enumerate(generators):
                if index == 0:
                    iterated = leading
                else:
                    iterated = self.expr(generator.iter)
                    self.reject_taint(iterated, "iterates")
                self.bind(generator.target, iterated.items if iterated.items is not None else EMPTY)
                for condition in generator.ifs:
                    self.reject_taint(self.expr(condition), "filters on")
            values = []
            for element in elements:
                value = self.expr(element)
                self.reject_taint(value, "collects")
                values.append(value)
            result = contained(values)
            leaked = {name: self.state[name] for name in scope.walrus if name in self.state}
        finally:
            self.pop_scope(loops)
        for name, value in leaked.items():
            self.reject_rebind(name, "leaks a comprehension assignment over")
            self.state[name] = value
        return result

    def _evaluate_ListComp(self, node: ast.ListComp, use: str) -> Value:
        return self._evaluate_comprehension(node.generators, [node.elt])

    def _evaluate_SetComp(self, node: ast.SetComp, use: str) -> Value:
        return self._evaluate_comprehension(node.generators, [node.elt])

    def _evaluate_GeneratorExp(self, node: ast.GeneratorExp, use: str) -> Value:
        return self._evaluate_comprehension(node.generators, [node.elt])

    def _evaluate_DictComp(self, node: ast.DictComp, use: str) -> Value:
        return self._evaluate_comprehension(node.generators, [node.key, node.value])

    # -- binding ---------------------------------------------------------

    def bind(self, target: ast.AST, value: Value) -> None:
        if isinstance(target, ast.Name):
            if "__" in target.id and target.id not in ALLOWED_DUNDER_NAMES:
                self.fail(f"binds undeclared dunder name {target.id!r}")
            if target.id in self.scope.escaping and value.carries():
                self.fail(
                    f"binds a module or capability identity to {target.id!r}, which a "
                    "global/nonlocal declaration connects to an outer scope"
                )
            self.reject_rebind(target.id, "binds")
            if self.bindings:
                self.bindings[-1].add(target.id)
            self.scope.summary[target.id] = (
                join_values(self.scope.summary.get(target.id), value) or EMPTY
            )
            if self.scope.kind == "module" or target.id in self.scope.escaping:
                # A module-level binding, or a `global` write from anywhere, is
                # visible to every function body in this module.
                self.record_global(target.id, value)
            if value.carries():
                self.state[target.id] = value
            else:
                self.state.pop(target.id, None)
            return
        if isinstance(target, (ast.Tuple, ast.List)):
            element = value.items if value.items is not None else EMPTY
            for member in target.elts:
                if isinstance(member, ast.Starred):
                    self.bind(member.value, Value(frozenset(), element, value.tainted))
                else:
                    self.bind(member, element)
            return
        if isinstance(target, ast.Starred):
            self.bind(target.value, Value(frozenset(), value.items, value.tainted))
            return
        if isinstance(target, ast.Attribute):
            base = self.evaluate(target.value)
            if base.caps:
                for capability in sorted(base.caps):
                    stored = f"{capability}.{target.attr}"
                    parts = stored.split(".")
                    if len(parts) >= 2 and parts[0] in MODULE_ROOTS:
                        self.fail(
                            f"assigns into another module's namespace ({stored}); a governed "
                            "source may not rewrite an imported module's attributes"
                        )
            self.reject_escaping_module(value, "stores into an attribute")
            self.expr(target, USE_ASSIGN)
            return
        if isinstance(target, ast.Subscript):
            self.reject_escaping_module(value, "stores into a subscript")
            self.expr(target, USE_ASSIGN)
            return
        self.fail(
            f"binds through the unmodelled target node {type(target).__name__}; the capability "
            "boundary refuses what it cannot analyze exactly"
        )

    def bind_targets(self, targets: list[ast.expr], value_node: ast.expr | None, value: Value) -> None:
        for target in targets:
            if (
                isinstance(target, (ast.Tuple, ast.List))
                and isinstance(value_node, (ast.Tuple, ast.List))
                and len(target.elts) == len(value_node.elts)
                and not any(isinstance(element, ast.Starred) for element in target.elts)
            ):
                for element, element_node in zip(target.elts, value_node.elts):
                    self.bind_targets([element], element_node, self.evaluate(element_node))
                continue
            self.bind(target, value)

    def _bind_parameters(self, arguments: ast.arguments) -> None:
        for argument in [
            *arguments.posonlyargs,
            *arguments.args,
            *arguments.kwonlyargs,
            *(item for item in (arguments.vararg, arguments.kwarg) if item is not None),
        ]:
            if argument.annotation is not None:
                self.expr(argument.annotation)
            self.reject_rebind(argument.arg, "binds parameter over")
            self.state.pop(argument.arg, None)

    # -- match patterns ---------------------------------------------------

    def bind_pattern(self, pattern: ast.pattern, subject: Value) -> None:
        """Bind every capture in a `case` pattern from the subject's identity."""
        if isinstance(pattern, ast.MatchValue):
            self.expr(pattern.value)
            return
        if isinstance(pattern, ast.MatchSingleton):
            return
        if isinstance(pattern, ast.MatchSequence):
            element = subject.items if subject.items is not None else EMPTY
            for member in pattern.patterns:
                if isinstance(member, ast.MatchStar):
                    self.bind_pattern(member, subject)
                else:
                    self.bind_pattern(member, element)
            return
        if isinstance(pattern, ast.MatchStar):
            if pattern.name is not None:
                self.bind_name(pattern.name, Value(frozenset(), subject.items, subject.tainted))
            return
        if isinstance(pattern, ast.MatchMapping):
            element = subject.items if subject.items is not None else EMPTY
            for key in pattern.keys:
                self.expr(key)
            for member in pattern.patterns:
                self.bind_pattern(member, element)
            if pattern.rest is not None:
                self.bind_name(pattern.rest, Value(frozenset(), subject.items, subject.tainted))
            return
        if isinstance(pattern, ast.MatchClass):
            self.expr(pattern.cls)
            collapsed = subject.collapsed()
            for member in pattern.patterns:
                self.bind_pattern(member, collapsed)
            for attribute, member in zip(pattern.kwd_attrs, pattern.kwd_patterns):
                if subject.caps:
                    attributed = Value(
                        frozenset(f"{capability}.{attribute}" for capability in subject.caps)
                    )
                else:
                    attributed = collapsed
                self.bind_pattern(member, attributed)
            return
        if isinstance(pattern, ast.MatchOr):
            for member in pattern.patterns:
                self.bind_pattern(member, subject)
            return
        if isinstance(pattern, ast.MatchAs):
            if pattern.pattern is not None:
                self.bind_pattern(pattern.pattern, subject)
            if pattern.name is not None:
                self.bind_name(pattern.name, subject)
            return
        self.fail(
            f"matches through the unmodelled pattern node {type(pattern).__name__}; the "
            "capability boundary refuses what it cannot analyze exactly"
        )

    def bind_name(self, name: str, value: Value) -> None:
        self.bind(ast.Name(id=name, ctx=ast.Store()), value)

    # -- statements -------------------------------------------------------

    def visit(self, tree: ast.Module) -> None:
        """Analyze the module to a fixpoint over its module-level bindings.

        A function body is written before it runs. `def exploit(): holder.x()`
        followed later by `import strict_json as holder` means the body sees a
        module identity that did not exist at the `def`, so one pass in source
        order is unsound. Passes repeat until the summary of what every
        module-level name may hold stops growing, and every function body is
        re-analyzed against the final summary.
        """
        body = list(tree.body)
        for _ in range(MAX_FIXPOINT_ROUNDS):
            before = dict(self.globals)
            self.scopes = [Scope("module", {})]
            self.loops = []
            self.bindings = []
            self.raise_points = None
            self.terminated = False
            self.block(body)
            if self.globals == before:
                return
        self.fail(
            "the module-level binding summary did not reach a fixpoint within the analyzer's "
            "bound; the boundary refuses a module it cannot summarise exactly"
        )

    def block(self, statements: list[ast.stmt]) -> bool:
        """Visit a suite; return True when every path leaves this block.

        Statements after a definite exit are still checked -- dead code is
        governed source too -- but their state is discarded, so an unreachable
        rebinding can neither erase nor invent an identity.
        """
        terminated = False
        for statement in statements:
            if terminated:
                saved = dict(self.state)
                self.terminated = False
                self.statement(statement)
                self.state = saved
                continue
            self.terminated = False
            self.statement(statement)
            terminated = self.terminated
            if self.raise_points is not None and not terminated:
                # Any statement may raise, so an enclosing `except` has to see
                # the state after *every* prefix of the protected suite, not
                # just after the last statement.
                self.raise_points.append(dict(self.state))
        self.terminated = terminated
        return terminated

    def branch(self, statements: list[ast.stmt], state: State) -> tuple[State, bool]:
        outer = self.state
        self.state = dict(state)
        try:
            terminated = self.block(statements)
            return self.state, terminated
        finally:
            self.state = outer

    def statement(self, node: ast.stmt) -> None:
        self.at(node)
        handler = STATEMENTS.get(type(node))
        if handler is None:
            self.fail(
                f"uses the unmodelled statement node {type(node).__name__}; the capability "
                "boundary refuses what it cannot analyze exactly"
            )
        handler(self, node)

    def _statement_Expr(self, node: ast.Expr) -> None:
        self.expr(node.value)

    def _statement_Assert(self, node: ast.Assert) -> None:
        self.expr(node.test)
        if node.msg is not None:
            # The message is only evaluated when the assertion fails.
            self._evaluate_branchpoint([node.msg], USE_VALUE)

    def _statement_Import(self, node: ast.Import) -> None:
        allowed = ALLOWED_IMPORTS | SOURCE_SENSITIVE_IMPORTS.get(self.relative_path, frozenset())
        for alias in node.names:
            if alias.name not in allowed:
                self.fail(f"imports undeclared module {alias.name!r}")
            self.imports.add(alias.name)
            bound = alias.asname or alias.name.split(".", 1)[0]
            replacement = alias.name.split(".", 1)[0]
            self.reject_rebind(bound, "imports over", replacement)
            if self.bindings:
                self.bindings[-1].add(bound)
            self.scope.summary[bound] = Value(frozenset({replacement}))
            if self.scope.kind == "module" or bound in self.scope.escaping:
                self.record_global(bound, Value(frozenset({replacement})))
            self.state[bound] = Value(frozenset({replacement}))

    def _statement_ImportFrom(self, node: ast.ImportFrom) -> None:
        if node.level or node.module is None:
            self.fail("uses a relative or anonymous import")
        allowed = ALLOWED_IMPORTS | SOURCE_SENSITIVE_IMPORTS.get(self.relative_path, frozenset())
        if node.module not in allowed:
            self.fail(f"imports undeclared module {node.module!r}")
        allowed_members = ALLOWED_FROM_IMPORTS.get(node.module)
        for alias in node.names:
            if alias.name == "*":
                self.fail("uses a star import")
            if allowed_members is None or alias.name not in allowed_members:
                self.fail(f"imports undeclared symbol {node.module}.{alias.name}")
            capability = f"{node.module}.{alias.name}"
            self.check_capability(capability, USE_VALUE)
            bound = alias.asname or alias.name
            self.reject_rebind(bound, "imports over", capability)
            if self.bindings:
                self.bindings[-1].add(bound)
            self.scope.summary[bound] = Value(frozenset({capability}))
            if self.scope.kind == "module" or bound in self.scope.escaping:
                self.record_global(bound, Value(frozenset({capability})))
            self.state[bound] = Value(frozenset({capability}))
        self.imports.add(node.module)

    def _statement_Assign(self, node: ast.Assign) -> None:
        value = self.expr(node.value)
        self.bind_targets(list(node.targets), node.value, value)

    def _statement_AnnAssign(self, node: ast.AnnAssign) -> None:
        self.expr(node.annotation)
        if node.value is None:
            return
        value = self.expr(node.value)
        self.bind_targets([node.target], node.value, value)

    def _statement_AugAssign(self, node: ast.AugAssign) -> None:
        value = self.expr(node.value)
        self.reject_taint(value, "accumulates")
        existing = self.state.get(node.target.id) if isinstance(node.target, ast.Name) else None
        self.bind(node.target, join_values(existing, value) or EMPTY)

    def _statement_TypeAlias(self, node: ast.TypeAlias) -> None:
        for parameter in node.type_params:
            self._visit_type_param(parameter)
        self.reject_carrier(self.expr(node.value), "declares a type alias from")
        self.bind(node.name, EMPTY)

    def _visit_type_param(self, parameter: ast.type_param) -> None:
        bound = type_parameter_bound(parameter)
        if bound is not None:
            self.reject_carrier(self.expr(bound), "bounds a type parameter with")
        default = type_parameter_default(parameter)
        if default is not None:
            self.reject_carrier(self.expr(default), "defaults a type parameter to")

    def _statement_Delete(self, node: ast.Delete) -> None:
        for target in node.targets:
            if isinstance(target, ast.Name):
                self.reject_rebind(target.id, "deletes")
                self.state.pop(target.id, None)
                continue
            self.expr(target, USE_ASSIGN)

    def _statement_If(self, node: ast.If) -> None:
        self.expr(node.test)
        entry = dict(self.state)
        body_state, body_terminated = self.branch(list(node.body), entry)
        orelse_state, orelse_terminated = self.branch(list(node.orelse), entry)
        survivors = [
            state
            for state, terminated in (
                (body_state, body_terminated),
                (orelse_state, orelse_terminated),
            )
            if not terminated
        ]
        self.state = join_states(survivors) if survivors else dict(entry)
        self.terminated = body_terminated and orelse_terminated

    def _loop(
        self, target: ast.expr | None, element: Value, body: list[ast.stmt], orelse: list[ast.stmt]
    ) -> None:
        """Zero, one or many iterations, joined to an actual fixpoint.

        A body that never runs must not erase an identity, one that runs must
        not hide a loop-carried one, `continue` must feed the next iteration and
        `break` must feed the state after the loop. The iteration bound below is
        a bug guard, not a truncation: reaching it is a refusal.
        """
        state = dict(self.state)
        breaks: list[State] = []
        for _ in range(MAX_FIXPOINT_ROUNDS):
            frame = LoopFrame()
            self.loops.append(frame)
            outer = self.state
            self.state = dict(state)
            try:
                if target is not None:
                    self.bind(target, element)
                body_terminated = self.block(body)
                body_state = self.state
            finally:
                self.state = outer
                self.loops.pop()
            reachable = [state, *frame.continues]
            if not body_terminated:
                reachable.append(body_state)
            merged = join_states(reachable)
            breaks = frame.breaks
            if merged == state:
                break
            state = merged
        else:
            self.fail(
                "loop state did not reach a fixpoint within the analyzer's bound; the boundary "
                "refuses a loop it cannot summarise exactly"
            )
        orelse_state, orelse_terminated = self.branch(orelse, state)
        after = [] if orelse_terminated else [orelse_state]
        self.state = join_states([*after, *breaks]) if (after or breaks) else dict(state)
        self.terminated = orelse_terminated and not breaks

    def _statement_While(self, node: ast.While) -> None:
        self.expr(node.test)
        self._loop(None, EMPTY, list(node.body), list(node.orelse))

    def _statement_For(self, node: ast.For) -> None:
        iterated = self.expr(node.iter)
        self.reject_taint(iterated, "iterates")
        element = iterated.items if iterated.items is not None else EMPTY
        self._loop(node.target, element, list(node.body), list(node.orelse))

    _statement_AsyncFor = _statement_For

    def _statement_With(self, node: ast.With) -> None:
        for item in node.items:
            value = self.expr(item.context_expr)
            self.reject_taint(value, "enters a context manager over")
            if item.optional_vars is not None:
                self.bind_targets([item.optional_vars], item.context_expr, value)
        self.block(list(node.body))

    _statement_AsyncWith = _statement_With

    def _protected(self, statements: list[ast.stmt], state: State) -> tuple[State, bool, list[State]]:
        """Run a suite while recording the state after every prefix of it."""
        points: list[State] = [dict(state)]
        outer_points = self.raise_points
        self.raise_points = points
        try:
            result_state, terminated = self.branch(statements, state)
        finally:
            self.raise_points = outer_points
        if outer_points is not None:
            outer_points.extend(points)
        return result_state, terminated, points

    def _statement_Try(self, node: ast.Try) -> None:
        """Normal, exceptional and abrupt exits, tracked separately.

        A handler starts after *any* raising prefix of the body, so it joins
        every intermediate state rather than only the state after the last
        statement. And every abrupt exit -- `break`, `continue`, `return`,
        propagating exception -- runs `finally` on its way out, so the states
        those exits carry are the states `finally` produced, not the ones it
        was handed.
        """
        entry = dict(self.state)
        # With a `finally`, break/continue must not reach the enclosing loop
        # until they have passed through it, so they are captured here first.
        captured = LoopFrame() if node.finalbody else None
        if captured is not None:
            self.loops.append(captured)
        try:
            body_state, body_terminated, raise_points = self._protected(list(node.body), entry)
            handler_entry = join_states(
                raise_points if body_terminated else [*raise_points, body_state]
            )
            outcomes: list[State] = []
            for handler in node.handlers:
                state, terminated = self.branch([handler], handler_entry)
                if not terminated:
                    outcomes.append(state)
            if not body_terminated:
                orelse_state, orelse_terminated = self.branch(list(node.orelse), body_state)
                if not orelse_terminated:
                    outcomes.append(orelse_state)
        finally:
            if captured is not None:
                self.loops.pop()
        joined = join_states(outcomes) if outcomes else dict(handler_entry)
        if not node.finalbody:
            self.state = joined
            self.terminated = not outcomes
            return
        # `finally` runs on the way out of every path, including the ones that
        # raise or leave, so it starts from everything any of them could hold.
        final_state, final_terminated = self.branch(
            list(node.finalbody), join_states([handler_entry, joined])
        )
        for state in captured.breaks:
            passed, _ = self.branch(list(node.finalbody), state)
            if self.loops:
                self.loops[-1].breaks.append(passed)
        for state in captured.continues:
            passed, _ = self.branch(list(node.finalbody), state)
            if self.loops:
                self.loops[-1].continues.append(passed)
        self.state = final_state
        self.terminated = final_terminated or not outcomes

    _statement_TryStar = _statement_Try

    def _statement_ExceptHandler(self, node: ast.ExceptHandler) -> None:
        if node.type is not None:
            self.expr(node.type)
        if node.name is not None:
            self.reject_rebind(node.name, "binds exception over")
            self.state.pop(node.name, None)
        self.block(list(node.body))

    def _statement_Match(self, node: ast.Match) -> None:
        """Captures bind before a guard runs, and survive a guard that fails.

        `case holder if False:` binds `holder` and then does not run the body,
        so the state after the match includes the capture without the body's
        effects. Joining only the entry state and the body states lost exactly
        that, which is how a module identity survived a `None` assignment the
        guard prevented.
        """
        subject = self.expr(node.subject)
        entry = dict(self.state)
        outcomes: list[State] = [entry]
        for case in node.cases:
            self.state = dict(entry)
            self.bind_pattern(case.pattern, subject)
            captured = dict(self.state)
            if case.guard is not None:
                self.expr(case.guard)
                # The guard may be false: the capture happened, the body did not.
                outcomes.append(dict(self.state))
            else:
                outcomes.append(captured)
            terminated = self.block(list(case.body))
            if not terminated:
                outcomes.append(self.state)
        # No case is guaranteed to match, so the entry state survives too.
        self.state = join_states(outcomes)
        self.terminated = False

    def _statement_Return(self, node: ast.Return) -> None:
        if node.value is not None:
            value = self.expr(node.value)
            self.reject_taint(value, "returns")
            self.reject_escaping_module(value, "returns")
        self.terminated = True

    def _statement_Raise(self, node: ast.Raise) -> None:
        for part in (node.exc, node.cause):
            if part is not None:
                self.expr(part)
        self.terminated = True

    def _statement_Break(self, node: ast.Break) -> None:
        if self.loops:
            self.loops[-1].breaks.append(dict(self.state))
        self.terminated = True

    def _statement_Continue(self, node: ast.Continue) -> None:
        if self.loops:
            self.loops[-1].continues.append(dict(self.state))
        self.terminated = True

    def _statement_Pass(self, node: ast.Pass) -> None:
        return

    def push_scope(self, kind: str, state: State) -> list[LoopFrame]:
        """Open a lexical scope; `break`/`continue` never cross one."""
        loops = self.loops
        self.loops = []
        self.scopes.append(Scope(kind, state))
        return loops

    def pop_scope(self, loops: list[LoopFrame]) -> Scope:
        scope = self.scopes.pop()
        self.loops = loops
        return scope

    def function_entry_state(self, body: list[ast.stmt], arguments: ast.arguments | None) -> State:
        """What a function body's free names may hold when it is *called*.

        A function is analyzed where it is written but runs when it is called,
        so its free names resolve against every value a module-level name may
        hold anywhere in the module -- including an import that appears after
        the `def`. Names the body itself binds are local for the whole body and
        are therefore not seeded from that summary.
        """
        local = local_binding_names(body, arguments)
        entry = {
            name: value
            for name, value in self.enclosing_value_state().items()
            if name not in local
        }
        for name, value in self.globals.items():
            if name in local:
                continue
            entry[name] = join_values(entry.get(name), value) or EMPTY
        return entry

    def _statement_FunctionDef(self, node: ast.FunctionDef) -> None:
        for decorator in node.decorator_list:
            self.reject_taint(self.expr(decorator), "decorates with")
        for parameter in node.type_params:
            self._visit_type_param(parameter)
        if node.returns is not None:
            self.expr(node.returns)
        for default in [
            *node.args.defaults,
            *(default for default in node.args.kw_defaults if default is not None),
        ]:
            value = self.expr(default)
            self.reject_taint(value, "uses as a default")
            self.reject_escaping_module(value, "uses as a parameter default")
        self.reject_rebind(node.name, "binds function over")
        self.state.pop(node.name, None)
        # The body is analyzed to a fixpoint over its own local bindings, for
        # the same reason the module is: a nested `def` that reads a name the
        # enclosing body binds *below* it still sees that binding at call time.
        summary: State = {}
        for _ in range(MAX_FIXPOINT_ROUNDS):
            entry = self.function_entry_state(list(node.body), node.args)
            loops = self.push_scope("function", entry)
            self.scope.summary = dict(summary)
            try:
                self._bind_parameters(node.args)
                self.block(list(node.body))
                produced = dict(self.scope.summary)
            finally:
                self.pop_scope(loops)
            if produced == summary:
                break
            summary = produced
        else:
            self.fail(
                "the local binding summary of this function did not reach a fixpoint within "
                "the lint's bound"
            )
        self.terminated = False

    _statement_AsyncFunctionDef = _statement_FunctionDef

    def _statement_ClassDef(self, node: ast.ClassDef) -> None:
        for decorator in node.decorator_list:
            self.reject_taint(self.expr(decorator), "decorates with")
        for parameter in node.type_params:
            self._visit_type_param(parameter)
        for base in [*node.bases, *(keyword.value for keyword in node.keywords)]:
            value = self.expr(base)
            self.reject_taint(value, "derives a class from")
            self.reject_escaping_module(value, "derives a class from")
        self.reject_rebind(node.name, "binds class over")
        self.state.pop(node.name, None)
        loops = self.push_scope("class", dict(self.enclosing_value_state()))
        self.bindings.append(set())
        try:
            self.block(list(node.body))
            namespace = dict(self.state)
            attributes = self.bindings[-1]
        finally:
            self.bindings.pop()
            self.pop_scope(loops)
        # A class body is a namespace an outer scope can read back through the
        # class object, which this analyzer does not model attribute by
        # attribute -- so an identity *bound by the body* is refused there
        # rather than lost. Names merely visible from the enclosing scope are
        # not class attributes, so only what the body actually bound counts.
        for name in sorted(attributes):
            value = namespace.get(name, EMPTY)
            self.reject_escaping_module(value, f"stores in the {node.name!r} class namespace")
            self.reject_taint(value, f"stores in the {node.name!r} class namespace")

    def _statement_Global(self, node: ast.Global) -> None:
        for name in node.names:
            self.reject_rebind(name, "declares global")
            self.scope.escaping.add(name)

    def _statement_Nonlocal(self, node: ast.Nonlocal) -> None:
        for name in node.names:
            self.reject_rebind(name, "declares nonlocal")
            self.scope.escaping.add(name)


def type_parameter_bound(parameter: ast.type_param) -> ast.expr | None:
    """A `TypeVar` bound, read structurally rather than through `getattr`."""
    if isinstance(parameter, ast.TypeVar):
        return parameter.bound
    return None


def type_parameter_default(parameter: ast.type_param) -> ast.expr | None:
    if isinstance(parameter, (ast.TypeVar, ast.ParamSpec, ast.TypeVarTuple)):
        return parameter.default_value
    return None


# Every value-bearing and binding node this analyzer models. Dispatch is an
# explicit table rather than a name lookup for two reasons: this analyzer is
# itself a governed source and may not use `getattr`, and a node type that is
# *absent* here must fail closed instead of being walked generically.
EVALUATORS = {
    ast.Attribute: CapabilityVisitor._evaluate_Attribute,
    ast.Await: CapabilityVisitor._evaluate_Await,
    ast.BinOp: CapabilityVisitor._evaluate_BinOp,
    ast.BoolOp: CapabilityVisitor._evaluate_BoolOp,
    ast.Call: CapabilityVisitor._evaluate_Call,
    ast.Compare: CapabilityVisitor._evaluate_Compare,
    ast.Constant: CapabilityVisitor._evaluate_Constant,
    ast.Dict: CapabilityVisitor._evaluate_Dict,
    ast.DictComp: CapabilityVisitor._evaluate_DictComp,
    ast.FormattedValue: CapabilityVisitor._evaluate_FormattedValue,
    ast.GeneratorExp: CapabilityVisitor._evaluate_GeneratorExp,
    ast.IfExp: CapabilityVisitor._evaluate_IfExp,
    ast.Interpolation: CapabilityVisitor._evaluate_Interpolation,
    ast.JoinedStr: CapabilityVisitor._evaluate_JoinedStr,
    ast.Lambda: CapabilityVisitor._evaluate_Lambda,
    ast.List: CapabilityVisitor._evaluate_List,
    ast.ListComp: CapabilityVisitor._evaluate_ListComp,
    ast.Name: CapabilityVisitor._evaluate_Name,
    ast.NamedExpr: CapabilityVisitor._evaluate_NamedExpr,
    ast.Set: CapabilityVisitor._evaluate_Set,
    ast.SetComp: CapabilityVisitor._evaluate_SetComp,
    ast.Slice: CapabilityVisitor._evaluate_Slice,
    ast.Starred: CapabilityVisitor._evaluate_Starred,
    ast.Subscript: CapabilityVisitor._evaluate_Subscript,
    ast.TemplateStr: CapabilityVisitor._evaluate_TemplateStr,
    ast.Tuple: CapabilityVisitor._evaluate_Tuple,
    ast.UnaryOp: CapabilityVisitor._evaluate_UnaryOp,
    ast.Yield: CapabilityVisitor._evaluate_Yield,
    ast.YieldFrom: CapabilityVisitor._evaluate_YieldFrom,
}

STATEMENTS = {
    ast.AnnAssign: CapabilityVisitor._statement_AnnAssign,
    ast.Assert: CapabilityVisitor._statement_Assert,
    ast.Assign: CapabilityVisitor._statement_Assign,
    ast.AsyncFor: CapabilityVisitor._statement_For,
    ast.AsyncFunctionDef: CapabilityVisitor._statement_FunctionDef,
    ast.AsyncWith: CapabilityVisitor._statement_With,
    ast.AugAssign: CapabilityVisitor._statement_AugAssign,
    ast.Break: CapabilityVisitor._statement_Break,
    ast.ClassDef: CapabilityVisitor._statement_ClassDef,
    ast.Continue: CapabilityVisitor._statement_Continue,
    ast.Delete: CapabilityVisitor._statement_Delete,
    ast.ExceptHandler: CapabilityVisitor._statement_ExceptHandler,
    ast.Expr: CapabilityVisitor._statement_Expr,
    ast.For: CapabilityVisitor._statement_For,
    ast.FunctionDef: CapabilityVisitor._statement_FunctionDef,
    ast.Global: CapabilityVisitor._statement_Global,
    ast.If: CapabilityVisitor._statement_If,
    ast.Import: CapabilityVisitor._statement_Import,
    ast.ImportFrom: CapabilityVisitor._statement_ImportFrom,
    ast.Match: CapabilityVisitor._statement_Match,
    ast.Nonlocal: CapabilityVisitor._statement_Nonlocal,
    ast.Pass: CapabilityVisitor._statement_Pass,
    ast.Raise: CapabilityVisitor._statement_Raise,
    ast.Return: CapabilityVisitor._statement_Return,
    ast.Try: CapabilityVisitor._statement_Try,
    ast.TryStar: CapabilityVisitor._statement_Try,
    ast.TypeAlias: CapabilityVisitor._statement_TypeAlias,
    ast.While: CapabilityVisitor._statement_While,
    ast.With: CapabilityVisitor._statement_With,
}

# `ast.pattern` subclasses are handled by `bind_pattern`, which refuses an
# unmodelled one the same way. Kept here so the coverage self-test can prove
# nothing in the grammar is silently unhandled.
PATTERNS = frozenset(
    {
        ast.MatchAs,
        ast.MatchClass,
        ast.MatchMapping,
        ast.MatchOr,
        ast.MatchSequence,
        ast.MatchSingleton,
        ast.MatchStar,
        ast.MatchValue,
    }
)


# The node types above, by name. Computed from the tables themselves so the
# two can never drift, and compared against a committed expectation by the
# boundary self-tests.
MODELLED_NODE_NAMES = (
    frozenset(node.__name__ for node in EVALUATORS)
    | frozenset(node.__name__ for node in STATEMENTS)
    | frozenset(node.__name__ for node in PATTERNS)
)


def unmodelled_nodes_in(source: bytes) -> tuple[str, ...]:
    """Every expression, statement or pattern node in `source` this analyzer
    does not model.

    Reflection over `ast`'s class tree is itself a forbidden traversal here, so
    coverage is proved against a real grammar corpus instead: the self-tests
    parse a source exercising every construct the pinned Python accepts and
    require this to be empty. A construct that is *not* in the corpus still
    fails closed at scan time -- it is refused, not walked generically.
    """
    unmodelled: set[str] = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, (ast.expr, ast.stmt, ast.pattern)) or isinstance(
            node, ast.excepthandler
        ):
            name = type(node).__name__
            if name not in MODELLED_NODE_NAMES:
                unmodelled.add(name)
    return tuple(sorted(unmodelled))


def scan_python_closure(
    entry: str,
    *,
    source_reader=read_source,
) -> tuple[str, ...]:
    if entry not in ENTRY_POINTS:
        raise CapabilityLintError(f"{entry} is not a declared production entry point")
    pending = [entry]
    closure: set[str] = set()
    while pending:
        relative_path = pending.pop()
        if relative_path in closure:
            continue
        _scan_source(relative_path, source_reader, pending)
        closure.add(relative_path)
    return tuple(sorted(closure))


def _scan_source(relative_path: str, source_reader, pending: list[str]) -> None:
    source = source_reader(relative_path)
    try:
        tree = ast.parse(source, filename=relative_path)
    except SyntaxError as error:
        raise CapabilityLintError(f"{relative_path}: invalid Python source: {error}") from error
    visitor = CapabilityVisitor(relative_path)
    visitor.visit(tree)
    for module in sorted(visitor.imports):
        local = _local_source(module)
        if local is not None:
            pending.append(local)


def all_executable_sources() -> tuple[str, ...]:
    actual = {f"scripts/{path.name}" for path in SCRIPTS.glob("*.py")}
    for relative_path in sorted(TRUSTED_SOURCES):
        if relative_path.endswith(".py") and relative_path not in EXECUTABLE_SOURCES:
            raise CapabilityLintError(
                f"{relative_path} is trusted computing base but is not a declared source"
            )
    if actual != EXECUTABLE_SOURCES:
        raise CapabilityLintError(
            "the executable source declaration does not match scripts/*.py; "
            f"missing {sorted(EXECUTABLE_SOURCES - actual)}, unexpected {sorted(actual - EXECUTABLE_SOURCES)}"
        )
    for relative_path, grants in SOURCE_SENSITIVE_CAPABILITIES.items():
        if relative_path not in EXECUTABLE_SOURCES:
            raise CapabilityLintError(f"{relative_path} has grants but is not a declared source")
        for capability, uses in grants.items():
            if not uses or not uses <= USE_SHAPES:
                raise CapabilityLintError(
                    f"{relative_path}: grant for {capability} declares unknown use shapes {sorted(uses)}"
                )
    scanned: set[str] = set()
    for entry in sorted(ENTRY_POINTS):
        scanned.update(scan_python_closure(entry))
    for relative_path in sorted(EXECUTABLE_SOURCES - scanned):
        _scan_source(relative_path, read_source, [])
        scanned.add(relative_path)
    return tuple(sorted(scanned))
