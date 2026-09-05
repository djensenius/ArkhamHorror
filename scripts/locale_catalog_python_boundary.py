"""Static capability boundary for the locale-catalog Python tooling.

Every declared repository Python source is parsed before any of it is
imported. Imports resolve only to declared stdlib, locked dependency, or
in-tree modules, and *every* dotted reference that reaches a sensitive root is
checked against a per-source grant that names the exact capability **and the
exact shape it may be used in** -- called, read as a value, assigned to, or
consumed as a call result. Granting `subprocess.run` for a call therefore does
not grant `subprocess`, `subprocess.Popen`, or storing `subprocess.run` itself.

On top of that exact-capability rule the analyzer carries conservative taint:
a value derived from a propagating capability (an importer, loader, `runpy`,
`ctypes`, or a serialization loader) may not be called, subscripted,
attribute-accessed, passed as an argument, stored, unpacked, returned,
yielded, defaulted, captured by a lambda, or bound by a `for`/`with`/`except`
target. Anything the analyzer cannot resolve but can see is sensitive fails
closed rather than being assumed safe.

The grant tables live in this file, which is itself a declared executable
source: it is capability-scanned like every other source, and its bytes are
folded into the locale-catalog fixture provenance digest through
`all_executable_sources()`, so a grant cannot be widened without moving a
governed contract hash.
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
    "io",
    "json",
    "math",
    "re",
    "shlex",
    "shutil",
    "stat",
    "tempfile",
    "time",
    "tarfile",
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
    "yaml",
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
    "scripts/test_locale_catalog_python_boundary.py": frozenset({"os", "subprocess"}),
    "scripts/validate-catalog-serving.py": frozenset({"os", "shutil", "subprocess", "sys", "urllib.request"}),
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
        "sys.path.insert": CALL,
        "sys.prefix": VALUE,
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
    },
    "scripts/validate-catalog-serving.py": {
        "os.environ.get": CALL,
        "os.environ.get": CALL,
        "shutil.copyfile": CALL,
        "shutil.rmtree": CALL,
        "subprocess.CompletedProcess": VALUE,
        "subprocess.PIPE": VALUE,
        "subprocess.Popen": CALL_VALUE,
        "subprocess.TimeoutExpired": VALUE,
        "subprocess.run": CALL,
        "sys.argv": VALUE,
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
    },
}

ALLOWED_FROM_IMPORTS = {
    "__future__": frozenset({"annotations"}),
    "collections": frozenset({"Counter"}),
    "builtins": frozenset({"__import__", "compile", "delattr", "eval", "exec", "getattr", "globals", "locals", "setattr", "vars"}),
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
    }
)

BUILTIN_CAPABILITY_NAMES = frozenset(
    {"__import__", "compile", "delattr", "eval", "exec", "getattr", "globals", "locals", "setattr", "vars"}
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


class SourceBoundaryError(ValueError):
    pass


def _source_path(relative_path: str) -> Path:
    path = ROOT / relative_path
    if relative_path not in EXECUTABLE_SOURCES:
        raise SourceBoundaryError(f"{relative_path} is not a declared executable source")
    if path.is_symlink() or not path.is_file():
        raise SourceBoundaryError(f"{relative_path} must be a regular source file")
    if path.resolve(strict=True) != path:
        raise SourceBoundaryError(f"{relative_path} resolves through a symlink")
    return path


def read_source(relative_path: str) -> bytes:
    return _source_path(relative_path).read_bytes()


def _local_source(module: str) -> str | None:
    candidate = LOCAL_MODULE_SOURCES.get(module)
    if candidate is None:
        return None
    _source_path(candidate)
    return candidate


class CapabilityVisitor(ast.NodeVisitor):
    """Walk one source, resolving aliases and propagating taint.

    Descent is controlled rather than generic wherever a dotted chain could be
    involved: an `Attribute`/`Call` that resolves to a capability is checked
    once, at its outermost node, and its own prefix is not re-checked as a bare
    module reference.
    """

    def __init__(self, relative_path: str) -> None:
        self.relative_path = relative_path
        self.grants = SOURCE_SENSITIVE_CAPABILITIES.get(relative_path, {})
        self.aliases: dict[str, str] = {}
        self.tainted: set[str] = set()
        self.imports: set[str] = set()

    # -- diagnostics ----------------------------------------------------

    def fail(self, message: str) -> None:
        raise SourceBoundaryError(f"{self.relative_path}: {message}")

    # -- resolution -----------------------------------------------------

    def resolve(self, node: ast.AST) -> str | None:
        if isinstance(node, ast.Name):
            if node.id in self.tainted:
                return None
            if node.id in self.aliases:
                return self.aliases[node.id]
            if node.id in BUILTIN_CAPABILITY_NAMES:
                return f"builtins.{node.id}"
            return None
        if isinstance(node, ast.Attribute):
            parent = self.resolve(node.value)
            return f"{parent}.{node.attr}" if parent is not None else None
        return None

    def is_tainted(self, node: ast.AST | None) -> bool:
        """True when any part of `node` may carry a propagating capability."""
        if node is None:
            return False
        if isinstance(node, ast.Name):
            return node.id in self.tainted
        if isinstance(node, ast.Call):
            capability = self.resolve(node.func)
            if capability is not None:
                if capability.split(".", 1)[0] not in PROPAGATING_ROOTS:
                    return False
                return USE_RESULT not in self.grants.get(capability, frozenset())
            return self.is_tainted(node.func)
        if isinstance(node, ast.Attribute):
            return self.resolve(node) is None and self.is_tainted(node.value)
        return any(self.is_tainted(child) for child in ast.iter_child_nodes(node))

    def reject_taint(self, node: ast.AST | None, context: str) -> None:
        if self.is_tainted(node):
            self.fail(f"{context} a value derived from a propagating capability")

    def reject_rebind(self, name: str, context: str, replacement: str | None = None) -> None:
        capability = self.aliases.get(name)
        if capability == replacement:
            return
        if capability is not None and capability.split(".", 1)[0] in SENSITIVE_ROOTS:
            self.fail(
                f"{context} sensitive capability alias {name!r} ({capability}); lexical "
                "shadowing and control-flow rebinding fail closed"
            )
        if name in self.tainted:
            self.fail(f"{context} tainted capability alias {name!r}")

    # -- capability checking --------------------------------------------

    def check_capability(self, capability: str, use: str) -> None:
        if capability in FORBIDDEN_CAPABILITIES:
            self.fail(f"uses forbidden dynamic capability {capability}")
        if capability.startswith("importlib.") and not capability.startswith("importlib.metadata"):
            self.fail(f"uses forbidden import-loader capability {capability}")
        parts = capability.split(".")
        if parts[-1] in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader capability {capability}")
        if parts[0] == "sys" and len(parts) >= 2 and parts[1] in FORBIDDEN_SYS_ATTRIBUTES:
            if use is not USE_CALL or capability not in self.grants:
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
            # one through another module's namespace is never legitimate.
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

    # -- expression walking ---------------------------------------------

    def visit_Name(self, node: ast.Name, use: str = USE_VALUE) -> None:
        if "__" in node.id and node.id not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder name {node.id!r}")
        if node.id in self.tainted:
            self.fail(f"uses {node.id!r}, which holds a value derived from a propagating capability")
        capability = self.resolve(node)
        if capability is not None:
            self.check_capability(capability, use)

    def visit_Attribute(self, node: ast.Attribute, use: str = USE_VALUE) -> None:
        if "__" in node.attr and node.attr not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder attribute {node.attr!r}")
        if node.attr in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader attribute {node.attr!r}")
        capability = self.resolve(node)
        if capability is not None:
            self.check_capability(capability, use)
            return
        if node.attr in FORBIDDEN_UNRESOLVED_ATTRIBUTES:
            self.fail(
                f"reads attribute {node.attr!r} through a chain the boundary cannot resolve"
            )
        self.reject_taint(node.value, "reads an attribute of")
        self.visit(node.value)

    def visit_Subscript(self, node: ast.Subscript, use: str = USE_VALUE) -> None:
        capability = self.resolve(node.value)
        if capability is not None:
            self.check_capability(capability, USE_ASSIGN if use is USE_ASSIGN else USE_VALUE)
        else:
            self.reject_taint(node.value, "subscripts")
            self.visit(node.value)
        self.visit(node.slice)

    def visit_Call(self, node: ast.Call) -> None:
        if isinstance(node.func, (ast.Call, ast.Subscript)):
            self.fail("calls a value derived from a call result or subscript, which is not statically resolvable")
        capability = self.resolve(node.func)
        if capability is not None:
            self.check_capability(capability, USE_CALL)
        else:
            self.reject_taint(node.func, "calls")
            self.visit(node.func)
        for argument in node.args:
            self.reject_taint(argument, "passes as an argument")
            self.visit(argument)
        for keyword in node.keywords:
            self.reject_taint(keyword.value, "passes as a keyword argument")
            self.visit(keyword.value)

    # -- binding ---------------------------------------------------------

    def bind(self, target: ast.AST, capability: str | None, tainted: bool) -> None:
        if isinstance(target, ast.Name):
            if "__" in target.id and target.id not in ALLOWED_DUNDER_NAMES:
                self.fail(f"binds undeclared dunder name {target.id!r}")
            self.reject_rebind(target.id, "binds")
            self.aliases.pop(target.id, None)
            self.tainted.discard(target.id)
            if capability is not None:
                self.aliases[target.id] = capability
            elif tainted:
                self.tainted.add(target.id)
            return
        if isinstance(target, ast.Starred):
            self.bind(target.value, None, tainted)
            return
        if isinstance(target, (ast.Tuple, ast.List)):
            for element in target.elts:
                self.bind(element, None, tainted)
            return
        if isinstance(target, ast.Attribute):
            resolved = self.resolve(target)
            if resolved is not None:
                self.check_capability(resolved, USE_ASSIGN)
            else:
                self.visit_Attribute(target, USE_ASSIGN)
            return
        if isinstance(target, ast.Subscript):
            self.visit_Subscript(target, USE_ASSIGN)
            return
        self.visit(target)

    def bind_value(self, targets: list[ast.AST], value: ast.AST | None) -> None:
        capability = self.resolve(value) if value is not None else None
        if capability is not None:
            self.check_capability(capability, USE_VALUE)
        tainted = capability is None and self.is_tainted(value)
        for target in targets:
            if isinstance(target, (ast.Tuple, ast.List)) and isinstance(value, (ast.Tuple, ast.List)):
                if len(target.elts) == len(value.elts):
                    for element, element_value in zip(target.elts, value.elts):
                        self.bind_value([element], element_value)
                    continue
            self.bind(target, capability, tainted)

    # -- statements -------------------------------------------------------

    def visit_Import(self, node: ast.Import) -> None:
        allowed = ALLOWED_IMPORTS | SOURCE_SENSITIVE_IMPORTS.get(self.relative_path, frozenset())
        for alias in node.names:
            if alias.name not in allowed:
                self.fail(f"imports undeclared module {alias.name!r}")
            self.imports.add(alias.name)
            bound = alias.asname or alias.name.split(".", 1)[0]
            replacement = alias.name.split(".", 1)[0]
            self.reject_rebind(bound, "imports over", replacement)
            self.aliases[bound] = replacement
            self.tainted.discard(bound)

    def visit_ImportFrom(self, node: ast.ImportFrom) -> None:
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
            self.aliases[bound] = capability
            self.tainted.discard(bound)
        self.imports.add(node.module)

    def visit_Assign(self, node: ast.Assign) -> None:
        self.visit(node.value)
        self.bind_value(list(node.targets), node.value)

    def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
        self.visit(node.annotation)
        if node.value is None:
            return
        self.visit(node.value)
        self.bind_value([node.target], node.value)

    def visit_AugAssign(self, node: ast.AugAssign) -> None:
        self.visit(node.value)
        self.reject_taint(node.value, "accumulates")
        self.bind(node.target, None, self.is_tainted(node.value))

    def visit_NamedExpr(self, node: ast.NamedExpr) -> None:
        self.visit(node.value)
        self.bind_value([node.target], node.value)

    def visit_For(self, node: ast.For) -> None:
        self.visit(node.iter)
        self.reject_taint(node.iter, "iterates")
        self.bind(node.target, None, False)
        for statement in [*node.body, *node.orelse]:
            self.visit(statement)

    visit_AsyncFor = visit_For

    def visit_With(self, node: ast.With) -> None:
        for item in node.items:
            self.visit(item.context_expr)
            self.reject_taint(item.context_expr, "enters a context manager over")
            if item.optional_vars is not None:
                self.bind_value([item.optional_vars], item.context_expr)
        for statement in node.body:
            self.visit(statement)

    visit_AsyncWith = visit_With

    def visit_ExceptHandler(self, node: ast.ExceptHandler) -> None:
        if node.type is not None:
            self.visit(node.type)
        if node.name is not None:
            self.reject_rebind(node.name, "binds exception over")
            aliases = dict(self.aliases)
            tainted = set(self.tainted)
            self.aliases.pop(node.name, None)
            self.tainted.discard(node.name)
        try:
            for statement in node.body:
                self.visit(statement)
        finally:
            if node.name is not None:
                self.aliases = aliases
                self.tainted = tainted

    def visit_Return(self, node: ast.Return) -> None:
        if node.value is not None:
            self.visit(node.value)
            self.reject_taint(node.value, "returns")
            capability = self.resolve(node.value)
            if capability is not None:
                self.check_capability(capability, USE_VALUE)

    def visit_Yield(self, node: ast.Yield) -> None:
        if node.value is not None:
            self.visit(node.value)
            self.reject_taint(node.value, "yields")

    def visit_YieldFrom(self, node: ast.YieldFrom) -> None:
        self.visit(node.value)
        self.reject_taint(node.value, "yields")

    def _visit_scope(self, node: ast.AST, arguments: ast.arguments, body: list[ast.AST]) -> None:
        for default in [*arguments.defaults, *(default for default in arguments.kw_defaults if default is not None)]:
            self.visit(default)
            self.reject_taint(default, "uses as a default")
        aliases = dict(self.aliases)
        tainted = set(self.tainted)
        for argument in [
            *arguments.posonlyargs,
            *arguments.args,
            *arguments.kwonlyargs,
            *(item for item in (arguments.vararg, arguments.kwarg) if item is not None),
        ]:
            if argument.annotation is not None:
                self.visit(argument.annotation)
            self.reject_rebind(argument.arg, "binds parameter over")
            self.aliases.pop(argument.arg, None)
            self.tainted.discard(argument.arg)
        try:
            for statement in body:
                self.visit(statement)
        finally:
            self.aliases = aliases
            self.tainted = tainted

    def visit_FunctionDef(self, node: ast.FunctionDef) -> None:
        for decorator in node.decorator_list:
            self.visit(decorator)
            self.reject_taint(decorator, "decorates with")
        if node.returns is not None:
            self.visit(node.returns)
        self.reject_rebind(node.name, "binds function over")
        self.aliases.pop(node.name, None)
        self.tainted.discard(node.name)
        self._visit_scope(node, node.args, list(node.body))

    visit_AsyncFunctionDef = visit_FunctionDef

    def visit_Lambda(self, node: ast.Lambda) -> None:
        self._visit_scope(node, node.args, [])
        self.visit(node.body)
        self.reject_taint(node.body, "captures in a lambda")

    def visit_ClassDef(self, node: ast.ClassDef) -> None:
        for decorator in node.decorator_list:
            self.visit(decorator)
            self.reject_taint(decorator, "decorates with")
        for base in [*node.bases, *(keyword.value for keyword in node.keywords)]:
            self.visit(base)
            self.reject_taint(base, "derives a class from")
        self.reject_rebind(node.name, "binds class over")
        self.aliases.pop(node.name, None)
        self.tainted.discard(node.name)
        aliases = dict(self.aliases)
        tainted = set(self.tainted)
        try:
            for statement in node.body:
                self.visit(statement)
        finally:
            self.aliases = aliases
            self.tainted = tainted

    def _visit_comprehension(self, node: ast.AST, generators: list[ast.comprehension], elements: list[ast.AST]) -> None:
        aliases = dict(self.aliases)
        tainted = set(self.tainted)
        try:
            for generator in generators:
                self.visit(generator.iter)
                self.reject_taint(generator.iter, "iterates")
                self.bind(generator.target, None, False)
                for condition in generator.ifs:
                    self.visit(condition)
                    self.reject_taint(condition, "filters on")
            for element in elements:
                self.visit(element)
                self.reject_taint(element, "collects")
        finally:
            self.aliases = aliases
            self.tainted = tainted

    def visit_Global(self, node: ast.Global) -> None:
        for name in node.names:
            self.reject_rebind(name, "declares global")

    def visit_Nonlocal(self, node: ast.Nonlocal) -> None:
        for name in node.names:
            self.reject_rebind(name, "declares nonlocal")

    def visit_ListComp(self, node: ast.ListComp) -> None:
        self._visit_comprehension(node, node.generators, [node.elt])

    def visit_SetComp(self, node: ast.SetComp) -> None:
        self._visit_comprehension(node, node.generators, [node.elt])

    def visit_GeneratorExp(self, node: ast.GeneratorExp) -> None:
        self._visit_comprehension(node, node.generators, [node.elt])

    def visit_DictComp(self, node: ast.DictComp) -> None:
        self._visit_comprehension(node, node.generators, [node.key, node.value])

    def _visit_collection(self, node: ast.AST, elements: list[ast.AST | None]) -> None:
        for element in elements:
            if element is None:
                continue
            self.visit(element)
            self.reject_taint(element, "collects")
            capability = self.resolve(element)
            if capability is not None:
                self.check_capability(capability, USE_VALUE)

    def visit_List(self, node: ast.List) -> None:
        self._visit_collection(node, list(node.elts))

    def visit_Tuple(self, node: ast.Tuple) -> None:
        self._visit_collection(node, list(node.elts))

    def visit_Set(self, node: ast.Set) -> None:
        self._visit_collection(node, list(node.elts))

    def visit_Dict(self, node: ast.Dict) -> None:
        self._visit_collection(node, [*node.keys, *node.values])

    def visit_Starred(self, node: ast.Starred) -> None:
        self.reject_taint(node.value, "unpacks")
        self.visit(node.value)


def scan_python_closure(
    entry: str,
    *,
    source_reader=read_source,
) -> tuple[str, ...]:
    if entry not in ENTRY_POINTS:
        raise SourceBoundaryError(f"{entry} is not a declared production entry point")
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
        raise SourceBoundaryError(f"{relative_path}: invalid Python source: {error}") from error
    visitor = CapabilityVisitor(relative_path)
    visitor.visit(tree)
    for module in sorted(visitor.imports):
        local = _local_source(module)
        if local is not None:
            pending.append(local)


def all_executable_sources() -> tuple[str, ...]:
    actual = {f"scripts/{path.name}" for path in SCRIPTS.glob("*.py")}
    if actual != EXECUTABLE_SOURCES:
        raise SourceBoundaryError(
            "the executable source declaration does not match scripts/*.py; "
            f"missing {sorted(EXECUTABLE_SOURCES - actual)}, unexpected {sorted(actual - EXECUTABLE_SOURCES)}"
        )
    for relative_path, grants in SOURCE_SENSITIVE_CAPABILITIES.items():
        if relative_path not in EXECUTABLE_SOURCES:
            raise SourceBoundaryError(f"{relative_path} has grants but is not a declared source")
        for capability, uses in grants.items():
            if not uses or not uses <= USE_SHAPES:
                raise SourceBoundaryError(
                    f"{relative_path}: grant for {capability} declares unknown use shapes {sorted(uses)}"
                )
    scanned: set[str] = set()
    for entry in sorted(ENTRY_POINTS):
        scanned.update(scan_python_closure(entry))
    for relative_path in sorted(EXECUTABLE_SOURCES - scanned):
        _scan_source(relative_path, read_source, [])
        scanned.add(relative_path)
    return tuple(sorted(scanned))
