"""Static executable-source boundary for locale-catalog Python tooling.

The production bootstrap scans every declared repository Python executable
before it imports a target. Imports resolve only to declared stdlib,
dependency, or in-tree modules; aliases are resolved before a capability is
accepted, so changing the spelling of an import/load primitive cannot evade
the boundary. The bootstrap itself has two narrowly scoped exceptions:
installing the already-verified repository and site-package roots, then
executing an allowlisted target with ``runpy``.
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
    "importlib.metadata",
    "json",
    "math",
    "os",
    "re",
    "runpy",
    "shlex",
    "shutil",
    "stat",
    "subprocess",
    "sys",
    "sysconfig",
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
    "yaml",
    "extract_backend_i18n_keys",
    "strict_json",
    "json_schema_subset",
    "locale_catalog_python_boundary",
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
        {"ENTRY_POINTS", "SourceBoundaryError", "all_executable_sources", "scan_python_closure"}
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

FORBIDDEN_ATTRIBUTE_NAMES = frozenset(
    {
        "create_module",
        "exec_module",
        "find_spec",
        "load_module",
        "module_from_spec",
        "spec_from_file_location",
        "spec_from_loader",
    }
)

BOOTSTRAP_SOURCE = "scripts/locale_catalog_runtime.py"
BOOTSTRAP_CAPABILITIES = frozenset({"runpy.run_path", "sys.path", "sys.path.insert"})
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


def _relative(path: Path) -> str:
    try:
        return path.relative_to(ROOT).as_posix()
    except ValueError as error:
        raise SourceBoundaryError(f"{path} is outside the repository") from error


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
    def __init__(self, relative_path: str) -> None:
        self.relative_path = relative_path
        self.aliases: dict[str, str] = {}
        self.imports: set[str] = set()

    def fail(self, message: str) -> None:
        raise SourceBoundaryError(f"{self.relative_path}: {message}")

    def resolve(self, node: ast.AST) -> str | None:
        if isinstance(node, ast.Name):
            if node.id in self.aliases:
                return self.aliases[node.id]
            if node.id in {"__import__", "compile", "delattr", "eval", "exec", "getattr", "globals", "locals", "setattr", "vars"}:
                return f"builtins.{node.id}"
            return None
        if isinstance(node, ast.Attribute):
            parent = self.resolve(node.value)
            return f"{parent}.{node.attr}" if parent is not None else None
        return None

    def check_capability(self, capability: str) -> None:
        if capability in BOOTSTRAP_CAPABILITIES and self.relative_path == BOOTSTRAP_SOURCE:
            return
        if capability in FORBIDDEN_CAPABILITIES:
            self.fail(f"uses forbidden dynamic capability {capability}")
        if capability.startswith("importlib.") and not capability.startswith("importlib.metadata"):
            self.fail(f"uses forbidden import-loader capability {capability}")
        parts = capability.split(".")
        if len(parts) >= 2 and parts[0] == "sys" and parts[1] in FORBIDDEN_SYS_ATTRIBUTES:
            self.fail(f"uses forbidden import-state capability {capability}")
        if parts[-1] in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader capability {capability}")
        if len(parts) >= 2 and parts[0] == "os" and (
            parts[1].startswith("exec") or parts[1].startswith("spawn") or parts[1] in {"fork", "popen", "system"}
        ):
            self.fail(f"uses forbidden process capability {capability}")

    def bind(self, target: ast.AST, capability: str | None) -> None:
        if capability is None:
            return
        if isinstance(target, ast.Name):
            self.aliases[target.id] = capability
        elif isinstance(target, (ast.Tuple, ast.List)):
            for element in target.elts:
                self.bind(element, None)

    def visit_Import(self, node: ast.Import) -> None:
        for alias in node.names:
            if alias.name not in ALLOWED_IMPORTS:
                self.fail(f"imports undeclared module {alias.name!r}")
            self.imports.add(alias.name)
            bound = alias.asname or alias.name.split(".", 1)[0]
            self.aliases[bound] = alias.name.split(".", 1)[0]

    def visit_ImportFrom(self, node: ast.ImportFrom) -> None:
        if node.level or node.module is None:
            self.fail("uses a relative or anonymous import")
        if node.module not in ALLOWED_IMPORTS:
            self.fail(f"imports undeclared module {node.module!r}")
        allowed_members = ALLOWED_FROM_IMPORTS.get(node.module)
        for alias in node.names:
            if alias.name == "*":
                self.fail("uses a star import")
            if allowed_members is None or alias.name not in allowed_members:
                self.fail(f"imports undeclared symbol {node.module}.{alias.name}")
            capability = f"{node.module}.{alias.name}"
            self.check_capability(capability)
            self.aliases[alias.asname or alias.name] = capability
        self.imports.add(node.module)

    def visit_Assign(self, node: ast.Assign) -> None:
        self.visit(node.value)
        capability = self.resolve(node.value)
        self.check_capability(capability) if capability is not None else None
        for target in node.targets:
            self.bind(target, capability)

    def visit_AnnAssign(self, node: ast.AnnAssign) -> None:
        if node.value is not None:
            self.visit(node.value)
            capability = self.resolve(node.value)
            self.check_capability(capability) if capability is not None else None
            self.bind(node.target, capability)

    def visit_NamedExpr(self, node: ast.NamedExpr) -> None:
        self.visit(node.value)
        capability = self.resolve(node.value)
        self.check_capability(capability) if capability is not None else None
        self.bind(node.target, capability)

    def visit_Name(self, node: ast.Name) -> None:
        if "__" in node.id and node.id not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder name {node.id!r}")
        capability = self.resolve(node)
        if capability is not None:
            self.check_capability(capability)

    def visit_Attribute(self, node: ast.Attribute) -> None:
        if "__" in node.attr and node.attr not in ALLOWED_DUNDER_NAMES:
            self.fail(f"uses undeclared dunder attribute {node.attr!r}")
        self.visit(node.value)
        capability = self.resolve(node)
        if capability is not None:
            self.check_capability(capability)
        elif node.attr in FORBIDDEN_ATTRIBUTE_NAMES:
            self.fail(f"uses forbidden loader attribute {node.attr!r}")

    def visit_Call(self, node: ast.Call) -> None:
        self.visit(node.func)
        for argument in [*node.args, *(keyword.value for keyword in node.keywords)]:
            self.visit(argument)
        capability = self.resolve(node.func)
        if capability is not None:
            self.check_capability(capability)


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
        source = source_reader(relative_path)
        try:
            tree = ast.parse(source, filename=relative_path)
        except SyntaxError as error:
            raise SourceBoundaryError(f"{relative_path}: invalid Python source: {error}") from error
        visitor = CapabilityVisitor(relative_path)
        visitor.visit(tree)
        closure.add(relative_path)
        for module in sorted(visitor.imports):
            local = _local_source(module)
            if local is not None:
                pending.append(local)
    return tuple(sorted(closure))


def all_executable_sources() -> tuple[str, ...]:
    actual = {f"scripts/{path.name}" for path in SCRIPTS.glob("*.py")}
    if actual != EXECUTABLE_SOURCES:
        raise SourceBoundaryError(
            "the executable source declaration does not match scripts/*.py; "
            f"missing {sorted(EXECUTABLE_SOURCES - actual)}, unexpected {sorted(actual - EXECUTABLE_SOURCES)}"
        )
    scanned: set[str] = set()
    for entry in sorted(ENTRY_POINTS):
        scanned.update(scan_python_closure(entry))
    for relative_path in sorted(EXECUTABLE_SOURCES - scanned):
        source = read_source(relative_path)
        try:
            tree = ast.parse(source, filename=relative_path)
        except SyntaxError as error:
            raise SourceBoundaryError(f"{relative_path}: invalid Python source: {error}") from error
        visitor = CapabilityVisitor(relative_path)
        visitor.visit(tree)
        scanned.add(relative_path)
    return tuple(sorted(scanned))
