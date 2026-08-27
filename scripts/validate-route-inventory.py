#!/usr/bin/env python3
# /// script
# requires-python = ">=3.14"
# dependencies = [
#   "pyyaml==6.0.3",
# ]
# ///

import json
import re
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from urllib.parse import urlparse

import yaml


ROOT = Path(__file__).resolve().parents[1]
INVENTORY_PATH = ROOT / "contracts" / "route-inventory.json"
OPENAPI_PATH = ROOT / "contracts" / "openapi.yaml"
CANONICAL_ROUTE_SOURCE = "backend/arkham-api/config/routes"
HTTP_METHODS = frozenset(
    {"DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT", "TRACE"}
)
SURFACES = frozenset(
    {
        "account",
        "achievements",
        "administration",
        "catalog",
        "decks",
        "diagnostics",
        "events",
        "games",
        "maintenance",
        "service",
    }
)
SCOPES = frozenset(
    {"administrator", "diagnostic", "native-client", "server-operator"}
)
COVERAGE_STATES = frozenset({"documented", "excluded", "required"})
INVENTORY_FIELDS = frozenset(
    {"coverage", "method", "path", "resource", "scope", "surface"}
)


@dataclass(frozen=True)
class Route:
    method: str
    path: str
    resource: str

    def describe(self) -> str:
        return f"{self.method} {self.path} ({self.resource})"


def require(condition: bool, message: str) -> None:
    if not condition:
        raise SystemExit(message)


def normalize_path(segments: list[str]) -> str:
    route = "".join("" if segment == "/" else segment for segment in segments) or "/"
    route = re.sub(r"#([A-Za-z][A-Za-z0-9_]*)", r"{\1}", route)
    require(route.startswith("/"), f"Route path is not absolute: {route}")
    return route


def parse_routes(path: Path) -> list[Route]:
    parent_segments: list[str] = []
    routes: list[Route] = []

    for line_number, raw_line in enumerate(
        path.read_text(encoding="utf-8").splitlines(), start=1
    ):
        if not raw_line.strip() or raw_line.lstrip().startswith("--"):
            continue

        require("\t" not in raw_line, f"{path}:{line_number}: tabs are not supported")
        indentation = len(raw_line) - len(raw_line.lstrip(" "))
        require(
            indentation % 2 == 0,
            f"{path}:{line_number}: indentation must use two-space levels",
        )
        depth = indentation // 2
        require(
            depth <= len(parent_segments),
            f"{path}:{line_number}: route nesting skips a parent level",
        )

        fields = raw_line.strip().split()
        require(
            len(fields) >= 2,
            f"{path}:{line_number}: expected a path and resource",
        )

        path_token, resource_token, *methods = fields
        require(
            path_token.startswith(("/", "!/")),
            f"{path}:{line_number}: unsupported route token {path_token}",
        )
        segment = path_token.removeprefix("!")
        is_parent = resource_token.endswith(":")
        resource = resource_token.removesuffix(":")
        require(
            re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", resource) is not None,
            f"{path}:{line_number}: invalid resource name {resource}",
        )

        parent_segments = parent_segments[:depth]
        normalized_path = normalize_path([*parent_segments, segment])

        if is_parent:
            require(
                not methods,
                f"{path}:{line_number}: parent routes cannot declare methods",
            )
            parent_segments.append(segment)
            continue

        require(methods, f"{path}:{line_number}: route does not declare a method")
        for method in methods:
            require(
                method in HTTP_METHODS,
                f"{path}:{line_number}: unsupported HTTP method {method}",
            )
            routes.append(Route(method, normalized_path, resource))

    duplicates = [route for route, count in Counter(routes).items() if count > 1]
    require(
        not duplicates,
        "Duplicate routes in source: "
        + ", ".join(route.describe() for route in duplicates),
    )
    return routes


def load_inventory() -> tuple[list[dict[str, str]], list[Route]]:
    with INVENTORY_PATH.open(encoding="utf-8") as handle:
        inventory: object = json.load(handle)

    require(isinstance(inventory, dict), "route-inventory.json must be an object")
    require(
        inventory.get("inventoryVersion") == 1,
        "route-inventory.json must use inventoryVersion 1",
    )
    require(
        inventory.get("source") == CANONICAL_ROUTE_SOURCE,
        f"route-inventory.json source must be {CANONICAL_ROUTE_SOURCE}",
    )
    raw_operations = inventory.get("operations")
    require(
        isinstance(raw_operations, list),
        "route-inventory.json operations must be a list",
    )

    operations: list[dict[str, str]] = []
    routes: list[Route] = []
    for index, operation in enumerate(raw_operations):
        location = f"route-inventory.json operations[{index}]"
        require(isinstance(operation, dict), f"{location} must be an object")
        require(
            operation.keys() == INVENTORY_FIELDS,
            f"{location} fields must be exactly {sorted(INVENTORY_FIELDS)}",
        )
        require(
            all(isinstance(value, str) for value in operation.values()),
            f"{location} values must all be strings",
        )
        require(
            operation["method"] in HTTP_METHODS,
            f"{location} has unsupported method {operation['method']}",
        )
        require(
            operation["surface"] in SURFACES,
            f"{location} has unsupported surface {operation['surface']}",
        )
        require(
            operation["scope"] in SCOPES,
            f"{location} has unsupported scope {operation['scope']}",
        )
        require(
            operation["coverage"] in COVERAGE_STATES,
            f"{location} has unsupported coverage {operation['coverage']}",
        )

        if operation["scope"] == "native-client":
            require(
                operation["coverage"] in {"documented", "required"},
                f"{location} native-client route cannot be excluded",
            )
        else:
            require(
                operation["coverage"] == "excluded",
                f"{location} non-client route must be excluded",
            )

        operations.append(operation)
        routes.append(
            Route(operation["method"], operation["path"], operation["resource"])
        )

    duplicates = [route for route, count in Counter(routes).items() if count > 1]
    require(
        not duplicates,
        "Duplicate routes in inventory: "
        + ", ".join(route.describe() for route in duplicates),
    )
    return operations, routes


def compare_routes(actual: list[Route], inventoried: list[Route]) -> None:
    if actual == inventoried:
        return

    actual_set = set(actual)
    inventory_set = set(inventoried)
    missing = [route for route in actual if route not in inventory_set]
    stale = [route for route in inventoried if route not in actual_set]
    details = ["Route inventory does not exactly match the Yesod route source."]

    if missing:
        details.append("Missing from inventory:")
        details.extend(f"  + {route.describe()}" for route in missing)
    if stale:
        details.append("No longer present in route source:")
        details.extend(f"  - {route.describe()}" for route in stale)
    if not missing and not stale:
        first_difference = next(
            index
            for index, pair in enumerate(zip(actual, inventoried, strict=True))
            if pair[0] != pair[1]
        )
        details.append(
            "Operations match but order differs at index "
            f"{first_difference}: source has {actual[first_difference].describe()}, "
            f"inventory has {inventoried[first_difference].describe()}."
        )

    raise SystemExit("\n".join(details))


def route_shape(path: str) -> str:
    return re.sub(r"\{[^}/]+\}", "{}", path)


def load_openapi_operations() -> set[tuple[str, str]]:
    with OPENAPI_PATH.open(encoding="utf-8") as handle:
        document = yaml.safe_load(handle)

    require(isinstance(document, dict), "openapi.yaml must be an object")
    servers = document.get("servers")
    require(
        isinstance(servers, list) and servers,
        "openapi.yaml must declare at least one server",
    )
    server = servers[0]
    require(
        isinstance(server, dict) and isinstance(server.get("url"), str),
        "openapi.yaml first server must declare a URL",
    )
    base_path = urlparse(server["url"]).path.rstrip("/")
    paths = document.get("paths")
    require(isinstance(paths, dict), "openapi.yaml paths must be an object")

    operations: set[tuple[str, str]] = set()
    for path, path_item in paths.items():
        require(
            isinstance(path, str) and path.startswith("/"),
            f"OpenAPI path is not absolute: {path}",
        )
        require(
            isinstance(path_item, dict),
            f"OpenAPI path item must be an object: {path}",
        )
        for method in HTTP_METHODS:
            if method.lower() not in path_item:
                continue
            operation = (method, route_shape(base_path + path))
            require(
                operation not in operations,
                f"Duplicate OpenAPI operation: {method} {path}",
            )
            operations.add(operation)
    return operations


def compare_documented_operations(operations: list[dict[str, str]]) -> None:
    documented_operations = [
        (operation["method"], route_shape(operation["path"]))
        for operation in operations
        if operation["coverage"] == "documented"
    ]
    duplicates = [
        operation
        for operation, count in Counter(documented_operations).items()
        if count > 1
    ]
    require(
        not duplicates,
        "Documented inventory routes have duplicate normalized shapes: "
        + ", ".join(f"{method} {path}" for method, path in duplicates),
    )
    documented = set(documented_operations)
    openapi = load_openapi_operations()
    if documented == openapi:
        return

    missing = sorted(documented - openapi)
    stale = sorted(openapi - documented)
    details = ["OpenAPI operations do not match documented route inventory coverage."]
    if missing:
        details.append("Documented but missing from OpenAPI:")
        details.extend(f"  + {method} {path}" for method, path in missing)
    if stale:
        details.append("Present in OpenAPI but not classified as documented:")
        details.extend(f"  - {method} {path}" for method, path in stale)
    raise SystemExit("\n".join(details))


def main() -> None:
    operations, inventoried = load_inventory()
    route_source = ROOT / CANONICAL_ROUTE_SOURCE
    require(route_source.is_file(), f"Missing route source: {route_source}")
    actual = parse_routes(route_source)
    compare_routes(actual, inventoried)
    compare_documented_operations(operations)

    api_count = sum(route.path.startswith("/api/v1") for route in actual)
    coverage = Counter(operation["coverage"] for operation in operations)
    print(
        f"Validated {len(actual)} exact route operations "
        f"({api_count} under /api/v1): "
        f"{coverage['documented']} documented, "
        f"{coverage['required']} required, "
        f"{coverage['excluded']} excluded."
    )


if __name__ == "__main__":
    main()
