#!/usr/bin/env python3
"""Generate the public locale catalog with the sealed Node authority."""

from __future__ import annotations

from pathlib import Path
import subprocess
import sys

import strict_json

ROOT = Path(__file__).resolve().parents[1]
FRONTEND = ROOT / "frontend"
GENERATOR = FRONTEND / "scripts" / "locale-catalog" / "generate.mjs"


def main() -> None:
    result = subprocess.run(
        [strict_json.trusted_node(), str(GENERATOR), *sys.argv[1:]],
        cwd=FRONTEND,
        check=False,
    )
    if result.returncode != 0:
        raise SystemExit(result.returncode)


if __name__ == "__main__":
    main()
