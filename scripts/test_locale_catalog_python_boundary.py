#!/usr/bin/env python3
"""Regression tests for the locale-catalog tooling's checks and policies.

What this project trusts and what it does not, so these tests can be read for
what they are:

* **Trusted.** Every committed executable file here -- workflows, shell,
  Python, Node, the frontend locale modules and their tests -- is reviewed
  through pull request. A malicious commit, reviewer or maintainer is out of
  scope, and nothing in this repository sandboxes its own code.
* **Executed but not contained.** Pull-request CI runs unreviewed PR code by
  design. What is bounded is its authority, not its behaviour: ephemeral jobs,
  `contents: read`, no secrets, no publication.
* **Untrusted and checked.** Externally produced tool and dependency
  artifacts, environment and input data, cache contents, and generated output.
  Those are the things these tests actually gate.

So the probes below fall into three honest groups: *drift* checks (a reviewed
file, a digest list or a generated artifact no longer matches what was
recorded), *identity* checks (an external tool or dependency is exactly the
pinned artifact), and *policy* checks (CI privilege, production
centralisation, dependency lifecycle, ownership-safe cleanup). None of them
claims to constrain committed code, and the capability lint they exercise is a
review aid, not a proof.

Every probe runs inside an invocation-owned temporary copy of the governed
tree. Nothing here creates, overwrites, truncates, restores or deletes a path
inside the canonical worktree, and `main()` proves it by re-checking the
canonical paths byte for byte at the end.
"""

from __future__ import annotations

import argparse
import base64
import csv
import hashlib
import json
import os
import re
from pathlib import Path
import shutil
import subprocess
import time
import uuid

import yaml

import locale_catalog_python_boundary
import strict_json

ROOT = Path(__file__).resolve().parents[1]
LAUNCHER_RELATIVE_PATH = "scripts/run-locale-catalog-python.sh"
BOUNDARY_TEST_RELATIVE_PATH = "scripts/test_locale_catalog_python_boundary.py"
RUNTIME_PROFILE_RELATIVE_PATH = "scripts/locale_catalog_python_runtime.json"
FIXTURE_ENTRY = "scripts/build-locale-catalog-fixture.py"
HELPER_SOURCE = "scripts/json_schema_subset.py"
GRANTED_SOURCE = "scripts/strict_json.py"

# The exact repository subtree every governed Python command reads. A probe
# copy missing one of these fails loudly during setup rather than silently
# proving nothing.
GOVERNED_TREE_PATHS = (
    "scripts",
    "contracts",
    "frontend/schemas",
    "frontend/scripts/locale-catalog",
    "frontend/package.json",
    "frontend/package-lock.json",
    "mise.toml",
    "pyproject.toml",
    "uv.lock",
    "Dockerfile",
    ".github/workflows",
    "offline/scripts/03-build-frontend.sh",
)

OWNER_SENTINEL = ".locale-catalog-boundary-owner"
PROBE_PREFIX = "locale-catalog-boundary-"
SCRATCH_OWNER_FILE = ".locale-catalog-boundary-scratch-owner"
WORKSPACE_PREFIX = ".locale-catalog-python."
WORKSPACE_OWNER_FILE = "owner"

# Canonical paths earlier revisions of this test created, overwrote, truncated,
# restored or removed by fixed name. They are witnessed here, never written.
CANONICAL_WITNESS_PATHS = (
    "locale-catalog-python-dependency-marker",
    "scripts/__pycache__",
    "scripts/build-locale-catalog-fixture.py",
    "scripts/strict_json.py",
    "scripts/json_schema_subset.py",
    "scripts/locale_catalog_python_boundary.py",
    "scripts/locale_catalog_runtime.py",
    "scripts/locale_catalog_python_runtime.json",
    "scripts/run-locale-catalog-python.sh",
    "scripts/locale-catalog-python-sealed.sh",
    "scripts/generate-locale-catalog.py",
    "pyproject.toml",
    "uv.lock",
    "frontend/schemas/locale-catalog/v1/manifest.schema.json",
    "contracts/manifest.json",
)

# Every reported bypass and variant the production visitor must reject, in the
# exact shape it was reported. Each is prepended to a real governed source.
LINT_REGRESSION_SNIPPETS: dict[str, bytes] = {
    "runpy.run_path": b'import runpy\nrunpy.run_path("payload.py")\n',
    "runpy.run_module": b'import runpy\nrunpy.run_module("payload")\n',
    "aliased runpy": b'import runpy as r\nr.run_path("payload.py")\n',
    "subprocess.run": b'import subprocess\nsubprocess.run(["id"])\n',
    "subprocess.Popen": b'import subprocess\nsubprocess.Popen(["id"])\n',
    "subprocess.call": b'import subprocess\nsubprocess.call(["id"])\n',
    "subprocess.check_call": b'import subprocess\nsubprocess.check_call(["id"])\n',
    "subprocess.check_output": b'import subprocess\nsubprocess.check_output(["id"])\n',
    "os.system": b'import os\nos.system("id")\n',
    "os.spawnv": b'import os\nos.spawnv(os.P_WAIT, "/bin/sh", ["sh"])\n',
    "os.execv": b'import os\nos.execv("/bin/sh", ["sh"])\n',
    "os.popen": b'import os\nos.popen("id")\n',
    "importlib.import_module": b'import importlib\nimportlib.import_module("os")\n',
    "importlib loader spec": (
        b"import importlib\n"
        b'spec = importlib.util.spec_from_file_location("m", "m.py")\n'
    ),
    "importlib.metadata entry point load": (
        b"import importlib.metadata\n"
        b"importlib.metadata.entry_points()[0].load()\n"
    ),
    "importlib.metadata entry point alias": (
        b"import importlib.metadata as md\n"
        b"loaded = md.entry_points()\n"
        b"loaded[0].load()\n"
    ),
    "zipimport loader": b'import zipimport\nzipimport.zipimporter("x").load_module("os")\n',
    "ctypes": b'import ctypes\nctypes.CDLL("libc.so.6")\n',
    "pickle.loads": b"import pickle\npickle.loads(b'')\n",
    "marshal.loads": b"import marshal\nmarshal.loads(b'')\n",
    "shelve.open": b'import shelve\nshelve.open("db")\n',
    "builtins exec": b'exec("x = 1")\n',
    "builtins eval": b'eval("1")\n',
    "builtins compile": b'compile("x = 1", "<s>", "exec")\n',
    "builtins __import__": b'__import__("os")\n',
    "aliased builtins exec": b'import builtins as runtime_builtins\nruntime_builtins.exec("x = 1")\n',
    "imported builtins exec": b'from builtins import exec as runtime_exec\nruntime_exec("x = 1")\n',
    "aliased builtins import": b'from builtins import __import__ as runtime_import\nruntime_import("os")\n',
    "getattr sys.path": b'import sys\ngetattr(sys, "path")\n',
    "setattr sys.path": b'import sys\nsetattr(sys, "path", [])\n',
    "vars traversal": b"import sys\nvars(sys)\n",
    "globals traversal": b'globals()["payload"]\n',
    "locals traversal": b"locals()\n",
    "dynamic code object": b'code = compile("x = 1", "<s>", "exec")\nexec(code)\n',
    "aliased sys.path append": b'import sys as runtime_sys\nruntime_sys.path.append("outside")\n',
    "imported sys.modules": b"from sys import modules\nmodules.clear()\n",
    "imported sys.meta_path": b"from sys import meta_path\nmeta_path.clear()\n",
    "imported sys.path_hooks": b"from sys import path_hooks\npath_hooks.clear()\n",
    "pathlib import path mutation": (
        b"import sys\nfrom pathlib import Path\nsys.path.insert(0, str(Path('x')))\n"
    ),
    "sys in list literal": b'import sys\nholder = [sys]\nholder[0].path.append("outside")\n',
    "sys in tuple literal": b"import sys\nholder = (sys,)\nholder[0].modules.clear()\n",
    "sys in set literal": b"import sys\nholder = {sys}\n",
    "sys in dict literal": b'import sys\nholder = {"s": sys}\nholder["s"].path.append("x")\n',
    "sys in comprehension": b'import sys\nholder = [module for module in (sys,)]\nholder[0].path.append("x")\n',
    "sys in dict comprehension": b'import sys\nholder = {"s": module for module in (sys,)}\n',
    "sys through unpacking": b'import sys\nfirst, second = sys, sys\nfirst.path.append("x")\n',
    "sys through starred unpacking": b'import sys\nfirst, *rest = [sys]\nfirst.path.append("x")\n',
    "sys through subscript": b'import sys\nholder = [sys]\nchosen = holder[0]\nchosen.path.append("x")\n',
    "sys through slice": b"import sys\nholder = [sys][0:1]\n",
    "sys through function return": (
        b"import sys\n\n\ndef expose():\n    return sys\n\n\nexpose().path.append('x')\n"
    ),
    "sys through generator yield": (
        b"import sys\n\n\ndef expose():\n    yield sys\n"
    ),
    "sys through yield from": (
        b"import sys\n\n\ndef expose():\n    yield from [sys]\n"
    ),
    "sys through parameter default": (
        b"import sys\n\n\ndef expose(module=sys):\n    return module.path\n"
    ),
    "sys through keyword-only default": (
        b"import sys\n\n\ndef expose(*, module=sys):\n    return module.modules\n"
    ),
    "sys through lambda": b"import sys\nexpose = lambda: sys\n",
    "sys through closure": (
        b"import sys\n\n\ndef outer():\n    captured = sys\n\n    def inner():\n"
        b"        return captured.path\n\n    return inner\n"
    ),
    "sys through global": (
        b"import sys\n\nEXPOSED = None\n\n\ndef expose():\n    global EXPOSED\n"
        b"    EXPOSED = sys\n"
    ),
    "sys through nonlocal": (
        b"import sys\n\n\ndef outer():\n    captured = None\n\n    def inner():\n"
        b"        nonlocal captured\n        captured = sys\n\n    return inner\n"
    ),
    "sys through class field": (
        b"import sys\n\n\nclass Holder:\n    module = sys\n"
    ),
    "sys through instance field": (
        b"import sys\n\n\nclass Holder:\n    def __init__(self):\n        self.module = sys\n"
    ),
    "sys through attribute store": (
        b"import sys\nfrom pathlib import Path\n\nholder = Path('x')\nholder.module = sys\n"
    ),
    "sys through context manager": (
        b"import sys\n\nwith sys as module:\n    module.path.append('x')\n"
    ),
    "sys through for target": b"import sys\n\nfor module in [sys]:\n    module.path.append('x')\n",
    "sys through except alias": (
        b"import sys\n\ntry:\n    pass\nexcept Exception:\n    holder = sys\n"
    ),
    "sys through decorator": (
        b"import sys\n\n\n@sys.audit\ndef decorated():\n    return None\n"
    ),
    "sys through augmented assignment": b"import sys\n\nholder = []\nholder += [sys]\n",
    "sys through walrus": b"import sys\n\nif (holder := sys):\n    holder.path.append('x')\n",
    "sensitive module through another namespace": (
        b'import strict_json\nstrict_json.os.system("id")\n'
    ),
    "sys through another module namespace": (
        b"import strict_json\nstrict_json.sys.path.append('x')\n"
    ),
    "loader dunder": b'__loader__.load_module("os")\n',
    "spec dunder traversal": b"holder = __spec__.origin\n",
    "class dunder traversal": b"holder = ().__class__.__bases__[0]\n",
    "subclasses dunder traversal": b"holder = ().__class__.__subclasses__()\n",
    "builtins dunder traversal": b'holder = __builtins__["payload"]\n',
    "import path mutation via path_hooks": (
        b"import sys\nsys.path_hooks.insert(0, None)\n"
    ),
    "import path importer cache": b"import sys\nsys.path_importer_cache.clear()\n",
    "meta path mutation": b"import sys\nsys.meta_path.insert(0, None)\n",
}

# The subset re-proved end to end through the real authoritative command, so
# the in-process matrix is never the only thing between a bypass and a
# governed byte.
LINT_END_TO_END_CASES = (
    "runpy.run_path",
    "subprocess.run",
    "os.system",
    "importlib.metadata entry point load",
    "sys in list literal",
    "getattr sys.path",
    "aliased sys.path append",
    "loader dunder",
)

# A source that already holds a narrow grant must not be able to widen it:
# these run against `strict_json.py`, which is granted `subprocess.run` as a
# call, `os.environ` as a value, and nothing else.
GRANT_WIDENING_SNIPPETS: dict[str, bytes] = {
    "granted source widening to subprocess.Popen": b'import subprocess\nsubprocess.Popen(["id"])\n',
    "granted source widening to subprocess.check_output": (
        b'import subprocess\nsubprocess.check_output(["id"])\n'
    ),
    "granted source widening to os.system": b'import os\nos.system("id")\n',
    "granted source storing its granted callee": b"import subprocess\nhandler = subprocess.run\n",
    "granted source aliasing its granted callee": (
        b"import subprocess\nfrom pathlib import Path\n\nholder = [subprocess.run]\n"
    ),
    "granted source widening os.environ": b"import os\nos.environ.clear()\n",
    "granted source widening to sys.path": b'import sys\nsys.path.append("x")\n',
    "granted source widening to sys.modules": b"import sys\nsys.modules.clear()\n",
}

SCOPE_LINT_REGRESSION_SNIPPETS: dict[str, bytes] = {
    "function parameter shadow": (
        b"import subprocess\n\ndef shadow(subprocess):\n    pass\n\nsubprocess.Popen(['id'])\n"
    ),
    "function assignment shadow": (
        b"import subprocess\n\ndef shadow():\n    subprocess = None\n\nsubprocess.Popen(['id'])\n"
    ),
    "conditional assignment shadow": (
        b"import subprocess\nif False:\n    subprocess = None\nsubprocess.Popen(['id'])\n"
    ),
    "class shadow": b"import subprocess\nclass subprocess:\n    pass\nsubprocess.Popen(['id'])\n",
    "exception alias shadow": (
        b"import subprocess\ntry:\n    pass\nexcept Exception as subprocess:\n    pass\nsubprocess.Popen(['id'])\n"
    ),
    "for target shadow": b"import subprocess\nfor subprocess in ():\n    pass\nsubprocess.Popen(['id'])\n",
    "with target shadow": (
        b"import subprocess\nwith object() as subprocess:\n    pass\nsubprocess.Popen(['id'])\n"
    ),
    "comprehension target shadow": (
        b"import subprocess\n[value for subprocess in ()]\nsubprocess.Popen(['id'])\n"
    ),
    "import alias shadow": b"import subprocess\nimport json as subprocess\nsubprocess.Popen(['id'])\n",
}

# ---------------------------------------------------------------------------
# The analyzer matrix: every reported bypass shape, and the controls that must
# still be accepted. Payloads are spliced into a real governed source so they
# reach the production analyzer as ordinary module-level code.
# ---------------------------------------------------------------------------

FUTURE_IMPORT = b"from __future__ import annotations\n"


def splice_snippet(original: bytes, snippet: bytes) -> bytes:
    """Put a payload where it will really be analyzed.

    Code cannot precede `from __future__`, so a naive prepend into a source
    that has one is refused as a syntax error and proves nothing about the
    capability boundary.
    """
    index = original.find(FUTURE_IMPORT)
    if index == -1:
        return snippet + original
    cut = index + len(FUTURE_IMPORT)
    return original[:cut] + b"\n" + snippet + original[cut:]


ANALYZER_PAYLOADS: dict[str, bytes] = {
    # The exact payloads the cumulative review reproduced against the previous
    # revision, in the shape they were reported.
    "late global bound after the def": (
        b'def exploit():\n    holder.subprocess.run(["id"])\n\n\n'
        b"import strict_json as holder\n\nexploit()\n"
    ),
    "late global through a nested def": (
        b"def outer():\n    def inner():\n        return holder.subprocess\n\n    return inner\n"
        b"\n\nimport strict_json as holder\n"
    ),
    "class local shadowing a global in a method": (
        b"import strict_json as holder\n\n\nclass C:\n    holder = None\n\n"
        b'    def exploit(self):\n        holder.subprocess.run(["id"])\n\n\nC().exploit()\n'
    ),
    "class local shadowing a global in a lambda": (
        b"import strict_json as holder\n\n\nclass C:\n    holder = None\n"
        b"    exploit = lambda self: holder.subprocess\n"
    ),
    "except handler after a raising prefix": (
        b"import strict_json\n\nholder = None\ntry:\n    holder = strict_json\n"
        b'    int("x")\n    holder = None\nexcept ValueError:\n'
        b'    holder.subprocess.run(["id"])\n'
    ),
    "finally on the break path": (
        b"import strict_json\n\nholder = None\nfor _ in range(1):\n    try:\n        break\n"
        b'    finally:\n        holder = strict_json\nholder.subprocess.run(["id"])\n'
    ),
    "finally on the continue path": (
        b"import strict_json\n\nholder = None\nfor _ in range(1):\n    try:\n"
        b"        continue\n    finally:\n        holder = strict_json\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "short-circuit walrus that never runs": (
        b"import strict_json\n\nholder = strict_json\nTrue or (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "conditional expression walrus that never runs": (
        b"import strict_json\n\nholder = strict_json\n1 if True else (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "match capture kept by a false guard": (
        b"import strict_json\n\nmatch strict_json:\n    case holder if False:\n"
        b'        holder = None\nholder.subprocess.run(["id"])\n'
    ),
    # The five categories the design review reproduced against the previous
    # revision. These are lint correctness bugs, not exploits: the code they
    # describe is trusted either way, and the point is that the lint reports
    # what it claims to report.
    "nested late-bound closure": (
        b"def outer():\n    def exploit():\n"
        b'        holder.subprocess.run(["id"])\n\n'
        b"    import strict_json as holder\n\n    exploit()\n\n\nouter()\n"
    ),
    "class-body comprehension resolving a free name": (
        b"import strict_json as holder\n\n\nclass C:\n    holder = None\n"
        b"    leaked = [holder.subprocess for _ in range(1)]\n"
    ),
    "exception after a tuple element changed state": (
        b"import strict_json\n\nholder = None\ntry:\n"
        b'    pair = ((holder := strict_json), int("x"), (holder := None))\n'
        b'except ValueError:\n    holder.subprocess.run(["id"])\n'
    ),
    "exception after a call argument changed state": (
        b"import strict_json\n\nholder = None\ntry:\n"
        b'    print((holder := strict_json), int("x"), (holder := None))\n'
        b'except ValueError:\n    holder.subprocess.run(["id"])\n'
    ),
    "exception after a comparison operand changed state": (
        b"import strict_json\n\nholder = None\ntry:\n"
        b'    flag = len([(holder := strict_json)]) < int("x")\n    holder = None\n'
        b'except ValueError:\n    holder.subprocess.run(["id"])\n'
    ),
    "chained comparison short circuit": (
        b"import strict_json\n\nholder = strict_json\n1 < 0 < (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "assert message skip path": (
        b"import strict_json\n\nholder = strict_json\nassert True, (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "ordered boolop skip path": (
        b"import strict_json\n\nholder = strict_json\nFalse and (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "three-operand boolop skip path": (
        b"import strict_json\n\nholder = strict_json\n"
        b"True or (holder := None) or (holder := None)\n"
        b'holder.subprocess.run(["id"])\n'
    ),
    "match capture": (
        b"import strict_json\n\nmatch strict_json:\n    case holder:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "match sequence capture": (
        b"import strict_json\n\nmatch [strict_json]:\n    case [holder]:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "match star capture": (
        b"import strict_json\n\nmatch [strict_json]:\n    case [*rest]:\n"
        b"        rest[0].subprocess.run(['id'])\n"
    ),
    "match mapping capture": (
        b"import strict_json\n\nmatch {'m': strict_json}:\n    case {'m': holder}:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "match mapping rest": (
        b"import strict_json\n\nmatch {'m': strict_json}:\n    case {**rest}:\n"
        b"        rest['m'].subprocess.run(['id'])\n"
    ),
    "match class attribute": (
        b"import strict_json\nfrom pathlib import Path\n\nmatch strict_json:\n"
        b"    case Path(subprocess=holder):\n        holder.run(['id'])\n"
    ),
    "match or capture": (
        b"import strict_json\n\nmatch strict_json:\n    case 1 | holder:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "match as capture": (
        b"import strict_json\n\nmatch strict_json:\n    case object() as holder:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "type alias value": b"import strict_json\n\ntype Alias = strict_json.subprocess\n",
    "type parameter bound": (
        b"import strict_json\n\n\ndef generic[T: strict_json.subprocess](value: T) -> T:\n"
        b"    return value\n"
    ),
    "template string interpolation": b"import strict_json\n\nholder = t'{strict_json}'\n",
    "class namespace module": b"import strict_json\n\n\nclass Holder:\n    module = strict_json\n",
    "class namespace rebind": (
        b"import strict_json\n\n\nclass Holder:\n    strict_json = strict_json\n"
    ),
    "container pop": (
        b"import strict_json\n\nholder = [strict_json]\nholder.pop().subprocess.run(['id'])\n"
    ),
    "container get": (
        b"import strict_json\n\nholder = {'m': strict_json}\n"
        b"holder.get('m').subprocess.run(['id'])\n"
    ),
    "container copy index": (
        b"import strict_json\n\nholder = [strict_json]\n"
        b"holder.copy()[0].subprocess.run(['id'])\n"
    ),
    "container slice": (
        b"import strict_json\n\nholder = [strict_json][0:1]\nholder[0].subprocess.run(['id'])\n"
    ),
    "container comprehension": (
        b"import strict_json\n\nholder = [module for module in [strict_json]][0]\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "comprehension walrus leak": (
        b"import strict_json\n\nvalues = [(holder := strict_json) for _ in range(1)]\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "global write": (
        b"import strict_json\n\nEXPOSED = None\n\n\ndef expose():\n    global EXPOSED\n"
        b"    EXPOSED = strict_json\n"
    ),
    "nonlocal write": (
        b"import strict_json\n\n\ndef outer():\n    captured = None\n\n    def inner():\n"
        b"        nonlocal captured\n        captured = strict_json\n\n    return inner\n"
    ),
    "false if": (
        b"import strict_json\n\nif False:\n    holder = strict_json\nelse:\n    holder = None\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "true if reversed": (
        b"import strict_json\n\nif True:\n    holder = None\nelse:\n    holder = strict_json\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "zero-iteration for": (
        b"import strict_json\n\nholder = strict_json\nfor holder in []:\n    pass\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "zero-iteration while": (
        b"import strict_json\n\nholder = strict_json\nwhile False:\n    holder = None\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "loop break carries": (
        b"import strict_json\n\nholder = None\nfor value in range(3):\n"
        b"    holder = strict_json\n    break\nholder.subprocess.run(['id'])\n"
    ),
    "loop continue carries": (
        b"import strict_json\n\nholder = None\nfor value in range(3):\n    if value:\n"
        b"        holder = strict_json\n        continue\n    holder = None\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "loop carried identity": (
        b"import strict_json\n\nholder = None\nfor value in range(3):\n    later = holder\n"
        b"    holder = strict_json\nlater.subprocess.run(['id'])\n"
    ),
    "untaken except": (
        b"import strict_json\n\ntry:\n    holder = strict_json\nexcept Exception:\n"
        b"    holder = None\nholder.subprocess.run(['id'])\n"
    ),
    "except prefix state": (
        b"import strict_json\n\nholder = None\ntry:\n    holder = strict_json\n"
        b"    raise ValueError\nexcept ValueError:\n    holder.subprocess.run(['id'])\n"
    ),
    "finally state": (
        b"import strict_json\n\ntry:\n    holder = strict_json\nfinally:\n"
        b"    holder.subprocess.run(['id'])\n"
    ),
    "conditional expression": (
        b"import strict_json\n\nholder = strict_json if False else None\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "short circuit or": (
        b"import strict_json\n\nholder = None or strict_json\nholder.subprocess.run(['id'])\n"
    ),
    "short circuit and": (
        b"import strict_json\n\nholder = strict_json and strict_json\n"
        b"holder.subprocess.run(['id'])\n"
    ),
    "async for capture": (
        b"import strict_json\n\n\nasync def run():\n    async for holder in [strict_json]:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "async with capture": (
        b"import strict_json\n\n\nasync def run():\n    async with strict_json as holder:\n"
        b"        holder.subprocess.run(['id'])\n"
    ),
    "await identity": (
        b"import strict_json\n\n\nasync def run():\n    holder = await strict_json.thing\n"
        b"    return holder\n"
    ),
    "subscript index": (
        b"import strict_json\n\nholder = [strict_json][0]\nholder.subprocess.run(['id'])\n"
    ),
    "nested container": (
        b"import strict_json\n\nholder = [[strict_json]][0][0]\nholder.subprocess.run(['id'])\n"
    ),
    "breakpoint builtin": b"breakpoint()\n",
    "breakpoint commands argument": b"breakpoint(commands=['import os'])\n",
    "aliased breakpoint": b"import builtins\nbuiltins.breakpoint()\n",
    "imported breakpoint": b"from builtins import breakpoint as stop\nstop()\n",
    "yaml load": b"import yaml\nyaml.load('x')\n",
    "yaml unsafe_load": b"import yaml\nyaml.unsafe_load('x')\n",
    "yaml full loader": b"import yaml\nyaml.load('x', Loader=yaml.FullLoader)\n",
    "yaml unsafe loader": b"import yaml\nyaml.load('x', Loader=yaml.UnsafeLoader)\n",
    "yaml constructor registration": b"import yaml\nyaml.add_constructor('!x', None)\n",
    "yaml safe_load without a grant": b"import yaml\nyaml.safe_load('x')\n",
    "tarfile extraction": b"import tarfile\ntarfile.open('x')\n",
    "io module": b"import io\nio.open('x')\n",
}

# Code that must still be *accepted*: a fail-closed analyzer that refuses
# ordinary Python is not a boundary, it is an outage.
ANALYZER_CONTROLS: dict[str, bytes] = {
    "method reading a class attribute through self": (
        b"class Holder:\n    value = 1\n\n    def get(self):\n        return self.value\n"
    ),
    "function with a local that shadows a global": (
        b"COUNT = 1\n\n\ndef compute():\n    COUNT = 2\n    return COUNT\n"
    ),
    "try except finally over data": (
        b"holder = 0\ntry:\n    holder = 1\n    int('1')\n    holder = 2\nexcept ValueError:\n"
        b"    holder = 3\nfinally:\n    holder = 4\n"
    ),
    "loop with try finally over data": (
        b"total = 0\nfor value in range(2):\n    try:\n        if value:\n            break\n"
        b"        continue\n    finally:\n        total += 1\n"
    ),
    "short-circuit walrus over data": (
        b"holder = 1\nTrue or (holder := 2)\nresult = holder\n"
    ),
    "match guard over data": (
        b"value = 1\nmatch value:\n    case found if found > 0:\n        result = found\n"
        b"    case _:\n        result = 0\n"
    ),
    "comprehension target stays local to the comprehension": (
        b"def outer():\n    names = [name for name in ('a', 'b')]\n    return names\n\n\n"
        b"def other():\n    name = 1\n    return name\n"
    ),
    "nested closure over data": (
        b"def outer():\n    def inner():\n        return total\n\n    total = 1\n"
        b"    return inner()\n"
    ),
    "class body comprehension over data": (
        b"class Holder:\n    values = [index for index in range(3)]\n"
    ),
    "chained comparison over data": b"low = 1\nhigh = 3\nflag = low < 2 < high\n",
    "assert with a message over data": b"count = 1\nassert count, f'count was {count}'\n",
    "try with a walrus over data": (
        b"holder = 0\ntry:\n    pair = ((holder := 1), int('1'), (holder := 2))\n"
        b"except ValueError:\n    holder = 3\n"
    ),
    "plain conditional": b"holder = 1 if True else 2\n",
    "loop accumulation": b"total = 0\nfor value in range(3):\n    total += value\n",
    "loop with break and continue": (
        b"for value in range(3):\n    if value:\n        continue\n    break\n"
    ),
    "match on data": (
        b"value = 1\nmatch value:\n    case 1:\n        result = 'one'\n"
        b"    case [first, *rest]:\n        result = first\n    case {'k': found}:\n"
        b"        result = found\n    case _:\n        result = 'other'\n"
    ),
    "type alias and generics": (
        b"type Alias = dict[str, int]\n\n\ndef generic[T](value: T) -> T:\n    return value\n"
    ),
    "formatted and template strings": (
        b"name = 'x'\ntext = f'{name}!'\ntemplate = t'{name}!'\n"
    ),
    "class with data and methods": (
        b"class Holder:\n    value = 1\n\n    def get(self):\n        return self.value\n"
    ),
    "comprehension walrus over data": (
        b"values = [(doubled := value * 2) for value in range(3)]\nresult = doubled\n"
    ),
    "global data write": b"COUNT = 0\n\n\ndef bump():\n    global COUNT\n    COUNT = 1\n",
    "try except else finally": (
        b"try:\n    value = 1\nexcept ValueError:\n    value = 2\nelse:\n    value = 3\n"
        b"finally:\n    value = 4\n"
    ),
    "nested containers of data": (
        b"holder = [[1, 2], [3]]\nfirst = holder[0][0]\nsliced = holder[0:1]\n"
    ),
    "with statement": (
        b"from pathlib import Path\n\nwith Path('x').open() as handle:\n    data = handle.read()\n"
    ),
}


TAMPER_TARGETS = (
    "scripts/build-locale-catalog-fixture.py",
    "scripts/locale-catalog-python-sealed.sh",
    "scripts/strict_json.py",
    "scripts/json_schema_subset.py",
    "scripts/locale_catalog_python_boundary.py",
    "scripts/locale_catalog_python_runtime.json",
    "frontend/schemas/locale-catalog/v1/manifest.schema.json",
)

HOSTILE_EXECUTABLES = (
    "python",
    "python3",
    "python3.14",
    "uv",
    "git",
    "env",
    "bash",
    "sh",
    "date",
    "mktemp",
)


class ProbeFailure(SystemExit):
    pass


def require(condition: object, message: str) -> None:
    if not condition:
        raise ProbeFailure(f"locale-catalog python boundary: {message}")


def find_single(root: Path, pattern: str, what: str) -> Path:
    """Exactly-one glob match, with a clear failure instead of StopIteration."""
    matches = sorted(root.glob(pattern))
    require(
        len(matches) == 1,
        f"expected exactly one {what} matching {pattern!r} under {root}, found {len(matches)}",
    )
    return matches[0]


def runtime_profile() -> dict:
    return json.loads((ROOT / RUNTIME_PROFILE_RELATIVE_PATH).read_text(encoding="utf-8"))


def sealed_root_from_environment() -> Path:
    raw = os.environ.get("LOCALE_CATALOG_MISE_ROOT")
    require(raw is not None, "this test must run through the sealed locale-catalog launcher")
    return Path(raw)


def sealed_uv_binary() -> Path:
    return find_single(
        sealed_root_from_environment() / "installs" / "uv", "0.12.6/uv-*/uv", "sealed uv binary"
    )


# ---------------------------------------------------------------------------
# Canonical-worktree witnesses
# ---------------------------------------------------------------------------


def witness(path: Path) -> str:
    if path.is_symlink():
        return f"symlink:{path.readlink()}"
    if not path.exists():
        return "absent"
    if path.is_dir():
        entries = sorted(child.name for child in path.iterdir())
        return "dir:" + hashlib.sha256("\n".join(entries).encode("utf-8")).hexdigest()
    return "file:" + hashlib.sha256(path.read_bytes()).hexdigest()


def capture_canonical_witnesses() -> dict[str, str]:
    return {
        relative_path: witness(ROOT / relative_path)
        for relative_path in CANONICAL_WITNESS_PATHS
    }


def require_canonical_unchanged(before: dict[str, str]) -> None:
    after = capture_canonical_witnesses()
    changed = sorted(key for key in before if before[key] != after[key])
    require(
        not changed,
        "the adversarial probes changed canonical worktree paths that must never be written: "
        f"{changed}",
    )
    stray = sorted(
        child.name
        for child in ROOT.iterdir()
        if child.name.startswith(PROBE_PREFIX)
        or child.name.startswith("locale-catalog-python-hook-")
        or child.name.startswith("locale-catalog-boundary-")
    )
    require(not stray, f"probe scratch leaked into the canonical worktree: {stray}")


# ---------------------------------------------------------------------------
# Invocation-owned probe trees
# ---------------------------------------------------------------------------


def git_environment() -> dict[str, str]:
    return {**os.environ, **strict_json.THROWAWAY_GIT_COMMIT_ENV_OVERRIDES}


def run_git(tree: Path, arguments: list[str]) -> None:
    result = subprocess.run(
        strict_json.git_argv(arguments),
        cwd=tree,
        capture_output=True,
        env=git_environment(),
        timeout=300,
    )
    require(
        result.returncode == 0,
        f"probe setup failure: {arguments!r} exited {result.returncode}: "
        f"{result.stderr.decode('utf-8', errors='replace')}",
    )


def create_probe_tree(
    scratch: Path,
    name: str,
    token: str,
    *,
    governed_paths: tuple[str, ...] = GOVERNED_TREE_PATHS,
    with_history: bool = True,
) -> Path:
    """Copy the exact governed tree into a fresh, uniquely named, token-owned
    directory. `mkdir()` without `exist_ok` makes the creation exclusive, so
    two concurrent invocations can never share one probe tree.
    """
    tree = scratch / f"{PROBE_PREFIX}{name}-{uuid.uuid4().hex}"
    tree.mkdir()
    (tree / OWNER_SENTINEL).write_text(token, encoding="utf-8")
    for relative_path in governed_paths:
        source = ROOT / relative_path
        require(
            source.exists(),
            f"probe setup failure: governed tree path {relative_path!r} is missing from the "
            "canonical worktree, so this probe would prove nothing",
        )
        destination = tree / relative_path
        destination.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, destination, symlinks=True)
        else:
            shutil.copy2(source, destination)
    if with_history:
        run_git(tree, ["git", "init", "-q"])
        run_git(tree, ["git", "add", "-A"])
        run_git(tree, ["git", "commit", "-q", "-m", "probe baseline"])
    return tree


def release_probe_tree(tree: Path, token: str) -> None:
    """Remove a probe tree only after re-proving this invocation owns it."""
    sentinel = tree / OWNER_SENTINEL
    if tree.is_symlink() or not tree.is_dir():
        return
    if sentinel.is_symlink() or not sentinel.is_file():
        return
    if sentinel.read_text(encoding="utf-8") != token:
        return
    shutil.rmtree(tree)


def owned_scratch() -> tuple[Path, str]:
    """Create a token-owned probe parent inside the checked-out repository.

    Every destructive probe below stays below this new directory.  Keeping it
    in the repository (rather than an ambient platform temp path) makes its
    ownership and cleanup checks explicit and lets the canonical-worktree
    witness prove it did not survive.
    """
    token = uuid.uuid4().hex
    scratch = ROOT / f"{PROBE_PREFIX}root-{token}"
    scratch.mkdir(mode=0o700)
    (scratch / SCRATCH_OWNER_FILE).write_text(token, encoding="ascii")
    return scratch, token


def release_owned_scratch(scratch: Path, token: str) -> None:
    owner = scratch / SCRATCH_OWNER_FILE
    require(
        scratch.is_dir() and not scratch.is_symlink(),
        f"probe scratch {scratch} is no longer a regular directory",
    )
    require(
        owner.is_file() and not owner.is_symlink() and owner.read_text(encoding="ascii") == token,
        f"probe scratch {scratch} ownership record changed; refusing cleanup",
    )
    shutil.rmtree(scratch)


# ---------------------------------------------------------------------------
# Running the real authoritative command inside a probe tree
# ---------------------------------------------------------------------------


def probe_environment(
    overrides: dict[str, str] | None = None, *, drop: tuple[str, ...] = ()
) -> dict[str, str]:
    environment = {**os.environ}
    environment.pop("ARKHAM_LOCALE_CATALOG_PYTHON_VENV", None)
    for key in drop:
        environment.pop(key, None)
    if overrides is not None:
        environment.update(overrides)
    return environment


def run_authoritative(
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
    timeout: int = 1800,
) -> subprocess.CompletedProcess:
    return subprocess.run(
        [str(tree / LAUNCHER_RELATIVE_PATH), *arguments],
        cwd=tree,
        env=probe_environment() if environment is None else environment,
        capture_output=True,
        text=True,
        check=False,
        timeout=timeout,
    )


def require_authoritative_failure(
    label: str,
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
) -> None:
    result = run_authoritative(tree, arguments, environment=environment)
    require(
        result.returncode != 0,
        f"{label} was accepted by the real authoritative command\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )


def require_authoritative_success(
    label: str,
    tree: Path,
    arguments: list[str],
    *,
    environment: dict[str, str] | None = None,
) -> None:
    result = run_authoritative(tree, arguments, environment=environment)
    require(
        result.returncode == 0,
        f"{label} was rejected by the real authoritative command\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )


# ---------------------------------------------------------------------------
# Probe: the production capability visitor rejects every reported bypass
# ---------------------------------------------------------------------------


def probe_tree_reader(tree: Path):
    def reader(relative_path: str) -> bytes:
        path = tree / relative_path
        require(
            relative_path in locale_catalog_python_boundary.EXECUTABLE_SOURCES,
            f"probe setup failure: {relative_path!r} is not a declared executable source",
        )
        require(
            not path.is_symlink() and path.is_file(),
            f"probe setup failure: {relative_path!r} is missing from the probe tree",
        )
        return path.read_bytes()

    return reader


def lint_rejects(tree: Path, reader, target: str, label: str, snippet: bytes) -> None:
    path = tree / target
    original = path.read_bytes()
    try:
        path.write_bytes(splice_snippet(original, snippet))
        rejected = False
        try:
            locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        except locale_catalog_python_boundary.CapabilityLintError:
            rejected = True
    finally:
        path.write_bytes(original)
    require(rejected, f"the production capability visitor accepted {label!r} in {target}")


def test_capability_lint_matrix(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "capability-matrix", token, with_history=False)
    try:
        reader = probe_tree_reader(tree)
        locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        checked = 0
        matrices = (
            ((FIXTURE_ENTRY, HELPER_SOURCE), LINT_REGRESSION_SNIPPETS),
            ((GRANTED_SOURCE,), {**GRANT_WIDENING_SNIPPETS, **SCOPE_LINT_REGRESSION_SNIPPETS}),
        )
        for targets, snippets in matrices:
            for target in targets:
                for label, snippet in sorted(snippets.items()):
                    lint_rejects(tree, reader, target, label, snippet)
                    checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


def test_capability_lint_end_to_end(scratch: Path, token: str) -> int:
    checked = 0
    for label in LINT_END_TO_END_CASES:
        snippet = LINT_REGRESSION_SNIPPETS[label]
        for target in (FIXTURE_ENTRY, HELPER_SOURCE):
            tree = create_probe_tree(scratch, "bypass", token, with_history=False)
            try:
                path = tree / target
                path.write_bytes(splice_snippet(path.read_bytes(), snippet))
                require_authoritative_failure(
                    f"{label} in {target}", tree, [FIXTURE_ENTRY, "--check"]
                )
                checked += 1
            finally:
                release_probe_tree(tree, token)
    return checked


def test_validated_target_really_executes(scratch: Path, token: str) -> int:
    """A successful preflight is not sufficient: the selected checker must run."""
    tree = create_probe_tree(scratch, "target-execution", token, with_history=False)
    try:
        result = run_authoritative(tree, [FIXTURE_ENTRY, "--must-not-be-ignored"])
        require(
            result.returncode != 0 and "unrecognized arguments: --must-not-be-ignored" in result.stderr,
            "the sealed boundary accepted an argument that only the governed target can reject; "
            "it verified the target without executing it\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        return 1
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: source, schema and bytecode tampering
# ---------------------------------------------------------------------------


def test_source_and_schema_drift(scratch: Path, token: str) -> int:
    checked = 0
    for relative_path in TAMPER_TARGETS:
        tree = create_probe_tree(scratch, "tamper", token)
        try:
            path = tree / relative_path
            path.write_bytes(path.read_bytes() + b"\n")
            require_authoritative_failure(
                f"tampered {relative_path}", tree, [FIXTURE_ENTRY, "--check"]
            )
            checked += 1
        finally:
            release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "symlink-source", token, with_history=False)
    try:
        target = tree / "scripts" / "strict_json.py"
        moved = tree / "scripts" / "strict_json_original.py"
        target.rename(moved)
        target.symlink_to(moved)
        require_authoritative_failure("symlinked governed source", tree, [FIXTURE_ENTRY, "--check"])
        checked += 1
    finally:
        release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "bytecode", token, with_history=False)
    try:
        cache = tree / "scripts" / "__pycache__"
        cache.mkdir()
        (cache / "strict_json.cpython-314.pyc").write_bytes(b"poisoned")
        require_authoritative_failure(
            "poisoned repository bytecode", tree, [FIXTURE_ENTRY, "--check"]
        )
        checked += 1
    finally:
        release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "undeclared-source", token, with_history=False)
    try:
        (tree / "scripts" / "smuggled.py").write_bytes(b"VALUE = 1\n")
        require_authoritative_failure(
            "undeclared executable source", tree, [FIXTURE_ENTRY, "--check"]
        )
        checked += 1
    finally:
        release_probe_tree(tree, token)

    for relative_path in (
        "scripts/locale_catalog_python_boundary/__init__.py",
        "scripts/json/__init__.py",
        "scripts/locale_catalog_python_boundary.cpython-314-darwin.so",
        "scripts/strict_json.cpython-314-darwin.dylib",
    ):
        tree = create_probe_tree(scratch, "nested-import-shadow", token, with_history=False)
        try:
            path = tree / relative_path
            path.parent.mkdir(exist_ok=True)
            path.write_bytes(b"non-executable shadow probe\n")
            require_authoritative_failure(
                f"nested importable shadow {relative_path}", tree, [FIXTURE_ENTRY, "--check"]
            )
            checked += 1
        finally:
            release_probe_tree(tree, token)
    return checked


def test_workflow_base_authority_wiring() -> int:
    text = (ROOT / ".github" / "workflows" / "contracts.yml").read_text(encoding="utf-8")
    require(
        "BASE_SHA: ${{ github.event_name" in text
        and "'mise run contracts:revision-drift -- \"$1\"' -- \"$BASE_SHA\"" in text
        and 'revision-drift -- "${{' not in text,
        "contracts workflow interpolates an Actions expression into shell source instead of "
        "passing the event base through the quoted BASE_SHA environment value",
    )
    offline = (ROOT / ".github" / "workflows" / "build-offline.yml").read_text(encoding="utf-8")
    require(
        "contents: write" in offline
        and "softprops/action-gh-release" not in offline
        and "gh release create" in offline
        and all(
            f"@{sha}" in offline
            for sha in (
                "11d5960a326750d5838078e36cf38b85af677262",
                "0057852bfaa89a56745cba8c7296529d2fc39830",
                "ea165f8d65b6e75b540449e92b4886f43607fa02",
                "d3f86a106a0bac45b974a628896c90dbdf5c8093",
            )
        )
        and "@v" not in offline,
        "build-offline.yml retains a mutable action in a contents:write release workflow",
    )
    return 2


def test_fixture_writer_ownership(scratch: Path, token: str) -> int:
    """A generated-looking but unowned sentinel must never become deletion input."""
    tree = create_probe_tree(scratch, "fixture-writer-ownership", token)
    try:
        sentinel = tree / "contracts" / "fixtures" / "locale-catalog-user-sentinel.json"
        bytes_before = b'{"this":"must survive"}\n'
        sentinel.write_bytes(bytes_before)
        result = run_authoritative(tree, [FIXTURE_ENTRY])
        require(
            result.returncode != 0
            and "refusing to delete pre-existing locale-catalog fixture paths" in result.stderr,
            "the governed fixture writer accepted an unowned generated-looking sentinel\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        require(
            sentinel.read_bytes() == bytes_before,
            "the governed fixture writer altered an unowned pre-existing sentinel",
        )
        return 1
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: toolchain root and interpreter attestation
# ---------------------------------------------------------------------------


def mirror_toolchain(sealed_root: Path, destination: Path, tampered: dict[str, bytes]) -> None:
    """Hard-link a sealed toolchain into an owned directory.

    Files named in `tampered` are written fresh instead of linked, so the real
    toolchain's inodes are never opened for writing by this test.
    """
    profile = runtime_profile()
    subtrees = (
        profile["interpreter"]["installRelativePath"],
        str(Path(profile["externalTools"]["node"]["binaryRelativePath"]).parent),
        sealed_uv_binary().parent.relative_to(sealed_root).as_posix(),
    )
    for subtree in subtrees:
        source_root = sealed_root / subtree
        (destination / subtree).mkdir(parents=True, exist_ok=True)
        for source in sorted(source_root.rglob("*")):
            relative_path = source.relative_to(sealed_root).as_posix()
            target = destination / relative_path
            target.parent.mkdir(parents=True, exist_ok=True)
            if source.is_symlink():
                target.symlink_to(source.readlink())
            elif source.is_dir():
                target.mkdir(exist_ok=True)
            elif relative_path in tampered:
                target.write_bytes(tampered[relative_path])
                target.chmod(source.stat().st_mode & 0o7777)
            else:
                target.hardlink_to(source)
    for relative_path, content in tampered.items():
        target = destination / relative_path
        if not target.exists():
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(content)


def test_toolchain_roots(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "toolchain", token)
    sealed_root = sealed_root_from_environment()
    profile = runtime_profile()
    stdlib_relative = profile["interpreter"]["stdlibRelativePath"]
    binary_relative = profile["interpreter"]["binaryRelativePath"]
    node_relative = profile["externalTools"]["node"]["binaryRelativePath"]
    uv_relative = sealed_uv_binary().relative_to(sealed_root).as_posix()
    checked = 0
    try:
        cases = {
            "absent toolchain root": {"LOCALE_CATALOG_MISE_ROOT": ""},
            "relative toolchain root": {"LOCALE_CATALOG_MISE_ROOT": "relative/mise"},
            "unnormalized toolchain root": {"LOCALE_CATALOG_MISE_ROOT": f"{sealed_root}/./"},
            "parent-traversing toolchain root": {
                "LOCALE_CATALOG_MISE_ROOT": f"{sealed_root}/../mise"
            },
            "missing toolchain root": {
                "LOCALE_CATALOG_MISE_ROOT": str(scratch / "definitely-absent")
            },
            "filesystem-root toolchain root": {"LOCALE_CATALOG_MISE_ROOT": "/"},
            "toolchain root pointed at the repository": {"LOCALE_CATALOG_MISE_ROOT": str(tree)},
            "relative explicit Stack authority": {"LOCALE_CATALOG_STACK": "stack"},
            "missing explicit Stack authority": {
                "LOCALE_CATALOG_STACK": str(scratch / "definitely-absent-stack")
            },
        }
        for label, overrides in sorted(cases.items()):
            require_authoritative_failure(
                label, tree, [FIXTURE_ENTRY, "--check"], environment=probe_environment(overrides)
            )
            checked += 1

        symlinked = scratch / f"symlinked-root-{uuid.uuid4().hex}"
        symlinked.symlink_to(sealed_root)
        require_authoritative_failure(
            "symlinked toolchain root",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(symlinked)}),
        )
        checked += 1

        traversed = scratch / f"traversed-root-{uuid.uuid4().hex}"
        traversed.mkdir()
        (traversed / "link").symlink_to(sealed_root)
        require_authoritative_failure(
            "toolchain root reached through a symlinked component",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(traversed / "link")}),
        )
        checked += 1

        # A faithful hard-linked mirror is accepted, proving the tampered
        # mirrors below fail for the tampering and not for being a copy.
        mirror = scratch / f"mirror-{uuid.uuid4().hex}"
        mirror.mkdir()
        mirror_toolchain(sealed_root, mirror, {})
        require_authoritative_success(
            "faithful toolchain mirror",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(mirror)}),
        )
        checked += 1

        # CPython normally accepts a hash-based unchecked pyc ahead of its
        # source. The sealed launcher must redirect every interpreter to its
        # fresh empty pycache prefix before the bootstrap imports `ast`; this
        # deliberately valid malicious cache therefore cannot execute.
        pycache_root = scratch / f"pycache-root-{uuid.uuid4().hex}"
        pycache_root.mkdir()
        mirror_toolchain(sealed_root, pycache_root, {})
        marker = scratch / f"stdlib-pyc-marker-{uuid.uuid4().hex}"
        pyc = (
            pycache_root
            / stdlib_relative
            / "__pycache__"
            / "ast.cpython-314.pyc"
        )
        pyc.parent.mkdir(exist_ok=True)
        # The mirror uses hard links for speed. Break this one link before
        # writing the adversarial cache so the test can never mutate the
        # actual mise installation's pre-existing bytecode.
        pyc.unlink(missing_ok=True)
        writer = (
            "import importlib.util,marshal,struct,sys;"
            "from pathlib import Path;"
            "code=compile(f\"from pathlib import Path; Path({sys.argv[2]!r}).write_text('ran')\","
            "'<poisoned-ast>','exec');"
            "Path(sys.argv[1]).write_bytes(importlib.util.MAGIC_NUMBER+struct.pack('<I',1)"
            "+b'01234567'+marshal.dumps(code))"
        )
        compiled = subprocess.run(
            [
                str(sealed_root / binary_relative),
                "-I",
                "-S",
                "-E",
                "-B",
                "-c",
                writer,
                str(pyc),
                str(marker),
            ],
            capture_output=True,
            text=True,
            check=False,
            timeout=300,
        )
        require(compiled.returncode == 0, f"could not construct poisoned stdlib pyc: {compiled.stderr}")
        require(not marker.exists(), "constructing a poisoned pyc unexpectedly executed its payload")
        require_authoritative_success(
            "unchecked stdlib bytecode under the normal cache path",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(pycache_root)}),
        )
        require(not marker.exists(), "unchecked stdlib bytecode executed before source attestation")
        checked += 1
        shutil.rmtree(pycache_root)

        missing_root = scratch / f"missing-stdlib-root-{uuid.uuid4().hex}"
        missing_root.mkdir()
        mirror_toolchain(sealed_root, missing_root, {})
        (missing_root / stdlib_relative / "json" / "__init__.py").unlink()
        require_authoritative_failure(
            "missing attested stdlib source",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(missing_root)}),
        )
        checked += 1
        shutil.rmtree(missing_root)

        original_module = (sealed_root / stdlib_relative / "json" / "__init__.py").read_bytes()
        tampered_roots = {
            "edited stdlib module": {
                f"{stdlib_relative}/json/__init__.py": original_module + b"\nPAYLOAD = 1\n"
            },
            "injected stdlib module": {f"{stdlib_relative}/smuggled_module.py": b"PAYLOAD = 1\n"},
            "replaced interpreter binary": {binary_relative: b"#!/bin/sh\nexit 0\n"},
            "replaced Node binary": {node_relative: b"#!/bin/sh\nexit 0\n"},
            "replaced uv binary": {uv_relative: b"#!/bin/sh\nexit 0\n"},
            # A compiled extension planted where a module the governed closure
            # really imports would be found. Nothing hashes it, and CPython's
            # path finder prefers an extension to a source file, so an
            # unattested `.so` beside an attested `.py` would win.
            "extension shadowing a reachable top-level module": {
                f"{stdlib_relative}/csv.probe.so": b"\x7fELF probe\n"
            },
            "extension shadowing a reachable package": {
                f"{stdlib_relative}/json/__init__.probe.so": b"\x7fELF probe\n"
            },
        }
        # `lib-dynload` is the one directory whose contents are neither hashed
        # nor refused: the invocation-owned copy *empties* it before the
        # interpreter starts, so a planted extension there cannot be loaded at
        # all. The property to prove is therefore neutralisation, not refusal --
        # the command still succeeds and the payload never resolves.
        neutralised = scratch / f"fake-root-{uuid.uuid4().hex}"
        neutralised.mkdir()
        mirror_toolchain(
            sealed_root,
            neutralised,
            {f"{stdlib_relative}/lib-dynload/_json.probe.so": b"\x7fELF probe\n"},
        )
        require_authoritative_success(
            "an extension planted in lib-dynload is emptied out of the invocation copy",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(neutralised)}),
        )
        shutil.rmtree(neutralised)
        checked += 1

        for label, tampered in sorted(tampered_roots.items()):
            fake = scratch / f"fake-root-{uuid.uuid4().hex}"
            fake.mkdir()
            mirror_toolchain(sealed_root, fake, tampered)
            require_authoritative_failure(
                label,
                tree,
                [FIXTURE_ENTRY, "--check"],
                environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(fake)}),
            )
            checked += 1
            shutil.rmtree(fake)

        symlinked_binary = scratch / f"symlinked-binary-root-{uuid.uuid4().hex}"
        symlinked_binary.mkdir()
        mirror_toolchain(sealed_root, symlinked_binary, {})
        binary = symlinked_binary / binary_relative
        binary.unlink()
        binary.symlink_to(sealed_root / binary_relative)
        require_authoritative_failure(
            "symlinked interpreter binary",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(symlinked_binary)}),
        )
        checked += 1
        shutil.rmtree(symlinked_binary)
        shutil.rmtree(mirror)

        system_python = Path("/usr/bin/python3")
        require(
            system_python.is_file(), "the test host has no system Python for wrong-runtime coverage"
        )
        unsealed = subprocess.run(
            [
                str(system_python),
                "-I",
                "-S",
                "-E",
                "-B",
                str(tree / "scripts" / "locale_catalog_runtime.py"),
                FIXTURE_ENTRY,
                "--check",
            ],
            cwd=tree,
            env=probe_environment(),
            capture_output=True,
            text=True,
            check=False,
            timeout=600,
        )
        require(
            unsealed.returncode != 0,
            f"an unsealed system interpreter ran the bootstrap\nstdout:\n{unsealed.stdout}\n"
            f"stderr:\n{unsealed.stderr}",
        )
        checked += 1

        unisolated = subprocess.run(
            [
                str(sealed_root / binary_relative),
                str(tree / "scripts" / "locale_catalog_runtime.py"),
                FIXTURE_ENTRY,
                "--check",
            ],
            cwd=tree,
            env=probe_environment(),
            capture_output=True,
            text=True,
            check=False,
            timeout=600,
        )
        require(
            unisolated.returncode != 0,
            "the sealed interpreter ran the bootstrap without -I -S -E -B\n"
            f"stdout:\n{unisolated.stdout}\nstderr:\n{unisolated.stderr}",
        )
        checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: caller environment, startup hooks and PATH shadowing
# ---------------------------------------------------------------------------


def test_caller_environment_is_discarded(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "hostile-env", token)
    try:
        hostile = scratch / f"hostile-{uuid.uuid4().hex}"
        markers = hostile / "markers"
        markers.mkdir(parents=True)
        hook_body = (
            "from pathlib import Path\n"
            "import uuid\n"
            f"Path({str(markers)!r}, uuid.uuid4().hex).write_text('executed')\n"
        )
        for name in ("sitecustomize.py", "usercustomize.py", "startup.py"):
            (hostile / name).write_text(hook_body, encoding="utf-8")
        bash_env = hostile / "bash_env.sh"
        bash_env.write_text(f'/usr/bin/touch "{markers}/bash-env-$$"\n', encoding="utf-8")
        fake_bin = hostile / "bin"
        fake_bin.mkdir()
        for name in HOSTILE_EXECUTABLES:
            executable = fake_bin / name
            executable.write_text(
                f'#!/bin/sh\n/usr/bin/touch "{markers}/exec-{name}-$$"\nexit 97\n',
                encoding="utf-8",
            )
            executable.chmod(0o755)
        hostile_home = hostile / "home"
        hostile_home.mkdir()
        for name in (".bashrc", ".bash_profile", ".profile"):
            (hostile_home / name).write_text(
                f'/usr/bin/touch "{markers}/{name}-$$"\n', encoding="utf-8"
            )

        environment = probe_environment(
            {
                "HOME": str(hostile_home),
                "PATH": f"{fake_bin}{os.pathsep}{os.environ['PATH']}",
                "BASH_ENV": str(bash_env),
                "ENV": str(bash_env),
                "CDPATH": str(hostile),
                "PYTHONHOME": str(hostile),
                "PYTHONPATH": str(hostile),
                "PYTHONSTARTUP": str(hostile / "startup.py"),
                "PYTHONUSERBASE": str(hostile / "user"),
                "PYTHONEXECUTABLE": str(fake_bin / "python3"),
                "GIT_DIR": str(hostile / "git-dir"),
                "GIT_INDEX_FILE": str(hostile / "git-index"),
                "GIT_CONFIG_GLOBAL": str(hostile / "gitconfig"),
                "UV_PROJECT_ENVIRONMENT": str(hostile / "uv-venv"),
                "UV_CACHE_DIR": str(hostile / "uv-cache"),
            }
        )
        require_authoritative_success(
            "authoritative command under a hostile caller environment",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment=environment,
        )
        planted = sorted(child.name for child in markers.iterdir())
        require(
            not planted,
            f"a caller-supplied startup hook or PATH-shadowed executable ran: {planted}",
        )

        # The launcher's *own* protocol variables are caller input too. A
        # caller that names the bound git, this invocation's dependency root,
        # or the sealing sentinel must not be able to choose any of them: the
        # sealed stage rebuilds all three from nothing. The fake `git` named
        # here is the same marker-emitting script, so if the runtime honoured
        # `LOCALE_CATALOG_GIT` the governed path self-tests would run it.
        require_authoritative_success(
            "authoritative command with caller-supplied launcher protocol variables",
            tree,
            [FIXTURE_ENTRY, "--check"],
            environment={
                **environment,
                "LOCALE_CATALOG_GIT": str(fake_bin / "git"),
                "LOCALE_CATALOG_NODE": str(fake_bin / "node"),
                "LOCALE_CATALOG_UV": str(fake_bin / "uv"),
                "LOCALE_CATALOG_SEALED_SHELL": "0",
                "ARKHAM_LOCALE_CATALOG_PYTHON_VENV": str(hostile / "uv-venv"),
            },
        )
        planted = sorted(child.name for child in markers.iterdir())
        require(
            not planted,
            f"a caller-supplied launcher protocol variable was honoured: {planted}",
        )
        return 2
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: dependency tampering never survives a fresh authoritative run
# ---------------------------------------------------------------------------


def test_dependency_integrity(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "dependency", token)
    try:
        marker = tree / "locale-catalog-python-dependency-marker"
        seeded = b"pre-existing probe sentinel\n"
        marker.write_bytes(seeded)

        priming = run_authoritative(tree, ["scripts/validate-contract-fixtures.py"])
        require(
            priming.returncode == 0,
            "the probe tree could not run the authoritative command before tampering\n"
            f"stdout:\n{priming.stdout}\nstderr:\n{priming.stderr}",
        )

        workspace = tree / f"{PROBE_PREFIX}venv-{uuid.uuid4().hex}"
        workspace.mkdir()
        try:
            build = subprocess.run(
                [
                    str(sealed_uv_binary()),
                    "sync",
                    "--locked",
                    "--no-cache",
                    "--link-mode",
                    "copy",
                    "--no-dev",
                    "--no-install-project",
                    "--quiet",
                ],
                cwd=tree,
                env=probe_environment({"UV_PROJECT_ENVIRONMENT": str(workspace / "venv")}),
                capture_output=True,
                text=True,
                check=False,
                timeout=1800,
            )
            require(
                build.returncode == 0,
                "probe setup failure: could not build a tamperable dependency root: "
                f"{build.stderr}",
            )
            site_packages = workspace / "venv" / "lib" / "python3.14" / "site-packages"
            dependency = find_single(site_packages, "jsonschema/__init__.py", "dependency module")
            record = find_single(site_packages, "jsonschema-*.dist-info/RECORD", "wheel RECORD")
            payload = tree / f"dependency-payload-{uuid.uuid4().hex}"
            dependency.write_bytes(
                dependency.read_bytes()
                + f"\nfrom pathlib import Path\nPath({str(payload)!r}).write_text('tampered')\n".encode(
                    "utf-8"
                )
            )
            rows = list(csv.reader(record.read_text(encoding="utf-8").splitlines()))
            for row in rows:
                if row and row[0] == "jsonschema/__init__.py":
                    row[1] = (
                        "sha256="
                        + base64.urlsafe_b64encode(hashlib.sha256(dependency.read_bytes()).digest())
                        .decode("ascii")
                        .rstrip("=")
                    )
            with record.open("w", encoding="utf-8", newline="") as handle:
                csv.writer(handle, lineterminator="\n").writerows(rows)

            result = run_authoritative(tree, ["scripts/validate-contract-fixtures.py"])
            require(
                result.returncode == 0 and not payload.exists(),
                "a tampered dependency with a rewritten RECORD survived lock-hashed "
                f"reinstallation; status {result.returncode}, payload {payload.exists()}\n"
                f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
            )

            # The same tampered root, offered as this invocation's workspace,
            # must be rejected outright rather than trusted.
            rejected = subprocess.run(
                [
                    str(sealed_root_from_environment() / runtime_profile()["interpreter"]["binaryRelativePath"]),
                    "-I",
                    "-S",
                    "-E",
                    "-B",
                    str(tree / "scripts" / "locale_catalog_runtime.py"),
                    "scripts/validate-contract-fixtures.py",
                ],
                cwd=tree,
                env=probe_environment(
                    {"ARKHAM_LOCALE_CATALOG_PYTHON_VENV": str(workspace / "venv")}
                ),
                capture_output=True,
                text=True,
                check=False,
                timeout=1800,
            )
            require(
                rejected.returncode != 0,
                "a RECORD-rewritten dependency root was accepted as an invocation workspace\n"
                f"stdout:\n{rejected.stdout}\nstderr:\n{rejected.stderr}",
            )
        finally:
            shutil.rmtree(workspace, ignore_errors=True)

        require(
            marker.read_bytes() == seeded,
            "the dependency probe overwrote a pre-existing dependency-marker sentinel",
        )
        return 3
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Probe: workspace ownership and kill recovery
# ---------------------------------------------------------------------------


def dead_process_identifier() -> int:
    reaped = subprocess.Popen(["/bin/echo"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    reaped.communicate(timeout=120)
    return reaped.pid


def test_workspace_ownership(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "workspace", token)
    try:
        stale = tree / f"{WORKSPACE_PREFIX}stale"
        stale.mkdir()
        (stale / WORKSPACE_OWNER_FILE).write_text(f"{dead_process_identifier()} 0 foreign stale\n", encoding="utf-8")
        live = tree / f"{WORKSPACE_PREFIX}live"
        live.mkdir()
        (live / WORKSPACE_OWNER_FILE).write_text(f"{os.getpid()} 0 foreign live\n", encoding="utf-8")
        long_lived = tree / f"{WORKSPACE_PREFIX}long-lived"
        long_lived.mkdir()
        (long_lived / WORKSPACE_OWNER_FILE).write_text(f"{os.getpid()} 0 foreign old\n", encoding="utf-8")
        unowned = tree / f"{WORKSPACE_PREFIX}unowned"
        unowned.mkdir()
        garbled = tree / f"{WORKSPACE_PREFIX}garbled"
        garbled.mkdir()
        (garbled / WORKSPACE_OWNER_FILE).write_text("not-an-owner-record\n", encoding="utf-8")

        require_authoritative_success(
            "authoritative command alongside pre-existing workspaces", tree, [FIXTURE_ENTRY, "--check"]
        )
        require(stale.exists(), "a stale-looking pre-existing workspace was removed")
        require(live.exists(), "a live workspace was removed")
        require(long_lived.exists(), "a long-running invocation's workspace was removed")
        require(unowned.exists(), "a workspace without an ownership record was removed")
        require(garbled.exists(), "a workspace with an unparsable ownership record was removed")
        remaining = sorted(
            child.name for child in tree.iterdir() if child.name.startswith(WORKSPACE_PREFIX)
        )
        require(
            remaining
            == [
                f"{WORKSPACE_PREFIX}garbled",
                f"{WORKSPACE_PREFIX}live",
                f"{WORKSPACE_PREFIX}long-lived",
                f"{WORKSPACE_PREFIX}stale",
                f"{WORKSPACE_PREFIX}unowned",
            ],
            f"a successful invocation did not release its own workspace; found {remaining}",
        )
        return 5
    finally:
        release_probe_tree(tree, token)


def test_kill_recovery(scratch: Path, token: str) -> int:
    tree = create_probe_tree(scratch, "kill", token)
    try:
        process = subprocess.Popen(
            [str(tree / LAUNCHER_RELATIVE_PATH), FIXTURE_ENTRY, "--check"],
            cwd=tree,
            env=probe_environment(),
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        deadline = time.monotonic() + 300
        leaked: list[Path] = []
        while time.monotonic() < deadline:
            leaked = sorted(tree.glob(f"{WORKSPACE_PREFIX}*"))
            if leaked and (leaked[0] / WORKSPACE_OWNER_FILE).is_file():
                break
            if process.poll() is not None:
                break
            time.sleep(0.02)
        process.kill()
        try:
            process.communicate(timeout=300)
        except subprocess.TimeoutExpired:
            require(False, "a killed authoritative invocation did not terminate")
        require(leaked, "the authoritative command never created an owned workspace to reclaim")

        # A hard kill skips the launcher's cleanup trap. Future invocations
        # must never decide that this path is theirs merely from a
        # caller-creatable owner record, regardless of its age.
        survivor = leaked[0]
        owner = survivor / WORKSPACE_OWNER_FILE
        require(owner.is_file(), "an abandoned workspace carries no ownership record")
        recorded = owner.read_text(encoding="utf-8").split()
        require(
            len(recorded) == 4 and recorded[0].isdigit() and recorded[1].isdigit(),
            f"an abandoned workspace has an unparsable ownership record: {recorded!r}",
        )
        require_authoritative_success(
            "authoritative command after a killed invocation", tree, [FIXTURE_ENTRY, "--check"]
        )
        require(survivor.exists(), "a killed invocation's workspace was reclaimed by another run")
        return 1
    finally:
        release_probe_tree(tree, token)


def test_injected_setup_failure(scratch: Path, token: str) -> int:
    failed = False
    try:
        create_probe_tree(
            scratch,
            "injected-setup-failure",
            token,
            governed_paths=(*GOVERNED_TREE_PATHS, "definitely/absent/governed/path"),
        )
    except ProbeFailure as error:
        failed = True
        require(
            "probe setup failure" in str(error) and "definitely/absent/governed/path" in str(error),
            f"an injected setup failure did not name the missing input: {error}",
        )
    require(failed, "an injected setup failure was not reported as a probe failure")

    foreign = scratch / f"{PROBE_PREFIX}foreign-{uuid.uuid4().hex}"
    foreign.mkdir()
    try:
        (foreign / OWNER_SENTINEL).write_text("a-different-invocations-token", encoding="utf-8")
        release_probe_tree(foreign, token)
        require(
            foreign.is_dir() and (foreign / OWNER_SENTINEL).is_file(),
            "cleanup removed a directory this invocation does not own",
        )
    finally:
        shutil.rmtree(foreign, ignore_errors=True)

    unowned = scratch / f"{PROBE_PREFIX}unowned-{uuid.uuid4().hex}"
    unowned.mkdir()
    try:
        release_probe_tree(unowned, token)
        require(unowned.is_dir(), "cleanup removed a directory with no ownership sentinel")
    finally:
        shutil.rmtree(unowned, ignore_errors=True)
    return 3


def test_parallel_self_tests(scratch: Path, token: str) -> int:
    trees = [create_probe_tree(scratch, f"parallel-{index}", token, with_history=False) for index in range(2)]
    started = []
    try:
        for tree in trees:
            started.append(
                (
                    tree,
                    subprocess.Popen(
                        [
                            str(tree / LAUNCHER_RELATIVE_PATH),
                            BOUNDARY_TEST_RELATIVE_PATH,
                            "--probe",
                            "capability-matrix",
                            "--owner-token",
                            token,
                        ],
                        cwd=tree,
                        env=probe_environment(),
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        text=True,
                    ),
                )
            )
        for tree, process in started:
            stdout, stderr = process.communicate(timeout=3600)
            require(
                process.returncode == 0,
                f"a parallel self-test invocation in {tree.name} failed\n"
                f"stdout:\n{stdout}\nstderr:\n{stderr}",
            )
            require(
                (tree / OWNER_SENTINEL).read_text(encoding="utf-8") == token,
                f"a parallel self-test invocation disturbed {tree.name}'s ownership sentinel",
            )
        return len(trees)
    finally:
        for tree in trees:
            release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Regression: the capability lint's own correctness matrix
# ---------------------------------------------------------------------------


def analyzer_verdict(tree: Path, reader, target: str, snippet: bytes) -> str | None:
    """Scan the governed closure with `snippet` spliced into `target`."""
    path = tree / target
    original = path.read_bytes()
    try:
        path.write_bytes(splice_snippet(original, snippet))
        try:
            locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        except locale_catalog_python_boundary.CapabilityLintError as error:
            return str(error)
        return None
    finally:
        path.write_bytes(original)


def test_analyzer_matrix(scratch: Path, token: str) -> int:
    """Every reported analyzer bypass is refused; ordinary Python is not.

    Both halves matter. A payload that is only refused as a *syntax* error
    never reached the analyzer and proves nothing, so that is a failure too --
    and a control that is refused would mean the fail-closed rules had eaten
    the language.
    """
    tree = create_probe_tree(scratch, "analyzer-matrix", token, with_history=False)
    try:
        reader = probe_tree_reader(tree)
        locale_catalog_python_boundary.scan_python_closure(FIXTURE_ENTRY, source_reader=reader)
        checked = 0
        for target in (FIXTURE_ENTRY, HELPER_SOURCE, GRANTED_SOURCE):
            for label, snippet in sorted(ANALYZER_PAYLOADS.items()):
                reason = analyzer_verdict(tree, reader, target, snippet)
                require(
                    reason is not None,
                    f"the production analyzer accepted {label!r} in {target}",
                )
                require(
                    "invalid Python source" not in reason,
                    f"{label!r} in {target} was only refused as a syntax error, so it never "
                    f"reached the analyzer: {reason}",
                )
                checked += 1
            for label, snippet in sorted(ANALYZER_CONTROLS.items()):
                reason = analyzer_verdict(tree, reader, target, snippet)
                require(
                    reason is None,
                    f"the production analyzer refused the accepted control {label!r} in "
                    f"{target}: {reason}",
                )
                checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


def test_analyzer_grammar_coverage() -> int:
    """No value-bearing or binding node in the governed tree is unmodelled.

    The analyzer refuses an unmodelled node at scan time, so a gap fails
    closed either way; this turns that gap into a *reported* failure instead of
    a mystery refusal, across every governed source plus a corpus exercising
    constructs the governed sources do not currently use.
    """
    corpus = b"".join(
        locale_catalog_python_boundary.read_source(relative)
        for relative in sorted(locale_catalog_python_boundary.EXECUTABLE_SOURCES)
    )
    corpus += b"".join(sorted(ANALYZER_PAYLOADS.values()))
    corpus += b"".join(sorted(ANALYZER_CONTROLS.values()))
    unmodelled = locale_catalog_python_boundary.unmodelled_nodes_in(corpus)
    require(
        not unmodelled,
        f"the capability analyzer does not model these grammar nodes: {unmodelled}",
    )
    return 1


# ---------------------------------------------------------------------------
# Drift: reviewed tooling no longer matches its recorded digest
# ---------------------------------------------------------------------------

DRIFT_PAYLOAD_MARKER = "locale-catalog-boundary-tcb-payload"


def drifted_lint_module(marker: Path) -> bytes:
    """A permissive scanner that announces itself the moment it is imported."""
    return (
        "from pathlib import Path\n"
        f"Path({str(marker)!r}).write_text('executed', encoding='utf-8')\n"
        "EXECUTABLE_SOURCES = frozenset()\n"
        "ENTRY_POINTS = frozenset()\n"
        "LOCAL_MODULE_SOURCES = {}\n"
        "ALLOWED_IMPORTS = set()\n"
        "SOURCE_SENSITIVE_IMPORTS = {}\n"
        "TRUSTED_SOURCES = frozenset()\n"
        "\n"
        "class CapabilityLintError(ValueError):\n"
        "    pass\n"
        "\n"
        "def read_source(relative_path):\n"
        "    return b''\n"
        "\n"
        "def scan_python_closure(entry, *, source_reader=None):\n"
        "    return ()\n"
        "\n"
        "def all_executable_sources():\n"
        "    return ()\n"
    ).encode("utf-8")


def test_reviewed_source_drift(scratch: Path, token: str) -> int:
    """A reviewed source that no longer matches its recorded digest stops the run.

    These files are trusted committed code; the digests in the runtime profile
    are a *drift record*, so that changing one has to be a coordinated,
    reviewed edit rather than a quiet difference between what ran and what the
    profile says ran. Repository code cannot make a statement about the shell
    that is already executing it, and nothing here pretends otherwise -- what
    it does do is notice when the recorded identity and the file disagree. For the
    capability lint the check happens before it is imported, which is worth
    having simply because a half-edited lint should not decide anything.
    """
    checked = 0
    for relative_path in sorted(locale_catalog_python_boundary.TRUSTED_SOURCES):
        tree = create_probe_tree(scratch, "trusted-source", token, with_history=False)
        try:
            marker = tree / DRIFT_PAYLOAD_MARKER
            path = tree / relative_path
            if relative_path == "scripts/locale_catalog_python_boundary.py":
                path.write_bytes(drifted_lint_module(marker))
            else:
                path.write_bytes(path.read_bytes() + b"\n# trusted source tamper probe\n")
            result = run_authoritative(tree, [FIXTURE_ENTRY, "--check"])
            require(
                result.returncode != 0
                and "does not match the identity committed" in result.stderr,
                f"a replaced trusted source ({relative_path}) was not refused against its "
                f"committed digest\nstdout:\n{result.stdout}\nstderr:\n{result.stderr}",
            )
            require(
                not marker.exists(),
                "the replacement capability analyzer executed its own top-level code before it "
                "its recorded identity was checked",
            )
            checked += 1
        finally:
            release_probe_tree(tree, token)
    return checked


# ---------------------------------------------------------------------------
# Identity: uv never starts against a project or lock that could build code
# ---------------------------------------------------------------------------

UV_ATTESTOR_SUCCESS = "hash-pinned registry distributions"
ATTRS_REGISTRY_SOURCE = 'source = { registry = "https://pypi.org/simple" }\nsdist'
UV_ATTESTATION_CASES: dict[str, tuple[str, str, str, str]] = {
    "a PEP 517 build backend": (
        "pyproject.toml",
        "[project]\n",
        '[build-system]\nrequires = ["hostile-backend"]\nbuild-backend = "hostile.backend"\n\n[project]\n',
        "never runs a PEP 517 backend",
    ),
    "a redirected tool.uv source": (
        "pyproject.toml",
        "[project]\n",
        '[tool.uv.sources]\njsonschema = { git = "https://example.invalid/jsonschema" }\n\n[project]\n',
        "[tool.uv.sources]",
    ),
    "a relaxed project interpreter pin": (
        "pyproject.toml",
        'requires-python = "==3.14.7"',
        'requires-python = ">=3.14"',
        "must pin requires-python",
    ),
    "an unpinned project dependency": (
        "pyproject.toml",
        '"jsonschema==4.26.0"',
        '"jsonschema>=4.26.0"',
        "not pinned to an exact version",
    ),
    "a git lock source": (
        "uv.lock",
        ATTRS_REGISTRY_SOURCE,
        'source = { git = "https://example.invalid/attrs" }\nsdist',
        "resolves through a git source",
    ),
    "a url lock source": (
        "uv.lock",
        ATTRS_REGISTRY_SOURCE,
        'source = { url = "https://example.invalid/attrs.tar.gz" }\nsdist',
        "resolves through a url source",
    ),
    "a path lock source": (
        "uv.lock",
        ATTRS_REGISTRY_SOURCE,
        'source = { path = "vendor/attrs" }\nsdist',
        "resolves through a path source",
    ),
    "a directory lock source": (
        "uv.lock",
        ATTRS_REGISTRY_SOURCE,
        'source = { directory = "vendor/attrs" }\nsdist',
        "resolves through a directory source",
    ),
    "an editable lock source": (
        "uv.lock",
        ATTRS_REGISTRY_SOURCE,
        'source = { editable = "." }\nsdist',
        "resolves through a editable source",
    ),
    "a non-sha256 wheel hash": (
        "uv.lock",
        'hash = "sha256:c647aa4a12dfbad9333ca4e71fe62ddc36f4e63b2d260a37a8b83d2f043ac309"',
        'hash = "md5:c647aa4a12dfbad9333ca4e71fe62ddc36f4e63b2d260a37a8b83d2f043ac309"',
        "without an exact sha256 hash",
    ),
    "a wheel outside the locked registry": (
        "uv.lock",
        'wheels = [\n    { url = "https://files.pythonhosted.org/packages/64/b4/17d4b0b2a2dc85a6df63d1157e028ed19f90d4cd97c36717afef2bc2f395/attrs-26.1.0-py3-none-any.whl"',
        'wheels = [\n    { url = "https://example.invalid/attrs-26.1.0-py3-none-any.whl"',
        "outside the locked registry",
    ),
    "a relaxed locked interpreter pin": (
        "uv.lock",
        'requires-python = "==3.14.7"',
        'requires-python = ">=3.14"',
        "must pin requires-python",
    ),
    "a redirected root project": (
        "uv.lock",
        'source = { virtual = "." }',
        'source = { editable = "." }',
        "must be the virtual project itself",
    ),
}


def require_refused_before_uv(label: str, tree: Path, expected: str) -> None:
    result = run_authoritative(tree, [FIXTURE_ENTRY, "--check"])
    require(
        result.returncode != 0 and expected in result.stderr,
        f"{label} was not refused before uv started\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
    )
    require(
        UV_ATTESTOR_SUCCESS not in result.stderr,
        f"{label} reached the dependency attestor's success path, so uv ran\n"
        f"stderr:\n{result.stderr}",
    )
    leftovers = sorted(tree.glob(f"{WORKSPACE_PREFIX}*/venv"))
    require(
        not leftovers,
        f"{label} let uv create a virtual environment before it was refused: {leftovers}",
    )


def test_dependency_source_attestation(scratch: Path, token: str) -> int:
    """A project or lock that could build code is refused before uv resolves it.

    These are checks over *externally produced* packages -- the class this
    project does treat as untrusted -- so "refused" is accurate here.
    """
    checked = 0
    for label, (relative_path, original, replacement, expected) in sorted(
        UV_ATTESTATION_CASES.items()
    ):
        tree = create_probe_tree(scratch, "uv-attestation", token, with_history=False)
        try:
            path = tree / relative_path
            text = path.read_text(encoding="utf-8")
            require(
                original in text,
                f"probe setup failure: {relative_path} no longer contains the {label} anchor",
            )
            path.write_text(text.replace(original, replacement, 1), encoding="utf-8")
            require_refused_before_uv(label, tree, expected)
            checked += 1
        finally:
            release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "uv-missing-wheel", token, with_history=False)
    try:
        lock = tree / "uv.lock"
        text = lock.read_text(encoding="utf-8")
        start = text.index('name = "attrs"')
        wheels_start = text.index("wheels = [", start)
        wheels_end = text.index("]\n", wheels_start) + 2
        lock.write_text(text[:wheels_start] + text[wheels_end:], encoding="utf-8")
        require_refused_before_uv("a locked distribution with no wheel", tree, "declares no wheels")
        checked += 1
    finally:
        release_probe_tree(tree, token)

    tree = create_probe_tree(scratch, "uv-incompatible-wheel", token, with_history=False)
    try:
        lock = tree / "uv.lock"
        text = lock.read_text(encoding="utf-8")
        start = text.index('name = "attrs"')
        wheels_start = text.index("wheels = [", start)
        wheels_end = text.index("]\n", wheels_start) + 2
        replacement = (
            'wheels = [\n    { url = "https://files.pythonhosted.org/packages/64/b4/'
            'attrs-26.1.0-py3-none-solaris_11_sparc.whl", hash = "sha256:'
            'c647aa4a12dfbad9333ca4e71fe62ddc36f4e63b2d260a37a8b83d2f043ac309" },\n]\n'
        )
        lock.write_text(text[:wheels_start] + replacement + text[wheels_end:], encoding="utf-8")
        require_refused_before_uv(
            "a locked distribution with no compatible wheel", tree, "no hash-pinned wheel compatible"
        )
        checked += 1
    finally:
        release_probe_tree(tree, token)
    return checked


# ---------------------------------------------------------------------------
# Probe: the Node module graph is enforced by the loader, not by reading text
# ---------------------------------------------------------------------------

GENERATOR_LAUNCHER_RELATIVE_PATH = "frontend/scripts/locale-catalog/generator-launcher.mjs"
GENERATOR_DIGESTS_NAME = "generator-module-digests.json"
GENERATOR_PAYLOAD_MARKER = "locale-catalog-node-payload-ran"

# The exact import syntaxes the review used to defeat the previous lexical
# scanner, plus the plain forms. None of them changes what the loader is asked
# to resolve, which is the whole point of enforcing at resolution time.
GENERATOR_IMPORT_DRIFT_CASES: dict[str, str] = {
    "leading whitespace static import": " import '../../payload.mjs'\n",
    "comment inside a dynamic import": "import /* unchecked */ ('../../payload.mjs')\n",
    "plain dynamic import": "await import('../../payload.mjs')\n",
    "computed dynamic import": (
        "const parts = ['..', '..', 'payload.mjs']\nawait import(parts.join('/'))\n"
    ),
    "export from": "export { payload } from '../../payload.mjs'\n",
    "tab indented static import": "\timport '../../payload.mjs'\n",
    "absolute path import": "await import(new URL('../../payload.mjs', import.meta.url).href)\n",
    "sibling module not in the allowlist": "await import('./unlisted.mjs')\n",
}


def pinned_node_binary() -> str:
    return strict_json.trusted_node()


def build_generator_probe(scratch: Path, entry_source: str, extra: dict[str, str]) -> tuple[Path, Path]:
    """A minimal frontend-shaped tree with the real launcher and a payload."""
    root = scratch / f"node-loader-{uuid.uuid4().hex}"
    generator = root / "scripts" / "locale-catalog"
    generator.mkdir(parents=True)
    shutil.copy2(ROOT / GENERATOR_LAUNCHER_RELATIVE_PATH, generator / "generator-launcher.mjs")
    marker = root / GENERATOR_PAYLOAD_MARKER
    (root / "payload.mjs").write_text(
        "import { writeFileSync } from 'node:fs'\n"
        f"writeFileSync({str(marker)!r}, 'executed')\n",
        encoding="utf-8",
    )
    (generator / "unlisted.mjs").write_text(
        "import { writeFileSync } from 'node:fs'\n"
        f"writeFileSync({str(marker)!r}, 'executed')\n",
        encoding="utf-8",
    )
    (generator / "entry.mjs").write_text(entry_source, encoding="utf-8")
    for name, source in extra.items():
        (generator / name).write_text(source, encoding="utf-8")
    allowlist = {
        name: hashlib.sha256((generator / name).read_bytes()).hexdigest()
        for name in sorted(["entry.mjs", *extra])
    }
    (generator / GENERATOR_DIGESTS_NAME).write_text(
        json.dumps(allowlist, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    return root, marker


def run_generator_probe(root: Path, entry: str = "entry.mjs") -> subprocess.CompletedProcess:
    launcher = root / "scripts" / "locale-catalog" / "generator-launcher.mjs"
    return subprocess.run(
        [pinned_node_binary(), str(launcher), entry],
        cwd=root,
        env=probe_environment(),
        capture_output=True,
        text=True,
        check=False,
        timeout=300,
    )


def test_generator_module_graph_drift(scratch: Path, token: str) -> int:
    """A live Node run reports any module outside the committed digest list.

    The cases are the exact import syntaxes a lexical scanner missed -- leading
    whitespace, a comment inside `import(...)`, computed specifiers. The digest
    of the *entry* module is recomputed for each one so what is being exercised
    is the module-graph rule rather than a stale hash. This is drift detection
    over trusted committed code, not a JavaScript sandbox: everything the
    launcher does permit runs with full Node privileges.
    """
    checked = 0
    for label, payload in sorted(GENERATOR_IMPORT_DRIFT_CASES.items()):
        root, marker = build_generator_probe(scratch, payload, {})
        try:
            result = run_generator_probe(root)
            require(
                result.returncode != 0 and "refusing to" in result.stderr,
                f"the sealed Node launcher accepted {label!r}\n"
                f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
            )
            require(
                not marker.exists(),
                f"{label!r} executed its payload before the loader refused it",
            )
            checked += 1
        finally:
            shutil.rmtree(root, ignore_errors=True)

    # Controls: builtins and allowlisted local modules must still load, and the
    # entry module must actually run -- a launcher that refuses everything
    # proves nothing.
    root, marker = build_generator_probe(
        scratch,
        "import { writeFileSync } from 'node:fs'\n"
        "import { value } from './helper.mjs'\n"
        f"writeFileSync({str(scratch / 'node-control-ok')!r}, String(value))\n",
        {"helper.mjs": "export const value = 41 + 1\n"},
    )
    control = scratch / "node-control-ok"
    try:
        result = run_generator_probe(root)
        require(
            result.returncode == 0 and control.exists() and control.read_text() == "42",
            "the sealed Node launcher refused a builtin plus an allowlisted local import\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        require(not marker.exists(), "the control run executed the payload module")
        checked += 1
    finally:
        control.unlink(missing_ok=True)
        shutil.rmtree(root, ignore_errors=True)

    # A tampered allowlisted module must be refused by digest, before it loads.
    root, marker = build_generator_probe(
        scratch,
        "import './helper.mjs'\n",
        {"helper.mjs": "export const value = 1\n"},
    )
    try:
        helper = root / "scripts" / "locale-catalog" / "helper.mjs"
        helper.write_text(
            "import { writeFileSync } from 'node:fs'\n"
            f"writeFileSync({str(marker)!r}, 'executed')\n",
            encoding="utf-8",
        )
        result = run_generator_probe(root)
        require(
            result.returncode != 0 and "committed digest" in result.stderr,
            "a rewritten allowlisted module was not refused against its digest\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}",
        )
        require(not marker.exists(), "the rewritten allowlisted module executed")
        checked += 1
    finally:
        shutil.rmtree(root, ignore_errors=True)
    return checked


# ---------------------------------------------------------------------------
# Policy: CI jobs that run governed commands keep least privilege
# ---------------------------------------------------------------------------

WORKFLOW_DIR = ROOT / ".github" / "workflows"
# What counts as "this job does governed work". Anything that reaches a mise
# governed task, the pinned runner, a generator entry, the npm build/prebuild
# path, or a local action/reusable workflow that does one of those.
GOVERNED_RUN_MARKERS = (
    "mise run contracts:",
    "mise run locale-catalog:",
    "run-locale-catalog-python.sh",
    "generator-launcher.mjs",
    "npm run build",
    "npm run prebuild",
    "offline/scripts/03-build-frontend.sh",
    "offline/scripts/05-package.sh",
)
FORBIDDEN_TRIGGERS = ("pull_request_target", "workflow_run")
PUBLISHING_ACTION_PREFIXES = (
    "actions/upload-artifact",
    "actions/upload-pages-artifact",
    "actions/deploy-pages",
    "softprops/action-gh-release",
    "docker/build-push-action",
    "docker/login-action",
    "ncipollo/release-action",
)
PUBLISHING_RUN_MARKERS = ("gh release", "docker push", "npm publish", "gh api --method POST")
PINNED_ACTION = re.compile(r"^[^@\s]+@[0-9a-f]{40}$")
SECRET_CONTEXT = re.compile(r"secrets\s*(?:\.\s*[A-Za-z_]|\[)")


def load_workflows(directory: Path) -> dict[str, object]:
    """Parse every workflow once, structurally.

    Both suffixes: GitHub accepts `.yml` and `.yaml`, and a policy that reads
    only one of them is a policy with a hole in it.
    """
    documents: dict[str, object] = {}
    for path in sorted(directory.iterdir()):
        if path.suffix not in {".yml", ".yaml"} or not path.is_file():
            continue
        documents[path.name] = yaml.safe_load(path.read_text(encoding="utf-8"))
    require(documents, f"no workflows were found under {directory}")
    return documents


def workflow_triggers(document: dict) -> set[str]:
    # `on` parses as the YAML 1.1 boolean `True`, which is exactly the kind of
    # thing a text scan gets wrong.
    triggers = document.get("on", document.get(True))
    if isinstance(triggers, str):
        return {triggers}
    if isinstance(triggers, list):
        return {str(item) for item in triggers}
    if isinstance(triggers, dict):
        return {str(key) for key in triggers}
    return set()


def node_text(value: object) -> str:
    """Flatten any nested YAML node to text, so aliases and lists are covered."""
    if isinstance(value, str):
        return value
    if isinstance(value, dict):
        return " ".join(f"{node_text(key)} {node_text(item)}" for key, item in value.items())
    if isinstance(value, list):
        return " ".join(node_text(item) for item in value)
    return "" if value is None else str(value)


def local_action_text(uses: str) -> str:
    """The body of a local composite action, so its steps are inspected too."""
    if not uses.startswith("./"):
        return ""
    base = ROOT / uses[2:]
    for candidate in (base, base / "action.yml", base / "action.yaml"):
        if candidate.is_file():
            return candidate.read_text(encoding="utf-8")
    return ""


def job_is_governed(job: object, documents: dict[str, object]) -> bool:
    if not isinstance(job, dict):
        return False
    reusable = job.get("uses")
    if isinstance(reusable, str):
        if reusable.startswith("./"):
            referenced = ROOT / reusable[2:]
            if referenced.is_file():
                text = referenced.read_text(encoding="utf-8")
                if any(marker in text for marker in GOVERNED_RUN_MARKERS):
                    return True
        # A remote reusable workflow is out of this repository's control, so it
        # is treated as governed and must satisfy the same posture.
        return True
    for step in job.get("steps", []) or []:
        if not isinstance(step, dict):
            continue
        run = step.get("run")
        if isinstance(run, str) and any(marker in run for marker in GOVERNED_RUN_MARKERS):
            return True
        uses = step.get("uses")
        if isinstance(uses, str):
            body = local_action_text(uses)
            if body and any(marker in body for marker in GOVERNED_RUN_MARKERS):
                return True
    return False


def check_ci_privilege(directory: Path) -> int:
    """Assert least privilege for every workflow job that does governed work.

    Pull-request CI runs unreviewed code by design and nothing here contains
    it; what is asserted is the blast radius. Everything is read from the
    parsed document, and secret references are matched as *contexts*
    (`secrets.NAME`, `secrets['NAME']`, `secrets: inherit`) rather than as the
    literal text `secrets.`, so an alternate spelling does not slip past.
    """
    checked = 0
    documents = load_workflows(directory)
    for name, document in sorted(documents.items()):
        require(isinstance(document, dict), f"{name} is not a mapping")
        jobs = document.get("jobs") or {}
        governed = {
            job_name: job
            for job_name, job in jobs.items()
            if job_is_governed(job, documents)
        }
        if not governed:
            continue
        checked += 1
        triggers = workflow_triggers(document)
        for trigger in FORBIDDEN_TRIGGERS:
            require(
                trigger not in triggers,
                f"{name} triggers on {trigger}, which would run pull-request code with the "
                "base repository's authority",
            )
        permissions = document.get("permissions")
        require(
            permissions == {"contents": "read"},
            f"{name} must declare `permissions: contents: read`, got {permissions!r}",
        )
        require(
            SECRET_CONTEXT.search(node_text(document)) is None,
            f"{name} references a secret context in a workflow that does governed work",
        )
        for job_name, job in sorted(governed.items()):
            checked += 1
            require(
                job.get("permissions", permissions) == {"contents": "read"},
                f"{name}:{job_name} widens permissions to "
                f"{job.get('permissions', permissions)!r}",
            )
            require(
                job.get("secrets") is None,
                f"{name}:{job_name} passes secrets ({job.get('secrets')!r}) to governed work",
            )
            require(
                job.get("environment") is None,
                f"{name}:{job_name} runs in a protected environment, which can carry secrets "
                "and deployment authority",
            )
            for step in job.get("steps", []) or []:
                if not isinstance(step, dict):
                    continue
                checked += 1
                uses = step.get("uses")
                run = step.get("run")
                if isinstance(run, str) and "pull_request" in triggers:
                    for marker in PUBLISHING_RUN_MARKERS:
                        require(
                            marker not in run,
                            f"{name}:{job_name} publishes with {marker!r} from a pull request",
                        )
                if uses is None:
                    continue
                uses = str(uses)
                if not uses.startswith("./"):
                    require(
                        PINNED_ACTION.match(uses) is not None,
                        f"{name}:{job_name} uses {uses!r}, which is not pinned to a 40-hex "
                        "commit",
                    )
                if uses.startswith("actions/checkout@"):
                    with_block = step.get("with") or {}
                    require(
                        with_block.get("persist-credentials") is False,
                        f"{name}:{job_name} checks out with credentials persisted",
                    )
                if any(uses.startswith(prefix) for prefix in PUBLISHING_ACTION_PREFIXES):
                    require(
                        "pull_request" not in triggers,
                        f"{name}:{job_name} publishes an artifact from a pull request",
                    )
    return checked


def test_ci_privilege_policy() -> int:
    """The real workflows, plus synthetic fixtures proving the checks bite."""
    checked = check_ci_privilege(WORKFLOW_DIR)
    require(checked > 10, f"the CI privilege scan only examined {checked} elements")
    return checked


HOSTILE_WORKFLOW_FIXTURES: dict[str, tuple[str, str]] = {
    "a .yaml workflow is discovered too": (
        "governed.yaml",
        """
on:
  pull_request:
permissions:
  contents: write
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run contracts:fixtures
""",
    ),
    "a bracket secret reference": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run contracts:fixtures
        env:
          TOKEN: ${{ secrets['DEPLOY_TOKEN'] }}
""",
    ),
    "secrets: inherit on a governed job": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    uses: ./.github/workflows/reusable-governed.yml
    secrets: inherit
""",
    ),
    "a protected environment": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    environment: production
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run locale-catalog:validate
""",
    ),
    "an unpinned action": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          persist-credentials: false
      - run: mise run contracts:routes
""",
    ),
    "a checkout that persists credentials": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
      - run: mise run contracts:routes
""",
    ),
    "artifact upload from a pull request": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run locale-catalog:generate
      - uses: actions/upload-artifact@2222222222222222222222222222222222222222
""",
    ),
    "a pull_request_target trigger": (
        "governed.yml",
        """
on:
  pull_request_target:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run contracts:fixtures
""",
    ),
    "governed work reached only through a wrapper script": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: write
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: bash offline/scripts/03-build-frontend.sh
""",
    ),
    "publishing with gh release from a pull request": (
        "governed.yml",
        """
on:
  pull_request:
permissions:
  contents: read
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@1111111111111111111111111111111111111111
        with:
          persist-credentials: false
      - run: mise run locale-catalog:generate && gh release upload v1 out.zip
""",
    ),
}


def test_ci_privilege_policy_fixtures(scratch: Path, token: str) -> int:
    """Synthetic workflows the policy must reject, and one it must accept.

    Without these, a policy that silently stopped discovering jobs would keep
    passing. Each fixture is written into an owned directory and parsed by the
    same code path the real workflows go through.
    """
    checked = 0
    for label, (filename, body) in sorted(HOSTILE_WORKFLOW_FIXTURES.items()):
        directory = scratch / f"workflow-fixture-{uuid.uuid4().hex}"
        directory.mkdir()
        try:
            (directory / filename).write_text(body.lstrip("\n"), encoding="utf-8")
            rejected = False
            try:
                check_ci_privilege(directory)
            except ProbeFailure:
                rejected = True
            require(rejected, f"the CI privilege policy accepted {label!r}")
            checked += 1
        finally:
            shutil.rmtree(directory, ignore_errors=True)

    directory = scratch / f"workflow-fixture-{uuid.uuid4().hex}"
    directory.mkdir()
    try:
        (directory / "governed.yaml").write_text(
            "on:\n  pull_request:\npermissions:\n  contents: read\njobs:\n"
            "  build:\n    runs-on: ubuntu-latest\n    steps:\n"
            "      - uses: actions/checkout@1111111111111111111111111111111111111111\n"
            "        with:\n          persist-credentials: false\n"
            "      - run: mise run contracts:fixtures\n",
            encoding="utf-8",
        )
        examined = check_ci_privilege(directory)
        require(examined > 0, "the accepted control workflow was not discovered as governed")
        checked += 1
    finally:
        shutil.rmtree(directory, ignore_errors=True)
    return checked


GENERATOR_ENTRY_MODULES = ("generate.mjs", "verify-dist.mjs")
GENERATOR_LAUNCHER_NAME = "generator-launcher.mjs"
COMMENT_PREFIXES = ("#", "//", "*", chr(34) * 3, chr(39) * 3)
EXECUTION_TOKENS = ("node", "NODE", "run", "RUN", "subprocess", "exec")

# Where production callers live. The inventory is *discovered* under these
# roots rather than listed by hand, so a new workflow, script or package
# command that runs the generator is covered the moment it is added.
PRODUCTION_ROOTS = (
    ".github/workflows",
    "Dockerfile",
    "frontend/package.json",
    "mise.toml",
    "offline/scripts",
    "scripts",
)
# Deliberately excluded: trusted tests and documentation. Frontend tests import
# generator modules directly and are allowed to; docs describe commands rather
# than running them.
PRODUCTION_EXCLUDED_PATHS = frozenset(
    {
        "frontend/scripts/locale-catalog/generator-launcher.mjs",
        "offline/scripts/test-frontend-cache-hash.sh",
        "offline/scripts/test-frontend-cache-hit.sh",
        "scripts/test_locale_catalog_python_boundary.py",
    }
)
PRODUCTION_EXCLUDED_SUFFIXES = (".md", ".txt")
# Callers that must be present *and* must reach generation through the
# launcher. A production path that stops appearing here is a discovery gap, so
# the inventory is checked against this floor rather than only scanned.
REQUIRED_PRODUCTION_CALLERS: dict[str, str] = {
    ".github/workflows/build-offline.yml": "offline/scripts",
    ".github/workflows/contracts.yml": "mise run contracts:",
    ".github/workflows/haskell.yml": "mise run locale-catalog:generate",
    ".github/workflows/locale-catalog.yml": "mise run locale-catalog:",
    "Dockerfile": GENERATOR_LAUNCHER_NAME,
    "frontend/package.json": GENERATOR_LAUNCHER_NAME,
    "mise.toml": "scripts/generate-locale-catalog.py",
    "offline/scripts/03-build-frontend.sh": GENERATOR_LAUNCHER_NAME,
    "offline/scripts/05-package.sh": GENERATOR_LAUNCHER_NAME,
    "scripts/generate-locale-catalog.py": "generator_launcher_argv",
    "scripts/validate-catalog-serving.py": "generator_launcher_argv",
    "scripts/validate-locale-catalog.py": "generator_launcher_argv",
}


def production_caller_files() -> list[Path]:
    """Every executable configuration or script that could start generation."""
    found: list[Path] = []
    for relative in PRODUCTION_ROOTS:
        path = ROOT / relative
        if path.is_file():
            found.append(path)
            continue
        if not path.is_dir():
            continue
        for child in sorted(path.rglob("*")):
            if not child.is_file() or child.is_symlink():
                continue
            if child.suffix in PRODUCTION_EXCLUDED_SUFFIXES:
                continue
            if child.relative_to(ROOT).as_posix() in PRODUCTION_EXCLUDED_PATHS:
                continue
            found.append(child)
    require(found, "the production caller inventory found no files")
    return found


def test_generator_production_policy() -> int:
    """Every production path starts generation through the one launcher.

    This is centralisation, not containment: the generator is trusted committed
    code either way. Running it from six different places is how behaviour
    drifts and how a provenance-bearing input quietly stops being hashed. The
    inventory is discovered rather than hand-listed, and checked against a floor
    of callers that must exist, so a *missing* production path is a failure too.
    """
    checked = 0
    violations: list[str] = []
    for relative, expected in sorted(REQUIRED_PRODUCTION_CALLERS.items()):
        path = ROOT / relative
        require(path.is_file(), f"production caller {relative} is missing from the repository")
        require(
            expected in path.read_text(encoding="utf-8"),
            f"{relative} no longer reaches generation through {expected!r}",
        )
        checked += 1
    for path in production_caller_files():
        relative = path.relative_to(ROOT).as_posix()
        try:
            text = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError):
            continue
        checked += 1
        for line_number, line in enumerate(text.splitlines(), start=1):
            if GENERATOR_LAUNCHER_NAME in line or "generator_launcher_argv" in line:
                continue
            stripped = line.strip()
            if stripped.startswith(COMMENT_PREFIXES):
                continue
            for module in GENERATOR_ENTRY_MODULES:
                if f"locale-catalog/{module}" not in line:
                    continue
                # A production path names a generator module only to *run* it,
                # so a mention with no command around it is documentation.
                if not any(token in line for token in EXECUTION_TOKENS):
                    continue
                violations.append(f"{relative}:{line_number}: {stripped}")
    require(
        not violations,
        "these production paths reach a generator module without the launcher:\n"
        + "\n".join(violations),
    )
    return checked


def test_npm_install_lifecycle_policy() -> int:
    """Dependency installs must not run package lifecycle scripts.

    A dependency's `postinstall` is externally produced code -- the one class
    this project does treat as untrusted -- and it would otherwise run before
    every check here. Turning lifecycle execution off where it is not needed is
    cheap; this asserts none of the production install steps forgot.
    """
    install_paths = (
        ".github/workflows/locale-catalog.yml",
        ".github/workflows/haskell.yml",
        "Dockerfile",
        "offline/scripts/03-build-frontend.sh",
    )
    checked = 0
    violations: list[str] = []
    command_prefixes = ("RUN", "run:", "if", "then", "else", "&&", "||", ";", "-")
    for relative in install_paths:
        path = ROOT / relative
        require(path.is_file(), f"dependency install path {relative} is missing")
        for line_number, line in enumerate(
            path.read_text(encoding="utf-8").splitlines(), start=1
        ):
            stripped = line.strip()
            if stripped.startswith("#") or stripped.startswith("//"):
                continue
            for token in ("npm ci", "npm install"):
                index = stripped.find(token)
                if index < 0:
                    continue
                if stripped[index:].startswith(("npm install-scripts", "npm install-test")):
                    continue
                prefix = stripped[:index].strip()
                if prefix and not prefix.endswith(command_prefixes):
                    continue
                checked += 1
                if "--ignore-scripts" not in stripped:
                    violations.append(f"{relative}:{line_number}: {stripped}")
    require(
        not violations,
        "these dependency installs still run package lifecycle scripts:\n"
        + "\n".join(violations),
    )
    require(checked >= 5, f"the npm lifecycle scan only examined {checked} invocations")
    return checked


# ---------------------------------------------------------------------------
# Identity: the whole import surface of the pinned interpreter prefix
# ---------------------------------------------------------------------------

PREFIX_PAYLOAD_MARKER = "locale-catalog-prefix-payload-ran"


def zip_import_root(marker: Path) -> bytes:
    """A `python314.zip` that shadows a stdlib module the bootstrap imports.

    `<prefix>/lib/python314.zip` is `sys.path[0]` before CPython reads a single
    source file, so a module inside it wins against every attested `.py`.
    """
    import zipfile
    import io

    buffer = io.BytesIO()
    payload = (
        "from pathlib import Path\n"
        f"Path({str(marker)!r}).write_text('executed', encoding='utf-8')\n"
    )
    with zipfile.ZipFile(buffer, "w") as archive:
        for name in ("csv.py", "base64.py", "encodings/__init__.py"):
            archive.writestr(name, payload)
    return buffer.getvalue()


def test_interpreter_prefix_import_surface(scratch: Path, token: str) -> int:
    """A planted zip root or shadowing extension must never execute.

    Hashing `*.py` is not enough: the zip root precedes every source directory,
    and inside a directory a compiled extension outranks a source module. Each
    payload here is planted in a mirrored toolchain and must be refused before
    the bootstrap's first import.
    """
    sealed_root = sealed_root_from_environment()
    profile = runtime_profile()
    stdlib_relative = profile["interpreter"]["stdlibRelativePath"]
    prefix_relative = profile["interpreter"]["installRelativePath"]
    checked = 0
    tree = create_probe_tree(scratch, "prefix-surface", token, with_history=False)
    try:
        marker = tree / PREFIX_PAYLOAD_MARKER
        payload = (
            "from pathlib import Path\n"
            f"Path({str(marker)!r}).write_text('executed', encoding='utf-8')\n"
        ).encode("utf-8")
        cases = {
            "zip import root shadowing the stdlib": {
                f"{prefix_relative}/lib/python314.zip": zip_import_root(marker)
            },
            "extension shadowing an attested source module": {
                f"{stdlib_relative}/csv.cpython-314-darwin.so": b"\x7fELF probe\n"
            },
            "extension shadowing an attested package": {
                f"{stdlib_relative}/json/__init__.cpython-314-darwin.so": b"\x7fELF probe\n"
            },
            "path configuration file in the prefix": {
                f"{stdlib_relative}/probe.pth": b"import probe_payload\n"
            },
            "planted bytecode beside an attested source": {
                f"{stdlib_relative}/csv.pyc": payload
            },
        }
        for label, tampered in sorted(cases.items()):
            fake = scratch / f"prefix-root-{uuid.uuid4().hex}"
            fake.mkdir()
            mirror_toolchain(sealed_root, fake, tampered)
            require_authoritative_failure(
                label,
                tree,
                [FIXTURE_ENTRY, "--check"],
                environment=probe_environment({"LOCALE_CATALOG_MISE_ROOT": str(fake)}),
            )
            require(
                not marker.exists(),
                f"{label} executed its payload before the boundary refused it",
            )
            shutil.rmtree(fake)
            checked += 1
        return checked
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Regression: the generator's owned build directory is always cleaned up
# ---------------------------------------------------------------------------

OWNED_BUILD_MODULE = "frontend/scripts/locale-catalog/owned-build-dir.mjs"
BUILD_DIRECTORY_PREFIX = ".locale-catalog-build-"
BUILD_OWNER_FILE = ".locale-catalog-build-owner"


def owned_build_probe_source(frontend: Path) -> str:
    """A Node probe over the real ownership helpers.

    Three outcomes have to hold, and the second is the one that failed in
    production: Vite is given the child directory and runs with
    `emptyOutDir: true`, so anything stored *inside* the output directory is
    erased before the generator can use it to prove ownership.
    """
    module = (ROOT / OWNED_BUILD_MODULE).resolve()
    return (
        "import { existsSync, mkdirSync, rmSync, writeFileSync, readdirSync } from 'node:fs'\n"
        "import { join } from 'node:path'\n"
        f"import {{ createOwnedBuildDirectory, releaseOwnedBuildDirectory }} from {str(module.as_uri())!r}\n"
        f"const frontend = {str(frontend)!r}\n"
        "const results = {}\n"
        "\n"
        "// 1. success path\n"
        "const first = createOwnedBuildDirectory(frontend)\n"
        "writeFileSync(join(first.out, 'entry.mjs'), 'export const value = 1\\n')\n"
        "results.successRemoved = releaseOwnedBuildDirectory(first) && !existsSync(first.root)\n"
        "\n"
        "// 2. failure path, with Vite's emptyOutDir behaviour reproduced\n"
        "const second = createOwnedBuildDirectory(frontend)\n"
        "let releasedAfterFailure = false\n"
        "try {\n"
        "  rmSync(second.out, { recursive: true, force: true })\n"
        "  mkdirSync(second.out)\n"
        "  throw new Error('generation failed after the output directory was emptied')\n"
        "} catch {\n"
        "  releasedAfterFailure = releaseOwnedBuildDirectory(second)\n"
        "}\n"
        "results.failureRemoved = releasedAfterFailure && !existsSync(second.root)\n"
        "\n"
        "// 3. a directory this call does not own is left alone\n"
        "const third = createOwnedBuildDirectory(frontend)\n"
        "writeFileSync(join(third.root, '.locale-catalog-build-owner'), 'someone-else')\n"
        "results.foreignKept = !releaseOwnedBuildDirectory(third) && existsSync(third.root)\n"
        "rmSync(third.root, { recursive: true, force: true })\n"
        "\n"
        "// 4. the marker lives outside the tree Vite erases\n"
        "const fourth = createOwnedBuildDirectory(frontend)\n"
        "results.markerOutsideOutput = !readdirSync(fourth.out).includes(\n"
        "  '.locale-catalog-build-owner',\n"
        ")\n"
        "releaseOwnedBuildDirectory(fourth)\n"
        "\n"
        "process.stdout.write(JSON.stringify(results))\n"
    )


def test_owned_build_directory_cleanup(scratch: Path, token: str) -> int:
    """Generated scratch is removed on success and on failure, never otherwise.

    This is a resource-correctness regression, not a security property: an
    earlier revision kept the ownership marker inside the directory Vite
    empties, so every failed generation leaked a full copy of `public/` --
    92 directories and 27 GiB of them before it was noticed.
    """
    frontend = scratch / f"owned-build-{uuid.uuid4().hex}"
    frontend.mkdir()
    probe = frontend / "probe.mjs"
    probe.write_text(owned_build_probe_source(frontend), encoding="utf-8")
    try:
        result = subprocess.run(
            [pinned_node_binary(), str(probe)],
            cwd=frontend,
            env=probe_environment(),
            capture_output=True,
            text=True,
            check=False,
            timeout=300,
        )
        require(
            result.returncode == 0,
            f"the owned build directory probe failed\nstdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}",
        )
        outcomes = json.loads(result.stdout)
        for name in ("successRemoved", "failureRemoved", "foreignKept", "markerOutsideOutput"):
            require(outcomes.get(name) is True, f"owned build directory probe: {name} did not hold")
        leaked = sorted(frontend.glob(f"{BUILD_DIRECTORY_PREFIX}*"))
        require(not leaked, f"the owned build directory probe leaked {leaked}")
        return len(outcomes)
    finally:
        shutil.rmtree(frontend, ignore_errors=True)


def test_generation_leaves_no_build_directory(scratch: Path, token: str) -> int:
    """A real generation run leaves no scratch directory behind."""
    tree = create_probe_tree(scratch, "build-cleanup", token, with_history=False)
    try:
        before = sorted((tree / "frontend").glob(f"{BUILD_DIRECTORY_PREFIX}*"))
        require(not before, f"the probe tree already contains build scratch: {before}")
        result = run_authoritative(tree, [FIXTURE_ENTRY, "--check"])
        require(
            result.returncode == 0,
            f"the fixture check failed in the cleanup probe\nstdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}",
        )
        after = sorted((tree / "frontend").glob(f"{BUILD_DIRECTORY_PREFIX}*"))
        require(not after, f"a governed command left build scratch behind: {after}")
        return 1
    finally:
        release_probe_tree(tree, token)


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def require_owned_probe_mode(token: str | None) -> str:
    """Destructive self-test mode is gated on the owning token.

    There is no root override to grant: a probe tree *is* its own root, so this
    check can never relax a production check. It only refuses to let a stray
    invocation mutate a tree this process cannot demonstrably prove it owns.
    """
    require(token is not None, "--probe requires the owning --owner-token")
    sentinel = ROOT / OWNER_SENTINEL
    require(
        not sentinel.is_symlink() and sentinel.is_file(),
        f"--probe may only run inside an invocation-owned probe tree; {ROOT} carries no "
        f"{OWNER_SENTINEL} sentinel",
    )
    require(
        sentinel.read_text(encoding="utf-8") == token,
        "--probe was given a token that does not match this probe tree's ownership sentinel",
    )
    return token


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--probe",
        choices=("capability-matrix", "generator-production-policy", "ci-privilege-policy"),
    )
    parser.add_argument("--owner-token")
    arguments = parser.parse_args()

    # The policy checks read committed files and mutate nothing, so they run
    # standalone without an owned probe tree -- which is what makes them usable
    # as a quick local gate.
    if arguments.probe == "generator-production-policy":
        checked = test_generator_production_policy()
        print(
            "locale-catalog policy: "
            f"{checked} production generation paths centralised on the generator launcher"
        )
        return
    if arguments.probe == "ci-privilege-policy":
        checked = test_ci_privilege_policy()
        print(f"locale-catalog policy: {checked} CI privilege assertions passed")
        return

    if arguments.probe is not None:
        token = require_owned_probe_mode(arguments.owner_token)
        scratch, scratch_token = owned_scratch()
        try:
            checked = test_capability_lint_matrix(scratch, token)
        finally:
            release_owned_scratch(scratch, scratch_token)
        print(f"locale-catalog python boundary: {checked} capability-matrix rejections proved")
        return

    before = capture_canonical_witnesses()
    token = uuid.uuid4().hex
    scratch, scratch_token = owned_scratch()
    totals: dict[str, int] = {}
    try:
        totals["interpreter prefix surface"] = test_interpreter_prefix_import_surface(scratch, token)
        totals["generator module graph drift"] = test_generator_module_graph_drift(scratch, token)
        totals["owned build directory cleanup"] = test_owned_build_directory_cleanup(scratch, token)
        totals["generation leaves no scratch"] = test_generation_leaves_no_build_directory(scratch, token)
        totals["ci privilege policy"] = test_ci_privilege_policy()
        totals["ci privilege fixtures"] = test_ci_privilege_policy_fixtures(scratch, token)
        totals["generator production policy"] = test_generator_production_policy()
        totals["npm lifecycle policy"] = test_npm_install_lifecycle_policy()
        totals["analyzer matrix"] = test_analyzer_matrix(scratch, token)
        totals["analyzer grammar coverage"] = test_analyzer_grammar_coverage()
        totals["reviewed source drift"] = test_reviewed_source_drift(scratch, token)
        totals["dependency source attestation"] = test_dependency_source_attestation(scratch, token)
        totals["capability lint matrix"] = test_capability_lint_matrix(scratch, token)
        totals["capability lint end to end"] = test_capability_lint_end_to_end(scratch, token)
        totals["target execution"] = test_validated_target_really_executes(scratch, token)
        totals["source and schema drift"] = test_source_and_schema_drift(scratch, token)
        totals["fixture writer ownership"] = test_fixture_writer_ownership(scratch, token)
        totals["workflow base authority"] = test_workflow_base_authority_wiring()
        totals["toolchain identity"] = test_toolchain_roots(scratch, token)
        totals["caller environment"] = test_caller_environment_is_discarded(scratch, token)
        totals["dependency integrity"] = test_dependency_integrity(scratch, token)
        totals["workspace ownership"] = test_workspace_ownership(scratch, token)
        totals["kill recovery"] = test_kill_recovery(scratch, token)
        totals["injected setup failure"] = test_injected_setup_failure(scratch, token)
        totals["parallel self-tests"] = test_parallel_self_tests(scratch, token)
    finally:
        release_owned_scratch(scratch, scratch_token)
    require_canonical_unchanged(before)
    summary = ", ".join(f"{name}: {count}" for name, count in sorted(totals.items()))
    print(
        "locale-catalog python boundary: "
        f"{sum(totals.values())} regression, drift, identity and policy checks passed ({summary}); "
        "the canonical worktree was never written"
    )


if __name__ == "__main__":
    main()
