# Public locale catalog

The backend sends story content as opaque `I18nEntry` keys and typed variables
(`backend/arkham-api/library/Arkham/I18n.hs`). The Vue client can resolve them
because its build bundles `frontend/src/locales/**`; native clients have no
such bundle. This catalog is the deployment-owned resource they resolve keys
from, published exactly like the generated card JSON under `/cards`.

It is a technical distribution and content-provenance boundary, not a new
license grant: the deployment already serves this text to browsers, and the
catalog moves nothing into a client repository.

## What this is, and what it is not

This is **infrastructure**: a versioned, verifiable resource that publishes the
locale text the deployment already has, and that states in its own schema what
it could not publish. It is **not** a claim of complete native gameplay
coverage.

* The catalog fails closed on anything it can render but would render wrongly,
  and it publishes its gaps rather than hiding them: `backend.untranslatedKeys`
  (a key the backend emits that no locale file defines), `backend.variableGaps`
  (a message that wants a substitution the backend was not seen to send),
  `backend.unknownVariableTypes` (a message whose slot the backend does not
  fill in a form that slot can render), and `{"form": "unsupported", "reason":
  …}` entries with no content. A key in `unknownVariableTypes` is published as
  `unsupported` with reason `unusable-variable-type` **in every locale**, so no
  consumer can mistake it for renderable content.
* **A consumer MUST treat a missing key, an `unsupported` entry, or an entry
  whose declared variables it cannot supply as unavailable and non-actionable.**
  Rendering a raw key, a partial string, or a placeholder as if it were an
  instruction is a correctness bug in the consumer, not a fallback.
* At the revision this was written, six backend-emitted keys have no text in
  any locale file, and 23 more are published as unavailable because the backend
  does not fill their slot in a form the message can render (the Vue client is
  affected by both today, for the same reasons).
  They are listed, with the emitter site that needs the text, in
  `frontend/scripts/locale-catalog/known-gaps.json`. Closing them is a content
  change and follow-up work; it is not something this catalog can do, and it is
  not claimed here.

## What is published

    /locale-catalog/manifest.json                index (revalidates by ETag)
    /locale-catalog/r/<revision>/manifest.json   identical bytes, immutable
    /locale-catalog/c/<sha256>.json              chunks, content-addressed

`<revision>` is `1.` plus the first 32 hex characters of the provenance digest.
A chunk's URL carries **only its content digest** — no revision — so a pack
whose content did not change keeps the same URL from one revision to the next.
That is what makes a rolling deploy safe: a client holding the new manifest can
still fetch every unchanged pack from a replica that is still serving the old
build.

Immutable paths (`/c/` and `/r/`) are served `Cache-Control: public,
max-age=31536000, immutable`; the stable manifest URL revalidates. Crucially,
that policy is chosen by **response status**: only 200 and 304 are cacheable,
and every 4xx/5xx — a missing chunk, a wrong method — is `no-store`. Ranges are
disabled for these paths (a partial JSON chunk is useless), so a `Range`
request returns the whole file and nginx never produces a 416, the one status
it finalizes *after* the cache headers are computed. A client that does race a deploy and gets
a 404 for a changed pack must re-fetch `manifest.json` and retry; because the
404 was never cached, the retry succeeds as soon as it reaches a replica of the
newer build. Deployments should still swap the static root atomically.

A chunk is one locale/pack slice, where the pack is a message key's first
dotted segment (`nightOfTheZealot`, `theScarletKeys`, …) and top-level keys
without a segment live in `core`. Packs let a client fetch only the campaign it
is playing instead of a whole locale.

Schemas: [`frontend/schemas/locale-catalog/v1/manifest.schema.json`](../frontend/schemas/locale-catalog/v1/manifest.schema.json)
and [`chunk.schema.json`](../frontend/schemas/locale-catalog/v1/chunk.schema.json).
Both are closed (`additionalProperties: false`, closed enums), so an unknown
node type or field is a validation failure rather than something to guess at.

## Locale resolution

`defaultLocale` is `en`. Every other locale declares `"fallback": "en"`: a key
absent from a locale's chunk — or a linked reference that does not resolve
there — is resolved against the default locale, which is what vue-i18n does in
the web client. Locales ship only the keys they actually translate, so a
partial locale (`de` today) is normal.

`languageResolution` records how the production resolver
(`preferredLanguage` + `uiLocaleFor` in `frontend/src/locales/language.ts`)
maps BCP-47 tags to catalog locales, including `zh-Hans`/`zh-CN`/`zh-Hant` →
`zh` and anything unsupported → `en`. It is generated by calling those
functions, so a native client cannot drift from the web client's behavior.

## Render AST

Entries are `{"form": "message" | "plural" | "unsupported"}`.

`message` carries `nodes`, `plural` carries one `nodes` array per vue-i18n
plural branch, and both declare every variable they reference in `variables`.
**Clients never execute HTML.** Node types:

| type | meaning |
| --- | --- |
| `text` | literal text (entities already decoded) |
| `var` | typed placeholder; `source` is `named` (`{name}`) or `list` (`{0}`), `role` is `text`, `icon`, or `presentation` (only ever reaches a `class`/`style`/`data-*` attribute, so a client with no value for it loses styling, never an instruction) |
| `linked` | vue-i18n `@:key` / `@:{var}` reference, with optional `upper`/`lower`/`capitalize` modifier |
| `paragraph`, `group` | block containers with `styles` presentation hints |
| `heading` | `level` 1–6 with `styles` |
| `emphasis` | `bold`, `italic`, `underline`, `strikethrough`, `small`, `smallCaps` |
| `list` | `ordered`, `styles`, and `items[]` (each with `styles` and `children`); nested lists nest |
| `break`, `rule` | `<br>`, `<hr>` |
| `image` | semantic asset reference: `role` (`encounterSet`, `card`, `token`, `chaosToken`, `campaign`, `homebrew`, `extra`, `other`), `assetPath` relative to `/img/arkham/`, `styles`, and optional `alt`, `width`, `align` |
| `cardRef` | text that names a card (`code`) plus its `children`; the web client shows that card's art on hover |
| `table` | `head` and `body` rows of cells (`header`, `styles`, `children`); the Dream-Eaters epilogue matrix is an instruction, so it keeps its structure |

A `paragraph`, `table` or table cell may also carry `data`: an allowlisted
`data-*` value (`count`, `selected`, `epilogue`) holding either a declared
variable or a short literal token. It
is data, never markup — the vote counters in *Congress of the Keys* mirror
their count into an attribute the stylesheet reads, and a client that ignores
`data` still renders the instruction.

`styles` are the source `class` tokens, restricted to `[A-Za-z][A-Za-z0-9-]*`.
They are hints only: a client may ignore any token without losing an
instruction. An element whose node type has no `styles` field (`emphasis`,
`break`, `rule`) refuses a `class` attribute rather than dropping it.

Inline CSS becomes `style`: a bounded list of `{property, value}` declarations
drawn from a **closed grammar** — an allowlisted property, and a value built
only from allowlisted keywords, numbers with allowlisted units, hex/plain
colors and the functions `calc`, `clamp`, `min`, `max`. Any other property,
function (`var()`, `image-set()`, an escaped `\75 rl()`, …) or token makes the
entry unsupported; a value is never a raw string. A `url(...)` is accepted only
as a whole value, only when it resolves inside `/img/arkham/`, and then becomes
`{property, asset}` — the same semantic asset reference an image node carries —
so a nested `image-set(url(/api/private))` cannot smuggle a resource through. A class built
from a placeholder (`class='{restfulNight3Status}'`) becomes a declared
`styleVars` entry, so the hint survives as a typed variable instead of being
dropped.

Variable coverage is checked by role *and* type, not by name: the catalog
declares a role for every slot, the backend registry proves a type for most
names, and a slot the registry cannot type is not a pass. Those entries become
`unsupported`, and — because a message that renders *through* one is no more
renderable than the one itself — link resolution and type compatibility settle
**together**, alternating until a whole round changes nothing. No supported
entry can reach an unusable one, directly, transitively, or through the
fallback locale.

Variables are **never interpolated at generation time**. The backend's
`I18nEntry.variables` are substituted by the client, exactly as vue-i18n does
for the web. A `role: "icon"` variable is one production `replaceIcons()` maps
to an icon glyph (`{skull}`, `{elderThing}`, homebrew icons, …); the catalog
preserves the placeholder instead of rendering it.

Image nodes carry no bytes and no URL. `{setImgPath}/rats.png` becomes
`{"role": "encounterSet", "assetPath": "encounter-sets/rats.png"}`; relative
`../` segments are resolved during generation and a path that escapes
`img/arkham/` is refused. No official card, mat, or set artwork is embedded.

### Backend-emitted keys are not optional

`scripts/extract-backend-i18n-keys.py` parses every backend module with
tree-sitter's Haskell grammar and resolves each key-emitting call site
(`ikey`, `i18n`, `i18nWithTitle`, the flavor-text DSL, `labeled'`, literal
`"$key"` tokens, …) together with the scope stack `scope`/`campaignI18n`/
`scenarioI18n`/`unscoped` put it in, and the variables `countVar`/`withXp`/
`nameVar`/… attach. Resolution follows Haskell scoping rather than proximity: an alias
(`campaignI18n`, `scenarioI18n`, …) is looked up through the imports that
actually carry it, including `module X` re-exports and the import lists that
narrow them, and a parameterized alias
(`scenarioI18n n a = campaignI18n $ scope ("part" <> tshow n) a`) is resolved
with the literal its call site supplies. Scopes and keys that are a conditional
or a local binding over literals fan out to every branch; a local helper whose
key is a parameter (`let interlude k = … p k`) is resolved from its call sites.
Presentation modifiers (`p.green`, `li.validate cond "key"`) keep their key, and
amount-prompt labels are recorded under `choice.` exactly as the web client
resolves them.

Resolution is also **scope-aware in both directions**. Which definition a name
means is decided by Haskell scoping, so two campaigns that each define
`scenarioFlavorText`, or one module that binds `let interlude k` twice under
two different `scope`s, never share their call sites. A local helper is filed
under the scope of the call site that invokes it, not the one it was written
in. A scope primitive only scopes the arguments it actually takes, so
`unscoped (countVar 1 $ labeled' "x") do …` resets the label and leaves the
block alone. And a condition that an enclosing `case`/`if` already decided is
not fanned out: `scope (if headedWest then "west" else "east")` around
`if headedWest then li "a" else li "b"` names two keys, not four.

Three fail-closed rules keep the registry honest:

* a module tree-sitter cannot parse is a **hard failure**. `preprocess` and
  `repair` neutralize the GHC constructs the grammar mis-reads — on code bytes
  only, never inside a string literal, and without moving an offset — and there
  is no waiver: skipping a module rests on a lexical guess about what an
  unreadable file contains, which is the reasoning this registry replaces;
* every site that cannot be resolved is committed in full in `dynamicSites`
  with a reason from a **closed vocabulary** (`runtime-key`, `runtime-scope`,
  `caller-scope`, `partial-key`, `scope-underflow`); an unclassifiable reason
  fails the run, and the drift gate fails when the set moves;
* the extractor has its own tests (`mise run locale-catalog:backend-keys-test`)
  that feed synthetic Haskell modules to the production code path, one rule at
  a time.

The result is committed as `backend/arkham-api/i18n-emitted-keys.json` and is
re-derived in CI (`mise run locale-catalog:backend-keys-check`), so adding a new
`ikey` to the backend fails until the registry is regenerated. The catalog
build consumes it and **fails** if any emitted key that the default locale
translates is unsupported — those keys are gameplay content, not decoration.
Keys the backend emits that no locale translates at all are a content gap in
the locale sources, not a rendering failure: writing the missing prose is not
something a build tool can do. Where the gap was an emitter naming a key no
locale ever had, the emitter was corrected to the canonical entry that does
exist (`cards.label.protectingTheAnirniq.return`,
`…teachingsOfTheOrder.removeAFloodTokenFromANon_sanctum_Location`,
`…yigsMercy.refused`, …) or the entry was moved to the key the backend and the
Vue client both ask for — both of which fix the web client too, which shows the
raw key today. Those, the entries whose markup the AST refuses,
and the keys whose text wants a variable the backend was not seen to send are
therefore pinned in `frontend/scripts/locale-catalog/known-gaps.json`. The
build **fails** when a gap appears that is not on that list *and* when a listed
gap has been fixed, so the set can only move deliberately
(`--update-known-gaps`, reviewed in the diff). Every entry carries a
justification from a closed list (`missing-from-locale-sources`,
`web-only-chrome`, `source-syntax`) and the emitter site that needs the text,
so the file is an actionable blocker report rather than a set of names; the
build fails if a justification is missing. The
file is part of the generator's provenance, so changing it changes the catalog
revision. The same
sets are also reported in the manifest under `backend.untranslatedKeys` and
`backend.variableGaps`.

### Unsupported entries

Markup the AST does not model is never guessed at, half-rendered, or silently
stripped. The entry becomes
`{"form": "unsupported", "reason": ..., "detail": ...}` with no content, and
the reason is one of a closed set: `message-syntax-error`,
`unsupported-message-syntax`, `html-parse-error`, `unsupported-element`,
`unsupported-attribute`, `placeholder-in-attribute`,
`asset-variable-outside-image`, `unsupported-image-source`,
`image-path-escape`, `invalid-style-token`, `invalid-style-declaration`,
`misplaced-list-item`, `unresolved-link`, `unsupported-link-target`,
`link-cycle`, `conflicting-variable-role`.

At the revision this was written that is 5 of 38,817 entries (0.013%), and
**none of them is a key the backend emits**: 4 `unsupported-element`
(`<a href>` and a table of contents on the About page — web chrome, never sent
by the backend) and 1 `message-syntax-error` (a `<style>` block whose CSS
braces are not valid vue-i18n message syntax, so the Vue client cannot render
it either). They are pinned, with justifications, in `known-gaps.json`. Source typos that used to swallow prose in the
web client too — `<p.`/`<li.`/`<pSon`/`<?p`/`<<ul` instead of a closed tag, a
`<ul>` whose first `<li>` was missing, an unterminated `<li`, a Korean
paragraph opened with `<끔찍한`, a linked key with a misspelled target, and two
backend keys with a trailing space — were fixed at the source rather than
papered over here. A client should show any remaining unsupported entry as
unavailable rather than blank.

## Generation and provenance

The sealed `mise run locale-catalog:generate` command writes
`frontend/public/locale-catalog/`, which Vite copies into `dist/` and
`scripts/precompress.cjs` gives `.gz`/`.br` siblings. The output is generated,
so it is git-ignored — the committed artifacts are the generator, the schemas,
and the tests.
`npm`'s `prebuild` only verifies those existing bytes; it never regenerates
them. CI runs the sealed command before each frontend build. The Docker
frontend stage uses a digest-pinned Node image and an explicit absolute Node
binary as its equivalent immutable generator boundary, then its `prebuild`
performs the same verification before shipping `dist/`.

Composition is not re-implemented: the generator runs Vite in SSR mode over
`src/locales/messages.ts`'s own `loadLocaleMessages`, so locale `.ts`
composition, JSON imports, and the `import.meta.glob` homebrew discovery behave
exactly as in the Vue build. Message parsing uses `@intlify/message-compiler`
(the parser vue-i18n compiles messages with) and markup parsing uses `parse5`.
Icon and literal-escape classification is derived by asking production
`formatContent()`/`replaceIcons()` what they would do with `{name}`.

`catalogRevision` is derived in two phases, so it cannot miss a change in
either direction. First the catalog's content is rendered with no revision and
no URLs in it and hashed (`provenance.outputSha256`). Then that digest is
hashed together with the complete provenance — every locale source file, the
production modules whose semantics are mirrored, the generator sources, both
schemas, the backend emitted-key registry, the exact `package-lock.json`
(integrity hashes included), the exact Node version, the versions of Vite,
vue-i18n/`@intlify`, parse5 and jsonc-parser, and the contract fixtures — and
the result becomes the revision. A dependency bump, a Node major change, a
locale byte, or a change in the rendered output alone therefore all produce a
new revision, and identical inputs always reproduce the same one.

Node is pinned to one **exact** version (`26.7.0`) across `mise.toml`,
`frontend/package.json` (`engines`), the `Dockerfile`, CI and the offline
installer; the generator refuses to run on any other version, down to the
patch, rather than silently producing different bytes. Identical inputs
therefore always produce identical bytes, and any input change is a new
revision. The manifest's `provenance` block republishes those digests, and the
manifest pins every chunk's size and SHA-256, so a client can verify what it
downloaded against what it was promised.

The Python contract commands have an equally narrow, enforceable boundary.

### What is trusted, what is checked, and what these checks are worth

Being precise about this matters more than the checks themselves, because a
check described as more than it is becomes a liability in review.

**Trusted.** Every committed executable file in this repository — workflows,
shell scripts, Python scripts, Node scripts, the frontend locale modules and
their tests — is trusted code. It is reviewed through pull request, and a
malicious commit, reviewer or maintainer is out of scope. **This project does
not sandbox its own code**, and nothing described below should be read as doing
so.

**Executed but not contained.** Pull-request CI runs unreviewed pull-request
code; that is what CI is for. What is bounded is its *authority*, not its
behaviour: the jobs are ephemeral, hold `permissions: contents: read`, receive
no secrets, check out without persisted credentials, and publish nothing.
Arbitrary PR code is not contained beyond that posture, and the CI privilege
policy test asserts the posture rather than any containment. It follows job and
step `uses:` through local reusable workflows and composite actions
recursively — with canonical path containment and cycle detection — so a
governed command reached through arbitrary local nesting is still judged, and a
remote reusable workflow named at job level must be an exact
`owner/repo/path@<40-hex>` reference, whether it appears at the root or several
local hops down. A remote reusable workflow is opaque, so reaching one from
anywhere in the chain classifies the whole chain as governed even when no
document in it contains a literal governed marker.

**Untrusted, and therefore checked.** Externally produced tool and dependency
artifacts (the CPython distribution, Node, uv, wheels, npm packages),
environment and settings and input data, filesystem and CI cache contents, and
generated or released output. Those are what the machinery in this section
actually gates.

**What that machinery guarantees.**

* Exact tool and dependency identity: pinned versions bound to committed
  SHA-256 digests before use.
* No install-time dependency code: `uv --no-build --no-sources` and npm
  `--ignore-scripts`, so an externally produced package cannot run a build
  backend or a lifecycle hook.
* Deterministic byte comparison and exact provenance: a governed artifact is
  re-derived and compared, and records the digests of the inputs that produced
  it.
* Explicit base SHA for the revision gate, schema validation of every governed
  document, ownership-proved cleanup of anything this tooling creates,
  least-privilege CI, and one centralised production generation path.

**What it does not guarantee.** The Python capability lint is a conservative
review aid and defence in depth — it is not a security boundary or a proof, and
its completeness is not a release invariant (see its `KNOWN_INCOMPLETE`
section). The Node generator launcher and its digest list enforce
production-entry centralisation, module-graph drift detection and exact input
identity for provenance; they are not a JavaScript sandbox, and the generator
and locale code they start run with full Node privileges. Neither is a defence
against another process on the same machine racing a check, and neither tries
to be.

Two names survive from an earlier design: `scripts/locale-catalog-python-sealed.sh`
and the `LOCALE_CATALOG_SEALED_SHELL` sentinel it checks. Read "sealed" there as
legacy naming for *the pinned, isolated runner* — the stage that discards the
caller's environment and runs governed commands against pinned toolchain paths.
It is not a seal against this repository's own code.

**One authoritative entry point.** Every mise/CI command that can generate,
verify, update, approve, hash or revision-check a governed artifact goes
through `scripts/run-locale-catalog-python.sh`. That file is deliberately a
POSIX `sh` stage: a `#!/bin/bash` entry point would source a caller-supplied
`BASH_ENV` script *before* its own first line ran, so no in-script guard could
ever be early enough. It does exactly one thing — rebuild the environment from
nothing with `/usr/bin/env -i` and hand `scripts/locale-catalog-python-sealed.sh`
(non-executable, and refusing to run without the sealing sentinel) the two
inputs that carry authority:

1. `LOCALE_CATALOG_MISE_ROOT`, an explicit, absolute, already-canonical,
   non-symlink toolchain root named by the trusted host. It is never
   defaulted, never derived from `$HOME`, and never guessed; the sealed stage
   also refuses the filesystem root and any unnormalized path.
2. `scripts/locale_catalog_python_runtime.json`, the committed toolchain lock.

The caller's `HOME`, `PATH`, `PYTHON*`, `BASH_ENV`, `ENV`, `SHELLOPTS`,
`CDPATH`, `IFS`, `GIT_*` and `UV_*` are gone before any authoritative byte is
read, and the child runs with a `HOME` that is this invocation's own empty
directory and a `PATH` that deliberately excludes the interpreter's `bin`
directory (which a local `pip install` can fill with arbitrary executables) and
the `uv` directory (invoked by absolute path). Local convenience tasks are
named `*-local`, are documented as non-authoritative, and cannot regenerate a
governed byte or satisfy a CI authority gate.

CI names the root in the workflow's sealed `defaults.run.shell`, which is
itself `/usr/bin/env -i ... /bin/bash --noprofile --norc`, so no step inherits a
runner environment either. A maintainer running these tasks locally names it
the same way and just as deliberately — for example
`LOCALE_CATALOG_MISE_ROOT="$HOME/.local/share/mise" mise run contracts:validate` — and
an unset or empty root is refused with that exact diagnostic rather than being
guessed from `$HOME`.
The real backend-probe task also receives one explicit host authority:
`LOCALE_CATALOG_STACK=/absolute/path/to/stack`; it rejects a bare `stack`
command or any PATH lookup. CI captures the absolute path from the reviewed
Haskell setup step and includes it in the otherwise empty shell environment.

**Identity and drift checks over the reviewed tooling.** The pinned runner
hashes the capability lint, the preflight, both launcher stages and the Node
launcher against the `trustedSources` block in
`scripts/locale_catalog_python_runtime.json`, and the preflight re-checks the
same block from inside the process. None of that is authentication against an
attacker — this is all trusted committed code — it is a coordinated-change
requirement: editing one of these files has to move its recorded identity too,
and a run where they disagree stops and says which file. The lint and the Node
launcher happen to be checked before anything imports or starts them, which is
worth having only because a half-edited lint should not be deciding anything.
The suite exercises this by replacing the lint with a permissive stand-in and
requiring the run to stop before that stand-in's top-level code executes.

**The importable prefix, not just its sources.** Hashing `*.py` under the
stdlib root leaves two ways into the process before any of it is checked:
`<prefix>/lib/python314.zip` is `sys.path[0]` — ahead of every source directory
— and inside any path entry a compiled extension outranks a source module, so a
`csv.so` beside `csv.py` wins. Before the interpreter starts, the sealed shell
therefore refuses a zip import root, a `.pth`, or any `.so`/`.dylib`/`.pyd`/
`.pyc` anywhere in the prefix outside `lib-dynload`; and in this invocation's
own copy it *removes* the zip root and empties `lib-dynload` outright rather
than pinning platform-specific binaries. (The directory itself stays: CPython
falls back to a build-time absolute path if it is missing.) A startup probe
then proves the reduced prefix still starts and can import exactly the module
set the bootstrap imports at its top level, and the bootstrap re-checks that
`sys.path` is the expected three entries and that every module already in
`sys.modules` with a file hashes to the committed lock. That is what makes the
preflight's own top-level imports resolve to bytes the committed lock
describes.

Dependency installs are part of the same ordering question: a committed
`postinstall` in any npm dependency would run before every preflight here, so
`npm ci`/`npm install` run with `--ignore-scripts` everywhere lifecycle
execution is not needed, and a policy test fails on any invocation that does
not.

**Environment attestation.** Before the first Python byte runs, the sealed
shell hashes the complete non-variant stdlib source inventory (path names and
contents), then hashes each exact CPython, Node and uv binary against the
platform-specific digest table. Each Python process uses a fresh, empty,
invocation-owned `-X pycache_prefix`, so it cannot consult the installation's
normal `__pycache__` at all; `-B` additionally prevents new bytecode writes.
The bootstrap repeats executable/source checks after startup, then proves its
interpreter path inside the toolchain root (the real
`python3.14`, not the `bin/python` symlink a `pip install` can retarget), base
prefix, version, cache tag and the `-I -S -E -B` isolation flags. It then
proves the stdlib as a closure — every `.py` file under the
sealed stdlib root must be a committed digest-table entry that hashes to the
recorded value, or one of the four exactly-named build-configuration modules
the lock records as platform variants (and which no importable module can
reach). The table is platform-independent by construction: every source it
pins was verified byte-identical in the pinned upstream CPython 3.14.7 tarball
and in the `python-build-standalone` builds the lock records.

That covers every stdlib `.py` byte. The rest of the standard library is
covered by an *import-closure* proof rather than by hashing platform-specific
binaries: starting from exactly the stdlib modules the capability boundary lets
a governed source name, and following those modules' own imports through their
attested sources, every reachable name must resolve to an attested `.py` — or
to no file at all, in which case only the pinned interpreter binary itself (a
builtin or frozen module, from the distribution the lock records by checksum)
can satisfy it. A name that resolves to a file-backed extension module, or to
one of the unhashed platform-variant modules, is refused. Resolution follows
CPython's own precedence, where a planted `csv.so` shadows the attested
`csv.py`, so that is exactly the shape the adversarial suite plants. The lock
also carries a floor for how many attested sources that walk must reach, so a
walk that silently stops resolving fails instead of passing vacuously.
Attestation is kept strictly out of generated content — nothing measured at run time is mixed
into a manifest, provenance digest or contract revision — so governed output is
byte-identical on every host carrying the same lock.

**Trusted git.** Governed tooling never resolves `git` through `PATH`. The
sealed stage verifies an absolute, non-symlink, executable system binary and
passes it in as `LOCALE_CATALOG_GIT`; `strict_json.git_argv()` rewrites every
plumbing command onto it, and a missing or non-absolute value fails closed.
That applies to every governed reader, including the catalog validator's
tracked-file and `check-ignore` lookups and the serving gate's clean-clone
assembly, which now take the bound executable instead of a `shutil.which("git")`
result; `validate-catalog-serving.py`'s helper resolver refuses the name `git`
outright. The caller cannot choose it either — `LOCALE_CATALOG_GIT` supplied by
the caller is discarded with the rest of the environment and rebuilt by the
sealed stage, which the adversarial suite proves with a marker-emitting fake
`git` offered both on `PATH` and as that variable.
Revision resolution is isolated in the drift gate, which takes a validated
exact lowercase 40-hex ancestor base SHA **positionally** from the workflow
event. A missing, all-zero, malformed, uppercase, unresolvable, self/HEAD or
non-ancestor value fails in CI, and no environment variable is ever consulted
as a base reference; the separately named `contracts:revision-drift-local` task
carries the local fallback chain and is not authoritative.

**Capability boundary.** The `scripts/` directory is flat and closed: any
nested directory, nested Python file, symlink or bytecode is refused before it
can shadow a standard-library or governed top-level import. Every declared
`scripts/*.py` source is then parsed before it is imported. Imports resolve only to declared stdlib, locked dependency or
in-tree modules, and every dotted reference reaching a sensitive root is
checked against a per-source grant naming the exact capability *and the exact
shape* it may appear in — called, read as a value, assigned to, or consumed as
a call result. Granting `subprocess.run` as a call therefore does not grant
`subprocess`, `subprocess.Popen`, or storing `subprocess.run` itself. On top of
that, conservative taint propagation refuses to let a value derived from a
propagating capability escape through assignment, unpacking, list/tuple/set/dict
literals, comprehensions, subscripts or slices, attributes, returns, yields,
parameters or defaults, lambdas, closures, globals/nonlocals, class or instance
fields, generators, context managers, exception aliases, decorators or aliases.
Lexical scope snapshots prevent an inner parameter, class, branch or exception
binding from erasing an outer sensitive alias; rebinding any such alias is
itself a refusal. Anything the analyzer cannot resolve but can see is sensitive
fails closed.

The analysis is path-sensitive and identity-preserving rather than
last-writer-wins. `if`/`else`, `while`, `for`, `try`/`except`/`else`/`finally`,
`match` and `with` are analyzed per branch and then *joined*: a name is
everything it may be on any path that can reach the join. A branch that always
returns or raises contributes nothing; `break` contributes its state to the
code after the loop and `continue` to the next iteration; a loop is iterated to
an actual fixpoint rather than a fixed number of passes, and a loop that will
not converge is refused instead of truncated. Conditional expressions and
short-circuit `and`/`or` join both operands. So a false `if`, a zero-iteration
loop, an untaken `except` and a `break` can no longer erase a module identity
another path bound.

Identities also survive containers and captures. A module placed in a list,
tuple, set, dict or comprehension is still that module when it comes back out
through a subscript, a slice, iteration, unpacking, a starred target, a `match`
capture (including sequence, mapping, class-attribute, `as` and `|` patterns),
or a container method such as `pop`, `get` or `copy` — `holder = [strict_json][0]`
followed by `holder.subprocess.run(...)` is refused exactly as the direct
spelling is. A walrus inside a comprehension binds in the enclosing scope, and
is tracked there. Where an identity would leave the reference the analyzer can
resolve exactly — passed as a call argument, returned, yielded, interpolated
into a t-string, captured by a lambda, used as a parameter default, stored into
an attribute, a subscript or a class namespace, or bound to a name a
`global`/`nonlocal` declaration connects to an outer scope — it fails closed.
Writing into an imported module's namespace at all is a refusal. Python 3.14's
`type` aliases, type parameters, and template strings are modelled explicitly,
as are the `async` forms of `for`, `with` and `def`.

Coverage is not assumed: every value-bearing and binding node type is named in
an explicit dispatch table, and a node type that is *not* there is refused
rather than walked generically, so a future language construct cannot silently
become a hole. A self-test parses every governed source plus the whole payload
and control matrix and requires that nothing in it is unmodelled.

`runpy`, `subprocess`, `os.system`/`spawn`/`exec`, importlib loaders,
`zipimport`, `ctypes`, `pickle`/`marshal`/`shelve`, `breakpoint` (which
`PYTHONBREAKPOINT` redirects by dotted name, and which Python 3.14 lets take a
pdb command script), dynamic code objects, import-path mutation and dunder
traversal are all rejected, including through aliases. YAML is a sensitive root
in its own right: only `yaml.safe_load` is grantable, and only in the one
source that declares it, while `yaml.load`, `yaml.unsafe_load`,
`yaml.full_load`, every loader class and constructor registration are forbidden
outright. Modules that exist only as deserialization or archive-extraction
surfaces are not in the shared allowlist at all. The grant tables live in
`scripts/locale_catalog_python_boundary.py`, which is both a declared source
and part of the trusted computing base: it is capability-scanned like any other
*and* digest-pinned before it runs, and its bytes are folded into
`generatorSha256`, so a grant cannot be widened without moving a governed
contract hash.

**Dependency boundary.** uv resolves, downloads, unpacks and *can build*
distributions, and a PEP 517 backend is arbitrary code that would run before
anything else got a say. So before uv starts, a throwaway interpreter runs the
bootstrap's `--attest-dependency-sources` mode under `-I -S -E -B`: it imports
only the standard library, executes no project code, creates no environment and
builds nothing. It requires `requires-python == 3.14.7` in both `pyproject.toml`
and `uv.lock`, exactly pinned project dependencies, no `[build-system]`, no
`[tool.uv.sources]`, a root project that is the virtual project itself, and —
for every other locked package — a single `registry` source pointing at the
locked index with at least one `sha256`-pinned wheel whose tags fit the declared
platform. A `git`, `url`, `path`, `directory` or `editable` source, a selectable
wheel without an exact hash, a wheel outside the locked registry, or a
distribution with no compatible wheel is refused before a virtual environment
exists. The attestor then writes the exact bytes it validated into this
invocation's own project directory and uv is pointed at *that* directory, so
what uv consumes is what was checked rather than a second read of the same
path; uv itself runs with `--locked --no-cache --link-mode copy --reinstall
--no-dev --no-build --no-sources --no-install-project --no-install-local`.

After installation the bootstrap verifies the installed distribution set
against the same attested lock bytes: exact names and versions, every wheel
`RECORD` hash reproduced, no unrecorded or missing file, no symlink, no
bytecode, no `sitecustomize`/`usercustomize`, and each installed distribution
bound back to a hash-pinned locked wheel candidate through its `WHEEL` tags.
That `RECORD` proof is an integrity check over an *externally produced*
artifact: it establishes that the installed tree corresponds to the locked,
hash-pinned wheels. It says nothing about another process on the machine
editing the tree afterwards, and is not meant to. The catalog-only schema checks use the small
fail-closed in-repository validator instead of `jsonschema`: it implements
exactly the keywords the published v1 schemas use, refuses every keyword,
`$ref` spelling and dialect it does not implement rather than ignoring one, and
compiles `pattern` with ECMA-262 semantics — `$` rewritten to `\Z`, `re.ASCII`
so `\d`/`\w`/`\s`/`\b` are not silently widened to Unicode, and a refusal
for every Python-only construct (named groups and backreferences, inline flags,
comments, atomic groups, conditionals, possessive quantifiers, `\A`/`\Z`/
`\z`/`\G`, `\N{...}`, `\p{...}` and numeric escapes). Its own fail-closed
proofs run on the governed routes that depend on it, so an unimplemented
keyword or a Python-only pattern construct is a CI failure rather than a latent
one.

**Workspace ownership.** Each invocation gets an exclusively created workspace
under the repository, recording a per-invocation token and its device/inode
identity. Cleanup re-checks both immediately before removal. There is no stale
workspace scavenger: a later invocation never removes an old, concurrent or
caller-created directory based on a forgeable filename or owner record.

`mise run locale-catalog:python-boundary-test` proves all of this
adversarially, and every one of its probes — source, schema, dependency,
interpreter, alias and tamper alike — runs inside an invocation-owned temporary
copy of the exact governed tree. It never writes inside the canonical worktree,
and it re-checks byte for byte the canonical paths earlier revisions of it used
to mutate.

These checks assume the machine running them is not actively hostile: the OS,
kernel, shell and the hash-verifying `uv` and `npm` downloaders are trusted at
the moment a command runs, and no other process is racing them. They do not
claim to defend against an actor who can rewrite the reviewed tooling *and* its
committed identity records — that is what code review is for. What they do
catch is a changed governed source, a replaced interpreter, Node or uv binary,
an injected or edited stdlib module, a zip or extension import candidate in the
interpreter prefix, a non-canonical toolchain root, startup hooks, shadow
bytecode, an added import or widened capability grant, an unsafe deserializer,
a PEP 517 build backend, a redirected or unhashed dependency source, and a
generator module graph that no longer matches its committed digests.

**Node module graph, enforced at run time.** The JavaScript side is bound by a
loader, not by reading source text. Every governed Node entry point starts
through `frontend/scripts/locale-catalog/generator-launcher.mjs`, which
installs Node's synchronous `module.registerHooks` resolve/load hooks *before*
importing the entry module. A resolution is permitted only if it is a `node:`
builtin, a path under `frontend/node_modules` (bound by `package-lock.json`,
which the preflight hashes, and where the generator's own invocation-owned Vite
output lives), or a generator module listed in the committed allowlist beside
the launcher — and an allowlisted module is served from bytes that still hash
to its committed digest. Everything else is refused: a repository file outside
the allowlist, an absolute path, a `data:` URL, an unsupported scheme.

That replaces an earlier lexical scan, which a leading space, a comment inside
`import(...)`, or a computed specifier all defeated. Hooks see what the loader
resolves, so there is no syntax to hide behind; the adversarial suite proves it
with live Node runs of exactly those payload shapes and a control that must
still load.

Around the loader, the Python entry point checks that the generator directory
holds only declared files, that the allowlist describes it exactly, and hashes
the whole closure — generator sources, launcher, allowlist, `package.json`,
`package-lock.json` and the Node binary — before Node starts and again after it
returns. The generator hashes the same directory into the catalog's own
`provenance` record, so the bytes validated are the bytes provenance describes.
That is drift detection over trusted, reviewed code — a coordinated-change
requirement and a provenance record, not an authorization root and not a
JavaScript sandbox. The generator and every module it is allowed to load run
with full Node privileges.

**Known blockers owned by the offline branch.** Two findings in
`offline/scripts/03-build-frontend.sh` are being replaced wholesale by
`fix/locale-offline-authority-final` and are deliberately *not* addressed here,
so the two branches do not fight over the same file: the offline build accepts
an already-present Node by version rather than by archive checksum, and its
`npm ci` path can fall back to `npm install` without a lockfile. Until that
branch merges, the offline installer lane does not have the tool-identity or
locked-dependency properties described above; this branch only kept its
generator-launcher references compiling. Nothing in this document should be
read as a guarantee about that lane.

**One route, everywhere.** `locale-catalog:generate`, the catalog validator,
the serving gate, npm's own `prebuild`, the container build, the offline
installer build and the packaging step all start the generator through that
launcher. There is no `node .../generate.mjs` left in a production path, and a
policy test proves it by *discovering* the callers rather than listing them:
everything executable under `.github/workflows`, `frontend/package.json`,
`frontend/scripts`, `mise.toml`, `offline/scripts`, `scripts` and the
`Dockerfile`, minus the launcher, its digest table and trusted tests. A new
wrapper dropped into `frontend/scripts` is therefore covered the moment it is
committed.

Each file is then read in its own grammar rather than scanned for a path and a
command word on the same line. ESM/CJS files are comment-stripped and matched
structurally, so a static import, an `export … from`, a literal `import()`, a
`require()`, a computed `import()` and a child-process argv are all seen —
including across line breaks and around an interleaved `/* … */`. Shell files
(and Dockerfile `RUN`, package scripts, mise task `run`, and workflow `run`)
are read after joining backslash continuations, then split — quote- and
escape-aware — into the commands that actually run: `;`, `&&`, `||`, `|`,
newlines, `(…)` subshells, `{ …; }` groups and `$(…)`/backtick substitutions
each start a new segment, while a quoted `;` or `&&` stays an argument. Within
a segment the launcher mediates only when it is the program Node actually
executes: leading `NAME=value` assignments and `env`-style wrappers (with their
own options) are consumed, the executable is identified by name or path
(`node`, `nodejs`, a pinned `/usr/local/bin/node`, or an unresolved
`"${OFFLINE_NODE}"`), Node's own options are separated from its script, and the
generator entry has to be the launcher's first argument — the launcher's real
CLI. A launcher path that is merely present as data mediates nothing, so a
trailing argument, an argument after `--`, an option value Node swallows
(`--require …/generator-launcher.mjs`), an environment assignment, a
redirection target or a nested command that never runs all leave a direct
`node …/generate.mjs` rejected, as does
`node …/generator-launcher.mjs generate.mjs && node …/generate.mjs` on its
second segment; assignments carry across segments the way the shell carries
them, so `GEN=…/generate.mjs; node "$GEN"` is rejected too, and an unbalanced
quote or group around a generator mention, or a command position this reader
cannot resolve, fails closed.

Node's own command line is read with Node's semantics rather than as a list of
words to skip. `-e`/`--eval`/`-p`/`--print` (in both the spaced and
`--eval=…` spellings) *are* the program: there is no later script, so a
launcher path after one of them is argument data, and a generator named or
mentioned in the evaluated source is reported. `-r`/`--require`, `--import`
and `--loader`/`--experimental-loader` run their value before the entry
module — before the launcher exists, and therefore outside the module graph it
binds — so `node --import …/generate.mjs …/generator-launcher.mjs generate.mjs`
is rejected on the preload, and a preload is refused even when its value is
benign; production starts the generator with no Node options at all, so nothing
real is lost by not guessing. `NODE_OPTIONS` assignments before Node, including
assignments passed through `env`, are parsed through those same rules: inert
configuration such as `--enable-source-maps` remains valid, while a preload,
eval mode, unknown arity or unresolved value cannot be hidden before a trailing
launcher. Ordinary options are consumed by their real arity (`--conditions
development`, `--enable-source-maps`, any `--name=value` or `--no-*` spelling),
`--` still ends Node's options, and an option this reader does not know could
swallow the next word or not, so a generator mention around one fails closed
rather than being read as mediated. Python callers are read from the AST: a
constant or `Path` join bound only for hashing is fine, the same value reaching
`subprocess` argv — directly or through a variable — is not. Path spellings are
normalised (`./`, `..`, duplicate separators, quoting) before comparison, and a
mention the grammar cannot resolve fails closed. This is reproducibility and
centralisation over trusted code, not containment.

The synthetic fixture hashes all declared executable sources, both launcher
stages, the lockfile, and the toolchain lock through `generatorSha256`, so the
catalog revision moves with every such governed input without adding a
Python-only field to the Node-produced public manifest.

Before any of that, the sources themselves are checked for content that would
be lost silently:

* **Duplicate keys.** Every raw locale JSON file is parsed with `jsonc-parser`
  and any key declared twice in the same object — including keys that are only
  equal after unescaping — fails the build with both locations. `JSON.parse`,
  and therefore Vite and the Vue build, would simply have kept the last one.
* **Composition collisions.** Ownership is taken from the module graph, not
  from matching values: a Vite plugin boxes every JSON string leaf with the
  file it came from, so each leaf in the composed tree has exactly one owner
  even when two files declare the same short string. It is checked per
  *(file, mount, leaf)*: a file mounted twice — an alias such as
  `returnToTheForgottenAge` — must survive at every mount, and a mount reached
  by reference is recorded even when every one of its leaves was overridden.

A table section (`thead`/`tbody`/`tfoot`) carries no presentation in this
model, so any attribute on one is refused rather than dropped, and
`unsupported.detail` is truncated to the bound read from the chunk schema
itself, so the generator and the schema cannot disagree. A file whose keys were
  overridden by another spread, or whose content is not in the composed tree at
  all (a translated file no module imports), fails the build naming the file
  that won. This is how the Spanish `label` namespace and the Chinese
  *Heart of the Elders* parts were found to be missing from the Vue build.

Generation then fails — rather than publishing a partial catalog — on an unsafe
key/pack/locale identifier, a key longer than the schema's bound, a non-string
leaf, an undeclared variable, a variable whose declared role contradicts its
use, a link that cannot be resolved (missing target, unsupported target, or a
cycle — the whole link graph is resolved with fallbacks, to a fixed point, so a
conflict or a downgrade deep in the graph propagates to every ancestor that
renders through it), a required key that is missing or unsupported, a chunk or catalog
that exceeds its size/count bound, or an output path that escapes the output
directory.

## Serving

`prod.nginxconf` publishes both shapes from a single `^~ /locale-catalog/`
location as unauthenticated static JSON: `default_type application/json`,
`X-Content-Type-Options: nosniff`, ETag, `Vary: Accept-Encoding`, byte ranges,
and the same brotli/gzip static handling as `/cards/*.json`. The prefix (rather
than regex) match means an unknown catalog path returns `404` instead of
falling through to the SPA shell and answering a JSON fetch with HTML. There
are no cookies, tokens, or request-specific data involved, so nothing sensitive
can be logged or leaked by these routes.

Brotli is negotiated rather than pattern-matched. `Accept-Encoding: br;q=0`
means brotli is *not* acceptable (RFC 9110 §12.5.3), so the config requires a
positive `br` token — bare, or weighted above zero — and lets any zero-weighted
`br` veto the encoding for the whole request. Matching is case-insensitive and
tolerates the optional whitespace around list separators; a `br` token carrying
a parameter the config does not recognise, and `*`, both fall back to gzip or
identity rather than guessing. `Vary: Accept-Encoding` is stated exactly once on
every branch (`gzip_vary` is off inside the location precisely so nginx does not
add a second copy). The offline package's generated config applies the same
rules, and `scripts/validate-catalog-serving.py` drives the whole matrix — 27
header forms — against both, checking the selected encoding *and* the bytes.

## Verification

For every governed Python-backed route below, first name the trusted
installation explicitly (for example, `export
LOCALE_CATALOG_MISE_ROOT=/absolute/path/to/mise-data`; additionally export
`LOCALE_CATALOG_STACK=/absolute/path/to/stack` before
`locale-catalog:capability-probe`). This is intentionally not inferred from
`HOME` or `PATH`.

    mise run locale-catalog:test                 # render-AST, source-integrity and generator tests
    mise run locale-catalog:backend-keys-test    # the key extractor's own rules, on synthetic modules
    mise run locale-catalog:backend-keys-check   # backend emitted-key registry drift
    mise run locale-catalog:offline-cache-test   # the offline build's cache key covers every input
    mise run locale-catalog:offline-cache-hit-test  # a restored cache verifies without frontend/public
    mise run locale-catalog:validate             # schemas, digests, provenance, deploy seam
    mise run locale-catalog:capability-settings  # a real manifest configures the advertised capability
    mise run locale-catalog:capability-probe     # ... and the real backend serves it, or refuses to start
    mise run locale-catalog:serving              # real nginx: status, cache, MIME, rollout

`scripts/validate-locale-catalog.py` regenerates the catalog twice, rebuilds it
in a scratch tree whose sources are only git-tracked files (reusing the
lockfile-installed dependencies rather than reinstalling them) and requires the
same revision byte for byte, validates the manifest and every chunk against the v1 schemas,
re-derives the provenance digests from the hashed sources, proves `--check`
detects stale output, proves generation fails when a required key is removed or
a locale file is malformed, and checks the nginx/container/build wiring.
It also proves the gates have teeth by mutating the scratch clone: a duplicate
key, unsupported markup on a backend-emitted key, a link cycle, a removed
required key and malformed JSON must each fail generation, and changing the
lockfile or the published content must change the revision.

`scripts/validate-catalog-serving.py` boots real nginx over `prod.nginxconf`
and over the config the offline packager generates, and asserts the status
matrix these paths can actually produce — 200, 304, 404 and 405, plus a `Range`
request returning the whole file as a 200 (ranges are disabled, so 206 and 416
never occur) — along with JSON MIME, `nosniff`, `Vary`, gzip and brotli
negotiation (each response carrying exactly one of each header, and the brotli
body inflating to the identity payload), byte-exact payloads against the
manifest digests, that a missing catalog path is never answered with the SPA
shell, and that a rolling deploy works in both directions (new manifest against
old static root and vice versa).

`frontend/scripts/locale-catalog/verify-dist.mjs` proves the built `dist/`
really contains the catalog with matching digests and precompressed siblings.
A restored manifest is untrusted input — it names every path that is read and
every digest that is compared — so:

* both manifests, and every chunk, are validated against the published v1
  schemas, parsed into null-prototype objects with `__proto__` refused, and
  checked with own-property lookups throughout, so `constructor`/`toString`
  cannot pose as declared fields. The evaluator is hand-written — this script
  runs at deploy seams where `node_modules/` does not exist yet, so it cannot
  import a validator — and it is held honest by a closed keyword set: each
  schema document is audited when it is loaded, and any assertion keyword the
  evaluator does not implement stops the run instead of being ignored. That is
  what makes `propertyNames` binding, which is how entry keys are held to the
  published message-key grammar;
* locale records are checked as a resolution graph: each locale appears exactly
  once (a locale split across two records would leave a client reading a slice
  that only looks complete), the default locale appears once and is the only one
  without a fallback, and every fallback and language-resolution target is a
  published locale. Self-references, cycles and chains that never reach the
  default are refused, as is a language tag that resolves two ways;
* the manifest must describe **the route it is served at**: `basePath`
  `/locale-catalog`, `manifestPath` `/locale-catalog/manifest.json`,
  `chunkPathPrefix` `/locale-catalog/c/` (all `const` in the schema), and a
  `revisionManifestPath` that its own `catalogRevision` derives;
* every chunk path is exactly `/locale-catalog/c/<sha256>.json` and the name
  must be the digest the descriptor promises;
* the supplied `--dist` root is canonicalized once, then every directory from
  it down to each artifact is opened `O_NOFOLLOW|O_DIRECTORY` and **kept open**
  with its `(dev, ino)` recorded; at the end every pin is re-checked, so a
  directory swapped for a symlink *after* it was validated is caught rather
  than followed;
* the manifest is checked against *itself* before anything is read: `(locale,
  pack)` pairs and content paths must be unique, a digest may name only one
  path, and every per-locale and global key/chunk/byte total is recomputed from
  the unique locale records and must match exactly — a manifest cannot attest
  small totals and then list the same 8 MiB chunk four thousand times;
* the filesystem is enumerated with a budget and stops the moment it exceeds
  what a valid catalog could hold, so a directory flooded with unlisted files
  is refused without building an array of them;
* every artifact is opened `O_NOFOLLOW`, then `fstat`ed and read through that
  same descriptor — regular file, `nlink == 1`, size within an explicit ceiling
  **before** a byte is allocated, exact EOF, and the same inode and size
  afterwards — so a symlink, a hard link, a sparse or multi-gigabyte file, or a
  file that grew or was truncated mid-read cannot substitute bytes or exhaust
  memory. The manifest's own totals and descriptor count are bounded too, in
  the schema and again here;
* after every read, and after a deterministic hook the tests use to swap a
  verified leaf or an intermediate directory, every artifact is re-read and
  compared with the bytes that were hashed, and every directory pin is
  re-checked;
* `--publish` closes the last gap between "these bytes were correct when I read
  them" and "these are the bytes that will be served": the catalog is rewritten
  from the verified buffers into a fresh mode-0700 directory and moved into
  place with `rename`. **Every path that ends up serving bytes uses it** — the
  offline fresh build, the offline cache hit, the offline packager (verifying
  the tree in its final destination, which is also what makes `--skip-frontend`
  unable to package an unverified catalog), and the Docker frontend stage before
  its `dist` is copied into the runtime image. Publication keeps a rollback copy
  where the filesystem allows one and falls back to replace-in-place on a
  layered filesystem, where renaming a lower-layer directory is `EXDEV`;
* the manifest and each chunk are checked against each other, not just against
  their digests: one content path belongs to exactly one descriptor, and every
  chunk is validated against the closed v1 chunk schema and must agree with its
  descriptor on locale, fallback, pack, entry count, unsupported count and byte
  count. Manifest totals — locales, keys, chunks, bytes, unsupported keys — are
  all recomputed from the descriptors;
* the tree is enumerated by streaming `opendirSync`, counting every entry
  (directories included) as it arrives, against a fixed topology: the manifest
  and exactly `c/` and `r/<revision>/` at bounded depth. A directory that is
  broader, deeper or differently shaped than a catalog is refused before it is
  walked, so a flood costs one `readdir` step rather than an array;
* `O_NOFOLLOW` and `O_DIRECTORY` are required to exist, since `undefined | 0`
  would silently turn a no-follow open into an ordinary one;
* the identity/`.gz`/`.br` artifact sets are compared with the manifest in both
  directions, so nothing unlisted can sit where nginx would serve it;
the offline build runs the same check against its own output — before both of
its cache-hit returns — and hashes every catalog provenance input into its
cache key, so a stale `_deps/frontend` can never be reused.
`offline/scripts/test-frontend-cache-hash.sh` runs that production hash
function against a synthetic tree and mutates each input class in turn,
including the cases where an input is missing and the hash must fail rather
than quietly hash nothing. A cache restores `offline/_deps` and not
`frontend/public`, so the cache-hit path verifies the restored output against
its *own* manifest (`verify-dist.mjs --dist-only`) and treats a failure as a
cache miss to rebuild from, never as a reason to delete a usable output;
`offline/scripts/test-frontend-cache-hit.sh` drives those branches with
`frontend/public/locale-catalog` absent. That check also decompresses every
`.gz`/`.br` sibling and compares it byte-for-byte with the JSON beside it,
under a size ceiling, and refuses any compressed artifact the manifest does not
list: nginx serves those bytes without ever reading the identity file, so a
cache that restored a stale, corrupt or oversized sibling would otherwise be
served as if it were the catalog. All of it runs in the `Locale catalog` GitHub Actions
workflow.

## Ownership

The catalog format is **frontend-owned** at v1: it is generated from frontend
sources by a frontend build step, and its schemas live under
`frontend/schemas/locale-catalog/v1/`. It is deliberately *not* in
`contracts/manifest.json`, because governed contract artifacts there are bound
to the backend's real Aeson encoders
(`backend/arkham-api/tests/Arkham/Api/JsonContractsSpec.hs`) and nothing in
this catalog is produced by the backend.

The binding to the contract is still machine-verifiable in both directions:
the catalog's required key set is extracted from
`contracts/fixtures/question-read.json` and
`contracts/fixtures/question-read-with-cards.json`, and the manifest records
those fixtures' digests plus `contracts/manifest.json`'s `schemaRevision`.

## Advertising the catalog

A catalog nobody can find is not useful to a native client, and a client that
has to guess `arkhamhorror.app` is not a client of *this* deployment. Contract
revision `0.1.23` therefore adds an optional `localeCatalog` object and the
`i18n.locale-catalog.v1` identifier to `GET /api/v1/capabilities`
([`contracts/README.md`](../contracts/README.md#locale-catalog-discovery)).
The backend still never holds, proxies or re-encodes catalog content: it
publishes six values that describe where *this* server's catalog is and what it
must hash to.

It is deployment configuration, so a hosted and a self-hosted install can
differ without recompiling anything
(`backend/arkham-api/config/settings.yml`):

| Setting | Environment variable | Example |
| --- | --- | --- |
| `locale-catalog-manifest-url` | `ARKHAM_LOCALE_CATALOG_MANIFEST_URL` | `/locale-catalog/manifest.json` |
| `locale-catalog-revision` | `ARKHAM_LOCALE_CATALOG_REVISION` | the manifest's `catalogRevision` |
| `locale-catalog-schema-version` | `ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION` | the manifest's `schemaVersion`, `1.0.0` |
| `locale-catalog-default-locale` | `ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE` | the manifest's `defaultLocale`, `en` |
| `locale-catalog-locales` | `ARKHAM_LOCALE_CATALOG_LOCALES` | the manifest's `locales[].locale`, comma-separated |
| `locale-catalog-manifest-sha256` | `ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256` | `sha256` of the bytes served at the manifest URL |

Every one of them blank (the default) means the deployment publishes no
pointer, and the capabilities response keeps its exact pre-`0.1.23` field and
capability shape — no object, no identifier, every other field unchanged. It is
not byte-identical: `schemaRevision` reports `0.1.23`, because it identifies the
server's contract bundle rather than this optional feature, and clients compare
its numeric components rather than the string. Supplying some but not all of them is a startup
error rather than a silent fallback to that legacy shape, so a half-removed
pointer cannot quietly disappear from a running deployment.

**The manifest URL is bound deliberately, not parsed permissively.** The
preferred value is the same-origin absolute path this catalog already
publishes; it needs no hostname, so the same configuration works for every
deployment of this server. An absolute URL is accepted only for a split/static
deployment and only over `https`. `http`, scheme-relative `//host/…`,
credentials in the authority, a query, a fragment, percent-escapes, `.`/`..`
segments, backslashes, platform paths such as `C:\…`, control characters,
non-ASCII, and any path not ending in `.json` are all refused at startup —
this is not a generic URL parser being strict, it is an allow-list of the two
spellings the contract defines, normalized to one form each (lowercase scheme
and host, redundant `:443` dropped). Startup also rejects an unsupported
`schemaVersion`, a malformed `catalogRevision`, a revision that does not belong
to the configured schema version, a digest that is not 64 lowercase hex
characters, an invalid or duplicated locale tag (compared *after*
canonicalization, so `en-us` and `en-US` collide), and a default locale that is
not in the supported list.

Values are read as ASCII: a setting holding any non-ASCII character is refused
on its raw spelling, *before* any trimming, so a value wrapped in a no-break,
thin or ideographic space, or in a byte-order mark, fails startup instead of
being stripped down to a valid remainder and published as something the
operator did not type. Only an ASCII space or tab is trimmed — around the whole
value and around each entry in the locale list — and any other control
character is refused. (`Data.Yaml.Config` re-parses each substituted value as a
YAML scalar, which removes a trailing line ending and a leading byte-order mark
before the settings parser ever sees them; the parser refuses those too, so the
guarantee is the parser's rather than the layer's.)

Diagnostics name the setting and the environment variable but never echo the
manifest URL, because a mistyped URL is exactly where credentials end up.

**The host must mean the same thing to every client.** An absolute URL's host
is accepted only as canonical dotted-decimal IPv4 — four decimal octets
`0-255`, no leading-zero padding — or as a registered name whose final label is
neither all decimal digits nor an `0x` hex literal. That last rule is WHATWG's
"ends in a number" test, and it is not pedantry: `127.1`, `2130706433`,
`0x7f000001` and `01.02.03.04` are every one of them `127.0.0.1` to a browser
and to `inet_aton`, while a stricter RFC 3986 reader rejects them, and
`999.999`, `256.0.0.1` or `1.2.3.4.5` are read differently again. A pointer
that resolves to different places depending on who fetched it is precisely
what this contract must not publish, so a bracketed address literal (`[::1]`)
is refused outright rather than normalized. `contracts/manifest.json`'s
`manifestUrlChecks` is the one governed table both the JSON Schema and the
Haskell validator are held to.

## Contract fixture versus production catalog

Two different artifacts describe a catalog, and conflating them is the mistake
this section exists to prevent.

`contracts/fixtures/locale-catalog-manifest.json` is a **synthetic** v1
manifest: a small, fixed, committed test artifact with three locales and no
narrative text. Nothing in it is asserted — every digest, count, path,
`outputSha256` and provenance value is *computed* from committed sibling
authorities (`locale-catalog-source-*.json`, `-backend-registry.json`,
`-chunk-*.json`) by `scripts/build-locale-catalog-fixture.py`, whose `--check`
mode rebuilds the whole set and whose self-tests mutate each claimed field to
prove the check bites. `generatorSha256` and `schemasSha256` are digests of the
real inputs — the generator's whole `scripts/` import closure (computed from
each module's AST, so `strict_json` is covered and a later helper cannot be
omitted; the boundary is fail-closed — symlinks, relative imports, packages or
repo-local modules outside `scripts/`, dynamic `importlib`/`__import__`,
`exec`/`eval`, `sys.path` machinery and any import it cannot classify as
generator-local, standard-library or a pinned dependency are all refusals, each
proven by injecting it into the real sources' own AST) and the published v1 schemas — so editing, adding, dropping or
renaming any of them moves the synthetic revision exactly as it would in
production, and every value those
schemas already pin as a `const` is read from them rather than re-typed. It is never generated from `frontend/src/locales/**`. The governed
capabilities fixture derives every advertised value from its exact bytes, which
is what lets the published contract stay still while catalog content moves: a
translator fixing a typo changes the production catalog's revision and digest
and touches no governed byte.

The catalog under `frontend/public/locale-catalog/` is the **production**
artifact. Its revision and digest are runtime configuration, read by an
operator (or a deploy script) out of the manifest the build just produced, and
they are deliberately absent from `contracts/`. `mise run
locale-catalog:capability-settings` closes the loop in CI: it derives the six
settings from the real generated manifest, requires the `localeCatalog` object
and the whole capabilities response they produce to satisfy the governed
schema, and asserts the generated manifest is *not* the synthetic one — so the
fixture can never quietly become a copy of production output, and the synthetic
shape can never drift into something no deployment can produce.

`mise run locale-catalog:capability-probe` closes it against the real backend
rather than a model of it. `backend/arkham-api/app-capabilities-probe` loads
settings through the same `loadYamlSettings` call `Application.appMain` uses,
builds the response with the handler's own `capabilitiesResponse`, and prints
`Data.Aeson.encode`'s bytes — the production `toEncoding` path, with nothing
appended, not even a newline. The driver asserts those **exact bytes** for both
the advertised and the disabled response before decoding anything, validates
them against the governed schema and the generated manifest's metadata,
exercises runtime settings files on the command line (a literal wins over the
environment; an `_env:` marker lets the environment through; a partial file is
merged over the compile-time value; an insecure literal cannot be rescued), and
then corrupts each setting in turn (insecure and ambiguous
URLs, a malformed revision, a mismatched schema version, an invalid, duplicate
or unsupported locale, a malformed digest, a value YAML reads as a number)
requiring the server to refuse to start every time.

Because generation is deterministic, the values are derivable from the
manifest the deployment just built:

```sh
ARKHAM_LOCALE_CATALOG_REVISION=$(jq -r .catalogRevision manifest.json)
ARKHAM_LOCALE_CATALOG_SCHEMA_VERSION=$(jq -r .schemaVersion manifest.json)
ARKHAM_LOCALE_CATALOG_DEFAULT_LOCALE=$(jq -r .defaultLocale manifest.json)
ARKHAM_LOCALE_CATALOG_LOCALES=$(jq -r '[.locales[].locale] | join(",")' manifest.json)
ARKHAM_LOCALE_CATALOG_MANIFEST_SHA256=$(shasum -a 256 manifest.json | cut -d' ' -f1)
```

The manifest served at `manifestPath` is byte-identical to the one at
`revisionManifestPath`, so pinning `sha256(manifest bytes)` is well defined,
and every chunk digest hangs off it. A deployment that rolls the catalog
forward must update the revision and digest together with the static root;
until it does, a client verifying the digest correctly rejects the new
manifest instead of trusting it.
