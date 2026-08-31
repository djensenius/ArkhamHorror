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
`unsupported`.

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

`frontend/scripts/locale-catalog/generate.mjs` runs during `npm run build`
(npm `prebuild`, alongside `slim-cards`) and writes
`frontend/public/locale-catalog/`, which Vite copies into `dist/` and
`scripts/precompress.cjs` gives `.gz`/`.br` siblings. The output is generated,
so it is git-ignored — the committed artifacts are the generator, the schemas,
and the tests.

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

## Verification

    mise run locale-catalog:test                 # render-AST, source-integrity and generator tests
    mise run locale-catalog:backend-keys-test    # the key extractor's own rules, on synthetic modules
    mise run locale-catalog:backend-keys-check   # backend emitted-key registry drift
    mise run locale-catalog:offline-cache-test   # the offline build's cache key covers every input
    mise run locale-catalog:offline-cache-hit-test  # a restored cache verifies without frontend/public
    mise run locale-catalog:validate             # schemas, digests, provenance, deploy seam
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

* both manifests are validated against the published v1 schema, parsed into
  null-prototype objects with `__proto__` refused, and checked with own-property
  lookups throughout, so `constructor`/`toString` cannot pose as declared
  fields;
* the manifest must describe **the route it is served at**: `basePath`
  `/locale-catalog`, `manifestPath` `/locale-catalog/manifest.json`,
  `chunkPathPrefix` `/locale-catalog/c/` (all `const` in the schema), and a
  `revisionManifestPath` that its own `catalogRevision` derives;
* every chunk path is exactly `/locale-catalog/c/<sha256>.json` and the name
  must be the digest the descriptor promises;
* every path component is `lstat`ed rather than followed, the catalog root
  included, and every artifact is opened `O_NOFOLLOW` then `fstat`ed and read
  through that same descriptor — regular file, `nlink == 1` — so neither a
  symlink, a hard link, nor a path swapped between the check and the read can
  substitute bytes;
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
this catalog is produced by the backend. `contracts/` revision `0.1.22` is
untouched by this change.

The binding to the contract is still machine-verifiable in both directions:
the catalog's required key set is extracted from
`contracts/fixtures/question-read.json` and
`contracts/fixtures/question-read-with-cards.json`, and the manifest records
those fixtures' digests plus `contracts/manifest.json`'s `schemaRevision`.

A backend capability advertising this catalog (issue #53) needs exactly two
values, both available without guessing: the stable manifest URL
(`manifestPath`, `/locale-catalog/manifest.json`) and `catalogRevision`. The
manifest served at that URL is byte-identical to the one at
`revisionManifestPath`, so pinning `sha256(manifest bytes)` is well defined,
and every chunk digest hangs off it.
