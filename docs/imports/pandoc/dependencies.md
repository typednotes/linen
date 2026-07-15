# `pandoc` module dependencies

Topological order of the **in-scope** modules of the
[`pandoc`](https://hackage.haskell.org/package/pandoc) universal document
converter (v3.10, source:
https://hackage-content.haskell.org/package/pandoc-3.10/src/pandoc.cabal — the
real `exposed-modules`/`other-modules`/`build-depends` fields were fetched and
read verbatim, not recalled from memory), together with the AST it is built on,
the separate [`pandoc-types`](https://hackage.haskell.org/package/pandoc-types)
package (v1.23.1,
https://hackage-content.haskell.org/package/pandoc-types-1.23.1/src/pandoc-types.cabal),
planned for import into `linen` per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention.

**Status: done.** All 34 in-scope modules are ported as `Linen.Text.Pandoc.*`
(and the top-level `Linen.Text.Pandoc` facade), each with a `Tests/`
counterpart, building cleanly under `lake build Linen Tests`. The `doclayout`
blocking prerequisite (#82) was imported first, per the plan below; the
`tagsoup` HTML-tokenizer and bounded-YAML-front-matter prerequisites were
resolved as bounded inline folds directly inside `Readers/HTML.lean` and
`Readers/Markdown.lean` respectively (the same single-consumer treatment
`Emoji.lean`/`MIME.lean` already gave their own folded-in subsets), not as
separate `docs/imports/` entries.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. A representative sample of module import lists was fetched from the
`pandoc-3.10` / `pandoc-types-1.23.1` source trees to fix the layering: the
`pandoc-types.cabal` module set (the whole AST — `Definition`/`Builder`/`Walk`/
`Generic`/`JSON`), `Text.Pandoc.Shared` (→ `Definition`, `Builder`, `Walk`,
`Extensions`, `Asciify`, plus the external `doclayout`/`tagsoup`/`emojis`/
`commonmark`/`zip-archive`), `Text.Pandoc.Class.PandocMonad` (→
`Class.CommonState`, `Definition`, `Error`, `Logging`, `MIME`, `MediaBag`,
`Shared`, `URI`, `Walk`, `UTF8`), `Text.Pandoc.Readers.Markdown` (→ `Parsing`,
`Options`, `Shared`, `Definition`, `Builder`, `Class.PandocMonad`, `Logging`,
`Emoji`, `Walk`, `Readers.HTML`, `Readers.LaTeX`, `Readers.Metadata`, `URI`,
`XML`), and `Text.Pandoc.Writers.HTML` (→ `Definition`, `Options`, `Shared`,
`Walk`, `Writers.Shared`, `Writers.Math`, `Writers.Blaze`, `Templates`, `XML`,
`Class.PandocMonad`, `Class.PandocPure`, `Error`, `Logging`, `MIME`,
`Translations`, plus external `blaze-html`/`doclayout`/`doctemplates`) were all
fetched and quoted. The remaining intra-tier edges are **inferred from
pandoc's directory structure and module-naming convention, not individually
verified** (flagged inline where relevant), consistent with the time-box on
this planning step.

## Headline finding: a blocking not-yet-ported prerequisite (`doclayout`), and a very large deferral

Unlike `hedis` (#80) and `streamly` (#81), `pandoc` is **not** self-contained
against what `linen` already has. Two facts dominate the plan:

1. **The AST lives in a separate package.** `Text.Pandoc.Definition`,
   `.Builder`, `.Walk`, `.Generic`, `.JSON` are *not* in the `pandoc`
   package — they come from `pandoc-types` (confirmed by reading both cabal
   files). This document folds `pandoc-types`'s 5 library modules in as the
   foundation tier (the same "raw dependency folded into the one wrapper that
   uses it" treatment `sqlite-simple` gives `direct-sqlite` and `Hasql` gives
   `postgresql-libpq`), because the AST *is* the point of the import and has no
   other consumer in `linen`.

2. **Every writer is built on `doclayout`** (`Text.DocLayout`, the `Doc`
   pretty-printing algebra — confirmed: `Shared`, `Writers.Shared`,
   `Writers.HTML`, `Writers.Markdown` all import it). `doclayout` is a small,
   self-contained Wadler/Leijen-style pretty-printer with no existing `linen`
   equivalent (`Linen.Data.PDF.*` and the ad hoc `ShowS`-style builders are
   not it). It is a genuine blocking prerequisite the way `profunctors` (#77)
   was for `lens` — **it must be imported first, as its own
   `docs/imports/doclayout/` entry (#82; this pandoc entry is #83)**, before
   pandoc's writers can be ported.
   This plan flags it rather than folding it in, because unlike `pandoc-types`
   it is a general-purpose library likely to gain other consumers.

`pandoc` is also **by far the largest package in this index** (v3.10 exposes
~40 readers, ~44 writers, plus the App/CLI, Lua-scripting/filter, PDF, and
citeproc subsystems). Following the exact scoping discipline used for
`streamly` (scoped to `streamly-core`, deferring the concurrency layer) and
`hedis` (RESP2 only), this import is scoped to a **focused, high-value core**:
the AST, the shared reader/writer infrastructure, and the two most central
formats — **Markdown and HTML** — for both reading and writing, plus the
AST-native `Native`/`JSON` round-trip. The long tail of exotic formats, the
binary/zip formats, the Lua/filter/scripting system, syntax highlighting,
citeproc, math typesetting, and the CLI are all **deferred** (see the Scope
note). This mirrors how `duckdb-ffi` scoped down from 44 upstream modules to
the 18 actually used, and `hip` excluded `Graphics.Image.IO.Histogram`.

## Namespace decision

Kept as upstream's own `Text.Pandoc.*` hierarchy, re-rooted as
**`Linen.Text.Pandoc.*`**. `Pandoc` is a **proper-noun tool name** (like
`Redis`, `SQLite`, `DuckDB`), not Haskell/GHC branding — so AGENTS.md's
Lean-ify rule does *not* require renaming it (the same reasoning `hedis`'s
`dependencies.md` gives for keeping `Redis`/`PubSub`/`Sentinel`, and `lens`'s
for keeping `Lens`/`Prism`/`Iso`; contrast `WaiAppStatic` → `WebApp.Static`,
which *is* a package-name rename). The `Text.` prefix maps cleanly to
`Linen.Text.` — `linen` already roots text-domain modules there
(`Linen.Data.Text.*`, and the broader `Text` Hackage port at index #39), and
keeping `Linen.Text.Pandoc.*` mirrors upstream's whole `Text.Pandoc` tree
without collision. The folded-in `pandoc-types` AST modules take the same
root: `Text.Pandoc.Definition` → `Linen.Text.Pandoc.Definition`, etc.

## External dependencies

Resolved against `pandoc-3.10.cabal`'s and `pandoc-types-1.23.1.cabal`'s
library `build-depends`, in Hackage-import precedence order (Lean stdlib >
existing `linen` Haskell port > new Hackage import > raw port).

### Blocking not-yet-ported prerequisite (import first, own entry)

- **`doclayout`** (`Text.DocLayout`) — the `Doc` pretty-printing algebra every
  writer renders through (`render`, `literal`, `charWidth`, `Doc`). No `linen`
  equivalent; small and self-contained; general-purpose. **Must be ported
  first as `docs/imports/doclayout/dependencies.md` (its own index entry, #82;
  this pandoc entry is #83)**, the same way `profunctors`/`indexed-traversable`
  were planned before `lens`.

### Already ported / covered by the Lean stdlib, reused as-is

- `base`, `bytestring`, `containers`, `mtl`, `text`, `time`, `vector`,
  `transformers`, `deepseq` → `Base`, `ByteString`, `Containers`, `Mtl`,
  `Text`, `Time`, `Vector`, Lean's `ExceptT`/`ReaderT`/`StateT`, and (for
  `deepseq`) nothing — Lean is eager (same call `hip`/`hedis` make).
- `aeson`, `aeson-pretty` → `Linen.Data.Json` (`Text.Pandoc.JSON` and every
  `ToJSON`/`FromJSON` AST instance; `.JSON` is the AST↔JSON bridge).
- `scientific` → `Linen.Data.Scientific`.
- `case-insensitive` → `Linen.Data.CaseInsensitive`.
- `mime-types` → `Linen.Network.Mime` / the `MimeTypes` port (#21); backs
  `Text.Pandoc.MIME`.
- `network-uri`, `network`, `http-types` → `network-uri` (#57), `Network`
  (#23), `HttpTypes` (#18); back `Text.Pandoc.URI` and the (deferred) remote
  resource fetcher.
- `data-default` → `Linen.Data.Default` (`DataDefault`, #12); `WriterOptions`/
  `ReaderOptions` defaults.
- `parsec` → `Std.Internal.Parsec` — `Text.Pandoc.Parsing` (and the Markdown/
  HTML readers) are parsec-based; the same substitution `netpbm`/`JuicyPixels`/
  `hedis` made for byte/char parsers.
- `random`, `filepath`, `directory`, `unix`, `process`, `temporary` →
  `Init.Data.Random`, `System.FilePath`, `IO.FS.*`, `IO.Process`,
  `IO.FS.createTempFile` — needed only by deferred IO/App/PDF paths.

### Substituted with an existing `linen` module / narrow inline (precedence rule)

- **`blaze-html` / `blaze-markup`** → **`Linen.Web.Html`** (+ `Linen.Web.Css`).
  `Writers.HTML` renders through `Text.Blaze.XHtml5`/`Text.Blaze.Html`; `linen`
  already has a typed HTML-construction library (`Linen.Web.Html`, a blaze-like
  smart-constructor tree with escaping — read verbatim). Per the precedence
  rule (an existing `linen` module outranks a fresh package import),
  `Writers.HTML`/`Writers.Blaze` target `Linen.Web.Html` rather than importing
  `blaze-*`. Where pandoc needs Blaze features `Linen.Web.Html` lacks (raw
  custom leaves/parents, XHtml1-Transitional variant), a thin
  `Writers.Blaze` shim adds them over the existing module.
- **`safe`** — used for a couple of total `head`/`read` helpers (`safeRead`,
  `headMay`); inlined as one-liners (same treatment `hedis`'s `errors` note).
- **`split`** — `splitOn`/`chunksOf` list helpers; `Linen.Data.List` covers
  these or they inline trivially.
- **`emojis`** (`Text.Emoji`) — the emoji-shortcode→char table behind
  `Text.Pandoc.Emoji`. A single data table; ported inline as
  `Linen.Text.Pandoc.Emoji`'s backing map rather than a separate package
  (bounded, no other consumer).
- **`exceptions`** (`Control.Monad.Catch`) — `Class.PandocIO`'s
  `bracket`/`catch`; ported against `Linen.Control.Exception` (the identical
  call `hedis`/`hoauth2`/`lens`/`streamly` each made) — and mostly in the
  deferred `PandocIO`/`Sandbox` layer anyway.
- **`base64-bytestring`** → `Linen.Data.Base64` (`Base64`, #5); data-URI
  handling in `URI`/`SelfContained`.

### New Hackage prerequisites for the in-scope readers (import first or bounded slice)

- **`tagsoup`** (`Text.HTML.TagSoup`) — the permissive HTML tokenizer the
  **HTML reader** (`Readers.HTML.Parsing`) and `Shared` (`renderTags`) are
  built on. `linen`'s `Linen.Web.Html` is HTML *construction*, not parsing —
  there is no existing tag parser. `tagsoup` is small and self-contained;
  planned as its own bounded prerequisite import (or, if scoped tightly, the
  tokenizer + `Tag`/`renderTags` slice `pandoc` actually uses). Flagged, not
  yet resolved to an index entry.
- **`yaml` / `libyaml`** — YAML front-matter parsing in
  `Readers.Metadata` (`yamlBsToMeta`), which the **Markdown reader** imports
  for `---`-delimited metadata blocks. `linen` has no YAML parser. Planned as a
  bounded YAML-subset parser over `Std.Internal.Parsec` (front-matter is a
  small, well-bounded YAML subset), not the full `libyaml` C binding; flagged.

### Deferred external packages (tied to deferred format subtrees — see Scope note)

`commonmark`/`commonmark-extensions`/`commonmark-pandoc` (the separate
CommonMark reader; the Markdown reader only borrows `HasAttributes`/`Cm` for
`addPandocAttributes`, a bounded slice), `skylighting`/`skylighting-core`
(syntax highlighting — `Highlighting`), `texmath` (`Writers.Math`, TeX↔MathML —
in-scope writers degrade to raw/MathML passthrough), `citeproc` (citations),
`doctemplates` (templating — in-scope writers run template-free,
`writerTemplate = Nothing`), `zip-archive`/`xml-conduit`/`xml-types`/`xml`
(the DOCX/ODT/EPUB/Pptx/Xlsx binary formats), `typst`, `djot`,
`jira-wiki-markup`, `haddock-library`, `gridtables`, `asciidoc`, `ipynb`,
`unicode-collation`/`unicode-data`/`unicode-transforms`,
`http-client`/`http-client-tls`/`crypton*`/`tls` (remote-resource fetching),
`JuicyPixels` (already ported at #71, but only the deferred `ImageSize` path
uses it), `pretty-show`, `text-conversions`, `attoparsec`, `binary`, `Glob`.

### Dropped outright (GHC-toolchain / metaprogramming, no Lean analogue)

- **`syb`** (Scrap-Your-Boilerplate generics) — backs
  `Text.Pandoc.Generic`'s `bottomUp`/`topDown` generic AST traversals.
  Lean has no `Data.Data`/`Typeable`-driven generic programming; the port
  reimplements `Text.Pandoc.Generic` directly over `Text.Pandoc.Walk`'s
  typed traversal (same category as `lens`'s dropped `Data.Data.Lens`).
- **`ghc-prim`, `template-haskell`** — GHC primops / Template Haskell; no Lean
  analogue (same treatment as `streamly`/`lens`).
- **`file-embed`** — compile-time file embedding for `Text.Pandoc.Data`'s
  bundled templates/reference docs (deferred subtree).
- **`QuickCheck`** — `Text.Pandoc.Arbitrary` (property-test `Arbitrary`
  instances for the AST); testing-only, dropped — `linen` tests use `#guard`.
- **`semigroups`** — GHC-`<8.0` compatibility shim; dead code for one pinned
  Lean toolchain (same call `hedis` made).

## In-scope topologically sorted modules

Scoped to the AST + shared infrastructure + Markdown/HTML/Native/JSON formats.
Tiers are in build order; within a tier, order is not load-bearing.

### Tier 0 — the document AST (folded-in `pandoc-types`, no internal deps)

1. `Text.Pandoc.Definition` → `Linen.Text.Pandoc.Definition` — the AST:
   `Pandoc`, `Meta`/`MetaValue`, `Block`, `Inline`, `Format`, `Attr`,
   `Citation`, `ListNumberStyle`, etc. Depends only on external
   `aeson`/`containers`/`text` (→ `Json`/`Containers`/`Text`). The crown jewel.
2. `Text.Pandoc.Walk` → `Linen.Text.Pandoc.Walk` — typed generic traversal
   (`walk`, `walkM`, `query`) over the AST. Depends on #1.
3. `Text.Pandoc.Generic` → `Linen.Text.Pandoc.Generic` — `bottomUp`/`topDown`
   generic transforms; reimplemented over #2 rather than `syb` (see drop note).
   Depends on #1, #2.
4. `Text.Pandoc.Builder` → `Linen.Text.Pandoc.Builder` — the monoidal
   `Blocks`/`Inlines` builder DSL every reader emits into. Depends on #1
   (confirmed: `Shared` imports `Builder (Blocks, Inlines, ToMetaValue)`).
5. `Text.Pandoc.JSON` → `Linen.Text.Pandoc.JSON` — the AST↔JSON bridge and
   `ToJSONFilter` helper. Depends on #1 (over `Linen.Data.Json`).

### Tier 1 — leaf infrastructure (enums, options, escaping; minimal deps)

6. `Text.Pandoc.Extensions` → `Linen.Text.Pandoc.Extensions` — the
   `Extension`/`Extensions` format-feature flag set. No internal deps
   (confirmed: `Shared` imports it standalone).
7. `Text.Pandoc.Options` → `Linen.Text.Pandoc.Options` — `ReaderOptions`/
   `WriterOptions`/`HTMLMathMethod`/`ReferenceLocation`. Depends on #6.
8. `Text.Pandoc.Error` → `Linen.Text.Pandoc.Error` — the `PandocError` sum.
   Depends on #1 (inferred; small).
9. `Text.Pandoc.Logging` → `Linen.Text.Pandoc.Logging` — `LogMessage`/
   `Verbosity`. Depends on #1 (inferred).
10. `Text.Pandoc.UTF8` → `Linen.Text.Pandoc.UTF8` — UTF-8 (de)coding helpers
    over `ByteString`/`Text`. No internal deps.
11. `Text.Pandoc.MIME` → `Linen.Text.Pandoc.MIME` — `getMimeType`/
    `mediaCategory` over the `MimeTypes` port. No internal deps.
12. `Text.Pandoc.URI` → `Linen.Text.Pandoc.URI` — `escapeURI`/`isURI`/
    `urlEncode`/`pBase64DataURI` over `network-uri`. No internal deps.
13. `Text.Pandoc.Asciify` → `Linen.Text.Pandoc.Asciify` — `toAsciiText`
    (diacritic stripping). No internal deps (confirmed: imported by `Shared`).
14. `Text.Pandoc.Emoji` → `Linen.Text.Pandoc.Emoji` — shortcode↔emoji table
    (folds in the `emojis` data table). No internal deps.
15. `Text.Pandoc.XML` → `Linen.Text.Pandoc.XML` — XML entity escaping/
    unescaping (`escapeStringForXML`, `fromEntities`, `toEntities`,
    HTML4/5/RDFa attribute tables). No internal deps.

### Tier 2 — sources, media, and the shared/monad layer

16. `Text.Pandoc.Sources` → `Linen.Text.Pandoc.Sources` — the multi-source
    parser input (`Sources`, `ToSources`) the parsec readers stream over.
    Depends on #12 (inferred).
17. `Text.Pandoc.MediaBag` → `Linen.Text.Pandoc.MediaBag` — the in-memory
    `MediaBag` of embedded images/resources. Depends on #11 (confirmed:
    `PandocMonad` imports `MediaBag`, `MIME`).
18. `Text.Pandoc.Shared` → `Linen.Text.Pandoc.Shared` — the grab-bag of
    reader/writer helpers (`stringify`, `normalizeSpaces`, `tshow`,
    `safeRead`, `blocksToInlines`, …). Depends on #1, #4, #2, #6, #13 (all
    confirmed via direct fetch); over external `doclayout` (charWidth) and the
    `tagsoup` renderer.
19. `Text.Pandoc.Translations` (+ `.Types`) → `Linen.Text.Pandoc.Translations`
    — localized UI terms (`Term`, `translateTerm`). Depends on #1, #8
    (inferred).
20. `Text.Pandoc.Class.CommonState` → `Linen.Text.Pandoc.Class.CommonState` —
    the `CommonState` record (verbosity, media bag, resource path, log). Depends
    on #9, #17 (confirmed: imported by `PandocMonad`).
21. `Text.Pandoc.Class.PandocMonad` → `Linen.Text.Pandoc.Class.PandocMonad` —
    the `PandocMonad` typeclass (`report`, `fetchItem`, `getTimestamp`, …).
    Depends on #20, #1, #8, #9, #11, #17, #18, #12, #2, #10 (confirmed via
    direct fetch).
22. `Text.Pandoc.Class.PandocPure` → `Linen.Text.Pandoc.Class.PandocPure` —
    the pure (`State`-only, no `IO`) `PandocMonad` instance (`runPure`).
    Depends on #20, #21 (inferred). **Only the pure instance is in scope;
    `Class.PandocIO`/`Class.IO`/`Class.Sandbox` are deferred (see Scope note).**
23. `Text.Pandoc.Parsing` (+ its `.General`/`.Types`/… submodules) →
    `Linen.Text.Pandoc.Parsing` — the shared parsec toolkit the readers build
    on (`ParserState`, `many1Char`, `emphasis`, table/list primitives). Depends
    on #1, #4, #7, #16, #18, #6, #12 (confirmed: imported by Markdown reader
    over `Std.Internal.Parsec`).

### Tier 3 — writer-shared infrastructure

24. `Text.Pandoc.Templates` → `Linen.Text.Pandoc.Templates` — template
    loading/rendering; **scoped to the template-free path** (`renderTemplate`
    with `Nothing`), deferring the full `doctemplates` engine. Depends on #7,
    #8, #21 (inferred).
25. `Text.Pandoc.Writers.Shared` → `Linen.Text.Pandoc.Writers.Shared` —
    writer helpers (`metaToContext`, `defField`, `gridTable`, layout combinators
    over `doclayout`). Depends on #1, #4, #7, #18, #2, #24 (inferred; confirmed
    imported by `Writers.HTML`).
26. `Text.Pandoc.Writers.Math` → `Linen.Text.Pandoc.Writers.Math` — math
    rendering; **scoped to raw/MathML passthrough**, deferring the `texmath`
    TeX→MathML engine. Depends on #7, #18 (inferred).
27. `Text.Pandoc.Writers.Blaze` → `Linen.Text.Pandoc.Writers.Blaze` — the
    `Doc`↔HTML layout shim; retargeted onto `Linen.Web.Html` (see blaze
    substitution). Depends on #18 (inferred).

### Tier 4 — the in-scope readers and writers

28. `Text.Pandoc.Readers.Native` → `Linen.Text.Pandoc.Readers.Native` — parses
    the AST's own `Haskell`-literal serialization. Depends on #1, #4, #18, #23
    (inferred; the lightest reader).
29. `Text.Pandoc.Writers.Native` → `Linen.Text.Pandoc.Writers.Native` — the
    inverse pretty-printer. Depends on #1, #25 (inferred; the lightest writer).
30. `Text.Pandoc.Readers.HTML` (+ `.Parsing`/`.Table`/`.Types`/
    `.TagCategories`) → `Linen.Text.Pandoc.Readers.HTML` — HTML→AST over the
    `tagsoup` tokenizer. Depends on #1, #4, #7, #18, #21, #23, #12, #15
    (inferred + the `tagsoup` prerequisite).
31. `Text.Pandoc.Writers.HTML` → `Linen.Text.Pandoc.Writers.HTML` — AST→HTML5/
    HTML4, the headline output format. Depends on #1, #7, #18, #2, #25, #26,
    #27, #24, #15, #21, #22, #8, #9, #11, #19 (confirmed via direct fetch);
    `Highlighting`/`ImageSize`/`Slides` usages degrade per the deferral notes.
32. `Text.Pandoc.Readers.Markdown` (+ `Readers.Metadata`) →
    `Linen.Text.Pandoc.Readers.Markdown` — Markdown→AST, the headline input
    format. Depends on #1, #4, #7, #18, #21, #9, #14, #2, #23, #30 (HTML
    reader), #12, #15 (confirmed via direct fetch), plus a **bounded slice of
    `Readers.LaTeX`** (`applyMacros`/`rawLaTeXBlock`/`rawLaTeXInline` — raw-TeX
    passthrough only, *not* the full LaTeX reader) and the YAML front-matter
    slice of `Readers.Metadata` (the `yaml` prerequisite).
33. `Text.Pandoc.Writers.Markdown` (+ `.Types`/`.Inline`/`.Table`) →
    `Linen.Text.Pandoc.Writers.Markdown` — AST→Markdown. Depends on #1, #4, #7,
    #18, #25, #26, #24, #2, #21 (inferred).
34. `Text.Pandoc` → `Linen.Text.Pandoc` — the top-level facade, **scoped** to
    re-exporting the in-scope readers/writers plus the reader/writer registries
    (`getReader`/`getWriter`) restricted to the in-scope formats. Depends on
    all of the above.

**Total: 34 in-scope modules** (5 AST from `pandoc-types` + 18 shared infra +
11 readers/writers/facade), out of `pandoc`'s ~170 library modules plus
`pandoc-types`'s 6. The `doclayout` prerequisite (a separate index entry) and
the `tagsoup`/`yaml` reader prerequisites sit *outside* this 34.

## Scope note: what is deferred, and why

Following the same "scope note" pattern the `hip` (#72), `duckdb-ffi` (#74),
`hedis` (#80), and `streamly` (#81) entries use, the bulk of `pandoc`'s ~170
library modules are **deferred out of this batch** — each is a self-contained
subtree that can be a later batch without changing the AST/Markdown/HTML core:

- **The exotic-format readers and writers** — LaTeX (beyond the raw-passthrough
  slice), ConTeXt, RST, Org, DocBook, JATS, MediaWiki, Textile, Jira, Man/Ms/
  Mdoc (roff), Muse, RTF, Texinfo, DokuWiki/XWiki/ZimWiki/TWiki/TikiWiki/
  Vimwiki/Creole, AsciiDoc, Haddock, Txt2Tags, FB2, TEI, ICML, BBCode, Vimdoc,
  OPML, Pod, CSV, Typst, Djot, CommonMark, BibTeX/EndNote/RIS/CslJson (the
  bibliography formats). These are the long tail; each is one or a few modules
  over the same shared core, exactly the "secondary layers built on the core"
  the `streamly` entry deferred.
- **The binary/zip formats** — DOCX, ODT, EPUB, Pptx, Xlsx and the shared
  `Readers/Writers.OOXML`, `ODT.*`, `Docx.*`, `Powerpoint.*` trees. These need
  `zip-archive` + a full streaming XML parser (`xml-conduit`/`xml-types`/`xml`)
  and OOXML/ODF schema handling — a heavy binary-format subsystem, deferred as
  its own future batch the way `streamly`'s `FileSystem.*` tree was.
- **The Lua-scripting / filter system** — `Text.Pandoc.Lua`, `.Filter`,
  `.Scripting`, and the JSON/Lua filter runners. A whole embedded-interpreter
  layer (needs `hslua`), orthogonal to the converter core.
- **Syntax highlighting** (`Highlighting`, `skylighting`/`skylighting-core`),
  **math typesetting** (`Writers.Math`'s `texmath` engine — kept only as
  raw/MathML passthrough), **citations** (`Citeproc`, `citeproc`), and
  **templating** (`doctemplates` — writers run template-free) — each a large
  self-contained engine the in-scope core degrades gracefully without.
- **The App/CLI, PDF, SelfContained, Chunks, and remote-fetch layers** —
  `Text.Pandoc.App*`, `.PDF`, `.SelfContained`, `.Chunks`, `.Data` (embedded
  data files via `file-embed`), and `Class.PandocIO`/`Class.IO`/`Class.Sandbox`
  (the `IO`-backed monad; only the pure `PandocPure` instance is in scope).
  The same "OS/IO-specific outer shell, out of scope" call `duckdb-ffi` and
  `streamly` both make.

The in-scope 34 give a complete, self-contained core: the universal document
AST (`pandoc-types`), the shared reader/writer infrastructure and the pure
`PandocMonad`, and a working Markdown↔AST↔HTML round-trip (plus the AST-native
`Native`/`JSON`), standing on its own once the `doclayout` prerequisite is in
place — with `tagsoup` and a bounded YAML parser as the two reader-side
prerequisites flagged above.
