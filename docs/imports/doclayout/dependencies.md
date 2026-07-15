# `doclayout` module dependencies

Topological order of every module of the
[`doclayout`](https://hackage.haskell.org/package/doclayout) package (v0.5.0.3,
source:
https://hackage-content.haskell.org/package/doclayout-0.5.0.3/src/doclayout.cabal
— the real `exposed-modules`/`other-modules`/`build-depends` fields, and every
module's import list, were fetched and read verbatim, not recalled from
memory), planned for import into `linen` per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention.

**Status: done.** All 4 in-scope modules have been ported to `Linen/Text/DocLayout*`
with `Tests/Linen/Text/DocLayout*` counterparts registered in `Tests.lean`. The
`render`/`offset`/`height` engine and its transitive callers are `unsafe`
(structural but non-well-founded reassociation), per the `Data.Conduit`/
`StreamK`/`Stream` precedent; everything else is total. The multi-thousand-entry
`baseEmojis` grapheme-cluster table is out of scope — `charWidth` uses a
wcwidth-style range approximation instead (documented in-module).

An edge **A → B** means *module A imports module B*, so **B must be built
before A**. Every intra-package edge below was confirmed by fetching each of
the four source files and reading its import list directly (not inferred).

## Headline finding: a genuinely new port, and a blocking prerequisite of `pandoc`

`doclayout` is a small, self-contained Wadler/Leijen-style pretty-printer — the
`Doc a` document algebra (`Text`/`BreakingSpace`/`Prefixed`/`Block`/`Concat`/
`Styled`/… constructors, a `Monoid`/`IsString` instance) plus the `render`
family (`render`/`renderANSI`), the layout combinators (`<+>`, `$$`, `vcat`,
`nest`, `hang`, `flush`, `lblock`/`rblock`/`cblock`), the `charWidth`/
`realLength` display-width machinery (combining marks, East-Asian wide chars,
emoji/ZWJ/skin-tone-aware widths), and an ANSI/OSC-8 styling layer.

**No `linen` equivalent exists.** The grep confirms `linen` has no
Wadler/Leijen pretty-printer: `Linen.Data.PDF.*` is PDF object/content-stream
handling, and the various `render`-named helpers scattered across the codebase
(e.g. the `Database.Redis.*` protocol serializers, the `Linen.Web.Html`
builder) are ad-hoc `ShowS`/`Text.Builder`-style emitters, not a
width-aware document-layout algebra. This is a genuine, foundational new port,
not a substitution.

It is a **blocking not-yet-ported prerequisite of
[`pandoc`](../pandoc/dependencies.md) (#83)** — every pandoc writer renders
through `doclayout`'s `Doc`/`render`/`literal`/`charWidth`
(`Text.Pandoc.Shared`, `Writers.Shared`, `Writers.HTML`, `Writers.Markdown`
all import it). It is therefore imported **first, as its own entry (#82)**, the
way `profunctors` (#77)/`indexed-traversable` (#78) were planned and ported
before `lens` (#79). It is flagged as its own entry rather than folded into
`pandoc` (unlike `pandoc-types`, which pandoc folds in) because it is a
general-purpose library likely to gain other consumers.

## Namespace decision

Kept as upstream's own `Text.DocLayout.*` hierarchy, re-rooted as
**`Linen.Text.DocLayout.*`**:

- `Text.DocLayout` → `Linen.Text.DocLayout`
- `Text.DocLayout.HasChars` → `Linen.Text.DocLayout.HasChars`
- `Text.DocLayout.ANSIFont` → `Linen.Text.DocLayout.ANSIFont`
- `Text.DocLayout.Attributed` → `Linen.Text.DocLayout.Attributed`

`DocLayout` is a **descriptive name** (document layout), not Haskell/GHC
branding, so AGENTS.md's Lean-ify rule does not require renaming it (contrast
`WaiAppStatic` → `WebApp.Static`). The candidate `Linen.Data.DocLayout` was
considered and rejected: upstream roots this in the `Text.` text-processing
domain, which maps cleanly to `Linen.Text.` — **the same mapping the (already
planned) `pandoc` entry makes for `Text.Pandoc` → `Linen.Text.Pandoc`**.
Placing `doclayout` at `Linen.Text.DocLayout` puts it as a direct sibling of
its principal consumer `Linen.Text.Pandoc` (which imports it in `Shared` and
every writer), mirroring upstream's own `Text.DocLayout`/`Text.Pandoc`
adjacency, and follows AGENTS.md's "place modules the way the Lean stdlib
would" rule (the stdlib keeps a `Text` namespace for text-processing;
`linen`'s text-domain ports already cluster under `Linen.Data.Text.*` from the
`Text` Hackage port at #39, and `Linen.Text.*` extends that domain for
whole-tool text trees). `doclayout` being ported first establishes the
`Linen.Text.*` root that `pandoc` then extends. There is no existing
pretty-printer module in `linen` for this to sit beside (confirmed by grep).

## External dependencies

Resolved against `doclayout-0.5.0.3.cabal`'s library `build-depends`, in
Hackage-import precedence order (Lean stdlib > existing `linen` Haskell port >
new Hackage import > raw port).

### Already ported / covered by the Lean stdlib, reused as-is

- **`base`** → `Base` / the Lean stdlib (`Prelude`, `Data.Char (isSpace, ord)`,
  `Data.List (foldl', intersperse, uncons)`, `Data.Maybe`, `Data.Foldable`,
  `Data.List.NonEmpty`, `Data.Bifunctor (second)`, `Data.String`).
- **`text`** → `Linen.Data.Text` (the `Text` port, #39) / Lean's native
  `String`. Backs the `Data.Text`/`Data.Text.Lazy`/`Data.Text.Lazy.Builder`
  uses (`HasChars`'s `Text`/lazy-`Text` instances, the builder-based
  `renderPlain`/`renderANSI` output).
- **`containers`** → `Linen.Data.IntMap` / `Linen.Data.Map` (the `Containers`
  port, #10). Backs the `Data.IntMap.Strict`/`Data.Map.Strict` Unicode
  width-lookup and emoji tables in `Text.DocLayout` (the `Data.Map.Internal`
  direct-constructor import is an internal micro-optimization that reduces to a
  plain `Map` lookup in the port). `Data.Sequence` (used by `Attributed`/
  `HasChars`) → `Linen`'s existing sequence/`Array`-backed structure or a plain
  `List`/`Array` (the `Attributed` payload is an ordered run of chunks).
- **`mtl`** → `Linen.Data.Mtl` (#22) / Lean's own `StateT`. Backs
  `Control.Monad.State.Strict` (the `DocState`/`RenderState` threading in the
  renderer).

### Substituted with a narrow inline (precedence rule)

- **`safe`** — only `lastMay` and `initSafe` are used (total `last`/`init`
  variants). Inlined as two one-liners (`List.getLast?`/dropping the last
  element), the same "narrow, fold it in" treatment the `pandoc` plan and
  `hedis`'s `errors` note give `safe`.
- **`emojis`** (`Text.Emoji`, `baseEmojis`) — the base emoji-codepoint table
  `Text.DocLayout` consults for emoji-aware `charWidth`/`realLength`. A single
  data table with no behaviour; the needed slice (the `baseEmojis` codepoint
  set) is folded inline as backing data for the width machinery rather than
  imported as a separate package (bounded, no other in-scope consumer here).
  Note: the (planned) `pandoc` entry (#83) independently folds the same
  `emojis` table into `Linen.Text.Pandoc.Emoji` for shortcode→char expansion;
  the two uses are disjoint (width lookup here, shortcode expansion there) and
  can share a single ported table if convenient when both are built.

### Dropped outright (GHC-toolchain / metaprogramming, no Lean analogue)

- **`GHC.Generics`** (`Generic` deriving) and **`Data.Data`** (`Data`/
  `Typeable`) — derived on `Doc`/`Attr`/`Attributed`/the `ANSIFont` style
  types for generic traversal/serialization. Lean has no
  `Data.Data`/`Typeable`-driven generic programming; the derived instances are
  dropped (the same category as `lens`'s dropped `Data.Data.Lens` and the
  `pandoc` plan's `syb` drop). The port derives Lean's own
  `Repr`/`DecidableEq`/`Inhabited` where useful instead.
- **CPP `#if MIN_VERSION_base(4,11,0)` shim** (the conditional
  `Data.Semigroup` import) — GHC-version compatibility for pre-4.11 `base`;
  dead code for one pinned Lean toolchain (same call `hedis`/`streamly` make
  for their `semigroups`/back-compat shims).

## Topologically sorted `doclayout` modules

All four library modules are in scope (there is no long tail to defer — this is
a single-purpose package). Tiers are in build order; within a tier, order is
not load-bearing.

### Tier 0 — the ANSI styling leaf (no internal deps)

1. `Text.DocLayout.ANSIFont` → `Linen.Text.DocLayout.ANSIFont` — the ANSI/
   terminal styling types (`Font`, `baseFont`, `Weight`/`Shape`/`Color8`/
   `Underline`/`Strikeout`/`Foreground`/`Background`/`StyleReq`), the `(~>)`
   style-application operator, and the SGR/OSC-8 escape-code renderers
   (`renderFont`, `renderOSC8`). **No internal deps** (confirmed: imports only
   `Data.Data`, `Data.String`, `Data.Text`).

### Tier 1 — attributed text runs

2. `Text.DocLayout.Attributed` → `Linen.Text.DocLayout.Attributed` — the
   `Link`/`Attr a`/`Attributed a` types (a sequence of font-attributed string
   chunks) with their `Semigroup`/`Monoid`/`IsString`/`Functor`/`Foldable`/
   `Traversable` instances. Depends on **#1** (confirmed: imports
   `Text.DocLayout.ANSIFont (Font, baseFont)`; plus `Data.Sequence`,
   `Data.String`, `Data.Text`).

### Tier 2 — the `HasChars` string-abstraction class

3. `Text.DocLayout.HasChars` → `Linen.Text.DocLayout.HasChars` — the `HasChars`
   typeclass generalizing char-folding/`isNull`/`splitLines`/`build` over
   string-like types, with instances for `Text`, `String`, lazy `Text`, and
   the sibling `Attr a`/`Attributed a`. Depends on **#2** (confirmed: imports
   `Text.DocLayout.Attributed`; plus `Data.Sequence`, `Data.Text`/lazy,
   `Data.Foldable`, `Data.List`, `Data.Maybe`).

### Tier 3 — the `Doc` algebra and renderer (the crown jewel)

4. `Text.DocLayout` → `Linen.Text.DocLayout` — the exposed module: the `Doc a`
   document type and its `Monoid`/`IsString` instances, the `literal` smart
   constructor, the layout combinators (`<+>`, `$$`, `vcat`, `nest`, `hang`,
   `flush`, `lblock`/`rblock`/`cblock`, `prefixed`, …), the styling helpers
   (`bold`, `italic`, `fg`, `bg`, `link`), the `charWidth`/`realLength`
   display-width machinery (folding in the `emojis` `baseEmojis` table per the
   note above), and the `render`/`renderANSI` renderers over the
   `State`-threaded layout engine. Depends on **#1, #2, #3** (confirmed:
   imports `Text.DocLayout.HasChars`, `Text.DocLayout.ANSIFont`,
   `Text.DocLayout.Attributed`; plus `Control.Monad.State.Strict`, `containers`
   maps, `Text.Emoji (baseEmojis)`, `Safe`).

**Total: 4 in-scope modules** (all of `doclayout`'s library modules — 1
exposed + 3 other-modules). Nothing is folded away or deferred; the `emojis`/
`safe` inlines and the `GHC.Generics`/`Data.Data` drops above sit *outside*
these 4 (they are external-dependency resolutions, not modules).
