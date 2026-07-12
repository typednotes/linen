/-
  Linen.Graphics.Image.Types — the package-level type/re-export facade

  ## Haskell equivalent

  `Graphics.Image.Types` from https://hackage.haskell.org/package/hip
  (module #26 of the `hip` import plan, see
  `docs/imports/hip/dependencies.md`). `raw.githubusercontent.com/lehins/
  hip/master/src/Graphics/Image/Types.hs` 404s (as with every other module
  in this sub-tree); fetched instead from the 1.5.6.0 release tarball
  (`hackage.haskell.org/package/hip-1.5.6.0/hip-1.5.6.0.tar.gz`,
  `src/Graphics/Image/Types.hs`, 90 lines, read in full).

  ## Surprise: no concrete per-colour-space type aliases

  The import plan anticipated a "large battery of concrete type aliases"
  here (`type RGBImage = Image VU RGB Double`-shaped). Checking the actual
  1.5.6.0 source shows this is **not** what upstream's `Types.hs` contains:
  a repository-wide `grep -rn "^type.*Image"` over the whole tarball turns
  up exactly two hits, both `AllReadable`/`AllWritable` constraint synonyms
  already ported inside `IO/Formats.hs` (#24) — no `RGBImage`, `GreyImage`,
  `BinaryImage`, or similar name exists anywhere in this version of `hip`.
  (Older/newer `hip` releases, or the package's Haddock-generated docs, may
  have offered such aliases at some point, but the pinned 1.5.6.0 tarball —
  the version this whole import targets, per `docs/imports/hip/
  dependencies.md` — does not; porting against what upstream *actually*
  ships takes precedence over the plan's own advance guess.) Instead,
  `Types.hs` is exactly what its own export list says: a **re-export
  facade** —

  ```
  module Graphics.Image.Types (
    module Graphics.Image.ColorSpace,
    module Graphics.Image.IO.Formats,
    Array, Image, MArray, MImage,
    Border(..),
    VU(..), VS(..), RSU(..), RPU(..), RSS(..), RPS(..)
    ) where
  ```

  — plus a 62-line `{-# RULES ... #-}` pragma block with no export-list
  counterpart at all.

  ## How each piece of the export list ports

  - `module Graphics.Image.ColorSpace` (#12) / `module Graphics.Image.IO.
    Formats` (#24) → plain `import`s below. As those two modules' own
    doc-comments already establish for the sub-facades *they* re-export,
    Lean's `import` is transitive: importing them here makes every
    colour-space (`Y`, `RGB`, `HSI`, `CMYK`, `YCbCr`, `Complex`, `X`,
    `Binary`, #4–#11) and format (`BMP`, `GIF`, `HDR`, `JPG`, `PNG`, `TGA`,
    `TIF`, `PBM`, `PGM`, `PPM`, #22–#23) name reachable by its full name
    with no further re-export step, unlike Haskell's export lists which
    must name every re-exported module explicitly.
  - `Array, MArray, MImage` → **out of scope, nothing to export.**
    `Interface.lean` (#3) already dropped the entire `BaseArray`/`Array`/
    `MArray` typeclass hierarchy and the `data family Image arr cs e`/
    `MImage s arr cs e` distinction in its own port (see that module's
    doc-comment): once `Image cs e` is hard-wired to a single `Manifest`
    representation with no abstract backend parameter, there is no
    "class of array backends" left for `Array`/`MArray` to name, and no
    persistent-vs-mutable pair left for `Image`/`MImage` to distinguish
    (mutation is dropped the same way `Linen.Codec.Picture.Types` already
    drops `MutableImage`/freeze/thaw). This is not a new simplification
    introduced by this module — it is this module observing a decision
    `Interface.lean` already made two steps earlier in the plan.
  - `Image` → re-exported below via `export Graphics.Image.Interface
    (Image)`, exactly as `Interface.lean` defined it (`Image cs e :=
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px` for the `px`
    determined by `[Pixel cs e px]`).
  - `Border(..)` → re-exported below via `export Graphics.Image.Interface
    (Border)`, exactly as `Interface.lean` defined it (the five-constructor
    `Fill`/`Wrap`/`Edge`/`Reflect`/`Continue` inductive).
  - `VU(..), VS(..), RSU(..), RPU(..), RSS(..), RPS(..)` → **out of scope,
    nothing to export.** These are the six representation-selector marker
    types from `Graphics.Image.Interface.Vector` (`VU`, `VS`) and
    `Graphics.Image.Interface.Repa` (`RSU`, `RPU`, `RSS`, `RPS`) — per
    `docs/imports/hip/dependencies.md`'s precedence-check note, those eight
    modules "need no separate port" because this port's `Image cs e` has no
    representation-backend type parameter at all to select between (it is
    hard-wired straight to `Manifest`, collapsing the whole
    vector/unboxed/storable/repa axis away). There is accordingly no marker
    type of this kind anywhere in this port for these six names to alias.
  - The `{-# RULES ... #-}` block (62 lines, rewriting `img ^ (n :: Int)` /
    `img ^ (n :: Integer)` into `map (^ n) img` for six concrete `(Image VU
    | RPU | RSU) (Y | RGB) Double` types) → **out of scope, dropped
    entirely.** GHC `RULES` pragmas are compiler-directed rewrite hints for
    the optimizer; they have no run-time semantics, no type-level meaning,
    and (per this port's own no-`partial`/no-representation-axis design)
    there are no longer six concrete `(representation, colour space,
    Double)` triples for such a rule to name even if Lean had an equivalent
    pragma mechanism, which it does not.

  ## Net result

  This module is deliberately thin: two `import`s (folding in every
  colour-space and format facade transitively) plus two `export`s (`Image`,
  `Border`, mirroring `Linen.Data.PDF.Core`'s own leaf-name re-export
  pattern for the names it owns directly, as opposed to the sub-facade
  names it receives purely through transitive `import`). This is a
  faithful, line-by-line accounting of upstream's own export list, not a
  simplification of it — upstream's `Types.hs` genuinely has no other
  content once the `RULES` pragmas (dropped as a GHC-only compiler
  directive) and the representation-selector re-exports (already
  eliminated at the `Interface`/`Interface.Vector`/`Interface.Repa` layer)
  are accounted for. -/
import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace
import Linen.Graphics.Image.IO.Formats

namespace Graphics.Image.Types

export Graphics.Image.Interface (Image Border)

end Graphics.Image.Types
