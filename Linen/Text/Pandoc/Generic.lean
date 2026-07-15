/-
  `Linen.Text.Pandoc.Generic` — generic transforms over the Pandoc AST.

  ## Haskell source

  Ported from `Text.Pandoc.Generic` in the `pandoc-types` package
  (v1.23.1, `src/Text/Pandoc/Generic.hs`).

  Provides `bottomUp`/`topDown`/`bottomUpM`/`queryWith`, the "apply a
  transformation everywhere" combinators.

  ### Deviations from upstream

  * Upstream is built on `syb`'s `Data.Data` generics (`everywhere`,
    `everywhere'`, `everywhereM`, `everything`).  Lean has no `Data`-style
    generic programming, so — as the import plan records — these are
    reimplemented directly over `Text.Pandoc.Walk`: `bottomUp`/`bottomUpM`
    are `Walk.walk`/`Walk.walkM`, `queryWith` is `Walk.query`, and `topDown`/
    `topDownM` use `Walk.walkTopDown`/`walkTopDownM`.
  * `topDown`/`topDownM` are `unsafe`, because top-down traversal (which
    recurses into the transformed node) has no Lean termination argument; see
    the note in `Text.Pandoc.Walk`.  `bottomUp`/`bottomUpM`/`queryWith` are
    total.
-/

import Linen.Text.Pandoc.Walk

namespace Linen.Text.Pandoc

/-- Apply a transformation on `a`s to matching elements in a `b`, moving from
    the bottom of the structure up. -/
def bottomUp [Walkable a b] (f : a → a) (x : b) : b := walk f x

/-- Like `bottomUp`, but with monadic transformations. -/
def bottomUpM [Walkable a b] {m : Type → Type} [Monad m] (f : a → m a) (x : b) : m b := walkM f x

/-- Run a query on matching `a` elements in a `b`, combining the results with
    the monoid `++`. -/
def queryWith [Walkable a b] {c : Type} [Append c] [Inhabited c] (f : a → c) (x : b) : c :=
  query f x

/-- Apply a transformation on `a`s to matching elements in a `b`, moving from
    the top of the structure down.  `unsafe`: see the module note. -/
unsafe def topDown [WalkableTD a b] (f : a → a) (x : b) : b := walkTopDown f x

/-- Like `topDown`, but with monadic transformations.  `unsafe`: see the
    module note. -/
unsafe def topDownM [WalkableTD a b] {m : Type → Type} [Monad m] (f : a → m a) (x : b) : m b :=
  walkTopDownM f x

end Linen.Text.Pandoc
