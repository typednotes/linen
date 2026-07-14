/-
  Linen.Control.Lens.Combinators — facade re-exporting every already-ported
  `Control.Lens.*` module

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Combinators` (fetched and
  read via the real source, not recalled from memory). Upstream's own doc
  comment: "This lets the subset of users who vociferously disagree about
  the full scope and set of operators that should be exported from lens to
  not have to look at any operator with which they disagree." Concretely,
  upstream's real source is:

  ```
  module Control.Lens.Combinators
      ( module Control.Lens
      ) where
  import Control.Lens hiding
    ( (<|), (|>), (^..), (^?), (^?!), (^@..), (^@?), (^@?!), (^.), (^@.)
    , (<.), (.>), (<.>), (%%~), (%%=), (&), (&~), (<&>), (??), (<%~), (<+~)
    , … {- ≈140 more infix operator names in total -} …
    , (:>), (:<)
    )
  ```

  i.e. `Control.Lens.Combinators` re-exports *everything* `Control.Lens`
  re-exports (types, classes, and every prose-named combinator: `view`,
  `over`, `set`, `prism`, `folded`, …) but explicitly `hiding`s every bare
  infix/symbolic operator (`(^.)`, `(.~)`, `(%~)`, `(^..)`, `(#)`, …),
  leaving those to the separate operator-only facade,
  `Linen.Control.Lens.Operators` (`Control.Lens.Operators` upstream).

  **Deviation (no export-list curation).** Lean has no equivalent of
  Haskell's explicit `module M (foo, bar, ...) where` export list, nor of an
  `import M hiding (...)` exclusion list: every `def`/`abbrev`/`class`/
  `notation` a module declares is visible, in full, to anything that
  imports it (or transitively imports something that imports it) — there is
  no mechanism to import a module's named functions while suppressing its
  notations, or vice versa. Consequently this facade **cannot** reproduce
  upstream's actual `hiding` split: importing `Linen.Control.Lens.
  Combinators` makes every operator declared by every module below (`^.`,
  `.~`, `%~`, `^..`, `^?`, `#`, …) visible exactly as if `Linen.Control.Lens.
  Operators` had also been imported. This module's entire content is
  therefore just the `import` list below — a thin, mechanical re-export of
  every module in this batch's scope, standing in for upstream's `module
  Control.Lens.Combinators (module Control.Lens) where` line with the
  `hiding` clause noted here as documentation only, not enforced.

  **Scope (batch B, #20–#42, plus `Zoom`/`Reified`).** Upstream's own
  `Control.Lens` re-exports #20–#42 of `docs/imports/lens/dependencies.md`
  (`Control.Lens.{Type,Equality,Getter,Setter,Lens,Iso,Prism,Review,Fold,
  Traversal,Indexed,Each,At,Cons,Empty,Plated,Level,Zoom,Reified,Tuple,
  Unsound,Wrapped,Extras}`), all of which are re-exported below. `Zoom`
  (#37, `Control.Lens.Zoom`'s `zoom`/`magnify`) and `Reified` (#38,
  `ReifiedLens`/`ReifiedGetter`/…) were the last two gaps in that range —
  see `Linen.Control.Lens.Zoom`/`.Reified`'s own module doc comments for
  what each actually ports (a scoped-down `zoom`/`magnify` specialized to
  the one transformer shape `linen` has, and the dozen-or-so reified optic
  wrappers this batch's scope requested).
-/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Equality
import Linen.Control.Lens.Getter
import Linen.Control.Lens.Setter
import Linen.Control.Lens.Lens
import Linen.Control.Lens.Iso
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Review
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Traversal
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Tuple
import Linen.Control.Lens.Unsound
import Linen.Control.Lens.At
import Linen.Control.Lens.Each
import Linen.Control.Lens.Cons
import Linen.Control.Lens.Empty
import Linen.Control.Lens.Wrapped
import Linen.Control.Lens.Extras
import Linen.Control.Lens.Plated
import Linen.Control.Lens.Level
import Linen.Control.Lens.Zoom
import Linen.Control.Lens.Reified
