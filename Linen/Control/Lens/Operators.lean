/-
  Linen.Control.Lens.Operators — facade re-exporting the infix operators
  declared across `Control.Lens.*`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Operators` (fetched and
  read via the real source, not recalled from memory). Upstream's own doc
  comment: "This module exists for users who like to work with qualified
  imports but want access to the operators from Lens", with the suggested
  usage `import qualified Control.Lens as L; import Control.Lens.Operators`.
  Its real source is a single `import Control.Lens` behind an export list
  that names *only* the infix operators, grouped by the submodule each is
  defined in (`Control.Lens.Cons`, `.Fold`, `.Getter`, `.Indexed`, `.Lens`,
  `.Plated`, `.Review`, `.Setter`, …) — well over a hundred operator names in
  total, with every prose-named function (`view`, `set`, `folded`, …)
  deliberately left out.

  **Deviation (no export-list curation).** As `Linen.Control.Lens.
  Combinators`'s own doc comment explains, Lean has no equivalent of
  Haskell's `module M (foo, bar) where` export-list curation: an `import`
  makes a module's entire public surface visible, operators and named
  functions alike, with no way to keep only one half. This facade therefore
  cannot actually restrict visibility to "operators only" the way upstream
  does — importing `Linen.Control.Lens.Operators` pulls in every name
  `Linen.Control.Lens.Combinators` does (transitively, via the same `import`
  mechanism upstream's own `Control.Lens.Operators` uses to reach every
  individual module). The module exists purely for parity with upstream's
  module inventory and as a documented, discoverable "I just want the
  operators" entry point; its content is nothing but the `import` below.

  **Operators this facade is nominally "for"** (grepped from every already-
  ported `Linen.Control.Lens.*` module's actual `infix`/`infixr`/`infixl`
  declarations — the exhaustive real list, not a guess):

  - `Linen.Control.Lens.Getter`: `(^.)` (view), `(^@.)` (indexed view).
  - `Linen.Control.Lens.Fold`: `(^..)` (toListOf), `(^?)` (preview).
  - `Linen.Control.Lens.Setter`: `(.~)` (set), `(%~)` (over), `(?~)`,
    `(<.~)`, `(<?~)`.
  - `Linen.Control.Lens.Lens`: `(%%~)`, `(<%~)`, `(<<%~)`.
  - `Linen.Control.Lens.Review`: `(#)` (review).
  - `Linen.Control.Lens.Indexed`: `(<.>)`, `(.>)` (index composition).

  Upstream's own `Control.Lens.Cons` operators `(<|)`/`(|>)` are **not**
  ported anywhere in this batch — `Linen.Control.Lens.Cons`'s own doc
  comment notes that a from-scratch `infixr " <| "` here would silently
  overload/clash with other notation, so `cons`/`uncons`/`snoc`/`unsnoc` are
  exposed as plain prose-named functions only, with no operator form to
  re-export here.

  This list is documentation only (mirroring upstream's per-submodule export
  grouping), not an enforced export set — see the deviation note above. -/

import Linen.Control.Lens.Combinators
