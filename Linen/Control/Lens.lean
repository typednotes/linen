/-
  Linen.Control.Lens — top-level facade re-exporting the whole `lens` port

  Port of Hackage's `lens-5.3.6`'s `Control.Lens` (fetched and read via the
  real source, not recalled from memory) — the package's main entry point,
  "listed first in upstream's own `exposed-modules` so `cabal repl` loads
  it." Upstream's real export list:

  ```
  module Control.Lens
    ( module Control.Lens.At
    , module Control.Lens.Cons
    , module Control.Lens.Each
    , module Control.Lens.Empty
    , module Control.Lens.Equality
    , module Control.Lens.Fold
    , module Control.Lens.Getter
    , module Control.Lens.Indexed
    , module Control.Lens.Iso
    , module Control.Lens.Lens
    , module Control.Lens.Level
    , module Control.Lens.Plated
    , module Control.Lens.Prism
    , module Control.Lens.Reified
    , module Control.Lens.Review
    , module Control.Lens.Setter
    , module Control.Lens.TH            -- (behind #ifndef DISABLE_TEMPLATE_HASKELL)
    , module Control.Lens.Traversal
    , module Control.Lens.Tuple
    , module Control.Lens.Type
    , module Control.Lens.Wrapped
    , module Control.Lens.Zoom
    ) where
  ```

  **Deviation (no export-list curation).** As with `Linen.Control.Lens.
  Combinators`/`.Operators`, Lean has no `module M (foo, ...) where`
  construct: this facade's entire content is the `import` list below, which
  makes every name from every module it (transitively) imports visible to
  anyone who imports `Linen.Control.Lens` — the union of `Linen.Control.
  Lens.Combinators` (batch B's 23 already-ported `Control.Lens.*` modules,
  the full #20–#42 range of `docs/imports/lens/dependencies.md`, including
  `Zoom`/`Reified` — see `Combinators.lean`'s own scope note) and
  `Linen.Control.Lens.Profunctor` (#45). `Linen.Control.Lens.Operators`
  (#44) is *not* imported separately here since it is already subsumed by
  `Combinators` (its "operators only" split is, per its own doc comment,
  unenforceable in Lean regardless).

  `Control.Lens.TH` (behind upstream's own `#ifndef DISABLE_TEMPLATE_HASKELL`
  guard) has no Lean counterpart at all — `docs/imports/lens/dependencies.md`
  documents `makeLenses`/`makePrisms`/`makeClassy` as permanently out of
  scope (no Template-Haskell-equivalent metaprogramming facility to port to;
  callers write each optic's smart-constructor definition by hand instead).

  **Scope (batch B: modules #20–#46).** This facade covers everything ported
  through this wave — the full van-Laarhoven/profunctor optics core plus the
  small `Profunctor`-interoperability layer. It does **not** yet include any
  of the per-container instance modules `docs/imports/lens/dependencies.md`
  plans from #47 onward (`Control.Exception.Lens`, `Data.Map.Lens`,
  `Data.Set.Lens`, `Data.Text.Lens`, `Data.Vector.Lens`, `Numeric.Lens`, …,
  matching upstream's own `Data.Map.Lens` etc. being separate exposed
  modules never re-exported from `Control.Lens` itself either). Whichever
  later batch ports one of those container-instance modules should add its
  import here, exactly mirroring how upstream's own `Control.Lens` never
  reaches past its own #20–#46 boundary either.
-/

import Linen.Control.Lens.Combinators
import Linen.Control.Lens.Profunctor
