/-
  Linen.Control.Profunctor — facade re-exporting the core `Profunctor` API

  Port of Hackage's `profunctors-5.6.3`'s top-level `Data.Profunctor` module
  (module #16 of `docs/imports/profunctors/dependencies.md`, the facade).

  **Placement note.** The plan suggests naming this facade
  `Control.Profunctor.Basic` to avoid a clash between the `Control.Profunctor`
  *class* (defined in `Linen.Control.Profunctor.Unsafe`) and a
  `Control.Profunctor` *namespace* used as the prefix for every sibling
  module in this directory. That clash does not actually arise in Lean 4 — a
  declaration name and a namespace prefix are allowed to coincide (the same
  pattern the standard library and `Mathlib` use routinely, e.g.
  `Function.Injective` naming both a declaration and implicitly opening
  `Function`), and this codebase already has the identical precedent at
  `Linen/Data/Array/Shaped.lean` coexisting with the `Linen/Data/Array/Shaped/`
  directory. This facade is therefore placed directly at
  `Linen/Control/Profunctor.lean` (module `Linen.Control.Profunctor`) rather
  than at `Control.Profunctor.Basic`, matching that existing convention.

  This module ports no new declarations of its own: it just imports the
  modules that upstream's own `Data.Profunctor` facade re-exports —
  `Types`, `Strong` (plus `uncurry'`), `Choice`, `Closed` (plus `curry'`),
  and `Mapping` — together with `Unsafe` (upstream's `Data.Profunctor.Unsafe`
  is a separate import there only because it also re-exports `(#.)`/`(.#)`,
  which this port omits; the base `Profunctor` class it defines is otherwise
  exactly what upstream's facade re-exports as `Profunctor(dimap,lmap,rmap)`).
-/

import Linen.Control.Profunctor.Choice
import Linen.Control.Profunctor.Closed
import Linen.Control.Profunctor.Mapping
import Linen.Control.Profunctor.Strong
import Linen.Control.Profunctor.Types
import Linen.Control.Profunctor.Unsafe
