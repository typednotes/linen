/-
  Linen.Control.Lens.Extras — `is`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Extras` (fetched and read
  via the real source, not recalled from memory). Upstream's module comment
  describes it as "a few extra names that didn't make it into Control.Lens";
  its only export besides a re-export of `Data.Data.Lens` (out of scope —
  `linen` has ported no `Data.Data`-style generic-programming machinery) is
  `is`.

  Upstream's real definition: `is :: APrism s t a b -> s -> Bool; is k = not
  . isn't k`, where `isn't k s = case matching k s of Left _ -> True; Right _
  -> False` (`Control.Lens.Prism`). `linen`'s `Linen.Control.Lens.Prism` has
  ported neither `APrism` (an alias collapsed away, the same way `AnIso` is
  collapsed to a concrete `Prism`/`Iso` argument throughout this codebase's
  `withPrism`/`withIso`-style helpers) nor `matching`/`isn't` themselves —
  this module ports `is` directly against `withPrism`
  (`Linen.Control.Lens.Prism`), inlining exactly what `matching`/`isn't`
  would have done, so that the net behaviour (and the `is = not . isn't`
  law) still holds without first needing to introduce `isn't` as a name
  nothing else in this batch's scope calls.

  **Universe note.** `withPrism`'s own signature ties its continuation's
  result type to the same universe `u` as `S`/`T`/`A`/`B` (`{S T A B R :
  Type u}`); since `is`'s result `Bool : Type 0` fixes that shared `u` to
  `0`, `is` is stated at `Type` rather than the fully universe-polymorphic
  `Type u` used elsewhere in this batch. Every concrete `Prism` this module
  or its tests build (`_Left`, `_Right`, `_Just`, `_Nothing`) already lives
  at `Type`, so this loses no call site in scope. -/

import Linen.Control.Lens.Prism

open Control.Lens

namespace Control.Lens

-- ── is ──────────────────────────────────────────

/-- `is :: APrism s t a b -> s -> Bool`: check to see if this `Prism`
    matches — `is k = not . isn't k`, i.e. `is k s = case matching k s of
    Left _ -> False; Right _ -> True` where `matching k = withPrism k $ \_
    seta -> seta`, matching upstream's real `is`/`isn't`/`matching` chain
    without needing `isn't` as an intermediate name. -/
@[inline] def is {S T A B : Type} (l : Prism S T A B) (s : S) : Bool :=
  withPrism l (fun (_ : B → T) (seta : S → T ⊕ A) =>
    match seta s with
    | .inl _ => false
    | .inr _ => true)

end Control.Lens
