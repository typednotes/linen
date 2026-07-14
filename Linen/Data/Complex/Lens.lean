/-
  Linen.Data.Complex.Lens — `_polar`, `_conjugate`

  Port of Hackage's `lens-5.3.6`'s `Data.Complex.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content:

  ```
  _polar :: RealFloat a => Iso' (Complex a) (a, a)
  _polar = iso polar (uncurry mkPolar)

  _conjugate :: RealFloat a => Iso' (Complex a) (Complex a)
  _conjugate = involuted conjugate
  ```

  translated against `Linen.Data.Complex`'s `Complex` structure.

  **Deviation (specialized to `Complex Float`).** Upstream's `RealFloat`
  constraint demands `sqrt`/`atan2` on the underlying real type; `linen`'s
  `Linen.Data.Complex` is generic over any `α` with no such constraint
  available (only `Add`/`Neg`/`Sub`/`Mul`), and no `RealFloat`-equivalent
  class exists anywhere in `linen` to add one against. `_polar` is therefore
  given only for `Complex Float`, using Lean core's own `Float.sqrt`/
  `Float.atan2`, the same kind of "narrow a generic upstream constraint down
  to the one concrete instantiation `linen` can actually support" deviation
  already used elsewhere in this batch (e.g. `Linen.Data.Array.Lens`'s
  `Nat`-indexing note). `_conjugate` needs no such narrowing — `conjugate`
  is already defined generically on `Complex α` for any `Neg α`.

  **Deviation (`involuted`, inlined).** Upstream builds `_conjugate` via
  `involuted :: (a -> a) -> Iso' a a` (`iso f f`, since `f` is its own
  inverse) from `Control.Lens.Iso`; `linen`'s `Linen.Control.Lens.Iso` has
  no such combinator (confirmed absent by grep). Rather than adding it to a
  module this batch does not touch, `_conjugate` is built directly as `iso
  conjugate conjugate`, definitionally the same optic `involuted conjugate`
  would produce. -/

import Linen.Control.Lens.Iso
import Linen.Data.Complex

namespace Control.Lens

open Data (Complex)

/-- The magnitude $|z| = \sqrt{\text{re}(z)^2 + \text{im}(z)^2}$ of a
    `Complex Float`, using Lean core's `Float.sqrt`. Named `complexMagnitude`
    (rather than `Complex.magnitude`) to avoid colliding, under dot
    notation, with `Data.Complex`'s own namespace: a bare `Complex.foo` name
    written inside `namespace Control.Lens` elaborates to
    `Control.Lens.Complex.foo`, which dot notation on a `Data.Complex`-typed
    value cannot find. -/
@[inline] def complexMagnitude (z : Complex Float) : Float :=
  Float.sqrt (z.re * z.re + z.im * z.im)

/-- The phase $\arg(z) = \operatorname{atan2}(\text{im}(z), \text{re}(z))$ of
    a `Complex Float`, using Lean core's `Float.atan2`. -/
@[inline] def complexPhase (z : Complex Float) : Float :=
  Float.atan2 z.im z.re

/-- `mkPolar :: RealFloat a => a -> a -> Complex a`: build a complex number
    from its magnitude and phase — $z = r(\cos\theta + i\sin\theta)$. -/
@[inline] def complexMkPolar (r θ : Float) : Complex Float :=
  ⟨r * Float.cos θ, r * Float.sin θ⟩

/-- `_polar :: RealFloat a => Iso' (Complex a) (a, a)`: a `Complex Float` is
    isomorphic to its `(magnitude, phase)` pair — `_polar = iso polar
    (uncurry mkPolar)`. -/
@[inline] def _polar : Iso' (Complex Float) (Float × Float) :=
  iso (fun z => (complexMagnitude z, complexPhase z)) (fun p => complexMkPolar p.1 p.2)

/-- `_conjugate :: RealFloat a => Iso' (Complex a) (Complex a)`: complex
    conjugation is its own inverse — `_conjugate = involuted conjugate`,
    inlined here as `iso conjugate conjugate` (see the module doc comment). -/
@[inline] def _conjugate {A : Type u} [Neg A] : Iso' (Complex A) (Complex A) :=
  iso Data.Complex.conjugate Data.Complex.conjugate

end Control.Lens
