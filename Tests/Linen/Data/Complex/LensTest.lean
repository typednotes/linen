/-
  Tests for `Linen.Data.Complex.Lens`.
-/
import Linen.Control.Lens.Iso
import Linen.Data.Complex.Lens

open Control.Lens
open Data (Complex)

namespace Tests.Linen.Data.Complex.Lens

-- ── `_polar` ─────────────────────────────────────
-- (run directly via `withIso`, since `Iso` is genuinely
-- profunctor-polymorphic and does not unify with `Getting`/`AReview`'s bare
-- function shape without an explicit instantiation — see the module's
-- own `withIso` doc comment.)

#guard withIso _polar (fun sa _ => (sa (Complex.mk (3.0 : Float) 0.0)).1 == (3.0 : Float))
#guard withIso _polar (fun sa _ => (sa (Complex.mk (3.0 : Float) 0.0)).2 == (0.0 : Float))
#guard withIso _polar (fun _ bt => (bt ((3.0 : Float), 0.0)).re == (3.0 : Float))
#guard withIso _polar (fun _ bt => (bt ((3.0 : Float), 0.0)).im == (0.0 : Float))

-- ── `_conjugate` ─────────────────────────────────

#guard withIso (_conjugate (A := Int)) (fun sa _ => sa (Complex.mk (3 : Int) 4)) == Complex.mk (3 : Int) (-4)
#guard withIso (_conjugate (A := Int)) (fun sa _ =>
    sa (sa (Complex.mk (3 : Int) 4))) == Complex.mk (3 : Int) 4

end Tests.Linen.Data.Complex.Lens
