/-
  Tests for `Linen.Control.Lens.Profunctor`.
-/
import Linen.Control.Lens.Profunctor

open Control Control.Profunctor Control.Lens.Internal Control.Lens

namespace Tests.Linen.Control.Lens.Profunctor

def fstL : Lens' (Nat × Nat) Nat := lens Prod.fst (fun s v => (v, s.2))

-- `fromLens`, run at the bare-function profunctor `Fun`: agrees with `over`.
#guard (fromLens (P := Fun) fstL ⟨(· + 1)⟩).apply (3, 4) = (4, 4)
#guard (fromLens (P := Fun) fstL ⟨(· + 1)⟩).apply (3, 4) = over fstL (· + 1) (3, 4)

-- `fromIso`, run at `Fun`: transports a function on the underlying values
-- through the isomorphism (`not` conjugated by `not`/`not` is `not` again).
def notIso : Iso' Bool Bool := iso not not
#guard (fromIso (P := Fun) notIso ⟨id⟩).apply true = true

-- `fromPrism`, run at `Fun`: behaves like `over` restricted to the matching
-- case, and passes the non-matching case through untouched.
#guard (fromPrism (P := Fun) (_Just (A := Nat) (B := Nat)) ⟨(· + 1)⟩).apply (some 5) = some 6
#guard (fromPrism (P := Fun) (_Just (A := Nat) (B := Nat)) ⟨(· + 1)⟩).apply none = none

-- `fromSetter`, run at `Fun`: agrees with `over`.
#guard (fromSetter (P := Fun) fstL ⟨(· + 1)⟩).apply (3, 4) = (4, 4)

-- `toLens`/`toSetter`/`toTraversal`, run at `Star Id`: recover an ordinary
-- `LensLike Id` from a `Profunctor`-based optic built via `fromLens`. `F` is
-- pinned explicitly to `Id` (rather than left to unify against the result's
-- expected type) to avoid a spurious higher-order-unification guess for the
-- otherwise-implicit `F`.
def viaLens : Nat × Nat :=
  toLens (F := Id) (fromLens (P := Star Id) fstL) (fun a => (a + 1 : Id Nat)) (3, 4)
#guard viaLens = (4, 4)

def viaSetter : Nat × Nat :=
  toSetter (F := Id) (fromLens (P := Star Id) fstL) (fun a => (a + 1 : Id Nat)) (3, 4)
#guard viaSetter = (4, 4)

def viaTraversal : Nat × Nat :=
  toTraversal (F := Id) (fromLens (P := Star Id) fstL) (fun a => (a + 1 : Id Nat)) (3, 4)
#guard viaTraversal = (4, 4)

-- `toIso`, run at `WrappedPafb Id Fun`: recovers an `Iso`-shaped van
-- Laarhoven optic (here, just the identity passthrough of the underlying
-- `Fun`).
def pIso : OpticP (WrappedPafb Id Fun) Bool Bool Bool Bool := id
def viaIso : Bool := (toIso (F := Id) pIso ⟨fun b => (not b : Id Bool)⟩).apply true
#guard viaIso = false

-- `toPrism`, run at `WrappedPafb Id Fun`: recovers a `Prism`-shaped van
-- Laarhoven optic, lifting the underlying `Fun` through `Option.map`.
def pPrism (x : WrappedPafb Id Fun Nat Nat) : WrappedPafb Id Fun (Option Nat) (Option Nat) :=
  ⟨⟨fun o => (o.map x.unwrapPafb.apply : Id (Option Nat))⟩⟩
def viaPrismSome : Option Nat := (toPrism (F := Id) pPrism ⟨fun a => (a + 1 : Id Nat)⟩).apply (some 5)
#guard viaPrismSome = some 6
def viaPrismNone : Option Nat := (toPrism (F := Id) pPrism ⟨fun a => (a + 1 : Id Nat)⟩).apply none
#guard viaPrismNone = none

end Tests.Linen.Control.Lens.Profunctor
