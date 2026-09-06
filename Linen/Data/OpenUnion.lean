/-
  `Data.OpenUnion` — an open union over a row of effect functors

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Data-OpenUnion.html
  (merged with `Data.OpenUnion.Internal`), module #1 of the `FreerSimple`
  import (see `docs/imports/FreerSimple/dependencies.md`).

  A `Union effs α` is a value of *exactly one* of the effect functors in the row
  `effs`, applied to `α` — the type-level analogue of a tagged union whose set of
  tags is a list of `Type → Type`. `Member eff effs` witnesses that `eff` occurs
  in `effs`, and provides the injection into, and partial projection out of, the
  union. `Control.Monad.Freer` builds the `Eff` monad on top of this.

  ## Substitutions / deviations

  - **The `Internal` split is dropped, and with it `unsafeCoerce`.** Upstream
    separates a safe `Member`-typeclass interface (`Data.OpenUnion`) from an
    unsafe implementation (`Data.OpenUnion.Internal`), in which `Union` is an
    `unsafeCoerce`d `(Int, Any)` pair so that dispatch is O(1) under GHC. Here
    the safe interface *is* the implementation: `Union` is an ordinary strictly
    positive inductive indexed by the row, in the shape of `List.Mem` evidence,
    so `Member`'s injection/projection are structurally obvious and need no
    coercion the kernel cannot see through. `Linen/System/IO.lean` records the
    same reasoning for dropping `unsafeInlineIO`, as does `Control.Monad.STM`
    for GHC's STM primops.

  - **Universe.** Upstream's row is `[* -> *]`, erased at compile time. In Lean
    `Type → Type` itself inhabits `Type 1`, so a constructor binding
    `{eff : Type → Type}` forces `Union` into `Type 1` even though its payload
    `eff α` stays in `Type 0`.
-/

namespace Data.OpenUnion

-- ── The open union ──────────────────────────────────────────────────────────

/-- A value of exactly one effect functor drawn from the row `effs`, applied to
    `α`.

    `here` holds a value of the row's head effect; `there` defers to the tail.
    The constructors mirror `List.Mem`: a `Union` *is* the evidence of which
    effect it came from.

    $$\text{Union}\ [\text{eff}_1, \dots, \text{eff}_n]\ \alpha \;\cong\;
      \text{eff}_1\ \alpha \;+\; \dots \;+\; \text{eff}_n\ \alpha$$ -/
inductive Union : List (Type → Type) → Type → Type 1 where
  /-- The value belongs to the row's head effect. -/
  | here  {eff : Type → Type} {effs : List (Type → Type)} {α : Type} :
      eff α → Union (eff :: effs) α
  /-- The value belongs to some effect in the row's tail. -/
  | there {eff : Type → Type} {effs : List (Type → Type)} {α : Type} :
      Union effs α → Union (eff :: effs) α

/-- The empty row admits no effects, so `Union [] α` is uninhabited and anything
    follows from it — the `Empty.elim` of effect rows.

    This is what lets `Control.Monad.Freer.Eff.run` discharge its otherwise
    impossible branch: a computation over the empty row cannot be performing an
    effect. -/
def Union.elim0 {α : Type} {C : Sort u} (u : Union [] α) : C := nomatch u

-- ── Membership ──────────────────────────────────────────────────────────────

/-- `Member eff effs` witnesses that the effect functor `eff` occurs in the row
    `effs`, by supplying the injection into and projection out of the union.

    Instance resolution performs the search: `instMemberHere` matches the head,
    `instMemberThere` recurses into the tail, so the instance found *is* the
    position of `eff` in `effs`. -/
class Member (eff : Type → Type) (effs : List (Type → Type)) where
  /-- Inject an effect value into the union. -/
  inj : {α : Type} → eff α → Union effs α
  /-- Project the union back to `eff`, or `none` if it holds a different effect. -/
  prj : {α : Type} → Union effs α → Option (eff α)

/-- `eff` is the head of the row. -/
instance instMemberHere {eff : Type → Type} {effs : List (Type → Type)} :
    Member eff (eff :: effs) where
  inj e := .here e
  prj u := match u with
    | .here e  => some e
    | .there _ => none

/-- `eff` occurs somewhere in the row's tail. -/
instance instMemberThere {eff eff' : Type → Type} {effs : List (Type → Type)}
    [Member eff effs] : Member eff (eff' :: effs) where
  inj e := .there (Member.inj e)
  prj u := match u with
    | .here _   => none
    | .there u' => Member.prj u'

end Data.OpenUnion
