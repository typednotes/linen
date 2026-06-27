/-
  Linen.Data.Proxy — Phantom type proxy

  `Proxy α` carries a phantom type parameter but no data — useful for passing
  type-level information at zero runtime cost. Lean core has no such type, so
  this ports Haskell's `Data.Proxy`.
-/

namespace Data

/-- The proxy type $\text{Proxy}(\alpha)$ carries a phantom type parameter
$\alpha$ but contains no data. It is terminal in the category of types: every
type has exactly one function into $\text{Proxy}(\alpha)$.

$$\text{Proxy} : \text{Type}\; u \to \text{Type}$$ -/
structure Proxy (α : Type u) : Type where
  mk ::
deriving Inhabited

namespace Proxy

/-- `BEq` — always true (there is only one value). -/
instance : BEq (Proxy α) where
  beq _ _ := true

/-- `Ord` — always `Ordering.eq`. -/
instance : Ord (Proxy α) where
  compare _ _ := .eq

/-- `Repr` — displays as `Proxy.mk`. -/
instance : Repr (Proxy α) where
  reprPrec _ _ := "Proxy.mk"

/-- `Hashable` — always hashes to `0`. -/
instance : Hashable (Proxy α) where
  hash _ := 0

instance : ToString (Proxy α) where
  toString _ := "Proxy"

/-- `Functor` — `map` is identity on the (data-free) structure. -/
instance : Functor Proxy where
  map _ _ := Proxy.mk

instance : Pure Proxy where
  pure _ := Proxy.mk

instance : Bind Proxy where
  bind _ _ := Proxy.mk

instance : Seq Proxy where
  seq _ _ := Proxy.mk

instance : SeqLeft Proxy where
  seqLeft _ _ := Proxy.mk

instance : SeqRight Proxy where
  seqRight _ _ := Proxy.mk

instance : Applicative Proxy where

instance : Monad Proxy where

-- ── Functor laws ──────────────────────────────

/-- Functor identity law. -/
theorem map_id (p : Proxy α) : Functor.map id p = p := by
  cases p; rfl

/-- Functor composition law. -/
theorem map_comp (f : β → γ) (g : α → β) (p : Proxy α) :
    Functor.map (f ∘ g) p = Functor.map f (Functor.map g p) := by
  cases p; rfl

-- ── Monad laws ────────────────────────────────

/-- Left identity: `bind (pure a) f = f a`. -/
theorem pure_bind (a : α) (f : α → Proxy β) : bind (pure a) f = f a := by
  cases (f a); rfl

/-- Right identity: `bind m pure = m`. -/
theorem bind_pure (p : Proxy α) : bind p pure = p := by
  cases p; rfl

/-- Associativity of `bind`. -/
theorem bind_assoc (p : Proxy α) (f : α → Proxy β) (g : β → Proxy γ) :
    bind (bind p f) g = bind p (fun x => bind (f x) g) := by
  cases p; rfl

end Proxy
end Data
