/-
  Linen.Data.Default — the `Default` typeclass

  Sensible default values, mirroring Haskell's `Data.Default`. Distinct from
  Lean's `Inhabited`: `Inhabited` only guarantees *some* inhabitant exists,
  whereas `Default` carries the intent of "the most commonly useful starting
  point" (e.g. `Default Bool = false`, `Default Nat = 0`).
-/

namespace Data

/-- A type with a sensible default value. -/
class Default (α : Type) where
  /-- The default value. -/
  default : α

instance : Default Bool where default := false
instance : Default Nat where default := 0
instance : Default Int where default := 0
instance : Default String where default := ""
instance : Default (List α) where default := []
instance : Default (Array α) where default := #[]
instance : Default (Option α) where default := none
instance [Default α] [Default β] : Default (α × β) where
  default := (Default.default, Default.default)
instance : Default Unit where default := ()

end Data
