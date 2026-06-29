/-
  Linen.Data.Unique — globally unique identifiers

  Mirrors Haskell's `Data.Unique`: opaque values, each distinct from every
  other, handed out from a process-global counter via `newUnique : IO Unique`.
  Core has no equivalent.
-/

namespace Data

/-- A globally unique identifier, allocated via `newUnique`.

    $$\text{Unique} \cong \mathbb{N}$$

    The constructor and underlying field are **private**, so `newUnique` is the
    only way to mint a value (mirroring Haskell's abstract `Data.Unique`). This
    is what makes the type's guarantee real rather than aspirational: two
    `Unique` values are equal iff they came from the same `newUnique` call, and
    derived `Ord` reflects allocation order. Use `hashUnique` to read the
    underlying `Nat`. -/
structure Unique where
  private mk ::
  /-- The underlying identifier (private — read it via `hashUnique`). -/
  private id : Nat
deriving BEq, Hashable, Repr, Ord

instance : ToString Unique where
  toString u := s!"Unique({u.id})"

/-- Process-global counter backing `newUnique`. -/
private initialize uniqueCounter : IO.Ref Nat ← IO.mkRef 0

/-- Allocate a fresh `Unique`. Each call returns a value distinct from — and
    greater than — every previously allocated one.

    $$\text{newUnique} : \text{IO}\,(\text{Unique})$$ -/
def newUnique : IO Unique := do
  let n ← uniqueCounter.get
  uniqueCounter.set (n + 1)
  pure ⟨n⟩

/-- Extract the underlying `Nat` (Haskell's `hashUnique`). -/
@[inline] def Unique.hashUnique (u : Unique) : Nat := u.id

/-- `hashUnique` is exactly the underlying identifier. -/
theorem Unique.hashUnique_eq (u : Unique) : u.hashUnique = u.id := rfl

end Data
