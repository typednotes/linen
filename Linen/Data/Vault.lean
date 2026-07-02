/-
  Linen.Data.Vault — Type-safe heterogeneous map

  A `Vault` is a type-safe heterogeneous container, keyed by `Key α` tokens.
  Each key is unique (allocated via IO) and typed: a `Key α` can only
  store and retrieve values of type `α`.

  ## Haskell equivalent
  `Data.Vault.Lazy` (https://hackage.haskell.org/package/vault)

  ## Design
  Internally backed by a `Std.HashMap` mapping `Nat` (unique key IDs) to
  type-erased values (`Erased`). Type safety is maintained by the `Key α`
  abstraction — lookup casts back to `α` using `unsafeCast`, which is sound
  because the same `Key α` that inserted the value is required to retrieve it.

  ## Axiom-dependent properties
  Type safety of `lookup` depends on the guarantee that `unsafeCast` from the
  erased type back to `α` is safe when the original value was of type `α`.
  This is guaranteed by construction (same `Key α` for insert and lookup).

  ## Lean stdlib reuse
  Keys are minted from `Linen.Data.Unique` rather than a bespoke counter; its
  `id` field is private to its own module, so key allocation goes through the
  public `Unique.hashUnique` accessor instead of a direct field projection.
-/
import Linen.Data.Unique
import Std.Data.HashMap

namespace Data

/-- Opaque type-erasure target for `Vault`'s internal store. Values of any
    type are `unsafeCast` into this type on insert and back out on lookup;
    the cast is a bare reinterpretation, so `Erased`'s own definition is
    irrelevant beyond providing a `Type` to cast through. -/
private opaque Erased : Type := Unit

/-- A typed key for vault access. A `Key α` can only store/retrieve values of type `α`.
    Keys are created via `Key.new` and are globally unique.

    $$\text{Key}(\alpha) \cong \mathbb{N}$$ -/
structure Key (α : Type) where
  private mk ::
  /-- The unique identifier for this key. -/
  private id : Nat
deriving BEq, Hashable

/-- A type-safe heterogeneous map. Values of different types can coexist,
    each accessible only through its corresponding typed `Key`.

    $$\text{Vault} = \text{HashMap}(\mathbb{N}, \text{Erased})$$ -/
structure Vault where
  private mk ::
  private store : Std.HashMap Nat Erased

namespace Key

/-- Allocate a fresh typed key. Each call returns a distinct key.
    $$\text{new} : \text{IO}(\text{Key}\ \alpha)$$ -/
def new : IO (Key α) := do
  let u ← Data.newUnique
  pure ⟨u.hashUnique⟩

end Key

namespace Vault

/-- The empty vault.
    $$\text{empty} : \text{Vault},\quad |\text{empty}| = 0$$ -/
@[inline] def empty : Vault := ⟨∅⟩

instance : Inhabited Vault := ⟨empty⟩

/-- Number of entries in the vault.
    $$\text{size}(v)$$ -/
@[inline] def size (v : Vault) : Nat := v.store.size

/-- Insert a value into the vault under the given key, unsafe impl. -/
@[inline] private unsafe def insertImpl (key : Key α) (val : α) (v : Vault) : Vault :=
  ⟨v.store.insert key.id (unsafeCast val)⟩

/-- Insert a value into the vault under the given key.
    If the key already has a value, it is replaced.
    $$\text{insert}(k, x, v) = v[k \mapsto x]$$ -/
@[implemented_by insertImpl]
opaque insert (key : Key α) (val : α) (v : Vault) : Vault

/-- Look up the value associated with a key, unsafe impl. -/
@[inline] private unsafe def lookupImpl (key : Key α) (v : Vault) : Option α :=
  v.store.get? key.id |>.map unsafeCast

/-- Look up the value associated with a key.
    Returns `none` if the key is not present.
    $$\text{lookup}(k, v) = v[k]$$ -/
@[implemented_by lookupImpl]
opaque lookup (key : Key α) (v : Vault) : Option α

/-- Delete a key and its associated value from the vault.
    $$\text{delete}(k, v) = v \setminus \{k\}$$ -/
@[inline] def delete (key : Key α) (v : Vault) : Vault :=
  ⟨v.store.erase key.id⟩

/-- Adjust the value at a key, if present.
    $$\text{adjust}(f, k, v) = \begin{cases} v[k \mapsto f(v[k])] & k \in v \\ v & k \notin v \end{cases}$$ -/
@[inline] def adjust (f : α → α) (key : Key α) (v : Vault) : Vault :=
  match v.lookup key with
  | some val => v.insert key (f val)
  | none => v

/-- Union of two vaults. Right-biased: values from `v2` take precedence.
    $$\text{union}(v_1, v_2) = v_2 \cup v_1$$ -/
def union (v1 v2 : Vault) : Vault :=
  ⟨v1.store.fold (init := v2.store) fun acc k v =>
    if acc.contains k then acc else acc.insert k v⟩

end Vault
end Data
