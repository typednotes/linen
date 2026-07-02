/-
  Tests for `Linen.Data.Vault`.

  `Key`/`Vault` fields are private and keys are minted via `IO` (`Key.new`
  wraps a fresh `Data.Unique`), so everything is exercised through `#eval`
  over freshly allocated keys — a thrown error fails the build.
-/
import Linen.Data.Vault

open Data

namespace Tests.Data.Vault

#eval show IO Unit from do
  let k1 ← Key.new (α := Nat)
  let k2 ← Key.new (α := String)
  let k3 ← Key.new (α := Nat)

  -- empty vault has size 0 and no entries
  unless Vault.size Vault.empty == 0 do throw (IO.userError "empty vault should have size 0")
  unless Vault.lookup k1 Vault.empty == none do
    throw (IO.userError "lookup in empty vault should be none")

  -- insert / lookup round trip, keyed by type
  let v1 := Vault.insert k1 (42 : Nat) Vault.empty
  unless Vault.lookup k1 v1 == some 42 do throw (IO.userError "expected lookup k1 = some 42")
  unless Vault.size v1 == 1 do throw (IO.userError "expected size 1 after one insert")

  let v2 := Vault.insert k2 "hello" v1
  unless Vault.lookup k2 v2 == some "hello" do
    throw (IO.userError "expected lookup k2 = some \"hello\"")
  unless Vault.lookup k1 v2 == some 42 do
    throw (IO.userError "k1 should still resolve after inserting k2")
  unless Vault.size v2 == 2 do throw (IO.userError "expected size 2 after two inserts")

  -- distinct keys of the same type don't collide
  unless Vault.lookup k3 v2 == none do
    throw (IO.userError "k3 was never inserted, lookup should be none")

  -- insert on an existing key replaces the value
  let v3 := Vault.insert k1 (7 : Nat) v2
  unless Vault.lookup k1 v3 == some 7 do throw (IO.userError "expected replaced value 7")
  unless Vault.size v3 == 2 do throw (IO.userError "replacing a key should not grow the vault")

  -- delete removes the entry
  let v4 := Vault.delete k1 v3
  unless Vault.lookup k1 v4 == none do throw (IO.userError "k1 should be gone after delete")
  unless Vault.size v4 == 1 do throw (IO.userError "expected size 1 after delete")

  -- adjust modifies present keys and is a no-op on absent ones
  let v5 := Vault.adjust (· + 1) k1 v3
  unless Vault.lookup k1 v5 == some 8 do throw (IO.userError "expected adjust to bump 7 -> 8")
  let v6 := Vault.adjust (· + 1) k1 v4
  unless Vault.lookup k1 v6 == none do
    throw (IO.userError "adjust on an absent key should be a no-op")

  -- union is right-biased: v2's value for shared keys wins
  let left := Vault.insert k1 (1 : Nat) Vault.empty
  let right := Vault.insert k1 (2 : Nat) Vault.empty
  let merged := Vault.union left right
  unless Vault.lookup k1 merged == some 2 do
    throw (IO.userError "union should be right-biased")

end Tests.Data.Vault
