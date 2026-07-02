/-
  Examples.Vault — `Data.Vault`'s type-safe heterogeneous map end-to-end.

  Mints two distinctly-typed keys with `Key.new`, stores unrelated payloads
  under each in the same `Vault`, and exercises `lookup`/`adjust`/`delete`/
  `union`/`size` — showing that each key only ever yields back the type it
  was minted for, and that a stale key (from a deleted/other vault) misses
  cleanly instead of returning garbage.

  Args: (none) -- runs every check below and exits non-zero on any mismatch
-/
import Linen.Data.Vault

open Data

namespace Examples.Vault

def demoHeterogeneousStorage : IO Bool := do
  IO.println "── Vault: two distinctly-typed keys, one map ──"
  let nameKey ← Key.new (α := String)
  let countKey ← Key.new (α := Nat)

  let v := Vault.empty
    |>.insert nameKey "linen"
    |>.insert countKey 3

  let name := v.lookup nameKey
  let count := v.lookup countKey
  IO.println s!"  lookup nameKey = {name}, lookup countKey = {count}, size = {v.size}"

  -- A key minted for the same vault but never inserted misses cleanly.
  let missingKey ← Key.new (α := Bool)
  let missing := v.lookup missingKey
  IO.println s!"  lookup on an unrelated key = {missing}"

  pure (name == some "linen" && count == some 3 && v.size == 2 && missing == none)

def demoAdjustDeleteUnion : IO Bool := do
  IO.println "── Vault: adjust / delete / union ──"
  let counterKey ← Key.new (α := Nat)
  let v1 := Vault.empty.insert counterKey 10
  let bumped := v1.adjust (· + 1) counterKey
  IO.println s!"  adjust (+1): {bumped.lookup counterKey}"

  let deleted := bumped.delete counterKey
  IO.println s!"  after delete: {deleted.lookup counterKey}, size = {deleted.size}"

  let otherKey ← Key.new (α := String)
  let v2 := Vault.empty.insert otherKey "from v2"
  let merged := Vault.union bumped v2
  IO.println s!"  union: counterKey = {merged.lookup counterKey}, otherKey = {merged.lookup otherKey}, size = {merged.size}"

  pure (bumped.lookup counterKey == some 11 &&
        deleted.lookup counterKey == none && deleted.size == 0 &&
        merged.lookup counterKey == some 11 && merged.lookup otherKey == some "from v2" &&
        merged.size == 2)

def run (_args : List String) : IO Unit := do
  let okHetero ← demoHeterogeneousStorage
  IO.println ""
  let okAdjust ← demoAdjustDeleteUnion
  if okHetero && okAdjust then
    IO.println "\nvault demo done · all checks passed"
  else
    throw (IO.userError "vault demo done · some checks failed")

end Examples.Vault
