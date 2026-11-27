/- The shared contract imported by the compiler fixture and its Lean consumer. -/
import Linen.System.Cell

namespace Integration.CellContract

open Lean (fromJson? toJson)
open System.Cell

/-- Two natural numbers in order, with a natural result and no effects. -/
abbrev addition : Signature where
  Input := Nat × Nat
  Output _ := Nat
  effects := []

/-- Hash one UTF-8 fixture identifier. -/
def hash (text : String) : IO Digest := Digest.ofBytes text.toUTF8

/-- Runtime used independently by the fixture compiler and consumer. -/
def runtime : IO Runtime := do
  return {
    leanToolchain := "leanprover/lean4:v4.36.0"
    target := System.Platform.target
    runtimeAbi := "integration-cell-worker-v1"
    auditVersion := "integration-audit-v1"
    codecSha256 := ⟨← hash "two ordered Lean JSON naturals, one Lean JSON natural"⟩ }

/-- The fixture’s pinned addition contract identity. -/
def contract : IO (Ref .contract) := return ⟨← hash "Nat × Nat to Eff [] Nat"⟩

/-- Typed positional JSON codecs for the addition contract. -/
def codecs (contract : Ref .contract) (runtime : Runtime) : Codecs addition where
  contract
  id := runtime.codecSha256
  encodeInput | (left, right) => [toJson left, toJson right]
  decodeInput
    | [left, right] => return (← fromJson? left, ← fromJson? right)
    | _ => .error "addition needs two naturals"
  encodeOutput _ := toJson
  decodeOutput _ := fromJson?

end Integration.CellContract
