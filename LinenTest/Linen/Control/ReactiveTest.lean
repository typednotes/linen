/-
  Tests for `Linen.Control.Reactive`: the module's documented example, end to
  end. The details of each part are tested with `Control.Reactive.Graph`,
  `.Builder` and `.Run`.
-/
import Linen.Control.Reactive

open Control.Reactive

namespace Tests.Control.Reactive

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩

/-- The example of the module documentation, returning its input and output. -/
def sheet : Reactive Id V (Subject Nat × Observable Nat) := do
  node clicks ← subject Nat
  node doubled ← clicks.map (· * 2)
  node total ← doubled.scan 0 (· + ·)
  node quiet ← total.debounceTime 100
  let out ← combineLatest (fun (a b : Nat) => a + b) quiet doubled
  pure (clicks, out)

def clicks := sheet.result.1
def out := sheet.result.2

def trace : Trace V :=
  sheet.graph!.run [.next clicks 0 (1 : Nat), .next clicks 30 (2 : Nat), .complete clicks 500]

/-- A node's values by label. -/
def valuesOf (l : Lean.Name) : List (Time × Nat) :=
  ((trace.find? (`Tests.Control.Reactive.sheet ++ l)).getD []).filterMap fun
    | (t, .next (.inl v)) => some (t, v)
    | _ => none

-- Every node's stream is in the trace — inputs, intermediate nodes, output.
#guard valuesOf `clicks == [(0, 1), (30, 2)]
#guard valuesOf `doubled == [(0, 2), (30, 4)]
#guard valuesOf `total == [(0, 2), (30, 6)]
-- `total` settles at 30, so the debounced value comes 100 later.
#guard valuesOf `quiet == [(130, 6)]
-- The output waits for `quiet`, then follows both of its sources.
#guard trace.values out == [(130, 10)]
#guard trace.completed out
#guard trace.faults == []
#guard trace.labels.size == 5

end Tests.Control.Reactive
