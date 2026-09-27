/-
  Tests for `Linen.System.GitFn.Reactive`: remote functions as graph nodes.

  A `Remote` here is an in-process function on JSON (the real workers, over
  stdio and HTTP, are exercised by `lake exe gitfn-integration`): it is
  registered in a graph over `Lean.Json`, applied like any function, mixed
  with local functions, and its failures become error notifications.
-/
import Linen.System.GitFn.Reactive

open Lean Control.Reactive System.GitFn

namespace Tests.System.GitFn.Reactive

/-- A stand-in for a worker: adds two numbers, or fails on anything else. -/
def adder : Remote := ⟨fun args => pure <| match args with
  | [a, b] => match a.getNat?, b.getNat? with
    | .ok x, .ok y => .ok (toJson (x + y))
    | _, _ => .error "Natural number expected"
  | _ => .error "wrong number of arguments"⟩

/-- `10 · (x + y)`, the addition done remotely. -/
def sheet : Reactive IO Json (Subject Nat × Subject Nat × Observable Nat × FnRef [Nat, Nat] Nat) := do
  node x ← subject Nat
  node y ← subject Nat
  node addFn ← Reactive.remote [Nat, Nat] Nat adder
  node sum ← addFn x y
  node scaled ← sum.map (· * 10)
  pure (x, y, scaled, addFn)

def r := sheet.result
def g : Graph IO Json := sheet.graph!

-- The remote function is an ordinary registered, labelled function.
#guard g.fnLabels.contains `Tests.System.GitFn.Reactive.sheet.addFn
#guard g.uses r.2.2.2.id == [⟨2⟩]

-- Codecs into `Lean.Json`, with their laws proven.
example (n : Nat) : Codec.decode (Codec.encode (V := Json) n) = Except.ok n := Codec.decode_encode n
#guard (Codec.encode (V := Json) (5 : Nat)) == Json.num 5
#guard (Codec.encode (V := Json) "x") == Json.str "x"

-- A run in `IO`: the remote sum, then the local scaling.
#eval show IO Unit from do
  let tr ← g.runM [.next r.1 1 (2 : Nat), .next r.2.1 2 (3 : Nat), .next r.1 3 (10 : Nat)]
  unless tr.values r.2.2.1 == [(2, 50), (3, 130)] do
    throw (IO.userError s!"unexpected values {tr.values r.2.2.1}")

-- A remote failure is an error notification of the node, propagated downstream.
#eval show IO Unit from do
  let tr ← g.runM [⟨1, r.1.id, .next (Json.str "two")⟩, .next r.2.1 2 (3 : Nat)]
  unless tr.error? r.2.2.1 == some "Natural number expected" do
    throw (IO.userError s!"expected an error, got {(tr.events r.2.2.1.id).length} events")

-- A graph over linen's JSON works too, through the bridge.
def overLinen : Reactive IO Data.Json.Value (FnRef [Nat, Nat] Nat) := Reactive.remote [Nat, Nat] Nat adder
#guard overLinen.graph!.fns.size == 1

end Tests.System.GitFn.Reactive
