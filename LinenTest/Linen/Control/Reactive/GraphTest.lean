/-
  Tests for `Linen.Control.Reactive.Graph`: the data of a reactive graph.

  Operators (names, functions, arities, shifting), the `WellFormed` decision
  (forward and self references, unknown functions, wrong arities), the empty
  graph, codecs, erasure (with its faithfulness theorems) and signatures.
-/
import Linen.Control.Reactive.Graph

open Control.Reactive

namespace Tests.Control.Reactive.Graph

deriving instance BEq for Except

-- ── Operators ───────────────────────────────────────────────────────────────

#guard Op.name (.map ⟨0⟩) == "map" && Op.name (.debounceTime 5) == "debounceTime"
#guard (Op.scan ⟨3⟩ ⟨4⟩).fns == [⟨3⟩, ⟨4⟩]
#guard (Op.take 2).fns == [] && (Op.zip ⟨1⟩).fns == [⟨1⟩]
-- Arities: a subject reads nothing, `merge` two, the n-ary operators at least one.
#guard Op.subject.arity 0 && !Op.subject.arity 1
#guard (Op.map ⟨0⟩).arity 1 && !(Op.map ⟨0⟩).arity 2
#guard Op.merge.arity 2 && !Op.merge.arity 1
#guard (Op.combineLatest ⟨0⟩).arity 3 && !(Op.combineLatest ⟨0⟩).arity 0
-- Shifting moves every function id, and nothing else.
#guard (Op.scan ⟨1⟩ ⟨2⟩).shiftFns 10 == .scan ⟨11⟩ ⟨12⟩
#guard (Op.take 3).shiftFns 10 == .take 3

-- ── Well-formedness ─────────────────────────────────────────────────────────

/-- A subject and a `map` of it. -/
def ok : Array Node := #[⟨.subject, []⟩, ⟨.map ⟨0⟩, [⟨0⟩]⟩]

#guard decide (WellFormed ok 1)
#guard decide (WellFormed #[] 0)
-- An unregistered function.
#guard !decide (WellFormed ok 0)
-- A forward and a self reference.
#guard !decide (WellFormed #[⟨.map ⟨0⟩, [⟨1⟩]⟩, ⟨.subject, []⟩] 1)
#guard !decide (WellFormed #[⟨.map ⟨0⟩, [⟨0⟩]⟩] 1)
-- Wrong arities: a `map` of nothing, a `merge` of one, a subject with a source.
#guard !decide (WellFormed #[⟨.map ⟨0⟩, []⟩] 1)
#guard !decide (WellFormed #[⟨.subject, []⟩, ⟨.merge, [⟨0⟩]⟩] 0)
#guard !decide (WellFormed #[⟨.subject, []⟩, ⟨.subject, [⟨0⟩]⟩] 0)

#guard decide (Labelled 2 #[`a, `b] 1 #[`f])
#guard !decide (Labelled 2 #[`a, `a] 1 #[`f])
#guard !decide (Labelled 2 #[`a] 1 #[`f])

#guard ({} : Graph Id Nat).size == 0
example : ({} : Graph Id Nat).size = 0 := Graph.size_empty

-- ── Codecs and erasure ──────────────────────────────────────────────────────

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩

#guard (Codec.decode (Codec.encode (V := V) (5 : Nat)) : Except String Nat) == .ok 5
#guard (Codec.decode (V := V) (.inr "x") : Except String Nat) == .error "expected a number"

-- A plain, a failing and an effectful function, erased.
#guard Id.run (Callable.erase (m := Id) (V := V) (fun (n : Nat) => n + 1) [.inl 1]) == .ok (some (.inl 2))
#guard Id.run (Callable.erase (m := Id) (V := V)
  (fun (n : Nat) => (if n = 0 then .error "zero" else .ok n : Except String Nat)) [.inl 0]) ==
  .error "zero"
#guard Id.run (Callable.erase (m := Id) (V := V) (fun (a b : Nat) => a * b) [.inl 3, .inl 4]) ==
  .ok (some (.inl 12))
-- Wrong arity and wrong shape are errors, not crashes.
#guard Id.run (Callable.erase (m := Id) (V := V) (fun (n : Nat) => n) []) ==
  .error "wrong number of arguments"
#guard Id.run (Callable.erase (m := Id) (V := V) (fun (n : Nat) => n) [.inr "x"]) ==
  .error "expected a number"

example (a : Nat) : Callable.erase (m := Id) (V := V) (fun (n : Nat) => n + 1) [Codec.encode a] =
    pure (.ok (some (Codec.encode (a + 1)))) :=
  Callable.erase_unary (fun (n : Nat) => n + 1) a

-- A signature is read off a function's type, independently of `m` and `V`.
example : Signature (Nat → String → Bool) [Nat, String] Bool := inferInstance
example : Signature (Nat → Except String Bool) [Nat] Bool := inferInstance

-- Hoisting keeps the structure.
example (g : Graph Id V) : (g.hoist (n := StateM Nat) fun x => pure (Id.run x)).nodes = g.nodes :=
  Graph.nodes_hoist _ g

end Tests.Control.Reactive.Graph
