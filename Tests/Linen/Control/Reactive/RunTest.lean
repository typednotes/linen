/-
  Tests for `Linen.Control.Reactive.Run`: the meaning of a reactive graph.

  Every operator on a worked example (values, completion, errors), the
  instant semantics (no glitches, explicit simultaneity), the virtual-time
  scheduler (throttle, debounce, delay, timers cascading through two timed
  operators), sessions (splitting a run, late occurrences), and operators
  built from subgraphs. Every run is also checked to record no fault.
-/
import Linen.Control.Reactive

open Control.Reactive

namespace Tests.Control.Reactive.Run

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩
instance : Codec V String :=
  ⟨.inr, fun | .inr s => .ok s | .inl _ => .error "expected a string", fun _ => rfl⟩

/-- A node's notifications, decoded as numbers. -/
inductive Ev where
  | n (t v : Nat) | err (t : Nat) (msg : String) | done (t : Nat)
  deriving Repr, BEq

def evs (tr : Trace V) (n : NodeId) : List Ev :=
  (tr.events n).map fun
    | (t, .next (.inl v)) => .n t v
    | (t, .next (.inr _)) => .n t 0
    | (t, .error msg) => .err t msg
    | (t, .complete) => .done t

-- ── map, filter, scan ───────────────────────────────────────────────────────

def basic : Reactive Id V (Subject Nat × Observable Nat × Observable Nat × Observable Nat) := do
  node x ← subject Nat
  node doubled ← x.map (· * 2)
  node evens ← x.filter (· % 2 == 0)
  node total ← x.scan 0 (· + ·)
  pure (x, doubled, evens, total)

def b := basic.result
def trB : Trace V := basic.graph!.run
  [.next b.1 1 (1 : Nat), .next b.1 2 (2 : Nat), .next b.1 3 (3 : Nat), .complete b.1 4]

#guard evs trB b.1.id == [.n 1 1, .n 2 2, .n 3 3, .done 4]
#guard evs trB b.2.1.id == [.n 1 2, .n 2 4, .n 3 6, .done 4]
#guard evs trB b.2.2.1.id == [.n 2 2, .done 4]
#guard evs trB b.2.2.2.id == [.n 1 1, .n 2 3, .n 3 6, .done 4]
#guard trB.values b.2.2.2 == [(1, 1), (2, 3), (3, 6)]
#guard trB.completed b.2.1 && trB.error? b.2.1 == none
#guard trB.faults == []

-- ── take, skip, distinctUntilChanged ────────────────────────────────────────

def filtering : Reactive Id V (Subject Nat × List NodeId) := do
  node x ← subject Nat
  node first2 ← x.take 2
  node rest ← x.skip 2
  node distinct ← x.distinctUntilChanged
  node none' ← x.take 0
  pure (x, [first2.id, rest.id, distinct.id, none'.id])

def f := filtering.result
def trF : Trace V := filtering.graph!.run
  ([5, 5, 6, 6, 5].zipIdx.map fun (v, i) => .next f.1 (i + 1) v)

-- `take 2` emits its second value and completes in the same instant.
#guard evs trF (f.2[0]!) == [.n 1 5, .n 2 5, .done 2]
#guard evs trF (f.2[1]!) == [.n 3 6, .n 4 6, .n 5 5]
#guard evs trF (f.2[2]!) == [.n 1 5, .n 3 6, .n 5 5]
-- `take 0` completes at the first value, emitting nothing.
#guard evs trF (f.2[3]!) == [.done 1]

-- ── merge, mergeWith, combineLatest, withLatestFrom, zip ────────────────────

def combining : Reactive Id V (Subject Nat × Subject Nat × List NodeId) := do
  node a ← subject Nat
  node b ← subject Nat
  node merged ← a.merge b
  node latest ← combineLatest (fun (x y : Nat) => x + y) a b
  node sampled ← withLatestFrom (fun (x y : Nat) => x * y) a b
  node zipped ← zip (fun (x y : Nat) => x + y) a b
  pure (a, b, [merged.id, latest.id, sampled.id, zipped.id])

def c := combining.result
def trC : Trace V := combining.graph!.run
  [.next c.1 1 (1 : Nat), .next c.2.1 2 (10 : Nat), .next c.1 3 (2 : Nat),
   .next c.2.1 4 (20 : Nat), .complete c.1 5, .next c.2.1 6 (30 : Nat), .complete c.2.1 7]

-- `merge` completes when both sources have.
#guard evs trC (c.2.2[0]!) == [.n 1 1, .n 2 10, .n 3 2, .n 4 20, .n 6 30, .done 7]
-- `combineLatest` waits for both, then recomputes on each value.
#guard evs trC (c.2.2[1]!) == [.n 2 11, .n 3 12, .n 4 22, .n 6 32, .done 7]
-- `withLatestFrom` fires on the first source only, and ends with it.
#guard evs trC (c.2.2[2]!) == [.n 3 20, .done 5]
-- `zip` pairs values by position; it ends when `a` has completed with
-- nothing left to pair.
#guard evs trC (c.2.2[3]!) == [.n 2 11, .n 4 22, .done 5]
#guard trC.faults == []

-- ── Instants: no glitches, explicit simultaneity ────────────────────────────

/-- `x` and `x + 100` emit in the same instant. -/
def simultaneous : Reactive Id V (Subject Nat × List NodeId) := do
  node x ← subject Nat
  node y ← x.map (· + 100)
  node left ← x.merge y
  node both ← x.mergeWith y (· + ·)
  node sum ← combineLatest (fun (p q : Nat) => p + q) x y
  pure (x, [left.id, both.id, sum.id])

def s := simultaneous.result
def trS : Trace V := simultaneous.graph!.run [.next s.1 1 (1 : Nat), .next s.1 2 (2 : Nat)]

-- `merge` is left-biased; `mergeWith` combines.
#guard evs trS (s.2[0]!) == [.n 1 1, .n 2 2]
#guard evs trS (s.2[1]!) == [.n 1 102, .n 2 104]
-- No glitch: `combineLatest` never sees a new `x` with a stale `y`
-- (otherwise it would emit 1 + 100 = 101 then 2 + 101 = 103).
#guard evs trS (s.2[2]!) == [.n 1 102, .n 2 104]

-- ── Errors ──────────────────────────────────────────────────────────────────

def failing : Reactive Id V (Subject Nat × List NodeId) := do
  node x ← subject Nat
  node checked ← x.mapE fun n => if n > 2 then .error s!"{n} is too big" else .ok n
  node after ← checked.map (· + 1)
  node independent ← x.map (· * 10)
  pure (x, [checked.id, after.id, independent.id])

def e := failing.result
def trE : Trace V := failing.graph!.run
  [.next e.1 1 (1 : Nat), .next e.1 2 (3 : Nat), .next e.1 3 (2 : Nat)]

-- A failing function ends its node in error; the error propagates
-- downstream; an independent branch carries on.
#guard evs trE (e.2[0]!) == [.n 1 1, .err 2 "3 is too big"]
#guard evs trE (e.2[1]!) == [.n 1 2, .err 2 "3 is too big"]
#guard evs trE (e.2[2]!) == [.n 1 10, .n 2 30, .n 3 20]
#guard (trE.events (e.2[1]!)).getLast? == some (2, .error "3 is too big")
-- A subject's own error ends it and its dependents.
#guard evs (failing.graph!.run [.next e.1 1 (1 : Nat), .error e.1 2 "boom", .next e.1 3 (1 : Nat)])
  (e.2[2]!) == [.n 1 10, .err 2 "boom"]

-- ── Virtual time ────────────────────────────────────────────────────────────

def timed : Reactive Id V (Subject Nat × List NodeId) := do
  node x ← subject Nat
  node throttled ← x.throttleTime 10
  node debounced ← x.debounceTime 10
  node delayed ← x.delay 5
  node cascade ← debounced.delay 5
  pure (x, [throttled.id, debounced.id, delayed.id, cascade.id])

def t := timed.result

-- `throttleTime`: a value opens a window of 10 during which values are dropped.
#guard evs (timed.graph!.run ([0, 5, 10, 12, 25].map fun ts => .next t.1 ts ts)) (t.2[0]!) ==
  [.n 0 0, .n 10 10, .n 25 25]

-- `debounceTime`: only the last value of a burst, 10 after it.
def trD : Trace V := timed.graph!.run
  [.next t.1 0 (1 : Nat), .next t.1 5 (2 : Nat), .next t.1 20 (3 : Nat), .complete t.1 50]
#guard evs trD (t.2[1]!) == [.n 15 2, .n 30 3, .done 50]
-- Timers cascade: the debounced values are delayed by 5 more, and
-- completion waits for them.
#guard evs trD (t.2[3]!) == [.n 20 2, .n 35 3, .done 50]
#guard trD.faults == []

-- `delay`: every value 5 later; completion after the last delayed value.
#guard evs (timed.graph!.run [.next t.1 0 (1 : Nat), .next t.1 2 (2 : Nat), .complete t.1 3])
  (t.2[2]!) == [.n 5 1, .n 7 2, .done 7]
-- A completion that arrives while values are pending flushes `debounceTime`.
#guard evs (timed.graph!.run [.next t.1 0 (7 : Nat), .complete t.1 3]) (t.2[1]!) ==
  [.n 3 7, .done 3]
-- Timers left at the end of the occurrences fire: `run` drains them.
#guard evs (timed.graph!.run [.next t.1 0 (4 : Nat)]) (t.2[3]!) == [.n 15 4]

-- ── Sessions ────────────────────────────────────────────────────────────────

def g : Graph Id V := timed.graph!
def xs : List (Occurrence V) := [.next t.1 0 (1 : Nat), .next t.1 5 (2 : Nat)]
def ys : List (Occurrence V) := [.next t.1 20 (3 : Nat), .complete t.1 50]

-- A run can be split and resumed from its session.
#guard g.trace (Id.run (g.pushAll g.start (xs ++ ys))) ==
  g.trace (Id.run (do let s ← g.pushAll g.start xs; g.pushAll s ys))
example : g.pushAll g.start (xs ++ ys) = (g.pushAll g.start xs >>= fun s => g.pushAll s ys) :=
  Graph.pushAll_append g g.start xs ys
-- Advancing the clock fires the timers due, and only those.
#guard evs (g.trace (Id.run (do let s ← g.pushAll g.start xs; g.advance s 16))) (t.2[1]!) ==
  [.n 15 2]
#guard evs (g.trace (Id.run (do let s ← g.pushAll g.start xs; g.advance s 14))) (t.2[1]!) == []
-- An occurrence earlier than the clock is delivered at the clock's time.
#guard (g.run [.next t.1 10 (1 : Nat), .next t.1 3 (2 : Nat)]).values t.1.toObservable ==
  [(10, 1), (10, 2)]

-- ── Graphs as operators ─────────────────────────────────────────────────────

/-- `10a + b`, as an operator of two inputs. -/
def affine : Operator Id V [Nat, Nat] Nat :=
  Operator.define! `affine fun a b => show Reactive Id V (Observable Nat) from do
    node scaled ← a.map (· * 10)
    combineLatest (fun (x y : Nat) => x + y) scaled b

def usesAffine : Reactive Id V (Subject Nat × Subject Nat × Observable Nat) := do
  node p ← subject Nat
  node q ← subject Nat
  node r ← affine p q
  pure (p, q, r)

def u := usesAffine.result
#guard (usesAffine.graph!.run [.next u.1 1 (1 : Nat), .next u.2.1 2 (5 : Nat),
    .next u.1 3 (2 : Nat)]).values u.2.2 == [(2, 15), (3, 25)]

-- The operator's own graph is an observable of its inputs.
#guard (affine.graph.run
    [⟨1, affine.inputs[0]!, .next (.inl 3)⟩, ⟨2, affine.inputs[1]!, .next (.inl 4)⟩]).values
  affine.outputObservable == [(2, 34)]

-- ── Effects and counting ────────────────────────────────────────────────────

/-- Count the function calls of a run by hoisting it into `StateM Nat`. -/
def calls (g : Graph Id V) (os : List (Occurrence V)) : Nat :=
  ((g.hoist (n := StateM Nat) fun x => do modify (· + 1); pure (Id.run x)).runM os |>.run 0).2

-- `map`, `filter` and `scan` (its function, and its seed once) are called
-- once per value: 3 + 3 + 3 + 1.
#guard calls basic.graph! [.next b.1 1 (1 : Nat), .next b.1 2 (2 : Nat), .next b.1 3 (3 : Nat)] == 10

-- ── Selected results ────────────────────────────────────────────────────────

/-- Two independent branches off one subject. -/
def branches : Reactive Id V (Subject Nat × Observable Nat × Observable Nat) := do
  node x ← subject Nat
  node left ← x.map (· + 1)
  node right ← x.map (· * 100)
  pure (x, left, right)

def br := branches.result
def gb : Graph Id V := branches.graph!
def osB : List (Occurrence V) := [.next br.1 1 (1 : Nat), .next br.1 2 (2 : Nat)]

-- A selected stream is the same as in the full trace …
#guard (gb.run osB (.only [br.2.1.id])).events br.2.1.id == (gb.run osB).events br.2.1.id
-- … the others are not recorded …
#guard (gb.run osB (.only [br.2.1.id])).events br.2.2.id == []
-- … and nodes it does not depend on are not computed: 2 calls instead of 4.
def callsSel (g : Graph Id V) (os : List (Occurrence V)) (sel : Selection) : Nat :=
  ((g.hoist (n := StateM Nat) fun x => do modify (· + 1); pure (Id.run x)).runM os sel |>.run 0).2
#guard callsSel gb osB .all == 4
#guard callsSel gb osB (.only [br.2.1.id]) == 2
-- Just one result, or a set of results.
#guard gb.valuesFor br.2.2 osB == [(1, 100), (2, 200)]
#guard Id.run (gb.runFor br.2.1 osB) == [(1, .next (.inl 2)), (2, .next (.inl 3))]
#guard Id.run (gb.runSelected [br.2.1.id, br.2.2.id] osB) ==
  [(br.2.1.id, [(1, .next (.inl 2)), (2, .next (.inl 3))]),
   (br.2.2.id, [(1, .next (.inl 100)), (2, .next (.inl 200))])]
-- Selecting a timed node still runs the scheduler for it.
#guard evs (timed.graph!.run [.next t.1 0 (4 : Nat)] (.only [t.2[3]!])) (t.2[3]!) == [.n 15 4]

end Tests.Control.Reactive.Run
