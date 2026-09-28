/-
  Tests for `Linen.Control.Reactive.Builder`: writing reactive graphs.

  The structure each operator builds, labels (`node x ← e`, generated labels,
  `scope`, duplicates, double labelling, `node` still an identifier),
  references from another build, graph queries, registered functions
  (`mapFn`, `scanFn`, an `FnRef` applied like a function, `rebind`),
  operators spliced from subgraphs, and the typing discipline.
-/
import Linen.Control.Reactive

open Control.Reactive

namespace Tests.Control.Reactive.Builder

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩
instance : Codec V String :=
  ⟨.inr, fun | .inr s => .ok s | .inl _ => .error "expected a string", fun _ => rfl⟩

/-- A label under a declaration of this test. -/
def L (d n : Lean.Name) : Lean.Name := `Tests.Control.Reactive.Builder ++ d ++ n

-- ── Structure ───────────────────────────────────────────────────────────────

def sheet : Reactive Id V (Observable Nat) := do
  node clicks ← subject Nat
  node doubled ← clicks.map (· * 2)
  node total ← doubled.scan 0 (· + ·)
  let quiet ← total.debounceTime 100
  combineLatest (fun (a b : Nat) => a + b) quiet doubled

def g : Graph Id V := sheet.graph!

#guard g.nodes == #[⟨.subject, []⟩, ⟨.map ⟨0⟩, [⟨0⟩]⟩, ⟨.scan ⟨1⟩ ⟨2⟩, [⟨1⟩]⟩,
  ⟨.debounceTime 100, [⟨2⟩]⟩, ⟨.combineLatest ⟨3⟩, [⟨3⟩, ⟨1⟩]⟩]
-- `node` labels follow the identifiers; the others are generated from the
-- operator's name (functions: `fn.k`).
#guard g.labels == #[L `sheet `clicks, L `sheet `doubled, L `sheet `total,
  .num `debounceTime 1, .num `combineLatest 1]
#guard g.fnLabels == #[.num `fn 1, .num `fn 2, .num `fn 3, .num `fn 4]

-- ── Queries ─────────────────────────────────────────────────────────────────

#guard g.subjects == [⟨0⟩]
#guard g.sources == [⟨0⟩] && g.sinks == [⟨4⟩]
#guard g.dependents ⟨1⟩ == [⟨2⟩, ⟨4⟩]
#guard g.dependencies ⟨4⟩ == [⟨3⟩, ⟨1⟩]
#guard g.ancestors ⟨4⟩ == [⟨0⟩, ⟨1⟩, ⟨2⟩, ⟨3⟩]
#guard g.descendants ⟨2⟩ == [⟨3⟩, ⟨4⟩]
#guard g.uses ⟨2⟩ == [⟨2⟩]
#guard g.find? (L `sheet `total) == some ⟨2⟩ && g.findFn? (.num `fn 4) == some ⟨3⟩

-- ── Labels ──────────────────────────────────────────────────────────────────

/-- `node` remains an ordinary identifier. -/
def identNode : Reactive Id V (Observable Nat) := do
  let node ← subject Nat
  node twice ← node.map (· * 2)
  pure twice

#guard identNode.graph!.labels == #[.num `subject 1, L `identNode `twice]

/-- Shadowing reuses a label: the build says which. -/
def shadowed : Reactive Id V (Observable Nat) := do
  node x ← subject Nat
  node x ← x.map (· + 1)
  pure x

#guard match shadowed.build with
  | .error e => e == s!"two nodes are labelled `{L `shadowed `x}`"
  | .ok _ => false

/-- Labelling a node that already has a label is reported. -/
def relabelled : Reactive Id V (Observable Nat) := do
  node x ← subject Nat
  node y ← pure x.toObservable
  pure y

#guard match relabelled.build with
  | .error e => e == s!"node 0 is already labelled `{L `relabelled `x}`; \
      cannot also label it `{L `relabelled `y}`"
  | .ok _ => false

/-- A sub-builder, used twice: without `scope` its labels collide. -/
def sub (x : Observable Nat) : Reactive Id V (Observable Nat) := do
  node inc ← x.map (· + 1)
  pure inc

def subsClash : Reactive Id V (Observable Nat) := do
  node x ← subject Nat
  let _ ← sub x
  sub x

#guard match subsClash.build with
  | .error e => e == s!"two nodes are labelled `{L `sub `inc}`"
  | .ok _ => false

def subs : Reactive Id V (Observable Nat) := do
  node x ← subject Nat
  let _ ← scope `left (sub x)
  scope `right (sub x)

#guard subs.graph!.labels == #[L `subs `x, `left ++ L `sub `inc, `right ++ L `sub `inc]

/-- A reference escaped from a bigger build is out of range here. -/
def escaped : Subject Nat := (do let _ ← subject Nat; subject Nat : Reactive Id V _).result

def usesEscaped : Reactive Id V (Observable Nat) := escaped.map (· + 1)

#guard match usesEscaped.build with
  | .error e => e.startsWith "node 0 is not well scoped"
  | .ok _ => false

-- ── Registered functions and rebinding ──────────────────────────────────────

def withFn : Reactive Id V (Subject Nat × Observable Nat × Observable Nat × FnRef [Nat] Nat) := do
  node double ← fn fun (n : Nat) => 2 * n
  node x ← subject Nat
  node y ← x.mapFn double
  node z ← double x                            -- an `FnRef` applies like a function
  pure (x, y, z, double)

def w := withFn.result
def gw : Graph Id V := withFn.graph!

-- Both nodes call the one registered function.
#guard gw.uses w.2.2.2.id == [⟨1⟩, ⟨2⟩]
#guard (gw.run [.next w.1 1 (5 : Nat)]).values w.2.1 == [(1, 10)]
#guard (gw.run [.next w.1 1 (5 : Nat)]).values w.2.2.1 == [(1, 10)]
-- Rebinding swaps the implementation everywhere, and nothing else.
def tripled : Graph Id V := gw.rebind w.2.2.2 fun (n : Nat) => 3 * n
#guard tripled.nodes == gw.nodes && tripled.labels == gw.labels
#guard (tripled.run [.next w.1 1 (5 : Nat)]).values w.2.2.1 == [(1, 15)]
example : tripled.nodes = gw.nodes := Graph.nodes_rebind _ _ _

-- ── Graphs as operators ─────────────────────────────────────────────────────

def affine : Operator Id V [Nat, Nat] Nat :=
  Operator.define! `affine fun a b => show Reactive Id V (Observable Nat) from do
    node scaled ← a.map (· * 10)
    combineLatest (fun (x y : Nat) => x + y) scaled b

-- The operator's own graph: two input subjects, a map, a combineLatest.
#guard affine.graph.nodes.size == 4 && affine.inputs == [⟨0⟩, ⟨1⟩] && affine.output == ⟨3⟩

def usesAffine : Reactive Id V (Observable Nat) := do
  node p ← subject Nat
  node q ← subject Nat
  node r ← affine p q
  pure r

def ga : Graph Id V := usesAffine.graph!

-- Spliced: the inputs are replaced by the arguments (no copy of the subjects),
-- the functions are appended, and the internal labels are prefixed by the
-- operator's name; the output takes the `node` label.
#guard ga.nodes == #[⟨.subject, []⟩, ⟨.subject, []⟩, ⟨.map ⟨0⟩, [⟨0⟩]⟩,
  ⟨.combineLatest ⟨1⟩, [⟨2⟩, ⟨1⟩]⟩]
#guard ga.labels == #[L `usesAffine `p, L `usesAffine `q, `affine ++ L `affine `scaled,
  L `usesAffine `r]

-- ── Typing is static ────────────────────────────────────────────────────────

-- A function of the wrong argument type does not elaborate.
/--
error: Application type mismatch: The argument
  fun s => s ++ "!"
has type
  String → String
but is expected to have type
  Nat → String
in the application
  x.map fun s => s ++ "!"
-/
#guard_msgs in
example : Reactive Id V (Observable String) := do
  let x ← subject Nat
  (x : Observable Nat).map fun (s : String) => s ++ "!"

end Tests.Control.Reactive.Builder
