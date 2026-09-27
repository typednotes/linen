/-
  `Control.Reactive.Graph` — the data of a reactive graph

  A reactive graph is a DAG of **observables**: each node is a stream of
  notifications (`next v`, then possibly `error e` or `complete`), produced
  either by a `subject` (an input, fed from outside) or by an **operator**
  (`map`, `filter`, `scan`, `merge`, `combineLatest`, `zip`, `debounceTime`,
  …) applied to earlier nodes. This module is the first-order data of such a
  graph — the operators (`Op`), the nodes, the function table, the labels —
  with the proofs that make it well formed; `Control.Reactive.Run` gives it
  its meaning and `Control.Reactive.Builder` is how it is written.

  ## References

  Nodes and functions are referred to by allocated ids (`NodeId`, `FnId`), the
  index-wrapper idiom of Lean's compiler IR. Their typed forms — `Observable α`,
  `Subject α`, `FnRef args β` — live in `Control.Reactive.Builder`, the only
  code that creates them.

  ## Invariants carried by every `Graph`

  - `WellFormed`: every node reads only **earlier** nodes (so the graph is
    acyclic and creation order is a topological order), uses only registered
    functions, and has the number of arguments its operator takes.
  - `Labelled`: every node and function has a `Lean.Name` label, unique among
    its kind — identity across versions of the graph, for serialisation and
    display.

  ## Values

  Functions are stored erased to a uniform value type `V` (`Impl m V`). A
  `Codec V α` moves values in and out of `V`; its round-trip law is part of
  the class, so a lossy encoding cannot be declared. `Callable` reads a plain
  Lean function's signature off its type.
-/
import Std.Data.HashMap
import Std.Data.HashSet

namespace Control.Reactive

-- ── References ──────────────────────────────────────────────────────────────

/-- A reference to a node: its index in `Graph.nodes`. -/
structure NodeId where
  /-- The node's index. -/
  idx : Nat
  deriving Repr, DecidableEq, Hashable, Ord, Inhabited

/-- A reference to a function: its index in `Graph.fns`. -/
structure FnId where
  /-- The function's index. -/
  idx : Nat
  deriving Repr, DecidableEq, Hashable, Ord

/-- Timestamps of the virtual clock (e.g. milliseconds). Only their order and
    differences matter. -/
abbrev Time := Nat

-- ── Operators ───────────────────────────────────────────────────────────────

/-- What a node does: the ReactiveX operator it applies to its arguments.
    Functions are referred to by id; the node's arguments are its sources. -/
inductive Op where
  /-- An input, fed from outside. -/
  | subject
  /-- Each value mapped by `f` (which may also fail). -/
  | map (f : FnId)
  /-- The values for which `p` holds. -/
  | filter (p : FnId)
  /-- The running accumulation of `f` from the value of `seed`. -/
  | scan (f : FnId) (seed : FnId)
  /-- The first `n` values, then complete. -/
  | take (n : Nat)
  /-- All values but the first `n`. -/
  | skip (n : Nat)
  /-- Values differing from the previous one emitted. -/
  | distinctUntilChanged
  /-- The values of either source; left-biased when both emit at once. -/
  | merge
  /-- The values of either source, combined by `f` when both emit at once. -/
  | mergeWith (f : FnId)
  /-- `f` of the latest value of every source, whenever one emits. -/
  | combineLatest (f : FnId)
  /-- `f` of each value of the first source and the latest of the others. -/
  | withLatestFrom (f : FnId)
  /-- `f` of the `n`-th values of all sources. -/
  | zip (f : FnId)
  /-- A value, then nothing for `duration`. -/
  | throttleTime (duration : Nat)
  /-- The last value of each burst, once `duration` has passed without another. -/
  | debounceTime (duration : Nat)
  /-- Every value, `duration` later. -/
  | delay (duration : Nat)
  deriving Repr, DecidableEq, Inhabited

namespace Op

/-- The operator's ReactiveX name. -/
def name : Op → String
  | .subject => "subject" | .map _ => "map" | .filter _ => "filter" | .scan _ _ => "scan"
  | .take _ => "take" | .skip _ => "skip" | .distinctUntilChanged => "distinctUntilChanged"
  | .merge => "merge" | .mergeWith _ => "mergeWith" | .combineLatest _ => "combineLatest"
  | .withLatestFrom _ => "withLatestFrom" | .zip _ => "zip"
  | .throttleTime _ => "throttleTime" | .debounceTime _ => "debounceTime" | .delay _ => "delay"

/-- The functions the operator calls. -/
def fns : Op → List FnId
  | .map f | .filter f | .mergeWith f | .combineLatest f | .withLatestFrom f | .zip f => [f]
  | .scan f s => [f, s]
  | _ => []

/-- Whether the operator takes `n` sources. -/
def arity : Op → Nat → Bool
  | .subject, n => n == 0
  | .merge, n | .mergeWith _, n => n == 2
  | .combineLatest _, n | .withLatestFrom _, n | .zip _, n => n ≥ 1
  | _, n => n == 1

/-- The same operator with every function id moved up by `k` (for splicing a
    graph's function table after another's). -/
def shiftFns (k : Nat) : Op → Op
  | .map f => .map ⟨f.idx + k⟩ | .filter f => .filter ⟨f.idx + k⟩
  | .scan f s => .scan ⟨f.idx + k⟩ ⟨s.idx + k⟩ | .mergeWith f => .mergeWith ⟨f.idx + k⟩
  | .combineLatest f => .combineLatest ⟨f.idx + k⟩
  | .withLatestFrom f => .withLatestFrom ⟨f.idx + k⟩ | .zip f => .zip ⟨f.idx + k⟩
  | op => op

end Op

-- ── Graphs ──────────────────────────────────────────────────────────────────

/-- One node: an operator and its sources, in order. -/
structure Node where
  /-- The operator. -/
  op : Op
  /-- The sources it reads. -/
  args : List NodeId := []
  deriving Repr, DecidableEq, Inhabited

/-- A node at index `i` of a graph with `k` functions reads only earlier
    nodes, calls only registered functions, and has its operator's arity:
    $$\text{args} < i \ \wedge\ \text{fns} < k \ \wedge\ \text{arity}(|\text{args}|).$$ -/
def Node.WellScoped (n : Node) (i k : Nat) : Prop :=
  (∀ f ∈ n.op.fns, f.idx < k) ∧ (∀ a ∈ n.args, a.idx < i) ∧ n.op.arity n.args.length = true

instance (n : Node) (i k : Nat) : Decidable (n.WellScoped i k) := by
  unfold Node.WellScoped; infer_instance

/-- Every node is well scoped:
    $$\forall i < |\text{nodes}|,\ \text{WellScoped}(\text{nodes}_i, i, k).$$
    This is acyclicity, stated so that creation order is a topological order. -/
def WellFormed (nodes : Array Node) (fnCount : Nat) : Prop :=
  ∀ i (h : i < nodes.size), (nodes[i]'h).WellScoped i fnCount

instance (nodes : Array Node) (k : Nat) : Decidable (WellFormed nodes k) :=
  Nat.decidableBallLT _ _

/-- One label per node and one per function, distinct within each. -/
def Labelled (n : Nat) (labels : Array Lean.Name) (k : Nat) (fnLabels : Array Lean.Name) : Prop :=
  labels.size = n ∧ fnLabels.size = k ∧ labels.toList.Nodup ∧ fnLabels.toList.Nodup

instance (n : Nat) (labels : Array Lean.Name) (k : Nat) (fnLabels : Array Lean.Name) :
    Decidable (Labelled n labels k fnLabels) := by
  unfold Labelled; infer_instance

/-- A function erased to the value type `V`: it receives its arguments in order
    and returns a value, no value (`none`, how a predicate drops one), or an
    error. -/
abbrev Impl (m : Type → Type) (V : Type) : Type := List V → m (Except String (Option V))

/-- A reactive graph: a function table and nodes in creation order, well formed
    and uniquely labelled by construction. -/
structure Graph (m : Type → Type) (V : Type) where
  /-- The nodes; `NodeId` `i` is `nodes[i]`. -/
  nodes : Array Node
  /-- The functions; `FnId` `i` is `fns[i]`. -/
  fns : Array (Impl m V)
  /-- The nodes' labels, by index. -/
  labels : Array Lean.Name
  /-- The functions' labels, by index. -/
  fnLabels : Array Lean.Name
  /-- Every node reads earlier nodes and registered functions, with its arity. -/
  wellFormed : WellFormed nodes fns.size
  /-- Every node and function has one label, unique among its kind. -/
  labelled : Labelled nodes.size labels fns.size fnLabels

/-- The empty graph. -/
instance {m : Type → Type} {V : Type} : EmptyCollection (Graph m V) :=
  ⟨⟨#[], #[], #[], #[], fun _ h => absurd h (Nat.not_lt_zero _),
    ⟨rfl, rfl, List.nodup_nil, List.nodup_nil⟩⟩⟩

instance {m : Type → Type} {V : Type} : Inhabited (Graph m V) := ⟨{}⟩

namespace Graph

variable {m : Type → Type} {V : Type}

/-- $$\text{size}(g) = |g.\text{nodes}|$$ -/
def size (g : Graph m V) : Nat := g.nodes.size

/-- The node a reference refers to, if any. -/
def node? (g : Graph m V) (i : NodeId) : Option Node := g.nodes[i.idx]?

/-- Every node, in creation (= evaluation) order. -/
def ids (g : Graph m V) : List NodeId := (List.range g.size).map (⟨·⟩)

/-- The sources of node `i` (`[]` for a subject or an absent node). -/
def argsOf (g : Graph m V) (i : NodeId) : List NodeId := ((g.node? i).map Node.args).getD []

/-- A node's label (`anonymous` for an absent node). -/
def label (g : Graph m V) (i : NodeId) : Lean.Name := g.labels[i.idx]?.getD .anonymous

/-- A function's label (`anonymous` for an absent function). -/
def fnLabel (g : Graph m V) (f : FnId) : Lean.Name := g.fnLabels[f.idx]?.getD .anonymous

/-- The node labelled `l`, if any (labels are unique). -/
def find? (g : Graph m V) (l : Lean.Name) : Option NodeId :=
  (g.labels.findIdx? (· == l)).map (⟨·⟩)

/-- The function labelled `l`, if any (labels are unique). -/
def findFn? (g : Graph m V) (l : Lean.Name) : Option FnId :=
  (g.fnLabels.findIdx? (· == l)).map (⟨·⟩)

/-- The subjects (inputs), in order. -/
def subjects (g : Graph m V) : List NodeId :=
  g.ids.filter fun i => ((g.node? i).map (·.op)) == some .subject

/-- The nodes calling function `f`, in order. -/
def uses (g : Graph m V) (f : FnId) : List NodeId :=
  g.ids.filter fun i => ((g.node? i).map fun n => n.op.fns.contains f).getD false

/-- The direct dependencies of node `i`: the distinct nodes it reads. -/
def dependencies (g : Graph m V) (i : NodeId) : List NodeId := (g.argsOf i).eraseDups

/-- The direct dependents of node `i`: the nodes that read it. -/
def dependents (g : Graph m V) (i : NodeId) : List NodeId :=
  g.ids.filter fun j => i ∈ g.argsOf j

/-- Sources: nodes that read nothing (the subjects). -/
def sources (g : Graph m V) : List NodeId := g.ids.filter fun i => g.argsOf i == []

/-- Sinks: nodes nothing reads. -/
def sinks (g : Graph m V) : List NodeId := g.ids.filter fun i => g.dependents i == []

/-- Mark, in one backward sweep, every node the `seeds` transitively read. -/
def upstreamMarks (g : Graph m V) (seeds : List NodeId) : Array Bool :=
  let init := seeds.foldl (fun mk i => mk.setIfInBounds i.idx true) (Array.replicate g.size false)
  (List.range g.size).reverse.foldl (fun mk i =>
    if mk[i]?.getD false then
      (g.argsOf ⟨i⟩).foldl (fun mk a => mk.setIfInBounds a.idx true) mk
    else mk) init

/-- Mark, in one forward sweep, every node that transitively reads a seed. -/
def downstreamMarks (g : Graph m V) (seeds : List NodeId) : Array Bool :=
  let init := seeds.foldl (fun mk i => mk.setIfInBounds i.idx true) (Array.replicate g.size false)
  (List.range g.size).foldl (fun mk i =>
    if (g.argsOf ⟨i⟩).any (fun a => mk[a.idx]?.getD false) then mk.setIfInBounds i true
    else mk) init

/-- The nodes set in a mark array, ascending. -/
private def marked (mk : Array Bool) : List NodeId :=
  ((List.range mk.size).filter fun i => mk[i]?.getD false).map (⟨·⟩)

/-- Every node `i` transitively depends on, excluding `i`, ascending. -/
def ancestors (g : Graph m V) (i : NodeId) : List NodeId :=
  (marked (g.upstreamMarks [i])).filter (· != i)

/-- Every node that transitively depends on `i`, excluding `i`, ascending. -/
def descendants (g : Graph m V) (i : NodeId) : List NodeId :=
  (marked (g.downstreamMarks [i])).filter (· != i)

/-- Run the same graph in another monad. -/
def hoist {n : Type → Type} (h : ∀ {α}, m α → n α) (g : Graph m V) : Graph n V :=
  ⟨g.nodes, g.fns.map (fun f vs => h (f vs)), g.labels, g.fnLabels,
    by simpa using g.wellFormed, by simpa using g.labelled⟩

end Graph

-- ── Codecs and callables ────────────────────────────────────────────────────

/-- A faithful encoding of `α` into the value type `V`. -/
class Codec (V : Type) (α : Type) where
  /-- Encode a value. -/
  encode : α → V
  /-- Decode a value, failing on one of the wrong shape. -/
  decode : V → Except String α
  /-- Decoding an encoded value gives it back. -/
  decode_encode : ∀ a, decode (encode a) = .ok a

/-- The value type encodes itself. -/
instance {V : Type} : Codec V V := ⟨id, .ok, fun _ => rfl⟩

/-- An erased function called with the wrong number of arguments. -/
def arityError {m : Type → Type} [Monad m] {V : Type} : m (Except String (Option V)) :=
  pure (.error "wrong number of arguments")

/-- A Lean function type `F`, of arguments `args` and result `β`, that can be
    erased to `Impl m V`. The signature is read off `F` itself. The result may
    be a plain `β`, an `Except String β` or an `ExceptT String m β`. -/
class Callable (m : Type → Type) (V : Type) (F : Type)
    (args : outParam (List Type)) (β : outParam Type) where
  /-- The erased function. -/
  erase : F → Impl m V

/-- A pure, total result (low priority: only when nothing more specific applies). -/
instance (priority := low) {m : Type → Type} {V β : Type} [Monad m] [Codec V β] :
    Callable m V β [] β where
  erase b vs := match vs with
    | [] => pure (.ok (some (Codec.encode b)))
    | _  => arityError

/-- A pure result that may fail. -/
instance {m : Type → Type} {V β : Type} [Monad m] [Codec V β] :
    Callable m V (Except String β) [] β where
  erase r vs := match vs with
    | [] => pure (r.map (some ∘ Codec.encode))
    | _  => arityError

/-- An effectful result that may fail. -/
instance {m : Type → Type} {V β : Type} [Monad m] [Codec V β] :
    Callable m V (ExceptT String m β) [] β where
  erase r vs := match vs with
    | [] => (·.map (some ∘ Codec.encode)) <$> r.run
    | _  => arityError

/-- One more argument, decoded from `V`. -/
instance {m : Type → Type} {V α F : Type} {as : List Type} {β : Type} [Monad m] [Codec V α]
    [Callable m V F as β] : Callable m V (α → F) (α :: as) β where
  erase f vs := match vs with
    | v :: vs => match Codec.decode v with
      | .ok a    => Callable.erase (f a) vs
      | .error e => pure (.error e)
    | [] => arityError

/-- The signature of a function-like `F` — a Lean function or an `FnRef`
    (whose instance is in `Control.Reactive.Builder`) —
    independent of the monad and value type, so that it can be read before
    they are known (it fixes the arity of `combineLatest f x y …`). -/
class Signature (F : Type) (args : outParam (List Type)) (β : outParam Type) : Prop

instance {α F β : Type} {as : List Type} [Signature F as β] : Signature (α → F) (α :: as) β := ⟨⟩
instance {β : Type} : Signature (Except String β) [] β := ⟨⟩
instance {m : Type → Type} {β : Type} : Signature (ExceptT String m β) [] β := ⟨⟩
instance (priority := low) {β : Type} : Signature β [] β := ⟨⟩

-- ── Properties ──────────────────────────────────────────────────────────────

/-- The empty graph has no nodes. -/
theorem Graph.size_empty {m : Type → Type} {V : Type} : ({} : Graph m V).size = 0 := rfl

/-- Hoisting changes no node. -/
theorem Graph.nodes_hoist {m n : Type → Type} {V : Type} (h : ∀ {α}, m α → n α)
    (g : Graph m V) : (g.hoist h).nodes = g.nodes := rfl

/-- In a graph, labels identify nodes. -/
theorem Graph.label_injective {m : Type → Type} {V : Type} (g : Graph m V) {i j : Nat}
    (hi : i < g.labels.size) (hj : j < g.labels.size) (h : g.labels[i] = g.labels[j]) :
    i = j := by
  have hnd := g.labelled.2.2.1
  have hi' : i < g.labels.toList.length := by simpa using hi
  have hj' : j < g.labels.toList.length := by simpa using hj
  exact (List.Nodup.getElem_inj hnd (hi := hi') (hj := hj')).mp (by simpa using h)

/-- Erasure is faithful: an erased unary function called on an encoded argument
    returns the encoded result. -/
theorem Callable.erase_unary {m : Type → Type} {V α β : Type} [Monad m] [Codec V α] [Codec V β]
    (f : α → β) (a : α) :
    Callable.erase (m := m) (V := V) f [Codec.encode a] = pure (.ok (some (Codec.encode (f a)))) := by
  simp [Callable.erase, Codec.decode_encode]

/-- The same for a binary function. -/
theorem Callable.erase_binary {m : Type → Type} {V α β γ : Type} [Monad m] [Codec V α]
    [Codec V β] [Codec V γ] (f : α → β → γ) (a : α) (b : β) :
    Callable.erase (m := m) (V := V) f [Codec.encode a, Codec.encode b] =
      pure (.ok (some (Codec.encode (f a b)))) := by
  simp [Callable.erase, Codec.decode_encode]

end Control.Reactive
