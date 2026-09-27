/-
  `Control.Reactive.Builder` — writing reactive graphs

  Graphs are written in the `Reactive` monad, with ReactiveX's vocabulary:

  ```lean
  def sheet : Reactive Id V (Observable Nat) := do
    node clicks ← subject Nat                    -- an input
    node doubled ← clicks.map (· * 2)            -- operators take plain functions …
    node total ← doubled.scan 0 (· + ·)
    node quiet ← total.debounceTime 100
    combineLatest (fun (a b : Nat) => a + b) quiet doubled   -- … or n-ary ones
  ```

  - **Subjects and operators.** `subject α` creates an input. Operators are
    methods of `Observable` (`map`, `filter`, `scan`, `take`, `skip`,
    `distinctUntilChanged`, `merge`, `mergeWith`, `throttleTime`,
    `debounceTime`, `delay`) and n-ary functions (`combineLatest`,
    `withLatestFrom`, `zip`) whose arity is read off the function given. They
    take plain Lean functions; `map`/`scan` have `…Fn` variants taking an
    `FnRef` from `fn`, to share a function or `rebind` it later.
  - **Every node is a hot, shared observable.** A node is computed once per
    instant and read by all its dependents, so ReactiveX's cold/hot and
    `share` distinctions do not arise.
  - **Subgraphs are operators.** `Operator.define` turns a builder over typed
    inputs into an `Operator args β` — the graph itself, as an observable of
    its inputs — and applying it (`op x y`) splices a copy into the graph
    being written: a graph is an observable that composes like any other.
  - **Labels.** `node x ← e` labels what `e` creates after the identifier `x`
    (`sheet.x`); anything else gets a generated label (`map.1`, `subject.1`,
    `fn.1`); `scope` prefixes the labels given inside it, so that a
    sub-builder or an operator can be used twice. Duplicates fail the build.
  - **References.** `Observable α`, `Subject α` and `FnRef args β` are
    defined here, with private constructors: only this module creates them.
  - **Safety.** `build` re-establishes the graph's invariants by decision, so
    a built graph always carries its proofs; it fails only on a duplicate
    label or a reference escaped from another build.
-/
import Linen.Control.Reactive.Graph

namespace Control.Reactive

-- ── Typed references ────────────────────────────────────────────────────────

/-- A node emitting values of type `α` (ReactiveX's `Observable<α>`). -/
structure Observable (α : Type) where
  private mk ::
  /-- The node. -/
  id : NodeId
  deriving Repr, DecidableEq

/-- An input node (ReactiveX's `Subject<α>`): it is fed from outside, by
    `Occurrence`s, and is also an observable. -/
structure Subject (α : Type) extends Observable α where
  private mk ::
  deriving Repr, DecidableEq

instance {α : Type} : Coe (Subject α) (Observable α) := ⟨Subject.toObservable⟩

/-- A registered function of arguments `args` returning a `β`. -/
structure FnRef (args : List Type) (β : Type) where
  private mk ::
  /-- The function. -/
  id : FnId
  deriving Repr, DecidableEq

instance {args : List Type} {β : Type} : Signature (FnRef args β) args β := ⟨⟩

-- ── The builder ─────────────────────────────────────────────────────────────

/-- A label under construction. -/
inductive Label where
  /-- Given by `Reactive.label` (or `node x ← …`). -/
  | explicit (name : Lean.Name)
  /-- To generate, within the scope in force at creation. -/
  | auto (scope : Lean.Name)
  /-- Carried over from a spliced `Operator`; a `node` label may replace it. -/
  | embedded (name : Lean.Name)
  deriving Repr, DecidableEq, Inhabited

/-- A graph under construction. -/
structure Builder (m : Type → Type) (V : Type) where
  /-- The nodes so far. -/
  nodes : Array Node := #[]
  /-- The functions so far. -/
  fns : Array (Impl m V) := #[]
  /-- The nodes' labels so far. -/
  labels : Array Label := #[]
  /-- The functions' labels so far. -/
  fnLabels : Array Label := #[]
  /-- The prefix of every label given now. -/
  scope : Lean.Name := .anonymous
  /-- Mislabellings, reported by `build`. -/
  errors : Array String := #[]

/-- The monad a reactive graph is written in. A `def`, not an `abbrev`, so the
    underlying state is not reachable: the operations below are the only ways
    to act on the graph. -/
def Reactive (m : Type → Type) (V : Type) (α : Type) : Type := StateM (Builder m V) α

instance {m : Type → Type} {V : Type} : Monad (Reactive m V) :=
  inferInstanceAs (Monad (StateM (Builder m V)))

/-- Relabel entry `i` explicitly as `l`, or say why not. -/
private def relabelAt (kind : String) (labels : Array Label) (i : Nat) (l : Lean.Name) :
    Array Label × Option String :=
  match labels[i]? with
  | some (.explicit old) =>
    (labels, some s!"{kind} {i} is already labelled `{old}`; cannot also label it `{l}`")
  | some _ => (labels.setIfInBounds i (.explicit l), none)
  | none => (labels, some s!"{kind} {i} is not in this graph (a reference from another build?)")

/-- What `Reactive.label` can label: what creating a node or registering a
    function returns. -/
class Labelable (α : Type) where
  /-- Label the node or function `a` refers to. -/
  relabel {m : Type → Type} {V : Type} (a : α) (l : Lean.Name) : Builder m V → Builder m V

private def relabelNode {m : Type → Type} {V : Type} (i : NodeId) (l : Lean.Name)
    (b : Builder m V) : Builder m V :=
  let (labels, err) := relabelAt "node" b.labels i.idx l
  { b with labels, errors := b.errors ++ err.toArray }

instance {α : Type} : Labelable (Observable α) := ⟨fun o => relabelNode o.id⟩
instance {α : Type} : Labelable (Subject α) := ⟨fun s => relabelNode s.id⟩

instance {args : List Type} {β : Type} : Labelable (FnRef args β) where
  relabel r l b :=
    let (fnLabels, err) := relabelAt "function" b.fnLabels r.id.idx l
    { b with fnLabels, errors := b.errors ++ err.toArray }

namespace Reactive

variable {m : Type → Type} {V : Type}

/-- Append a node and return its reference. -/
def addNode (op : Op) (args : List NodeId) (β : Type) : Reactive m V (Observable β) :=
  fun b => (⟨⟨b.nodes.size⟩⟩,
    { b with nodes := b.nodes.push ⟨op, args⟩, labels := b.labels.push (.auto b.scope) })

/-- Append a function and return its id. -/
def register (impl : Impl m V) : Reactive m V FnId :=
  fun b => (⟨b.fns.size⟩,
    { b with fns := b.fns.push impl, fnLabels := b.fnLabels.push (.auto b.scope) })

/-- A new input of values of type `α` (ReactiveX's `new Subject<α>()`). -/
def subject (α : Type) : Reactive m V (Subject α) :=
  (fun (o : Observable α) => ⟨o⟩) <$> addNode .subject [] α

/-- Register a plain Lean function and return a reference to it. -/
def fn {F : Type} {args : List Type} {β : Type} [Callable m V F args β] (f : F) :
    Reactive m V (FnRef args β) :=
  (⟨·⟩) <$> register (Callable.erase f)

/-- Run `r` and label what it returns `l`, under the current scope. -/
def label {α : Type} [Labelable α] (l : Lean.Name) (r : Reactive m V α) : Reactive m V α :=
  fun b => let (a, b') := r b; (a, Labelable.relabel a (b'.scope ++ l) b')

/-- Run `r` with every label it gives prefixed by `s`. -/
def scope {α : Type} (s : Lean.Name) (r : Reactive m V α) : Reactive m V α :=
  fun b => let (a, b') := r { b with scope := b.scope ++ s }; (a, { b' with scope := b.scope })

/-- The last string component of a name: the base of a generated label. -/
private def lastString : Lean.Name → Option String
  | .str _ s   => some s
  | .num p _   => lastString p
  | .anonymous => none

/-- Resolve labels: explicit and embedded ones are kept; the `k`-th generated
    label of base `b` is `b.k`. A generated label ends in a number and a `node`
    label in a string, so the two never collide. -/
private def resolve (labels : Array Label) (base : Nat → Lean.Name → Lean.Name) :
    Array Lean.Name :=
  let step := fun (acc : Array Lean.Name × Std.HashMap Lean.Name Nat) (l : Label) =>
    let (out, counts) := acc
    match l with
    | .explicit n | .embedded n => (out.push n, counts)
    | .auto sc =>
      let b := base out.size sc
      let k := counts.getD b 0 + 1
      (out.push (.num b k), counts.insert b k)
  (labels.foldl step (#[], {})).1

/-- The first name occurring twice, if any. -/
private def firstDuplicate (names : Array Lean.Name) : Option Lean.Name :=
  (names.foldl (init := (({} : Std.HashSet Lean.Name), (none : Option Lean.Name)))
    fun (seen, dup) n => match dup with
      | some _ => (seen, dup)
      | none   => if seen.contains n then (seen, some n) else (seen.insert n, none)).2

/-- The first node that is not well scoped. -/
private def firstIllScoped (b : Builder m V) : Option Nat :=
  (List.range b.nodes.size).find? fun i =>
    !decide ((b.nodes[i]?.getD default).WellScoped i b.fns.size)

/-- Run a builder from the empty graph: its result and the graph it built.
    Generated labels are resolved here (`fn.k` for a function, and an
    operator's name numbered for a node: `subject.1`, `map.2`, …). Fails on a
    double labelling, a duplicate label, or a node that is not well scoped (a
    reference from another build). -/
def build {α : Type} (r : Reactive m V α) : Except String (α × Graph m V) := do
  let (a, b) := r {}
  if let some e := b.errors[0]? then throw e
  let fnLabels := resolve b.fnLabels fun _ sc => sc.str "fn"
  let labels := resolve b.labels fun i sc => sc.str ((b.nodes[i]?.map (·.op.name)).getD "node")
  if h : WellFormed b.nodes b.fns.size then
    if h' : Labelled b.nodes.size labels b.fns.size fnLabels then
      pure (a, ⟨b.nodes, b.fns, labels, fnLabels, h, h'⟩)
    else throw <| match firstDuplicate labels, firstDuplicate fnLabels with
      | some l, _ => s!"two nodes are labelled `{l}`"
      | _, some l => s!"two functions are labelled `{l}`"
      | none, none => "internal error: labels do not match the nodes"
  else throw s!"node {(firstIllScoped b).getD 0} is not well scoped: it reads a later node or \\
    an unknown function, or has the wrong number of sources (a reference from another build?)"

/-- The builder's result alone — typically the references it returns. -/
def result {α : Type} (r : Reactive m V α) : α := (r {}).1

/-- The graph a builder builds; panics where `build` fails. -/
def graph! {α : Type} (r : Reactive m V α) : Graph m V :=
  match r.build with
  | .ok (_, g) => g
  | .error e   => panic! e

end Reactive

export Reactive (subject fn scope)

-- ── Erasure helpers ─────────────────────────────────────────────────────────

/-- Erase a one-argument function. -/
private def erase1 {m : Type → Type} [Monad m] {V α : Type} [Codec V α]
    (k : α → m (Except String (Option V))) : Impl m V
  | [v] => match Codec.decode v with
    | .ok a => k a
    | .error e => pure (.error e)
  | _ => arityError

/-- Erase a two-argument function. -/
private def erase2 {m : Type → Type} [Monad m] {V α β : Type} [Codec V α] [Codec V β]
    (k : α → β → m (Except String (Option V))) : Impl m V
  | [v, w] => match Codec.decode v, Codec.decode w with
    | .ok a, .ok b => k a b
    | .error e, _ | _, .error e => pure (.error e)
  | _ => arityError

-- ── Operators ───────────────────────────────────────────────────────────────

namespace Observable

variable {m : Type → Type} [Monad m] {V α β σ : Type}

/-- Each value mapped by `f`. -/
def map [Codec V α] [Codec V β] (o : Observable α) (f : α → β) : Reactive m V (Observable β) := do
  let r ← Reactive.register (erase1 (m := m) fun a => pure (.ok (some (Codec.encode (V := V) (f a)))))
  Reactive.addNode (.map r) [o.id] β

/-- Each value mapped by `f`, which may fail (ending the stream in error). -/
def mapE [Codec V α] [Codec V β] (o : Observable α) (f : α → Except String β) :
    Reactive m V (Observable β) := do
  let r ← Reactive.register (erase1 (m := m) fun a => pure ((f a).map (some ∘ Codec.encode (V := V))))
  Reactive.addNode (.map r) [o.id] β

/-- Each value mapped by the effectful `f`, which may fail. -/
def mapM [Codec V α] [Codec V β] (o : Observable α) (f : α → ExceptT String m β) :
    Reactive m V (Observable β) := do
  let r ← Reactive.register (erase1 (m := m) fun a =>
    (·.map (some ∘ Codec.encode (V := V))) <$> (f a).run)
  Reactive.addNode (.map r) [o.id] β

/-- Each value mapped by a registered function. -/
def mapFn (o : Observable α) (f : FnRef [α] β) : Reactive m V (Observable β) :=
  Reactive.addNode (.map f.id) [o.id] β

/-- The values for which `p` holds. -/
def filter [Codec V α] (o : Observable α) (p : α → Bool) : Reactive m V (Observable α) := do
  let r ← Reactive.register (erase1 (m := m) fun a =>
    pure (.ok (if p a then some (Codec.encode (V := V) a) else none)))
  Reactive.addNode (.filter r) [o.id] α

/-- The running accumulation `seed, f seed a₁, f (f seed a₁) a₂, …` (emitted
    from the first value on). -/
def scan [Codec V α] [Codec V σ] (o : Observable α) (seed : σ) (f : σ → α → σ) :
    Reactive m V (Observable σ) := do
  let fr ← Reactive.register (erase2 (m := m) fun s a => pure (.ok (some (Codec.encode (V := V) (f s a)))))
  let sr ← Reactive.register (m := m) (Callable.erase (V := V) seed)
  Reactive.addNode (.scan fr sr) [o.id] σ

/-- `scan` with a registered function. -/
def scanFn [Codec V σ] (o : Observable α) (seed : σ) (f : FnRef [σ, α] σ) :
    Reactive m V (Observable σ) := do
  let sr ← Reactive.register (m := m) (Callable.erase (V := V) seed)
  Reactive.addNode (.scan f.id sr) [o.id] σ

/-- The first `n` values, then complete. -/
def take (o : Observable α) (n : Nat) : Reactive m V (Observable α) :=
  Reactive.addNode (.take n) [o.id] α

/-- All values but the first `n`. -/
def skip (o : Observable α) (n : Nat) : Reactive m V (Observable α) :=
  Reactive.addNode (.skip n) [o.id] α

/-- Values differing from the previous one emitted. -/
def distinctUntilChanged (o : Observable α) : Reactive m V (Observable α) :=
  Reactive.addNode .distinctUntilChanged [o.id] α

/-- The values of either; the left one when both emit at once. -/
def merge (a b : Observable α) : Reactive m V (Observable α) :=
  Reactive.addNode .merge [a.id, b.id] α

/-- The values of either, combined by `f` when both emit at once. -/
def mergeWith [Codec V α] (a b : Observable α) (f : α → α → α) : Reactive m V (Observable α) := do
  let r ← Reactive.register (erase2 (m := m) fun x y => pure (.ok (some (Codec.encode (V := V) (f x y)))))
  Reactive.addNode (.mergeWith r) [a.id, b.id] α

/-- A value, then nothing for `duration`. -/
def throttleTime (o : Observable α) (duration : Nat) : Reactive m V (Observable α) :=
  Reactive.addNode (.throttleTime duration) [o.id] α

/-- The last value of each burst, once `duration` has passed without another. -/
def debounceTime (o : Observable α) (duration : Nat) : Reactive m V (Observable α) :=
  Reactive.addNode (.debounceTime duration) [o.id] α

/-- Every value, `duration` later. -/
def delay (o : Observable α) (duration : Nat) : Reactive m V (Observable α) :=
  Reactive.addNode (.delay duration) [o.id] α

end Observable

-- ── n-ary operators ─────────────────────────────────────────────────────────

/-- The type of an n-ary operator applied to its sources: one `Observable` per
    argument, then the result.
    $$\text{Combine}([\alpha_1..\alpha_n], \beta) = \text{Observable}\ \alpha_1 \to
      \cdots \to \text{Reactive}\ (\text{Observable}\ \beta)$$ -/
@[reducible] def Combine (m : Type → Type) (V : Type) : List Type → Type → Type
  | [],      β => Reactive m V (Observable β)
  | α :: as, β => Observable α → Combine m V as β

/-- Collect the sources (reversed in `acc`), then run `k` on them. -/
def Combine.collect {m : Type → Type} {V β : Type} (k : List NodeId → Reactive m V (Observable β))
    (acc : List NodeId) : (args : List Type) → Combine m V args β
  | []      => k acc.reverse
  | _ :: as => fun o => Combine.collect k (o.id :: acc) as

/-- A function-like argument — a Lean function or an `FnRef` — turned into a
    registered function. -/
class IntoFn (m : Type → Type) (V : Type) (F : Type) (args : List Type) (β : Type) where
  /-- Register (or reuse) the function. -/
  intoFn : F → Reactive m V (FnRef args β)

instance {m : Type → Type} {V : Type} {args : List Type} {β : Type} :
    IntoFn m V (FnRef args β) args β := ⟨pure⟩

instance (priority := low) {m : Type → Type} {V F : Type} {args : List Type} {β : Type}
    [Callable m V F args β] : IntoFn m V F args β := ⟨Reactive.fn⟩

/-- An n-ary operator over the function `f`. -/
private def nary {m : Type → Type} {V F β : Type} {args : List Type} [IntoFn m V F args β]
    (op : FnId → Op) (f : F) : Combine m V args β :=
  Combine.collect (fun ids => do
    let r ← IntoFn.intoFn (m := m) (V := V) (args := args) (β := β) f
    Reactive.addNode (op r.id) ids β) [] args

/-- `f` of the latest value of every source, whenever one emits (once all
    have): `combineLatest (fun (a : Nat) (b : Nat) => a + b) x y`. -/
def combineLatest {m : Type → Type} {V F β : Type} {args : List Type} [Signature F args β]
    [IntoFn m V F args β] (f : F) : Combine m V args β :=
  nary .combineLatest f

/-- `f` of each value of the first source and the latest of the others. -/
def withLatestFrom {m : Type → Type} {V F β : Type} {args : List Type} [Signature F args β]
    [IntoFn m V F args β] (f : F) : Combine m V args β :=
  nary .withLatestFrom f

/-- `f` of the sources' `n`-th values. -/
def zip {m : Type → Type} {V F β : Type} {args : List Type} [Signature F args β]
    [IntoFn m V F args β] (f : F) : Combine m V args β :=
  nary .zip f

-- ── Graphs as operators ─────────────────────────────────────────────────────

/-- A graph over typed inputs with an output: the graph itself, as an operator
    `Observable α₁ → … → Observable β` (ReactiveX's `OperatorFunction`). -/
structure Operator (m : Type → Type) (V : Type) (args : List Type) (β : Type) where
  /-- The prefix of the labels of its nodes when spliced. -/
  name : Lean.Name
  /-- The graph. -/
  graph : Graph m V
  /-- Its input subjects, in argument order. -/
  inputs : List NodeId
  /-- Its output node. -/
  output : NodeId

instance {m : Type → Type} {V : Type} {args : List Type} {β : Type} :
    Inhabited (Operator m V args β) := ⟨⟨.anonymous, {}, [], ⟨0⟩⟩⟩

/-- Create the subjects of an operator's inputs and run its body on them. -/
class Subjects (args : List Type) where
  /-- The inputs' ids and the body's output. -/
  make {m : Type → Type} {V β : Type} : Combine m V args β → Reactive m V (List NodeId × Observable β)

instance : Subjects [] := ⟨fun body => do let o ← body; pure ([], o)⟩

instance {α : Type} {as : List Type} [Subjects as] : Subjects (α :: as) where
  make body := do
    let s ← Reactive.subject α
    let (ids, o) ← Subjects.make (body s.toObservable)
    pure (s.id :: ids, o)

namespace Operator

variable {m : Type → Type} {V : Type} {args : List Type} {β : Type}

/-- Build an operator from a body over its inputs:

    ```lean
    def affine : Operator Id V [Nat, Nat] Nat :=
      Operator.define! `affine fun a b => show Reactive Id V (Observable Nat) from do
        let scaled ← a.map (· * 10)
        combineLatest (fun (x y : Nat) => x + y) scaled b
    ```

    The `show … from` is needed: the body's type is `Combine m V args β`,
    which `do`-notation does not unfold to the `Reactive` it stands for. -/
def define [Subjects args] (name : Lean.Name) (body : Combine m V args β) :
    Except String (Operator m V args β) := do
  let ((ids, o), g) ← (Subjects.make body).build
  pure ⟨name, g, ids, o.id⟩

/-- `define`, panicking where it fails. -/
def define! [Subjects args] (name : Lean.Name) (body : Combine m V args β) : Operator m V args β :=
  match define name body with
  | .ok op => op
  | .error e => panic! e

/-- The operator's output, as an observable of its own graph. -/
def outputObservable (op : Operator m V args β) : Observable β := ⟨op.output⟩

/-- Splice a copy of the operator's graph into the graph being built, its
    inputs replaced by `argIds`; labels are prefixed by the scope and the
    operator's name. -/
def splice (op : Operator m V args β) (argIds : List NodeId) : Reactive m V (Observable β) :=
  fun b =>
    let fnBase := b.fns.size
    let pfx := b.scope ++ op.name
    let b := { b with
      fns := b.fns ++ op.graph.fns
      fnLabels := b.fnLabels ++ op.graph.fnLabels.map fun l => .embedded (pfx ++ l) }
    let (b, ids) := (List.range op.graph.nodes.size).foldl (init := (b, (#[] : Array NodeId)))
      fun (b, ids) j =>
        match op.inputs.findIdx? (· == ⟨j⟩) with
        | some p => (b, ids.push (argIds.getD p ⟨0⟩))
        | none =>
          let n := op.graph.nodes[j]?.getD default
          let n' : Node := { op := n.op.shiftFns fnBase, args := n.args.map fun a => ids.getD a.idx ⟨0⟩ }
          ({ b with
              nodes := b.nodes.push n'
              labels := b.labels.push (.embedded (pfx ++ op.graph.label ⟨j⟩)) },
            ids.push ⟨b.nodes.size⟩)
    (⟨ids.getD op.output.idx ⟨0⟩⟩, b)

/-- Apply the operator to sources: `op x y`. -/
def apply (op : Operator m V args β) : Combine m V args β :=
  Combine.collect op.splice [] args

end Operator

/-- An operator applies like a function: `op x y` is `op.apply x y`. -/
instance {m : Type → Type} {V : Type} {args : List Type} {β : Type} :
    CoeFun (Operator m V args β) (fun _ => Combine m V args β) := ⟨Operator.apply⟩

/-- A registered function applies like `map`/`combineLatest` over it:
    `f x y` is `combineLatest f x y` for an `FnRef`. -/
instance {args : List Type} {β : Type} :
    CoeFun (FnRef args β) (fun _ => {m : Type → Type} → {V : Type} → Combine m V args β) :=
  ⟨fun f => nary .combineLatest f⟩

-- ── `node x ← e` ────────────────────────────────────────────────────────────

/-- `node x ← e`, in a `Reactive` do-block, is `let x ← e` that also labels
    what `e` creates `D.x`, where `D` is the enclosing declaration. The label
    follows the source identifier, so it survives every edit except renaming
    `x`. `node` is a non-reserved keyword: it stays usable as an identifier
    (`let node := …`, `def node`, reassigning a mutable `node ← e`). The one
    thing it takes over, in a module importing this one, is a do-block pattern
    reassignment starting with an identifier `node` (`node l r ← e`); write it
    `(.node l r) ← e`. -/
syntax (name := nodeElem) (priority := high) &"node " ident " ← " term : doElem

macro_rules
  | `(doElem| node $x:ident ← $e:term) =>
    `(doElem| let $x:ident ←
        Reactive.label (decl_name% ++ $(Lean.quote x.getId.eraseMacroScopes)) $e)

-- ── Rebinding ───────────────────────────────────────────────────────────────

/-- Replace the implementation behind a function reference — a test double, a
    remote call — leaving the structure and the labels untouched. -/
def Graph.rebind {m : Type → Type} {V F : Type} {args : List Type} {β : Type}
    [Callable m V F args β] (g : Graph m V) (r : FnRef args β) (f : F) : Graph m V :=
  ⟨g.nodes, g.fns.setIfInBounds r.id.idx (Callable.erase f), g.labels, g.fnLabels,
    by simpa using g.wellFormed, by simpa using g.labelled⟩

/-- Rebinding changes no node. -/
theorem Graph.nodes_rebind {m : Type → Type} {V F : Type} {args : List Type} {β : Type}
    [Callable m V F args β] (g : Graph m V) (r : FnRef args β) (f : F) :
    (g.rebind r f).nodes = g.nodes := rfl

/-- Rebinding changes no label. -/
theorem Graph.labels_rebind {m : Type → Type} {V F : Type} {args : List Type} {β : Type}
    [Callable m V F args β] (g : Graph m V) (r : FnRef args β) (f : F) :
    (g.rebind r f).labels = g.labels := rfl

end Control.Reactive
