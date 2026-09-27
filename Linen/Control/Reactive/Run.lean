/-
  `Control.Reactive.Run` — what a reactive graph means: streams over virtual time

  A run feeds a graph a sequence of **occurrences** — a notification (`next`,
  `error`, `complete`) delivered to a subject at a time — and returns the
  **trace**: every node's stream of timestamped notifications, the
  intermediate nodes' as well as the outputs'. A run can instead return only
  some results (`Selection.only`, `runFor`, `valuesFor`, `runSelected`): the
  nodes none of them depends on are then not computed at all, which cannot
  change the selected streams, since a node reads only its ancestors.

  ## Semantics (reactive-banana's, with ReactiveX's notifications)

  - Time advances in **instants**. An instant is one occurrence, or one timer
    of a timed operator firing. Within an instant every node is visited once,
    in creation order (a topological order), so a node sees what its sources
    emitted in the same instant — there are no glitches.
  - In an instant a node emits **at most one** `next`, possibly followed by a
    terminal notification (`error` or `complete`), after which it emits
    nothing more. Simultaneity is explicit, as in reactive-banana: when both
    sources of `merge` emit at once the left one wins, and `mergeWith f`
    combines them.
  - Errors are ReactiveX's: a function that fails, or a source's `error`,
    terminates the node with an `error` that propagates downstream; branches
    that do not depend on it carry on.
  - `throttleTime` only needs timestamps. `debounceTime` and `delay` schedule
    their own emissions: timers are kept per node, and fire before any
    occurrence at the same or a later time (earliest first, ties by node
    order). `run` ends by letting time pass until no timer is left.
  - A run is deterministic: the same graph and occurrences give the same
    trace, so a recorded log of occurrences replays exactly — which is also
    how a new version of a graph is brought up to date.

  ## Termination of the scheduler

  Firing the timer of node `k` makes only nodes after `k` act (their sources
  come first), each adding at most one pending timer, while `k` loses one. So
  the weight $\sum_i \text{pending}_i \cdot 2^{N-i}$ strictly decreases with
  every timer fired. `fireDue` recurses on that weight, checking the decrease
  as it goes: the recursion is total by construction, and should the argument
  ever fail it stops and records a `fault` instead of looping. (The decrease
  is argued here and checked at run time, not proven.)
-/
import Linen.Control.Reactive.Builder

namespace Control.Reactive

-- ── Notifications ───────────────────────────────────────────────────────────

/-- What an observable emits (ReactiveX's `Notification`). -/
inductive Notification (V : Type) where
  /-- A value. -/
  | next (v : V)
  /-- The stream failed; nothing follows. -/
  | error (message : String)
  /-- The stream ended; nothing follows. -/
  | complete
  deriving Repr, BEq, DecidableEq

/-- How a stream ends. -/
inductive Terminal where
  /-- In failure. -/
  | error (message : String)
  /-- Normally. -/
  | complete
  deriving Repr, BEq, DecidableEq

/-- What one node emits in one instant: at most one value, then possibly how
    it ends. -/
structure Emission (V : Type) where
  /-- The value, if any. -/
  next : Option V := none
  /-- The end, if the stream ends now. -/
  terminal : Option Terminal := none

/-- The notifications of an emission, in order. -/
def Emission.notifications {V : Type} (e : Emission V) : List (Notification V) :=
  e.next.toList.map .next ++ e.terminal.toList.map fun
    | .error msg => .error msg
    | .complete => .complete

/-- A notification delivered to a subject at a time. -/
structure Occurrence (V : Type) where
  /-- When. -/
  time : Time
  /-- To which subject. -/
  subject : NodeId
  /-- What. -/
  event : Notification V
  deriving Repr, BEq

namespace Occurrence

variable {V α : Type}

/-- A value for a subject. -/
def next [Codec V α] (s : Subject α) (time : Time) (a : α) : Occurrence V :=
  ⟨time, s.id, .next (Codec.encode a)⟩

/-- A failure of a subject. -/
def error (s : Subject α) (time : Time) (message : String) : Occurrence V :=
  ⟨time, s.id, .error message⟩

/-- The end of a subject. -/
def complete (s : Subject α) (time : Time) : Occurrence V :=
  ⟨time, s.id, .complete⟩

end Occurrence

-- ── Operator state ──────────────────────────────────────────────────────────

/-- The state of one node between instants. One record serves every operator;
    each uses the fields it needs. -/
structure Cell (V : Type) where
  /-- The stream has ended. -/
  done : Bool := false
  /-- Which sources have completed. -/
  completed : List Bool := []
  /-- How many values have been seen (`take`, `skip`). -/
  count : Nat := 0
  /-- The accumulator (`scan`). -/
  acc : Option V := none
  /-- The latest value of each source (`combineLatest`, `withLatestFrom`). -/
  latest : List (Option V) := []
  /-- The unmatched values of each source (`zip`). -/
  queues : List (List V) := []
  /-- The last value emitted (`distinctUntilChanged`). -/
  last : Option V := none
  /-- Until when values are dropped (`throttleTime`). -/
  gate : Option Time := none
  /-- Scheduled emissions, earliest first (`debounceTime`, `delay`). -/
  pending : List (Time × V) := []
  /-- The source completed while emissions were pending (`delay`). -/
  completing : Bool := false

instance {V : Type} : Inhabited (Cell V) := ⟨{}⟩

/-- The initial state of a node. -/
def Cell.initial {V : Type} (n : Node) : Cell V :=
  { completed := List.replicate n.args.length false
    latest := List.replicate n.args.length none
    queues := List.replicate n.args.length [] }

/-- What starts an instant. -/
inductive Source (V : Type) where
  /-- An occurrence at a node. -/
  | external (node : Nat) (event : Notification V)
  /-- The timer of a node. -/
  | timer (node : Nat)

-- ── One node, one instant ───────────────────────────────────────────────────

namespace Graph

variable {m : Type → Type} {V : Type}

/-- Call function `f`. -/
def call [Monad m] (g : Graph m V) (f : FnId) (args : List V) : m (Except String (Option V)) :=
  match g.fns[f.idx]? with
  | some impl => impl args
  | none => pure (.error s!"function {f.idx} is not registered")

/-- End the stream: emit `next`, then `t`. -/
private def finish (c : Cell V) (next : Option V) (t : Terminal) : Cell V × Emission V :=
  ({ c with done := true, pending := [] }, { next, terminal := some t })

/-- A one-source operator: `onNext` handles a value (returning the new state,
    the value to emit, and whether the stream completes now); a source's end
    is passed on. -/
private def unary [Monad m] (c : Cell V) (e : Emission V)
    (onNext : Cell V → V → m (Except String (Cell V × Option V × Bool))) :
    m (Cell V × Emission V) := do
  let r ← match e.next with
    | none => pure (.ok (c, none, false))
    | some v => onNext c v
  match r with
  | .error msg => pure (finish c none (.error msg))
  | .ok (c', next, true) => pure (finish c' next .complete)
  | .ok (c', next, false) =>
    match e.terminal with
    | some term => pure (finish c' next term)
    | none => pure (c', { next })

/-- The value of an erased call, as a `next`, an absent value, or a failure. -/
private def called (r : Except String (Option V)) (c : Cell V) :
    Except String (Cell V × Option V × Bool) :=
  r.map fun w => (c, w, false)

/-- A several-source operator: the first source error ends the node; otherwise
    `combine` computes the value and whether the stream completes, from the
    state with the sources' completions recorded. -/
private def multi [Monad m] (c : Cell V) (ins : List (Emission V))
    (combine : Cell V → m (Except String (Cell V × Option V × Bool))) :
    m (Cell V × Emission V) := do
  match ins.findSome? (fun e => match e.terminal with
      | some (.error msg) => some msg
      | _ => none) with
  | some msg => pure (finish c none (.error msg))
  | none =>
    let completed := (c.completed.zip ins).map fun (b, e) => b || e.terminal == some .complete
    match ← combine { c with completed } with
    | .error msg => pure (finish c none (.error msg))
    | .ok (c', next, true) => pure (finish c' next .complete)
    | .ok (c', next, false) => pure (c', { next })

/-- A subject delivers its occurrence. -/
private def stepSubject (c : Cell V) (external : Option (Notification V)) : Cell V × Emission V :=
  match external with
  | some (.next v) => (c, { next := some v })
  | some (.error msg) => finish c none (.error msg)
  | some .complete => finish c none .complete
  | none => (c, {})

/-- `scan`: the seed (computed once), then the running accumulation. -/
private def stepScan [Monad m] (g : Graph m V) (f seed : FnId) (c : Cell V) (v : V) :
    m (Except String (Cell V × Option V × Bool)) := do
  let s ← match c.acc with
    | some s => pure (Except.ok (some s))
    | none => g.call seed []
  match s with
  | .error msg => pure (.error msg)
  | .ok none => pure (.error "scan: the seed has no value")
  | .ok (some s) =>
    let r ← g.call f [s, v]
    pure (r.map fun
      | some s' => ({ c with acc := some s' }, some s', false)
      | none => ({ c with acc := some s }, none, false))

/-- `debounceTime d`: a value waits `d`, replaced by any newer one; a timer
    emits it; completion flushes it. -/
private def stepDebounce (t : Time) (fire : Bool) (d : Nat) (e : Emission V) (c : Cell V) :
    Cell V × Emission V :=
  let (c, next) := if fire then
      match c.pending with
      | (_, v) :: _ => ({ c with pending := [] }, some v)
      | [] => (c, none)
    else (c, none)
  let c := match e.next with
    | some v => { c with pending := [(t + d, v)] }
    | none => c
  match e.terminal with
  | some (.error msg) => finish c next (.error msg)
  | some .complete => finish c ((c.pending.head?.map (·.2)).orElse fun _ => next) .complete
  | none => (c, { next })

/-- `delay d`: every value is emitted `d` later; completion waits for them. -/
private def stepDelay (t : Time) (fire : Bool) (d : Nat) (e : Emission V) (c : Cell V) :
    Cell V × Emission V :=
  let (c, next) := if fire then
      match c.pending with
      | (_, v) :: rest => ({ c with pending := rest }, some v)
      | [] => (c, none)
    else (c, none)
  if fire && c.completing && c.pending.isEmpty then finish c next .complete else
  let c := match e.next with
    | some v => { c with pending := c.pending ++ [(t + d, v)] }
    | none => c
  match e.terminal with
  | some (.error msg) => finish c next (.error msg)
  | some .complete =>
    if c.pending.isEmpty then finish c next .complete else ({ c with completing := true }, { next })
  | none => (c, { next })

/-- `mergeWith f`: either value, combined by `f` when both come at once. -/
private def stepMergeWith [Monad m] (g : Graph m V) (f : FnId) (ins : List (Emission V))
    (c : Cell V) : m (Except String (Cell V × Option V × Bool)) := do
  let done := c.completed.all id
  match ins.filterMap (·.next) with
  | [] => pure (.ok (c, none, done))
  | [v] => pure (.ok (c, some v, done))
  | v :: w :: _ => (·.map fun r => (c, r, done)) <$> g.call f [v, w]

/-- The latest value of each source, updated by what they emitted now. -/
private def updateLatest (c : Cell V) (ins : List (Emission V)) : List (Option V) :=
  (c.latest.zip ins).map fun (o, e) => e.next.orElse fun _ => o

/-- `combineLatest f`: `f` of every source's latest value, whenever one emits
    and all have emitted; completes when all have completed, or when one
    completes without ever emitting. -/
private def stepCombineLatest [Monad m] (g : Graph m V) (f : FnId) (ins : List (Emission V))
    (c : Cell V) : m (Except String (Cell V × Option V × Bool)) := do
  let latest := updateLatest c ins
  let c := { c with latest }
  let done := c.completed.all id || (c.completed.zip latest).any fun (fin, l) => fin && l.isNone
  if ins.any (·.next.isSome) && latest.all Option.isSome then
    (·.map fun r => (c, r, done)) <$> g.call f (latest.filterMap id)
  else pure (.ok (c, none, done))

/-- `withLatestFrom f`: `f` of each value of the first source and the latest of
    the others (their values of the same instant included); completes with
    the first source. -/
private def stepWithLatestFrom [Monad m] (g : Graph m V) (f : FnId) (ins : List (Emission V))
    (c : Cell V) : m (Except String (Cell V × Option V × Bool)) := do
  let latest := updateLatest c ins
  let c := { c with latest }
  let done := c.completed.headD false
  match ins.head? >>= (·.next) with
  | some v =>
    if (latest.drop 1).all Option.isSome then
      (·.map fun r => (c, r, done)) <$> g.call f (v :: (latest.drop 1).filterMap id)
    else pure (.ok (c, none, done))
  | none => pure (.ok (c, none, done))

/-- `zip f`: `f` of the sources' `n`-th values; completes when a completed
    source has nothing left to pair. -/
private def stepZip [Monad m] (g : Graph m V) (f : FnId) (ins : List (Emission V))
    (c : Cell V) : m (Except String (Cell V × Option V × Bool)) := do
  let queues := (c.queues.zip ins).map fun (q, e) => q ++ e.next.toList
  if queues.all (!·.isEmpty) then
    let rest := queues.map (·.drop 1)
    let c := { c with queues := rest }
    let done := (c.completed.zip rest).any fun (fin, q) => fin && q.isEmpty
    (·.map fun r => (c, r, done)) <$> g.call f (queues.filterMap (·.head?))
  else
    let c := { c with queues }
    pure (.ok (c, none, (c.completed.zip queues).any fun (fin, q) => fin && q.isEmpty))

/-- Node `node`, with state `c`, in the instant at `t`: `ins` is what its
    sources emitted, `external` the occurrence delivered to it, `fire` whether
    its timer fires. -/
def step [Monad m] [BEq V] (g : Graph m V) (t : Time) (fire : Bool)
    (external : Option (Notification V)) (node : Node) (ins : List (Emission V))
    (c : Cell V) : m (Cell V × Emission V) :=
  if c.done then pure (c, {}) else
  let e : Emission V := ins.headD {}
  match node.op with
  | .subject => pure (stepSubject c external)
  | .map f => unary c e fun c v => (called · c) <$> g.call f [v]
  | .filter p => unary c e fun c v => (called · c) <$> g.call p [v]
  | .scan f seed => unary c e (stepScan g f seed)
  | .take n => unary c e fun c v => pure (.ok (
      if c.count < n then ({ c with count := c.count + 1 }, some v, c.count + 1 ≥ n)
      else (c, none, true)))
  | .skip n => unary c e fun c v => pure (.ok (
      if c.count < n then ({ c with count := c.count + 1 }, none, false) else (c, some v, false)))
  | .distinctUntilChanged => unary c e fun c v => pure (.ok (
      if c.last == some v then (c, none, false) else ({ c with last := some v }, some v, false)))
  | .throttleTime d => unary c e fun c v => pure (.ok (
      if c.gate.all (· ≤ t) then ({ c with gate := some (t + d) }, some v, false)
      else (c, none, false)))
  | .debounceTime d => pure (stepDebounce t fire d e c)
  | .delay d => pure (stepDelay t fire d e c)
  | .merge => multi c ins fun c =>
      pure (.ok (c, (ins.filterMap (·.next)).head?, c.completed.all id))
  | .mergeWith f => multi c ins (stepMergeWith g f ins)
  | .combineLatest f => multi c ins (stepCombineLatest g f ins)
  | .withLatestFrom f => multi c ins (stepWithLatestFrom g f ins)
  | .zip f => multi c ins (stepZip g f ins)

-- ── One instant ─────────────────────────────────────────────────────────────

/-- An instant at `t`, started by `src`: every active node, in order (an
    inactive node — one no selected result depends on — is skipped). -/
def tick [Monad m] [BEq V] (g : Graph m V) (t : Time) (src : Source V)
    (active : Array Bool) (cells : Array (Cell V)) : m (Array (Cell V) × Array (Emission V)) :=
  (List.range g.nodes.size).foldlM (init := (cells, (#[] : Array (Emission V))))
    fun (cells, ems) i => do
      if !(active[i]?.getD true) then return (cells, ems.push {})
      let node := g.nodes[i]?.getD default
      let ins := node.args.map fun a => ems[a.idx]?.getD {}
      let external := match src with
        | .external j ev => if j == i then some ev else none
        | .timer _ => none
      let fire := match src with
        | .timer j => j == i
        | .external _ _ => false
      let (c, e) ← g.step t fire external node ins (cells[i]?.getD default)
      pure (cells.setIfInBounds i c, ems.push e)

end Graph

-- ── Traces and sessions ─────────────────────────────────────────────────────

/-- Every node's stream: timestamped notifications, per node, in order. -/
structure Trace (V : Type) where
  /-- The nodes' labels, by index. -/
  labels : Array Lean.Name
  /-- The nodes' streams, by index. -/
  streams : Array (List (Time × Notification V))
  /-- Internal faults (see `Graph.fireDue`); empty in a sound run. -/
  faults : List String := []
  deriving Repr, BEq

namespace Trace

variable {V : Type}

/-- The stream of a node. -/
def events (tr : Trace V) (n : NodeId) : List (Time × Notification V) :=
  tr.streams[n.idx]?.getD []

/-- The stream of the node labelled `l`. -/
def find? (tr : Trace V) (l : Lean.Name) : Option (List (Time × Notification V)) :=
  (tr.labels.findIdx? (· == l)) >>= (tr.streams[·]?)

/-- The values an observable emitted, decoded, with their times. -/
def values {α : Type} [Codec V α] (tr : Trace V) (o : Observable α) : List (Time × α) :=
  (tr.events o.id).filterMap fun
    | (t, .next v) => (Codec.decode v).toOption.map (t, ·)
    | _ => none

/-- Whether an observable completed. -/
def completed {α : Type} (tr : Trace V) (o : Observable α) : Bool :=
  (tr.events o.id).any fun (_, n) => match n with
    | .complete => true
    | _ => false

/-- How an observable failed, if it did. -/
def error? {α : Type} (tr : Trace V) (o : Observable α) : Option String :=
  (tr.events o.id).findSome? fun (_, n) => match n with
    | .error msg => some msg
    | _ => none

/-- Every node's label with its stream, in node order. -/
def toList (tr : Trace V) : List (Lean.Name × List (Time × Notification V)) :=
  tr.labels.toList.zip tr.streams.toList

end Trace

/-- A run in progress: the clock, every node's state, and the streams so far. -/
structure Session (V : Type) where
  /-- The current time. -/
  now : Time := 0
  /-- Every node's state, by index. -/
  cells : Array (Cell V) := #[]
  /-- Every node's stream so far, by index. -/
  streams : Array (Array (Time × Notification V)) := #[]
  /-- The nodes computed (all of them, or those the selection depends on). -/
  active : Array Bool := #[]
  /-- The nodes whose streams are recorded (all of them, or the selection). -/
  recorded : Array Bool := #[]
  /-- Internal faults. -/
  faults : Array String := #[]

/-- Which results a run returns: every node's stream, or only some. -/
inductive Selection where
  /-- Every node's stream: inputs, intermediate nodes and outputs. -/
  | all
  /-- Only these nodes' streams; nodes none of them depends on are not even
      computed. -/
  | only (nodes : List NodeId)
  deriving Repr, BEq

/-- The weight of the pending timers: $\sum_i \text{pending}_i \cdot 2^{N-i}$. -/
def weight {V : Type} (cells : Array (Cell V)) : Nat :=
  (List.range cells.size).foldl (fun acc i =>
    acc + ((cells[i]?.map (·.pending.length)).getD 0) * 2 ^ (cells.size - i)) 0

/-- The earliest pending timer of a live node: its time and node, ties by node. -/
def nextTimer {V : Type} (cells : Array (Cell V)) : Option (Time × Nat) :=
  (List.range cells.size).foldl (fun best i =>
    match cells[i]? with
    | some c =>
      if c.done then best else
      match c.pending.head?, best with
      | some (due, _), some (bt, _) => if due < bt then some (due, i) else best
      | some (due, _), none => some (due, i)
      | none, _ => best
    | none => best) none

namespace Graph

variable {m : Type → Type} {V : Type}

/-- A fresh session: time 0, every node in its initial state; `select` says
    which streams are computed and recorded. -/
def start (g : Graph m V) (select : Selection := .all) : Session V :=
  let (active, recorded) := match select with
    | .all => (Array.replicate g.nodes.size true, Array.replicate g.nodes.size true)
    | .only ns =>
      (g.upstreamMarks ns,
        ns.foldl (fun mk n => mk.setIfInBounds n.idx true) (Array.replicate g.nodes.size false))
  { cells := g.nodes.map Cell.initial, streams := g.nodes.map fun _ => #[], active, recorded }

/-- Run the instant at `t` started by `src`, recording what every node emits. -/
def tickAt [Monad m] [BEq V] (g : Graph m V) (t : Time) (src : Source V) (s : Session V) :
    m (Session V) := do
  let (cells, ems) ← g.tick t src s.active s.cells
  pure { s with
    now := t, cells
    streams := s.streams.mapIdx fun i st =>
      if s.recorded[i]?.getD true then
        st ++ ((ems[i]?.getD {}).notifications.map ((t, ·))).toArray
      else st }

/-- Fire the pending timers due by `limit` (all of them if `none`), earliest
    first. Recursion is on the timers' `weight`, whose decrease is checked; if
    it ever failed, the session records a fault and stops. -/
def fireDue [Monad m] [BEq V] (g : Graph m V) (limit : Option Time) (s : Session V) :
    m (Session V) :=
  match nextTimer s.cells with
  | none => pure s
  | some (t, k) =>
    if limit.all (t ≤ ·) then do
      let s' ← g.tickAt (max t s.now) (.timer k) s
      if _h : weight s'.cells < weight s.cells then g.fireDue limit s'
      else
        let fault := s!"scheduler: the timer of node {k} at {t} did not reduce pending work"
        pure { s' with faults := s'.faults.push fault }
    else pure s
termination_by weight s.cells

/-- Deliver an occurrence: first the timers due by its time, then it. An
    occurrence earlier than the clock is delivered at the clock's time. -/
def push [Monad m] [BEq V] (g : Graph m V) (s : Session V) (o : Occurrence V) :
    m (Session V) := do
  let s ← g.fireDue (some o.time) s
  g.tickAt (max o.time s.now) (.external o.subject.idx o.event) s

/-- Deliver occurrences in order. -/
def pushAll [Monad m] [BEq V] (g : Graph m V) (s : Session V) (os : List (Occurrence V)) :
    m (Session V) :=
  os.foldlM g.push s

/-- Let time pass until `t`, firing the timers due. -/
def advance [Monad m] [BEq V] (g : Graph m V) (s : Session V) (t : Time) : m (Session V) := do
  let s ← g.fireDue (some t) s
  pure { s with now := max t s.now }

/-- Let time pass until no timer is left. -/
def drain [Monad m] [BEq V] (g : Graph m V) (s : Session V) : m (Session V) :=
  g.fireDue none s

/-- The trace of a session. -/
def trace (g : Graph m V) (s : Session V) : Trace V :=
  { labels := g.labels, streams := s.streams.map (·.toList), faults := s.faults.toList }

/-- Run a graph on occurrences, then let time pass until no timer is left;
    return every node's stream — or, with `select := .only ns`, only those of
    `ns` (the others are empty), computing only what they depend on. -/
def runM [Monad m] [BEq V] (g : Graph m V) (os : List (Occurrence V))
    (select : Selection := .all) : m (Trace V) := do
  let s ← g.pushAll (g.start select) os
  let s ← g.drain s
  pure (g.trace s)

/-- `runM` for a graph of pure functions. -/
def run [BEq V] (g : Graph Id V) (os : List (Occurrence V)) (select : Selection := .all) :
    Trace V :=
  Id.run (g.runM os select)

/-- Just one result: the notifications of `o`, computing only what it
    depends on. -/
def runFor [Monad m] [BEq V] {α : Type} (g : Graph m V) (o : Observable α)
    (os : List (Occurrence V)) : m (List (Time × Notification V)) := do
  pure ((← g.runM os (.only [o.id])).events o.id)

/-- Just one result's values, decoded, for a graph of pure functions. -/
def valuesFor [BEq V] {α : Type} [Codec V α] (g : Graph Id V) (o : Observable α)
    (os : List (Occurrence V)) : List (Time × α) :=
  (g.run os (.only [o.id])).values o

/-- A set of results: each selected node with its notifications, computing only
    what they depend on. -/
def runSelected [Monad m] [BEq V] (g : Graph m V) (ns : List NodeId) (os : List (Occurrence V)) :
    m (List (NodeId × List (Time × Notification V))) := do
  let tr ← g.runM os (.only ns)
  pure (ns.map fun n => (n, tr.events n))

end Graph

-- ── Properties ──────────────────────────────────────────────────────────────

/-- Delivering `xs ++ ys` is delivering `xs`, then `ys`: a run can be split at
    any point and resumed from the session. -/
theorem Graph.pushAll_append {m : Type → Type} {V : Type} [Monad m] [LawfulMonad m] [BEq V]
    (g : Graph m V) (s : Session V) (xs ys : List (Occurrence V)) :
    g.pushAll s (xs ++ ys) = g.pushAll s xs >>= fun s' => g.pushAll s' ys := by
  simp [pushAll, List.foldlM_append]

/-- A run is a session fed every occurrence, then drained. -/
theorem Graph.runM_eq {m : Type → Type} {V : Type} [Monad m] [BEq V]
    (g : Graph m V) (os : List (Occurrence V)) (select : Selection) :
    g.runM os select =
      (do let s ← g.pushAll (g.start select) os; let s ← g.drain s; pure (g.trace s)) := rfl

/-- The initial session has one state and one (empty) stream per node. -/
theorem Graph.start_sizes {m : Type → Type} {V : Type} (g : Graph m V) (select : Selection) :
    (g.start select).cells.size = g.nodes.size ∧ (g.start select).streams.size = g.nodes.size := by
  cases select <;> simp [start]

end Control.Reactive
