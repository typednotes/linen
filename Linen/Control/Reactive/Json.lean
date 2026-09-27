/-
  `Control.Reactive.Json` — JSON for reactive graphs, occurrences and traces

  Everything first-order about a `Control.Reactive` graph serialises: its
  structure (operators, their parameters and sources), its labels, a log of
  occurrences, and the trace of a run. Functions do not — a closure has no
  representation — so a graph is written with each function's **label**, and
  read back against a `Registry` binding labels to implementations
  (typically the registry of the same graph built in code: `Graph.registry`).

  ```json
  { "format": "linen.reactive.graph/1",
    "functions": ["fn.1"],
    "nodes": [
      { "label": "sheet.clicks", "op": "subject" },
      { "label": "sheet.doubled", "op": "map", "function": "fn.1",
        "args": ["sheet.clicks"] },
      { "label": "sheet.quiet", "op": "debounceTime", "duration": 100,
        "args": ["sheet.doubled"] } ] }
  ```

  A log of occurrences is a complete description of a run (runs are
  deterministic), so storing the log and replaying it against the current
  version of a graph is how a stored run is brought up to date.

  ## Design notes

  - **Nodes refer to each other by label, not by index.** The document reads
    without counting, and occurrences and traces stay meaningful across
    versions of a graph. Reading resolves each argument to an *earlier*
    node and each function to a declared one, then re-establishes `Graph`'s
    invariants (`WellFormed`, `Labelled`) by decision — a document is data
    from outside, so nothing about it is assumed.
  - **Labels round-trip exactly.** A label is written in Lean's dotted syntax
    (`sheet.x`, `double.1`, `«a.b».c`) when `parseLabel` reads that back to
    the same name, and otherwise as an array of components
    (`["a", 1, "b"]`, strings and numbers). `Name.toString` alone is not
    enough: its own documentation says names with numeric components or `»`
    may not round-trip, and `String.toName` reaches `unreachable!` on a
    component such as `1a`. `parseLabel` is total and reports such input as an
    error instead.
  - **Values are the caller's.** Occurrences and traces carry values of the
    graph's value type `V`, written through `ToJSON V`/`FromJSON V`. Their
    fidelity is theirs to ensure; note that `Data.Json` numbers are IEEE
    doubles, so an integer above $2^{53}$ is better written as a string.
  - **Each document carries a `format` tag** (`linen.reactive.graph/1`,
    `linen.reactive.occurrences/1`, `linen.reactive.trace/1`), checked on
    reading, so a document of one kind is never read as another and the
    format can evolve.
-/
import Linen.Control.Reactive
import Linen.Data.Json

namespace Control.Reactive

open Data.Json

-- ── Labels ──────────────────────────────────────────────────────────────────

/-- The state of `parseLabel`: the component being read (reversed), whether we
    are inside `«…»`, whether the current component was escaped, and the
    components read so far with their escapedness. -/
private structure LabelState where
  cur : List Char := []
  inEscape : Bool := false
  escaped : Bool := false
  comps : Array (String × Bool) := #[]

/-- Read one character of a dotted label. -/
private def labelStep (st : LabelState) (c : Char) : Except String LabelState :=
  if st.inEscape then
    if c == '»' then .ok { st with inEscape := false }
    else .ok { st with cur := c :: st.cur }
  else if c == '«' then
    if st.cur.isEmpty && !st.escaped then .ok { st with inEscape := true, escaped := true }
    else .error "misplaced `«`"
  else if c == '.' then
    if st.cur.isEmpty && !st.escaped then .error "empty component"
    else .ok { comps := st.comps.push (String.ofList st.cur.reverse, st.escaped) }
  else if st.escaped then .error "text after `»`"
  else .ok { st with cur := c :: st.cur }

/-- One component: escaped text is a string; unescaped digits are a number;
    anything else must not start with a digit. -/
private def labelComponent (n : Lean.Name) : String × Bool → Except String Lean.Name
  | (s, true)  => .ok (.str n s)
  | (s, false) =>
    if s.all Char.isDigit then .ok (.num n s.toNat!)
    else if s.front.isDigit then .error s!"component `{s}` starts with a digit"
    else .ok (.str n s)

/-- Parse a label in Lean's dotted syntax: components separated by `.`, each
    either `«…»`-escaped text, a number, or text not starting with a digit.
    Total: malformed input is an error, never a panic. -/
def parseLabel (s : String) : Except String Lean.Name := do
  let st ← s.toList.foldlM labelStep {}
  if st.inEscape then throw s!"label `{s}`: unterminated `«`"
  if st.cur.isEmpty && !st.escaped then throw s!"label `{s}`: empty component"
  let comps := st.comps.push (String.ofList st.cur.reverse, st.escaped)
  comps.foldlM labelComponent .anonymous |>.mapError (s!"label `{s}`: " ++ ·)

/-- A label's components, as JSON strings and numbers. -/
private def labelComponents : Lean.Name → List Value
  | .anonymous => []
  | .str p s   => labelComponents p ++ [.string s]
  | .num p k   => labelComponents p ++ [.number (Float.ofNat k)]

/-- Write a label: its dotted form when that reads back exactly, otherwise the
    array of its components. -/
def labelToJSON (n : Lean.Name) : Value :=
  let s := n.toString
  match parseLabel s with
  | .ok n' => if n' == n then .string s else .array (labelComponents n).toArray
  | .error _ => .array (labelComponents n).toArray

/-- Read a label written by `labelToJSON` (either form). -/
def labelFromJSON : Value → Except String Lean.Name
  | .string s => parseLabel s
  | .array cs => cs.foldlM (init := .anonymous) fun n c => match c with
    | .string s => .ok (.str n s)
    | .number _ => (.num n ·) <$> FromJSON.parseJSON (α := Nat) c
    | v => .error s!"a label component must be a string or a number, got {repr v}"
  | v => .error s!"a label must be a string or an array, got {repr v}"

-- ── Documents ───────────────────────────────────────────────────────────────

/-- The format tag of a serialised graph. -/
def graphFormat : String := "linen.reactive.graph/1"
/-- The format tag of a serialised log of occurrences. -/
def occurrencesFormat : String := "linen.reactive.occurrences/1"
/-- The format tag of a serialised trace. -/
def traceFormat : String := "linen.reactive.trace/1"

/-- Check a document's format tag. -/
private def checkFormat (v : Value) (format : String) : Except String Unit := do
  match ← v.getField "format" with
  | .string f => if f == format then pure () else
      throw s!"expected a `{format}` document, got `{f}`"
  | _ => throw "the `format` field must be a string"

/-- An array field. -/
private def arrayField (v : Value) (key : String) : Except String (Array Value) := do
  match ← v.getField key with
  | .array a => pure a
  | _ => throw s!"the `{key}` field must be an array"

/-- A string field. -/
private def stringField (v : Value) (key : String) : Except String String := do
  match ← v.getField key with
  | .string s => pure s
  | _ => throw s!"the `{key}` field must be a string"

/-- A natural-number field. -/
private def natField (v : Value) (key : String) : Except String Nat := do
  FromJSON.parseJSON (← v.getField key)

/-- A notification as JSON fields: `next`, `error` or `complete`. -/
private def notificationFields {V : Type} [ToJSON V] : Notification V → List (String × Value)
  | .next v => [("next", ToJSON.toJSON v)]
  | .error msg => [("error", .string msg)]
  | .complete => [("complete", .bool true)]

/-- Read a notification from `next`, `error` or `complete`. -/
private def notificationFromJSON {V : Type} [FromJSON V] (v : Value) :
    Except String (Notification V) := do
  if let some x := v.lookup "next" then return .next (← FromJSON.parseJSON x)
  if let some x := v.lookup "error" then
    match x with
    | .string msg => return .error msg
    | _ => throw "`error` must be a string"
  if v.lookup "complete" == some (.bool true) then return .complete
  throw "a notification needs a `next`, `error` or `complete` field"

-- ── Graphs ──────────────────────────────────────────────────────────────────

/-- Implementations by label, to bind the functions of a graph read from JSON. -/
structure Registry (m : Type → Type) (V : Type) where
  /-- The implementations, by function label. -/
  impls : Std.HashMap Lean.Name (Impl m V) := {}

namespace Registry

variable {m : Type → Type} {V : Type}

/-- Register an erased implementation under a label. -/
def insert (r : Registry m V) (l : Lean.Name) (impl : Impl m V) : Registry m V :=
  ⟨r.impls.insert l impl⟩

/-- Register a plain Lean function under a label (see `Callable`). -/
def add {F : Type} {args : List Type} {β : Type} [Callable m V F args β]
    (r : Registry m V) (l : Lean.Name) (f : F) : Registry m V :=
  r.insert l (Callable.erase f)

end Registry

namespace Graph

variable {m : Type → Type} {V : Type}

/-- The functions of a graph, by label — to read back documents it (or an
    edited version of it) wrote. -/
def registry (g : Graph m V) : Registry m V :=
  ⟨(g.fnLabels.zip g.fns).foldl (fun r (l, f) => r.insert l f) {}⟩

/-- Node `i` as JSON: its label, operator, parameters and sources (by label). -/
private def nodeToJSON (g : Graph m V) (i : NodeId) : Value :=
  let n := (g.node? i).getD default
  let fn (key : String) (f : FnId) : String × Value := (key, labelToJSON (g.fnLabel f))
  let params : List (String × Value) := match n.op with
    | .map f | .filter f | .mergeWith f | .combineLatest f | .withLatestFrom f | .zip f =>
      [fn "function" f]
    | .scan f seed => [fn "function" f, fn "seed" seed]
    | .take k | .skip k => [("count", ToJSON.toJSON k)]
    | .throttleTime d | .debounceTime d | .delay d => [("duration", ToJSON.toJSON d)]
    | .subject | .distinctUntilChanged | .merge => []
  let args : List (String × Value) :=
    if n.args.isEmpty then [] else [("args", .array (n.args.map (labelToJSON ∘ g.label)).toArray)]
  .object ([("label", labelToJSON (g.label i)), ("op", .string n.op.name)] ++ params ++ args)

/-- A graph's structure: its functions' labels and its nodes. -/
instance : ToJSON (Graph m V) where
  toJSON g := .object [
    ("format", .string graphFormat),
    ("functions", .array (g.fnLabels.map labelToJSON)),
    ("nodes", .array (g.ids.toArray.map g.nodeToJSON))]

/-- Read an operator: its name and parameters, functions resolved by `fnOf`. -/
private def opFromJSON (fnOf : String → Except String FnId) (label : Lean.Name) (v : Value) :
    Except String Op := do
  match ← stringField v "op" with
  | "subject" => pure .subject
  | "map" => .map <$> fnOf "function"
  | "filter" => .filter <$> fnOf "function"
  | "scan" => return .scan (← fnOf "function") (← fnOf "seed")
  | "take" => .take <$> natField v "count"
  | "skip" => .skip <$> natField v "count"
  | "distinctUntilChanged" => pure .distinctUntilChanged
  | "merge" => pure .merge
  | "mergeWith" => .mergeWith <$> fnOf "function"
  | "combineLatest" => .combineLatest <$> fnOf "function"
  | "withLatestFrom" => .withLatestFrom <$> fnOf "function"
  | "zip" => .zip <$> fnOf "function"
  | "throttleTime" => .throttleTime <$> natField v "duration"
  | "debounceTime" => .debounceTime <$> natField v "duration"
  | "delay" => .delay <$> natField v "duration"
  | op => throw s!"node `{label}`: unknown operator `{op}`"

/-- Read a graph, binding each function label through `reg`. Fails on a
    function with no implementation, a duplicate label, a source that is not
    an earlier node, an operator with the wrong number of sources, or a
    malformed document. -/
def fromJSON (reg : Registry m V) (v : Value) : Except String (Graph m V) := do
  checkFormat v graphFormat
  let fnLabels ← (← arrayField v "functions").mapM labelFromJSON
  let fns ← fnLabels.mapM fun l => match reg.impls[l]? with
    | some f => pure f
    | none   => throw s!"no implementation registered for function `{l}`"
  let fnIndex ← fnLabels.foldlM (init := (({} : Std.HashMap Lean.Name Nat), 0))
    fun (idx, i) l =>
      if idx.contains l then throw s!"two functions are labelled `{l}`"
      else pure (idx.insert l i, i + 1)
  let (nodes, labels, _) ← (← arrayField v "nodes").foldlM
    (init := ((#[] : Array Node), (#[] : Array Lean.Name), ({} : Std.HashMap Lean.Name Nat)))
    fun (nodes, labels, index) nv => do
      let label ← labelFromJSON (← nv.getField "label")
      if index.contains label then throw s!"two nodes are labelled `{label}`"
      let fnOf (key : String) : Except String FnId := do
        let fl ← labelFromJSON (← nv.getField key)
        match fnIndex.1[fl]? with
        | some f => pure ⟨f⟩
        | none => throw s!"node `{label}` calls `{fl}`, which is not a declared function"
      let op ← opFromJSON fnOf label nv
      let argLabels ← match nv.lookup "args" with
        | some (.array a) => a.toList.mapM labelFromJSON
        | some _ => throw s!"node `{label}`: `args` must be an array"
        | none => pure []
      let args ← argLabels.mapM fun a => match index[a]? with
        | some j => pure (NodeId.mk j)
        | none   => throw s!"node `{label}` reads `{a}`, which is not an earlier node"
      unless op.arity args.length do
        throw s!"node `{label}`: `{op.name}` cannot take {args.length} sources"
      pure (nodes.push ⟨op, args⟩, labels.push label, index.insert label nodes.size)
  if h : WellFormed nodes fns.size then
    if h' : Labelled nodes.size labels fns.size fnLabels then
      pure ⟨nodes, fns, labels, fnLabels, h, h'⟩
    else throw "internal error: labels do not match the nodes"
  else throw "internal error: a resolved reference is out of range"

end Graph

-- ── Occurrences ─────────────────────────────────────────────────────────────

namespace Occurrence

variable {m : Type → Type} {V : Type}

/-- A log of occurrences for `g`, subjects by label. -/
def logToJSON [ToJSON V] (g : Graph m V) (os : List (Occurrence V)) : Value :=
  .object [
    ("format", .string occurrencesFormat),
    ("occurrences", .array <| (os.map fun o => Value.object
      ([("time", ToJSON.toJSON o.time), ("subject", labelToJSON (g.label o.subject))] ++
        notificationFields o.event)).toArray)]

/-- Read a log of occurrences for `g`, resolving labels to its subjects. -/
def logFromJSON [FromJSON V] (g : Graph m V) (v : Value) : Except String (List (Occurrence V)) := do
  checkFormat v occurrencesFormat
  (← arrayField v "occurrences").toList.mapM fun ov => do
    let l ← labelFromJSON (← ov.getField "subject")
    let some n := g.find? l | throw s!"`{l}` is not a node of this graph"
    unless (g.node? n).map (·.op) == some .subject do throw s!"`{l}` is not a subject"
    pure ⟨← natField ov "time", n, ← notificationFromJSON ov⟩

end Occurrence

-- ── Traces ──────────────────────────────────────────────────────────────────

/-- A trace: every node's label and timestamped notifications, and the faults. -/
instance {V : Type} [ToJSON V] : ToJSON (Trace V) where
  toJSON tr := .object [
    ("format", .string traceFormat),
    ("nodes", .array <| (tr.toList.map fun (l, evs) => Value.object [
      ("label", labelToJSON l),
      ("events", .array <| (evs.map fun (t, n) =>
        Value.object ([("time", ToJSON.toJSON t)] ++ notificationFields n)).toArray)]).toArray),
    ("faults", .array (tr.faults.map Value.string).toArray)]

instance {V : Type} [FromJSON V] : FromJSON (Trace V) where
  parseJSON v := do
    checkFormat v traceFormat
    let nodes ← (← arrayField v "nodes").mapM fun nv => do
      let label ← labelFromJSON (← nv.getField "label")
      let events ← (← arrayField nv "events").toList.mapM fun ev => do
        pure (← natField ev "time", ← notificationFromJSON ev)
      pure (label, events)
    let faults ← match v.lookup "faults" with
      | some (.array a) => a.toList.mapM fun
        | .string s => pure s
        | _ => throw "a fault must be a string"
      | _ => pure []
    pure { labels := nodes.map (·.1), streams := nodes.map (·.2), faults }

-- ── Properties ──────────────────────────────────────────────────────────────

/-- A label written as a string is read back by `parseLabel` as itself — the
    string form is only chosen when this holds. -/
theorem labelToJSON_string {n : Lean.Name} {s : String} (h : labelToJSON n = .string s) :
    labelFromJSON (labelToJSON n) = .ok n := by
  unfold labelToJSON at h ⊢
  dsimp only at h ⊢
  cases hp : parseLabel n.toString with
  | error e => simp [hp] at h
  | ok n' =>
    simp only [hp] at h ⊢
    by_cases hn : n' = n
    · subst hn; simp [labelFromJSON, hp]
    · simp [hn] at h

end Control.Reactive
