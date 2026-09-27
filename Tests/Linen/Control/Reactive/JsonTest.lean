/-
  Tests for `Linen.Control.Reactive.Json`.

  Covers label parsing and writing (dotted form, the component-array fallback,
  malformed labels), graphs (exact document, round trips as values and as
  text, binding functions through a registry, every rejection), logs of
  occurrences and traces (round trips, rejections), and the cross-process
  use: a log written against one version of a graph replays against the next.
-/
import Linen.Control.Reactive.Json

open Control.Reactive Data.Json

namespace Tests.Control.Reactive.Json

-- ── A value universe with JSON ──────────────────────────────────────────────

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩
instance : Codec V String :=
  ⟨.inr, fun | .inr s => .ok s | .inl _ => .error "expected a string", fun _ => rfl⟩

instance : ToJSON V where
  toJSON
    | .inl n => .object [("nat", ToJSON.toJSON n)]
    | .inr s => .object [("string", .string s)]

instance : FromJSON V where
  parseJSON v :=
    match v.lookup "nat", v.lookup "string" with
    | some n, _ => .inl <$> FromJSON.parseJSON n
    | _, some s => .inr <$> FromJSON.parseJSON s
    | _, _ => .error "expected {\"nat\": …} or {\"string\": …}"

/-- `Except` compared on its success value only, for guards. -/
def ok? {α : Type} [BEq α] (e : Except String α) (a : α) : Bool :=
  match e with | .ok b => b == a | .error _ => false

/-- The error message, for guards. -/
def err? {α : Type} (e : Except String α) (msg : String) : Bool :=
  match e with | .error m => m == msg | .ok _ => false

-- ── Labels ──────────────────────────────────────────────────────────────────

#guard ok? (parseLabel "sheet.x") `sheet.x
#guard ok? (parseLabel "double.1") (.num `double 1)
#guard ok? (parseLabel "«a.b».c") (.str (.str .anonymous "a.b") "c")
#guard ok? (parseLabel "«»") (.str .anonymous "")
#guard ok? (parseLabel "7") (.num .anonymous 7)
#guard err? (parseLabel "") "label ``: empty component"
#guard err? (parseLabel "a..b") "empty component"
#guard err? (parseLabel "a.") "label `a.`: empty component"
#guard err? (parseLabel "a.1x") "label `a.1x`: component `1x` starts with a digit"
#guard err? (parseLabel "a.«b") "label `a.«b`: unterminated `«`"
#guard err? (parseLabel "«a»b") "text after `»`"

-- The dotted form is used when it reads back exactly …
#guard labelToJSON `sheet.x == .string "sheet.x"
#guard labelToJSON (.num `double 1) == .string "double.1"
#guard labelToJSON (.str .anonymous "a.b") == .string "«a.b»"
-- … and the component array otherwise (`anonymous` prints as `[anonymous]`).
#guard labelToJSON .anonymous == .array #[]

/-- Names that are awkward in dotted syntax. -/
def awkward : List Lean.Name :=
  [`sheet.x, .num `double 1, .str .anonymous "a.b", .str .anonymous "", .anonymous,
   .num .anonymous 3, .str (.num `a 1) "b", .str .anonymous "1", .str .anonymous "a»b",
   .str .anonymous "with space", .str `x "«"]

-- Every one of them round-trips, whichever form it is written in.
#guard awkward.all fun n => ok? (labelFromJSON (labelToJSON n)) n
#guard ok? (labelFromJSON (.array #[.string "a", .number 1, .string "b"])) (.str (.num `a 1) "b")
#guard err? (labelFromJSON (.bool true)) "a label must be a string or an array, got Data.Json.Value.bool true"

-- ── A graph ─────────────────────────────────────────────────────────────────

def sheet : Reactive Id V (Subject Nat × Observable Nat) := do
  node x ← subject Nat
  node checked ← x.mapE fun n => if n > 100 then .error "too big" else .ok n
  node total ← checked.scan 0 (· + ·)
  node quiet ← total.debounceTime 10
  pure (x, quiet)

def g : Graph Id V := sheet.graph!
def x := sheet.result.1
def quiet := sheet.result.2
def log : List (Occurrence V) := [.next x 0 (1 : Nat), .next x 5 (2 : Nat), .complete x 50]

/-- Graph equality on everything but the functions. -/
def sameShape (a b : Graph Id V) : Bool :=
  a.nodes == b.nodes && a.labels == b.labels && a.fnLabels == b.fnLabels

-- The exact document (`Data.Json` escapes `/` as `\/`, which is valid JSON).
#guard Encode.encode (ToJSON.toJSON g) ==
  "{\"format\":\"linen.reactive.graph\\/1\",\"functions\":[\"fn.1\",\"fn.2\",\"fn.3\"],\
\"nodes\":[{\"label\":\"Tests.Control.Reactive.Json.sheet.x\",\"op\":\"subject\"},\
{\"label\":\"Tests.Control.Reactive.Json.sheet.checked\",\"op\":\"map\",\"function\":\"fn.1\",\
\"args\":[\"Tests.Control.Reactive.Json.sheet.x\"]},\
{\"label\":\"Tests.Control.Reactive.Json.sheet.total\",\"op\":\"scan\",\"function\":\"fn.2\",\
\"seed\":\"fn.3\",\"args\":[\"Tests.Control.Reactive.Json.sheet.checked\"]},\
{\"label\":\"Tests.Control.Reactive.Json.sheet.quiet\",\"op\":\"debounceTime\",\"duration\":10,\
\"args\":[\"Tests.Control.Reactive.Json.sheet.total\"]}]}"

-- Round trip as a value, binding the functions of the graph built in code …
#guard match Graph.fromJSON g.registry (ToJSON.toJSON g) with
  | .ok h => sameShape h g && h.run log == g.run log
  | .error _ => false
-- … and through text.
#guard match Decode.decode (Encode.encode (ToJSON.toJSON g)) >>= Graph.fromJSON g.registry with
  | .ok h => sameShape h g && h.run log == g.run log
  | .error _ => false

-- The registry decides the implementations: the scan function rebound at load.
def doubling : Registry Id V := g.registry.add (.num `fn 2) fun (s n : Nat) => s + 2 * n
#guard match Graph.fromJSON doubling (ToJSON.toJSON g) with
  | .ok h => (h.run log).values quiet == [(15, 6)]
  | .error _ => false

-- ── Graph rejections ────────────────────────────────────────────────────────

def doc (fns : List Value) (nodes : List (List (String × Value))) (format := graphFormat) : Value :=
  .object [("format", .string format), ("functions", .array fns.toArray),
    ("nodes", .array (nodes.map Value.object).toArray)]

def reg : Registry Id V := ({} : Registry Id V).add `inc fun (n : Nat) => n + 1

def subjectNode (l : String) : List (String × Value) := [("label", .string l), ("op", .string "subject")]
def mapNode (l f : String) (args : List String) : List (String × Value) :=
  [("label", .string l), ("op", .string "map"), ("function", .string f),
   ("args", .array (args.map Value.string).toArray)]

def fromDoc (v : Value) : Except String (Graph Id V) := Graph.fromJSON reg v

#guard match fromDoc (doc [.string "inc"] [subjectNode "x", mapNode "y" "inc" ["x"]]) with
  | .ok h => h.nodes == #[⟨.subject, []⟩, ⟨.map ⟨0⟩, [⟨0⟩]⟩] && h.labels == #[`x, `y]
  | .error _ => false
#guard err? (fromDoc (doc [.string "inc"] [subjectNode "x"] (format := traceFormat)))
  "expected a `linen.reactive.graph/1` document, got `linen.reactive.trace/1`"
#guard err? (fromDoc (doc [.string "dec"] [])) "no implementation registered for function `dec`"
#guard err? (fromDoc (doc [.string "inc", .string "inc"] [])) "two functions are labelled `inc`"
#guard err? (fromDoc (doc [.string "inc"] [subjectNode "x", subjectNode "x"]))
  "two nodes are labelled `x`"
#guard err? (fromDoc (doc [.string "inc"] [mapNode "y" "inc" ["x"], subjectNode "x"]))
  "node `y` reads `x`, which is not an earlier node"
#guard err? (fromDoc (doc [.string "inc"] [subjectNode "x", mapNode "y" "dec" ["x"]]))
  "node `y` calls `dec`, which is not a declared function"
#guard err? (fromDoc (doc [.string "inc"] [subjectNode "x", mapNode "y" "inc" ["x", "x"]]))
  "node `y`: `map` cannot take 2 sources"
#guard err? (fromDoc (doc [] [[("label", .string "x"), ("op", .string "flatMap")]]))
  "node `x`: unknown operator `flatMap`"
#guard err? (fromDoc (doc [] [[("label", .string "a..b"), ("op", .string "subject")]]))
  "empty component"
#guard err? (fromDoc (.object [("format", .string graphFormat)])) "key 'functions' not found"

-- ── Logs of occurrences ─────────────────────────────────────────────────────

#guard Encode.encode (Occurrence.logToJSON g [.next x 3 (7 : Nat), .complete x 9]) ==
  "{\"format\":\"linen.reactive.occurrences\\/1\",\"occurrences\":[\
{\"time\":3,\"subject\":\"Tests.Control.Reactive.Json.sheet.x\",\"next\":{\"nat\":7}},\
{\"time\":9,\"subject\":\"Tests.Control.Reactive.Json.sheet.x\",\"complete\":true}]}"
#guard ok? (Occurrence.logFromJSON g (Occurrence.logToJSON g log)) log
#guard ok? (Occurrence.logFromJSON g (Occurrence.logToJSON g [.error x 1 "boom"])) [.error x 1 "boom"]
#guard err? (Occurrence.logFromJSON g (.object [("format", .string occurrencesFormat),
    ("occurrences", .array #[.object [("time", .number 1), ("subject", .string "nope"),
      ("complete", .bool true)]])]))
  "`nope` is not a node of this graph"
#guard err? (Occurrence.logFromJSON g (.object [("format", .string occurrencesFormat),
    ("occurrences", .array #[.object [("time", .number 1),
      ("subject", .string "Tests.Control.Reactive.Json.sheet.total"), ("complete", .bool true)]])]))
  "`Tests.Control.Reactive.Json.sheet.total` is not a subject"

-- ── Traces ──────────────────────────────────────────────────────────────────

def failing : List (Occurrence V) := [.next x 0 (1 : Nat), .next x 1 (500 : Nat)]

#guard ok? (FromJSON.parseJSON (ToJSON.toJSON (g.run log))) (g.run log)
#guard ok? (FromJSON.parseJSON (ToJSON.toJSON (g.run failing))) (g.run failing)
#guard ok? (Decode.decodeAs (Encode.encode (ToJSON.toJSON (g.run failing)))) (g.run failing)
#guard (Encode.encode (ToJSON.toJSON (g.run failing))).contains "\"error\":\"too big\"" 

-- ── Across processes and versions ───────────────────────────────────────────

/-- Version 2: the debounce window is now 20. -/
def sheet2 : Reactive Id V (Subject Nat × Observable Nat) := do
  let x ← Reactive.label `Tests.Control.Reactive.Json.sheet.x (subject Nat)
  let checked ← Reactive.label `Tests.Control.Reactive.Json.sheet.checked
    (x.mapE fun n => if n > 100 then .error "too big" else .ok n)
  let total ← Reactive.label `Tests.Control.Reactive.Json.sheet.total (checked.scan 0 (· + ·))
  let quiet ← Reactive.label `Tests.Control.Reactive.Json.sheet.quiet (total.debounceTime 20)
  pure (x, quiet)

-- Process 1 writes its log as text; process 2 reads it against version 2 and
-- replays it: the run of version 2 on the same inputs.
def saved : String := Encode.encode (Occurrence.logToJSON g log)

#guard match Decode.decode saved >>= Occurrence.logFromJSON sheet2.graph! with
  | .ok replayed => (sheet2.graph!.run replayed).values sheet2.result.2 == [(25, 3)]
  | .error _ => false

end Tests.Control.Reactive.Json
