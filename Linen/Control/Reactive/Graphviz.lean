/-
  `Control.Reactive.Graphviz` — reactive graphs as Graphviz DOT

  `Graph.toDot` draws a reactive graph as a typed
  `Graphics.Graphviz.Graph .directed`: one DOT node per observable (subjects
  as ellipses, operators as rounded boxes, labelled with the operator and its
  parameter: `take 2`, `debounceTime 100`), one edge per source, from the
  source to the operator reading it (numbered when an operator has several,
  so `combineLatest f x x` shows both). `Graph.toDotWithTrace` also shows a
  run: each node's number of values and last value, and how it ended —
  completed (grey) or in error (red, with the message).

  Display labels are short — a label's last component (`sheet.total` ↦
  `total`, `map.1` stays `map.1`) — and the full label, the functions and the
  sources are in the tooltip. The DOT is well formed by construction (see
  `Graphics.Graphviz`); edges are built from the graph's own `WellFormed`
  proof, so there is no index to check and nothing that could dangle.
-/
import Linen.Control.Reactive
import Linen.Graphics.Graphviz

namespace Control.Reactive

open Graphics

/-- A short display form of a label: its last component, keeping the number of
    a generated label (`sheet.total` ↦ `total`, `map.1` ↦ `map.1`). -/
def displayLabel : Lean.Name → String
  | .str _ s          => s
  | .num (.str _ s) k => s!"{s}.{k}"
  | l                 => l.toString

/-- An operator with its parameter, for display. -/
def Op.display : Op → String
  | .take n => s!"take {n}"
  | .skip n => s!"skip {n}"
  | .throttleTime d => s!"throttleTime {d}"
  | .debounceTime d => s!"debounceTime {d}"
  | .delay d => s!"delay {d}"
  | op => op.name

namespace Graph

variable {m : Type → Type} {V : Type}

/-- The DOT edges of `g`: one per source, source ↦ node. The endpoints are
    `Fin`s obtained from `g.wellFormed` — a source precedes its node. -/
def dotEdges (g : Graph m V) : List (Graphviz.Edge g.nodes.size) :=
  (List.finRange g.nodes.size).flatMap fun (i : Fin g.nodes.size) =>
    have hw : ∀ a ∈ (g.nodes[i.val]'i.isLt).args, a.idx < i.val :=
      (g.wellFormed i.val i.isLt).2.1
    let args := (g.nodes[i.val]'i.isLt).args
    args.attach.mapIdx fun j ⟨a, ha⟩ =>
      { src := ⟨a.idx, Nat.lt_trans (hw a ha) i.isLt⟩
        dst := i
        attrs := if args.length ≥ 2 then [Graphviz.label (toString (j + 1))] else [] }

/-- How a node's stream is shown: a line of text and its style. -/
private def streamSummary {V : Type} (showValue : V → String)
    (events : List (Time × Notification V)) : Option (String × List (Graphviz.Attr .node)) :=
  let values := events.filterMap fun (_, n) => match n with
    | .next v => some v
    | _ => none
  let count := match values.length with
    | 1 => "1 value"
    | k => s!"{k} values"
  let last := match values.getLast? with
    | some v => s!"{count}, last {showValue v}"
    | none => count
  let error := events.findSome? fun (_, n) => match n with
    | .error msg => some msg
    | _ => none
  let completed := events.any fun (_, n) => match n with
    | .complete => true
    | _ => false
  match error with
  | some msg => some (s!"{last}\n✗ {msg}",
      [Graphviz.fillcolor (.rgb 0xfd 0xe7 0xe9), Graphviz.color (.named .red)])
  | none =>
    if completed then some (s!"{last}\n✓ completed", [Graphviz.fillcolor (.named .lightgray)])
    else if values.isEmpty then none
    else some (last, [Graphviz.fillcolor (.rgb 0xe6 0xf4 0xea)])

/-- The attributes of node `i`'s DOT node, given its stream, if a run is shown. -/
private def dotNode (g : Graph m V) (showValue : V → String)
    (events : Option (List (Time × Notification V))) (i : NodeId) : Graphviz.Node :=
  let n := (g.node? i).getD default
  let name := displayLabel (g.label i)
  let heading := match n.op with
    | .subject => name
    | op => s!"{name}\n{op.display}"
  let fns := n.op.fns.map fun f => toString (g.fnLabel f)
  let tip := s!"{g.label i}: {n.op.display}" ++
    (if fns.isEmpty then "" else s!" [{", ".intercalate fns}]") ++
    (if n.args.isEmpty then "" else s!" of {", ".intercalate (n.args.map (toString ∘ g.label))}")
  let shapeAttrs : List (Graphviz.Attr .node) := match n.op with
    | .subject => [Graphviz.shape .ellipse]
    | _ => [Graphviz.shape .box]
  let base : List Graphviz.NodeStyle := match n.op with
    | .subject => []
    | _ => [.rounded]
  let (text, styled) : String × List (Graphviz.Attr .node) :=
    match events.bind (streamSummary showValue) with
    | none => (heading, [Graphviz.style base])
    | some (line, attrs) => (s!"{heading}\n{line}", Graphviz.style (.filled :: base) :: attrs)
  { attrs := [Graphviz.label text, Graphviz.tooltip tip, Graphviz.id (toString (g.label i))] ++
      shapeAttrs ++ styled }

/-- Draw `g`, with each node's stream where `events` gives one. -/
private def dotWith (g : Graph m V) (showValue : V → String)
    (events : NodeId → Option (List (Time × Notification V))) (name : String) :
    Graphviz.Graph .directed :=
  { name
    attrs := [Graphviz.rankdir .LR]
    nodeDefaults := [Graphviz.fontname "Helvetica", Graphviz.fontsize 12]
    edgeDefaults := [Graphviz.fontname "Helvetica", Graphviz.fontsize 10,
                     Graphviz.color (.named .darkgray)]
    nodes := Array.ofFn (n := g.nodes.size) fun i => g.dotNode showValue (events ⟨i.val⟩) ⟨i.val⟩
    edges := g.dotEdges.map (Graphviz.Edge.cast (Array.size_ofFn ..).symm) }

/-- Draw `g`'s structure. -/
def toDot (g : Graph m V) (name : String := "reactive") : Graphviz.Graph .directed :=
  g.dotWith (fun _ => "") (fun _ => none) name

/-- Draw `g` with a run of it, each value rendered by `showValue`. -/
def toDotWithTrace (g : Graph m V) (tr : Trace V) (showValue : V → String)
    (name : String := "reactive") : Graphviz.Graph .directed :=
  g.dotWith showValue (fun i => some (tr.events i)) name

end Graph

-- ── Properties ──────────────────────────────────────────────────────────────

/-- One DOT node per node. -/
theorem Graph.toDot_nodes_size {m : Type → Type} {V : Type} (g : Graph m V) (name : String) :
    (g.toDot name).nodes.size = g.nodes.size := by
  simp [toDot, dotWith]

end Control.Reactive
