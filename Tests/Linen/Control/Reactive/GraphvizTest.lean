/-
  Tests for `Linen.Control.Reactive.Graphviz`.

  Covers display labels and operators, the structure-only drawing (one DOT
  node per observable, one edge per source, numbered when an operator has
  several), the drawing of a run (values, completion, errors), and the exact
  DOT of a small graph.
-/
import Linen.Control.Reactive.Graphviz

open Control.Reactive

namespace Tests.Control.Reactive.Graphviz

abbrev V := Sum Nat String

instance : Codec V Nat :=
  ⟨.inl, fun | .inl n => .ok n | .inr _ => .error "expected a number", fun _ => rfl⟩

/-- `s` contains `sub`. -/
def has (s sub : String) : Bool := (s.splitOn sub).length > 1

-- ── Display ─────────────────────────────────────────────────────────────────

#guard displayLabel `sheet.total == "total"
#guard displayLabel (.num `map 1) == "map.1"
#guard displayLabel (.num .anonymous 3) == "3"
#guard Op.display (.take 2) == "take 2" && Op.display (.debounceTime 100) == "debounceTime 100"
#guard Op.display (.map ⟨0⟩) == "map"

-- ── A graph ─────────────────────────────────────────────────────────────────

def sheet : Reactive Id V (Subject Nat × Subject Nat) := do
  node x ← subject Nat
  node y ← subject Nat
  node sum ← combineLatest (fun (a b : Nat) => a + b) x x
  node checked ← sum.mapE fun n => if n > 10 then .error "too big" else .ok n
  node first ← y.take 1
  let _ := (checked, first)
  pure (x, y)

def g : Graph Id V := sheet.graph!
def dot := g.toDot "sheet"

-- One DOT node per observable (also proven: `Graph.toDot_nodes_size`) …
#guard dot.nodes.size == g.nodes.size
example : dot.nodes.size = g.nodes.size := Graph.toDot_nodes_size g "sheet"
-- … and one edge per source: `combineLatest f x x` gives two, numbered.
#guard (dot.edges.map fun e => (e.src.val, e.dst.val)) == [(0, 2), (0, 2), (2, 3), (1, 4)]
#guard has dot.render "n0 -> n2 [label=\"1\"];" && has dot.render "n0 -> n2 [label=\"2\"];"
#guard has dot.render "n2 -> n3;"
-- Subjects are ellipses; operators are rounded boxes naming the operator.
#guard has dot.render
  "n0 [label=\"x\", tooltip=\"Tests.Control.Reactive.Graphviz.sheet.x: subject\", \
id=\"Tests.Control.Reactive.Graphviz.sheet.x\", shape=\"ellipse\", style=\"\"];"
#guard has dot.render
  "n4 [label=\"first\\ntake 1\", tooltip=\"Tests.Control.Reactive.Graphviz.sheet.first: take 1 \
of Tests.Control.Reactive.Graphviz.sheet.y\", id=\"Tests.Control.Reactive.Graphviz.sheet.first\", \
shape=\"box\", style=\"rounded\"];"
#guard has dot.render "tooltip=\"Tests.Control.Reactive.Graphviz.sheet.sum: combineLatest [fn.1] \
of Tests.Control.Reactive.Graphviz.sheet.x, Tests.Control.Reactive.Graphviz.sheet.x\""
#guard has dot.render "digraph \"sheet\" {\n  graph [rankdir=\"LR\"];"

-- ── With a run ──────────────────────────────────────────────────────────────

def x := sheet.result.1
def y := sheet.result.2
def tr : Trace V := g.run [.next x 1 (2 : Nat), .next x 2 (7 : Nat), .next y 3 (1 : Nat)]
def showV : V → String | .inl n => toString n | .inr s => s
def dotR := g.toDotWithTrace tr showV "sheet"

-- Values: how many, and the last; live streams are green.
#guard has dotR.render "label=\"sum\\ncombineLatest\\n2 values, last 14\""
#guard has dotR.render "fillcolor=\"#e6f4ea\""
-- A failure is red, with its message …
#guard has dotR.render "label=\"checked\\nmap\\n1 value, last 4\\n✗ too big\""
#guard has dotR.render "fillcolor=\"#fde7e9\", color=\"red\""
-- … and a completed stream is grey.
#guard has dotR.render "label=\"first\\ntake 1\\n1 value, last 1\\n✓ completed\""
#guard has dotR.render "fillcolor=\"lightgray\""

-- ── The exact DOT of a small graph ──────────────────────────────────────────

def tiny : Reactive Id V (Observable Nat) := do
  node x ← subject Nat
  x.map (· + 1)

#guard tiny.graph!.toDot.render ==
  "digraph \"reactive\" {\n  graph [rankdir=\"LR\"];\n  \
node [fontname=\"Helvetica\", fontsize=\"12\"];\n  \
edge [fontname=\"Helvetica\", fontsize=\"10\", color=\"darkgray\"];\n  \
n0 [label=\"x\", tooltip=\"Tests.Control.Reactive.Graphviz.tiny.x: subject\", \
id=\"Tests.Control.Reactive.Graphviz.tiny.x\", shape=\"ellipse\", style=\"\"];\n  \
n1 [label=\"map.1\\nmap\", tooltip=\"map.1: map [fn.1] of Tests.Control.Reactive.Graphviz.tiny.x\", \
id=\"map.1\", shape=\"box\", style=\"rounded\"];\n  \
n0 -> n1;\n}\n"

end Tests.Control.Reactive.Graphviz
