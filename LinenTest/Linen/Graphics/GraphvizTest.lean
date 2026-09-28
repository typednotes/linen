/-
  Tests for `Linen.Graphics.Graphviz`.

  Covers quoting (escapes, the lexer model and `lex_quote` on hostile text),
  attribute rendering, directed / undirected / strict graphs with defaults,
  and the typing discipline: attributes are rejected on the wrong target and
  edges cannot name a node that does not exist.
-/
import Linen.Graphics.Graphviz

open Graphics.Graphviz

namespace Tests.Graphics.Graphviz

-- ── Quoting ─────────────────────────────────────────────────────────────────

#guard quote "plain" == "\"plain\""
#guard quote "say \"hi\"" == "\"say \\\"hi\\\"\""
#guard quote "back\\slash" == "\"back\\\\slash\""
#guard quote "two\nlines" == "\"two\\nlines\""
#guard quote "" == "\"\""
-- `\N` (DOT's "node name" escape) stays literal: the backslash is doubled.
#guard quote "\\N" == "\"\\\\N\""

-- The lexer model stops at the first unescaped quote …
#guard lexQuoted "ab\"rest".toList == some ("ab".toList, "rest".toList)
#guard lexQuoted "a\\\"b\"r".toList == some ("a\\\"b".toList, "r".toList)
#guard lexQuoted "unterminated".toList == none
-- … and escaped text, however hostile, is read back whole.
#guard ["\" ] ; evil -> x [", "\\", "\\\"", "a\nb", "\\\\\""].all fun s =>
  lexQuoted (escape s.toList ++ '"' :: "tail".toList) == some (escape s.toList, "tail".toList)
example (s : List Char) : lexQuoted (escape s ++ ['"']) = some (escape s, []) := lex_quote s []

-- ── Attributes ──────────────────────────────────────────────────────────────

#guard (label "x" : Attr .node).render == "label=\"x\""
#guard (shape .box).render == "shape=\"box\""
#guard (style [.filled, .rounded]).render == "style=\"filled,rounded\""
#guard (fillcolor (.rgb 0xe6 0xf4 0x0a)).render == "fillcolor=\"#e6f40a\""
#guard (color (.named .red) : Attr .edge).render == "color=\"red\""
#guard (fontsize 12 : Attr .graph).render == "fontsize=\"12\""
#guard (penwidth 2 : Attr .edge).render == "penwidth=\"2\""
#guard (arrowhead .vee).render == "arrowhead=\"vee\""
#guard (rankdir .LR).render == "rankdir=\"LR\""

-- ── Graphs ──────────────────────────────────────────────────────────────────

/-- Two nodes and an edge; endpoints are `Fin`s of the node count. -/
def small : Graph .directed :=
  { name := "small"
    attrs := [rankdir .LR]
    nodeDefaults := [shape .box]
    nodes := #[{ attrs := [label "a"] }, { attrs := [label "b \"quoted\""] }]
    edges := [{ src := ⟨0, by decide⟩, dst := ⟨1, by decide⟩, attrs := [label "a→b"] }] }

#guard small.render ==
  "digraph \"small\" {\n  graph [rankdir=\"LR\"];\n  node [shape=\"box\"];\n  \
n0 [label=\"a\"];\n  n1 [label=\"b \\\"quoted\\\"\"];\n  n0 -> n1 [label=\"a→b\"];\n}\n"

/-- The same shape, undirected and strict: the operator follows the kind. -/
def undirected : Graph .undirected :=
  { strict := true
    nodes := #[{}, {}]
    edges := [{ src := ⟨0, by decide⟩, dst := ⟨1, by decide⟩ }] }

#guard undirected.render == "strict graph \"G\" {\n  n0;\n  n1;\n  n0 -- n1;\n}\n"
#guard (({} : Graph .directed)).render == "digraph \"G\" {\n}\n"
-- A hostile graph name stays inside its quotes.
#guard ({ name := "x\" { evil }" } : Graph .directed).render ==
  "digraph \"x\\\" { evil }\" {\n}\n"

-- ── Typing ──────────────────────────────────────────────────────────────────

-- A node attribute is not an edge attribute.
/--
error: Application type mismatch: The argument
  shape Shape.box
has type
  Attr Target.node
but is expected to have type
  Attr Target.edge
in the application
  List.cons (shape Shape.box)
-/
#guard_msgs in
example : Edge 2 := { src := ⟨0, by decide⟩, dst := ⟨1, by decide⟩, attrs := [shape .box] }

-- `penwidth` does not apply to the graph.
/--
error: could not synthesize default value for parameter '_notGraph' using tactics
---
error: Tactic `decide` proved that the proposition
  Target.graph ≠ Target.graph
is false
-/
#guard_msgs in
example : Attr .graph := penwidth 2

-- An edge to a node that does not exist does not typecheck.
/--
error: Tactic `decide` proved that the proposition
  2 < #[{ }, { }].size
is false
-/
#guard_msgs in
example : Graph .directed :=
  { nodes := #[{}, {}], edges := [{ src := ⟨0, by decide⟩, dst := ⟨2, by decide⟩ }] }

end Tests.Graphics.Graphviz
