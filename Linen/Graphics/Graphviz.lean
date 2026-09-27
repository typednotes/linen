/-
  `Graphics.Graphviz` — typed Graphviz DOT documents

  A DOT document built from this module's types always renders to
  syntactically valid DOT. Rather than checking text after the fact, every
  way DOT can be malformed is ruled out by construction:

  ## Lean 4 dependent-type guarantees

  - **No dangling edges.** A `Graph` stores its nodes in an array, and each
    `Edge` of it has endpoints of type `Fin nodes.size`: an edge to a node
    that does not exist does not typecheck. Nodes are written with generated
    identifiers (`n0`, `n1`, …), so no user text is ever an identifier.
  - **The edge operator follows the graph kind.** `Graph .directed` is
    written `digraph … { a -> b }` and `Graph .undirected` `graph … { a -- b }`
    — the operator is not data, so the two cannot be mixed.
  - **Attributes are typed by where they may appear.** `Attr .node`,
    `Attr .edge` and `Attr .graph` are distinct types, and `Attr`'s
    constructor is private: every attribute comes from a smart constructor
    (`shape`, `arrowhead`, `rankdir`, …) that fixes both its name and the
    targets it applies to, and whose value is typed (`Shape`, `Color`, `Nat`
    sizes — never a `Float` that could be `NaN`).
  - **Text cannot escape its quotes.** Every free-text value (labels,
    tooltips, the graph name) is written as a DOT quoted string by `quote`.
    `lex_quote` proves, against a model of how DOT's lexer reads a quoted
    string, that for *any* text the literal ends exactly at the closing quote
    we emit — so no label can terminate its string early and inject DOT.

  What is modelled is the part of DOT a program needs to draw a graph: nodes,
  edges, graph/node/edge attributes and defaults, `strict`. Not modelled:
  subgraphs and clusters, ports, HTML-like labels (`label=<…>`) and record
  shapes (whose labels have their own syntax) — so none of them can be
  produced malformed, because none of them can be produced at all.

  ## Prior art

  Haskell's `graphviz` package (`Data.GraphViz`) renders graph values to DOT
  with attributes as one sum type (`Attribute`), usable on nodes, edges and
  graphs alike — which targets accept which attribute is documented, not
  typed; Rust's `dot` crate and `petgraph::dot` likewise write generated node
  ids, as here, with attributes as strings. This module keeps their shape — a
  graph value rendered to DOT — and moves the remaining checks into types:
  attributes indexed by target, `Fin` endpoints, and a proven quoting.
-/

namespace Graphics.Graphviz

-- ── Quoted strings ──────────────────────────────────────────────────────────

/-- Escape one character for a DOT quoted string. A backslash is doubled, so
    that none of DOT's `\N`/`\G`/`\l`/… label escapes is triggered by accident;
    a line break becomes `\n`, DOT's centred line break. -/
def escapeChar : Char → List Char
  | '"'  => ['\\', '"']
  | '\\' => ['\\', '\\']
  | '\n' => ['\\', 'n']
  | c    => [c]

/-- Escape text for a DOT quoted string. -/
def escape (s : List Char) : List Char := s.flatMap escapeChar

/-- A model of DOT's lexer inside a quoted string: it reads up to the first
    quote not preceded by a backslash, taking a backslash and the character
    after it together. Returns the body read and the rest of the input. -/
def lexQuoted : List Char → Option (List Char × List Char)
  | [] => none
  | '"' :: rest => some ([], rest)
  | '\\' :: c :: rest => (lexQuoted rest).map fun (b, r) => ('\\' :: c :: b, r)
  | c :: rest => (lexQuoted rest).map fun (b, r) => (c :: b, r)

/-- One escaped character is read whole, and never ends the string. -/
theorem lex_escapeChar (c : Char) (l : List Char) :
    lexQuoted (escapeChar c ++ l) = (lexQuoted l).map fun (b, r) => (escapeChar c ++ b, r) := by
  by_cases h1 : c = '"'
  · subst h1; simp [escapeChar, lexQuoted]
  by_cases h2 : c = '\\'
  · subst h2; simp [escapeChar, lexQuoted]
  by_cases h3 : c = '\n'
  · subst h3; simp [escapeChar, lexQuoted]
  have hc : escapeChar c = [c] := by unfold escapeChar; split <;> simp_all
  rw [hc, List.singleton_append, lexQuoted.eq_def]
  split <;> simp_all

/-- **Escaped text cannot break out of its quotes.** Whatever `s` is, lexing
    `escape s` followed by a quote reads exactly `escape s` and stops at that
    quote, leaving the rest untouched. -/
theorem lex_quote (s rest : List Char) :
    lexQuoted (escape s ++ '"' :: rest) = some (escape s, rest) := by
  induction s with
  | nil => simp [escape, lexQuoted]
  | cons c s ih =>
    simp only [escape, List.flatMap_cons, List.append_assoc] at ih ⊢
    rw [lex_escapeChar, ih]
    rfl

/-- A DOT quoted string for any text. -/
def quote (s : String) : String := String.ofList ('"' :: escape s.toList ++ ['"'])

-- ── Attribute values ────────────────────────────────────────────────────────

/-- Node shapes (Graphviz's polygon shapes; record shapes, whose labels have
    their own syntax, are deliberately absent). -/
inductive Shape where
  | box | ellipse | oval | circle | doublecircle | point | plain | plaintext
  | diamond | note | tab | folder | box3d | component | cylinder | hexagon
  | octagon | parallelogram | house | invhouse | underline
  deriving Repr, DecidableEq

/-- The DOT name of a shape. -/
def Shape.name : Shape → String
  | .box => "box" | .ellipse => "ellipse" | .oval => "oval" | .circle => "circle"
  | .doublecircle => "doublecircle" | .point => "point" | .plain => "plain"
  | .plaintext => "plaintext" | .diamond => "diamond" | .note => "note" | .tab => "tab"
  | .folder => "folder" | .box3d => "box3d" | .component => "component"
  | .cylinder => "cylinder" | .hexagon => "hexagon" | .octagon => "octagon"
  | .parallelogram => "parallelogram" | .house => "house" | .invhouse => "invhouse"
  | .underline => "underline"

/-- Named colours (a portable subset of Graphviz's X11 scheme). -/
inductive ColorName where
  | black | white | gray | lightgray | darkgray | red | green | blue | yellow
  | orange | purple | brown | pink | transparent
  deriving Repr, DecidableEq

/-- A colour: named, or 8-bit RGB. -/
inductive Color where
  | named (c : ColorName)
  | rgb (r g b : UInt8)
  deriving Repr, DecidableEq

/-- Two lowercase hex digits. -/
private def hex2 (n : UInt8) : String :=
  let d (k : Nat) : Char := "0123456789abcdef".toList.getD k '0'
  String.ofList [d (n.toNat / 16), d (n.toNat % 16)]

/-- The DOT spelling of a colour. -/
def Color.render : Color → String
  | .named c => match c with
    | .black => "black" | .white => "white" | .gray => "gray" | .lightgray => "lightgray"
    | .darkgray => "darkgray" | .red => "red" | .green => "green" | .blue => "blue"
    | .yellow => "yellow" | .orange => "orange" | .purple => "purple" | .brown => "brown"
    | .pink => "pink" | .transparent => "transparent"
  | .rgb r g b => "#" ++ hex2 r ++ hex2 g ++ hex2 b

/-- Layout direction. -/
inductive RankDir where
  | TB | LR | BT | RL
  deriving Repr, DecidableEq

/-- Node styles. -/
inductive NodeStyle where
  | filled | rounded | dashed | dotted | bold | solid | invis
  deriving Repr, DecidableEq

/-- Edge styles. -/
inductive EdgeStyle where
  | dashed | dotted | bold | solid | invis
  deriving Repr, DecidableEq

/-- Arrow shapes. -/
inductive Arrow where
  | normal | none | vee | dot | odot | diamond | box | tee | inv
  deriving Repr, DecidableEq

private def RankDir.name : RankDir → String
  | .TB => "TB" | .LR => "LR" | .BT => "BT" | .RL => "RL"
private def NodeStyle.name : NodeStyle → String
  | .filled => "filled" | .rounded => "rounded" | .dashed => "dashed" | .dotted => "dotted"
  | .bold => "bold" | .solid => "solid" | .invis => "invis"
private def EdgeStyle.name : EdgeStyle → String
  | .dashed => "dashed" | .dotted => "dotted" | .bold => "bold" | .solid => "solid"
  | .invis => "invis"
private def Arrow.name : Arrow → String
  | .normal => "normal" | .none => "none" | .vee => "vee" | .dot => "dot" | .odot => "odot"
  | .diamond => "diamond" | .box => "box" | .tee => "tee" | .inv => "inv"

-- ── Attributes ──────────────────────────────────────────────────────────────

/-- Where an attribute appears. -/
inductive Target where
  | graph | node | edge
  deriving Repr, DecidableEq

/-- An attribute for target `t`. The constructor is private: every attribute
    comes from a smart constructor below, which fixes its name and the targets
    it is valid on. Its value is always written quoted. -/
structure Attr (t : Target) where
  private mk ::
  /-- The attribute's name. -/
  key : String
  /-- Its value, before quoting. -/
  value : String
  deriving Repr, DecidableEq

/-- `key="value"`. -/
def Attr.render {t : Target} (a : Attr t) : String := a.key ++ "=" ++ quote a.value

/-- The label (any target). -/
def label {t : Target} (s : String) : Attr t := ⟨"label", s⟩
/-- A tooltip, shown on hover in SVG output (any target). -/
def tooltip {t : Target} (s : String) : Attr t := ⟨"tooltip", s⟩
/-- An identifier for the SVG element produced (any target). -/
def id {t : Target} (s : String) : Attr t := ⟨"id", s⟩
/-- The line / text colour (any target). -/
def color {t : Target} (c : Color) : Attr t := ⟨"color", c.render⟩
/-- The text colour (any target). -/
def fontcolor {t : Target} (c : Color) : Attr t := ⟨"fontcolor", c.render⟩
/-- The font family (any target). -/
def fontname {t : Target} (s : String) : Attr t := ⟨"fontname", s⟩
/-- The font size in points (any target). -/
def fontsize {t : Target} (pt : Nat) : Attr t := ⟨"fontsize", toString pt⟩
/-- An external label, beside the node or edge. -/
def xlabel {t : Target} (s : String) (_notGraph : t ≠ .graph := by decide) : Attr t := ⟨"xlabel", s⟩
/-- The line width in points (nodes and edges). -/
def penwidth {t : Target} (pt : Nat) (_notGraph : t ≠ .graph := by decide) : Attr t :=
  ⟨"penwidth", toString pt⟩
/-- The node shape. -/
def shape (s : Shape) : Attr .node := ⟨"shape", s.name⟩
/-- The node styles, combined. -/
def style (ss : List NodeStyle) : Attr .node := ⟨"style", ",".intercalate (ss.map NodeStyle.name)⟩
/-- The fill colour (with `style [.filled]`). -/
def fillcolor (c : Color) : Attr .node := ⟨"fillcolor", c.render⟩
/-- The edge styles, combined. -/
def edgeStyle (ss : List EdgeStyle) : Attr .edge :=
  ⟨"style", ",".intercalate (ss.map EdgeStyle.name)⟩
/-- The arrow at the head of an edge. -/
def arrowhead (a : Arrow) : Attr .edge := ⟨"arrowhead", a.name⟩
/-- The arrow at the tail of an edge. -/
def arrowtail (a : Arrow) : Attr .edge := ⟨"arrowtail", a.name⟩
/-- The layout direction. -/
def rankdir (d : RankDir) : Attr .graph := ⟨"rankdir", d.name⟩
/-- The background colour. -/
def bgcolor (c : Color) : Attr .graph := ⟨"bgcolor", c.render⟩

-- ── Graphs ──────────────────────────────────────────────────────────────────

/-- Directed (`digraph`, `->`) or undirected (`graph`, `--`). -/
inductive Kind where
  | directed | undirected
  deriving Repr, DecidableEq

/-- The keyword introducing a graph of this kind. -/
def Kind.keyword : Kind → String
  | .directed => "digraph" | .undirected => "graph"

/-- The edge operator of this kind. -/
def Kind.edgeOp : Kind → String
  | .directed => "->" | .undirected => "--"

/-- A node: only attributes — its identity is its position. -/
structure Node where
  /-- The node's attributes. -/
  attrs : List (Attr .node) := []
  deriving Repr, DecidableEq

/-- An edge between two of the `n` nodes of a graph. -/
structure Edge (n : Nat) where
  /-- The tail. -/
  src : Fin n
  /-- The head. -/
  dst : Fin n
  /-- The edge's attributes. -/
  attrs : List (Attr .edge) := []
  deriving Repr, DecidableEq

/-- The same edge, between the same nodes counted by a provably equal number. -/
def Edge.cast {n m : Nat} (h : n = m) (e : Edge n) : Edge m :=
  ⟨e.src.cast h, e.dst.cast h, e.attrs⟩

/-- A DOT graph of kind `k`. Its edges can only connect its own nodes. -/
structure Graph (k : Kind) where
  /-- The graph's name. -/
  name : String := "G"
  /-- `strict`: at most one edge between two nodes. -/
  strict : Bool := false
  /-- Graph attributes. -/
  attrs : List (Attr .graph) := []
  /-- Attributes every node gets unless it overrides them. -/
  nodeDefaults : List (Attr .node) := []
  /-- Attributes every edge gets unless it overrides them. -/
  edgeDefaults : List (Attr .edge) := []
  /-- The nodes. -/
  nodes : Array Node := #[]
  /-- The edges, between the nodes above. -/
  edges : List (Edge nodes.size) := []

/-- The identifier a node is written with. -/
def nodeId (i : Nat) : String := s!"n{i}"

/-- ` [a="…", b="…"]`, or nothing. -/
private def attrList {t : Target} : List (Attr t) → String
  | [] => ""
  | as => " [" ++ ", ".intercalate (as.map Attr.render) ++ "]"

/-- Render a graph as a DOT document. -/
def Graph.render {k : Kind} (g : Graph k) : String :=
  let defaults (kw : String) (as : List String) : List String :=
    if as.isEmpty then [] else [s!"  {kw} [{", ".intercalate as}];"]
  let lines :=
    defaults "graph" (g.attrs.map Attr.render) ++
    defaults "node" (g.nodeDefaults.map Attr.render) ++
    defaults "edge" (g.edgeDefaults.map Attr.render) ++
    ((List.range g.nodes.size).map fun i =>
      s!"  {nodeId i}{attrList ((g.nodes[i]?.map Node.attrs).getD [])};") ++
    g.edges.map fun e =>
      s!"  {nodeId e.src} {k.edgeOp} {nodeId e.dst}{attrList e.attrs};"
  (if g.strict then "strict " else "") ++ k.keyword ++ " " ++ quote g.name ++ " {\n" ++
    String.join (lines.map (· ++ "\n")) ++ "}\n"

end Graphics.Graphviz
