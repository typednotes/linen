/-
  Linen.Web.Css — a typed CSS construction library

  Every CSS declaration is produced by a *smart constructor* (`color`,
  `margin`, `display`, …) that pins down both the property name and the
  Lean type of its value (`Color`, `Length`, `Display`, …). `Declaration`'s
  constructor is private, so the only way to build one is through these
  typed functions — writing `color (.block)` (a `Display`, not a `Color`)
  or inventing an arbitrary `property := value` pair is a compile-time
  error, not a runtime CSS mistake.

  ## Lean 4 Dependent-Type Guarantees (compile-time, zero-cost)

  - **Property/value pairing:** `Declaration`'s `private` constructor means
    application code can only produce one via `color`, `margin`, `display`,
    etc. — the property name and the shape of its value can never drift
    apart.
  - **Units at the type level:** `Length` (`px`/`pct`/`em`/`rem`/`vw`/`vh`/
    `auto`/`zero`) rules out unit-less or misspelled-unit values.
  - **Bounded numeric font weights:** `FontWeight.numeric`'s proof field
    (`by decide`) rejects any value outside the CSS-legal 100–900 range at
    compile time.
-/

namespace Web.Css

-- ── Length ──

/-- A CSS `<length>` or `<percentage>` value. -/
inductive Length where
  | px (n : Int)
  | pct (n : Int)
  | em (n : Float)
  | rem (n : Float)
  | vw (n : Int)
  | vh (n : Int)
  | auto
  | zero
deriving Repr, BEq

/-- Render a `Length` as CSS. -/
def Length.render : Length → String
  | .px n => toString n ++ "px"
  | .pct n => toString n ++ "%"
  | .em n => toString n ++ "em"
  | .rem n => toString n ++ "rem"
  | .vw n => toString n ++ "vw"
  | .vh n => toString n ++ "vh"
  | .auto => "auto"
  | .zero => "0"

/-- The four sides of a box (for `margin`/`padding` shorthand), in CSS's
    top/right/bottom/left order. -/
structure BoxSides where
  top : Length
  right : Length
  bottom : Length
  left : Length
deriving Repr, BEq

/-- The same `Length` on all four sides. -/
def BoxSides.all (l : Length) : BoxSides := ⟨l, l, l, l⟩

/-- Render a `BoxSides` as a CSS shorthand value (`top right bottom left`). -/
def BoxSides.render (b : BoxSides) : String :=
  b.top.render ++ " " ++ b.right.render ++ " " ++ b.bottom.render ++ " " ++ b.left.render

-- ── Color ──

/-- A CSS `<color>` value. -/
inductive Color where
  /-- A named color (`"crimson"`, `"transparent"`, …). -/
  | named (name : String)
  | rgb (r g b : UInt8)
  /-- A 6-digit hex color, without the leading `#`. -/
  | hex (code : String)
deriving Repr, BEq

/-- Render a `Color` as CSS. -/
def Color.render : Color → String
  | .named n => n
  | .rgb r g b => "rgb(" ++ toString r.toNat ++ ", " ++ toString g.toNat ++ ", " ++ toString b.toNat ++ ")"
  | .hex c => "#" ++ c

-- ── Small enumerated properties ──

/-- The CSS `display` keyword. -/
inductive Display where
  | block | inline | inlineBlock | flex | inlineFlex | grid | none_
deriving Repr, BEq

def Display.render : Display → String
  | .block => "block"
  | .inline => "inline"
  | .inlineBlock => "inline-block"
  | .flex => "flex"
  | .inlineFlex => "inline-flex"
  | .grid => "grid"
  | .none_ => "none"

/-- The CSS `position` keyword. -/
inductive Position where
  | static | relative | absolute | fixed | sticky
deriving Repr, BEq

def Position.render : Position → String
  | .static => "static"
  | .relative => "relative"
  | .absolute => "absolute"
  | .fixed => "fixed"
  | .sticky => "sticky"

/-- The CSS `text-align` keyword. -/
inductive TextAlign where
  | left | right | center | justify
deriving Repr, BEq

def TextAlign.render : TextAlign → String
  | .left => "left" | .right => "right" | .center => "center" | .justify => "justify"

/-- The CSS `text-decoration-line` keyword. -/
inductive TextDecoration where
  | none_ | underline | lineThrough
deriving Repr, BEq

def TextDecoration.render : TextDecoration → String
  | .none_ => "none" | .underline => "underline" | .lineThrough => "line-through"

/-- The CSS `cursor` keyword. -/
inductive Cursor where
  | default_ | pointer | text_ | notAllowed
deriving Repr, BEq

def Cursor.render : Cursor → String
  | .default_ => "default" | .pointer => "pointer" | .text_ => "text" | .notAllowed => "not-allowed"

/-- The CSS `border-style` keyword. -/
inductive BorderStyle where
  | none_ | solid | dashed | dotted
deriving Repr, BEq

def BorderStyle.render : BorderStyle → String
  | .none_ => "none" | .solid => "solid" | .dashed => "dashed" | .dotted => "dotted"

/-- The CSS `list-style-type` keyword. -/
inductive ListStyleType where
  | none_ | disc | decimal | circle
deriving Repr, BEq

def ListStyleType.render : ListStyleType → String
  | .none_ => "none" | .disc => "disc" | .decimal => "decimal" | .circle => "circle"

/-- The CSS `flex-direction` keyword. -/
inductive FlexDirection where
  | row | column | rowReverse | columnReverse
deriving Repr, BEq

def FlexDirection.render : FlexDirection → String
  | .row => "row" | .column => "column"
  | .rowReverse => "row-reverse" | .columnReverse => "column-reverse"

/-- The CSS `justify-content` keyword. -/
inductive JustifyContent where
  | flexStart | flexEnd | center | spaceBetween | spaceAround
deriving Repr, BEq

def JustifyContent.render : JustifyContent → String
  | .flexStart => "flex-start" | .flexEnd => "flex-end" | .center => "center"
  | .spaceBetween => "space-between" | .spaceAround => "space-around"

/-- The CSS `align-items` keyword. -/
inductive AlignItems where
  | flexStart | flexEnd | center | stretch | baseline
deriving Repr, BEq

def AlignItems.render : AlignItems → String
  | .flexStart => "flex-start" | .flexEnd => "flex-end" | .center => "center"
  | .stretch => "stretch" | .baseline => "baseline"

-- ── Font weight (bounded numeric values) ──

/-- A CSS `font-weight`. The proof-carrying `numeric` smart constructor
    rejects out-of-range values (must be a multiple of 100 in `[100, 900]`)
    at compile time — erased at runtime. -/
structure FontWeight where
  private mk ::
  value : String
deriving Repr, BEq

def FontWeight.normal : FontWeight := ⟨"normal"⟩
def FontWeight.bold : FontWeight := ⟨"bold"⟩

/-- A numeric font weight. The proof argument defaults to `by decide`, so
    `FontWeight.numeric 450` fails to compile (`450 % 100 ≠ 0`) while
    `FontWeight.numeric 600` compiles. -/
def FontWeight.numeric (n : Nat) (_h : n % 100 = 0 ∧ 100 ≤ n ∧ n ≤ 900 := by decide) : FontWeight :=
  ⟨toString n⟩

def FontWeight.render (w : FontWeight) : String := w.value

-- ── Declarations (the type-safe core) ──

/-- A single `property: value;` declaration. The constructor is private:
    every `Declaration` in application code is produced by one of the
    typed smart constructors below, so a property and a value of the
    wrong kind for it can never be paired. -/
structure Declaration where
  private mk ::
  property : String
  value : String
deriving Repr, BEq

/-- Render a `Declaration` as CSS (without indentation). -/
def Declaration.render (d : Declaration) : String := d.property ++ ": " ++ d.value ++ ";"

def color (c : Color) : Declaration := ⟨"color", c.render⟩
def backgroundColor (c : Color) : Declaration := ⟨"background-color", c.render⟩
def fontFamily (names : List String) : Declaration := ⟨"font-family", ", ".intercalate names⟩
def fontSize (l : Length) : Declaration := ⟨"font-size", l.render⟩
def fontWeight (w : FontWeight) : Declaration := ⟨"font-weight", w.render⟩
def margin (l : Length) : Declaration := ⟨"margin", l.render⟩
def marginBox (b : BoxSides) : Declaration := ⟨"margin", b.render⟩
def padding (l : Length) : Declaration := ⟨"padding", l.render⟩
def paddingBox (b : BoxSides) : Declaration := ⟨"padding", b.render⟩
def width (l : Length) : Declaration := ⟨"width", l.render⟩
def maxWidth (l : Length) : Declaration := ⟨"max-width", l.render⟩
def height (l : Length) : Declaration := ⟨"height", l.render⟩
def minHeight (l : Length) : Declaration := ⟨"min-height", l.render⟩
def display (d : Display) : Declaration := ⟨"display", d.render⟩
def position (p : Position) : Declaration := ⟨"position", p.render⟩
def textAlign (t : TextAlign) : Declaration := ⟨"text-align", t.render⟩
def textDecoration (t : TextDecoration) : Declaration := ⟨"text-decoration", t.render⟩
def cursor (c : Cursor) : Declaration := ⟨"cursor", c.render⟩
def borderRadius (l : Length) : Declaration := ⟨"border-radius", l.render⟩
def border (w : Length) (s : BorderStyle) (c : Color) : Declaration :=
  ⟨"border", w.render ++ " " ++ s.render ++ " " ++ c.render⟩
def listStyleType (t : ListStyleType) : Declaration := ⟨"list-style-type", t.render⟩
def flexDirection (d : FlexDirection) : Declaration := ⟨"flex-direction", d.render⟩
def justifyContent (j : JustifyContent) : Declaration := ⟨"justify-content", j.render⟩
def alignItems (a : AlignItems) : Declaration := ⟨"align-items", a.render⟩
def gap (l : Length) : Declaration := ⟨"gap", l.render⟩
def boxSizing (borderBox : Bool) : Declaration :=
  ⟨"box-sizing", if borderBox then "border-box" else "content-box"⟩

-- ── Selectors ──

/-- A CSS selector, built compositionally. -/
inductive Selector where
  | tag (name : String)
  | class_ (name : String)
  | id_ (name : String)
  | universal
  /-- Compound selector (no separator), e.g. `div.todo`. -/
  | and (a b : Selector)
  /-- Descendant combinator (` `), e.g. `ul li`. -/
  | descendant (a b : Selector)
  /-- Child combinator (`>`), e.g. `ul > li`. -/
  | child (a b : Selector)
  | hover (a : Selector)
  | focus (a : Selector)
deriving Repr, BEq

def Selector.render : Selector → String
  | .tag n => n
  | .class_ n => "." ++ n
  | .id_ n => "#" ++ n
  | .universal => "*"
  | .and a b => a.render ++ b.render
  | .descendant a b => a.render ++ " " ++ b.render
  | .child a b => a.render ++ " > " ++ b.render
  | .hover a => a.render ++ ":hover"
  | .focus a => a.render ++ ":focus"

-- ── Rules and stylesheets ──

/-- A selector paired with the declarations that apply to it. -/
structure Rule where
  selector : Selector
  declarations : List Declaration
deriving Repr, BEq

/-- Render a `Rule` as a CSS block. -/
def Rule.render (r : Rule) : String :=
  r.selector.render ++ " {\n" ++
    "\n".intercalate (r.declarations.map (fun d => "  " ++ d.render)) ++
    "\n}"

/-- A stylesheet is simply an ordered list of rules. -/
abbrev Stylesheet := List Rule

/-- Render a `Stylesheet` as CSS text. -/
def Stylesheet.render (ss : Stylesheet) : String :=
  "\n\n".intercalate (ss.map Rule.render)

-- ── Syntax ──

/-- `rule! selector { decl, decl, … }` builds a `Rule`, e.g.

    ```
    rule! (.class_ "todo") { color (.named "crimson"), padding (.px 8) }
    ```

    reads close to real CSS while every declaration still goes through the
    typed smart constructors above. -/
syntax "rule!" "(" term ")" "{" term,* "}" : term

macro_rules
  | `(rule! ($sel) { $decls,* }) => `(Rule.mk $sel [$decls,*])

end Web.Css
