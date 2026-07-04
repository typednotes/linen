/-
  Linen.Web.Html — a typed HTML5 construction library

  `Html` is indexed by a `Category` describing where a node may legally
  appear (flow content, phrasing/inline content, a `<li>`, a `<tr>`, a
  `<td>`). Each element constructor fixes the category of the children it
  accepts, so illegal nesting — a `<div>` inside a `<p>`, a `<li>` outside
  a `<ul>`/`<ol>`, children on a void element like `<img>` — is a Lean
  type error, not a browser auto-correction. Attributes go through the
  same discipline as `Web.Css.Declaration`: `Attr`'s constructor is
  private, so every attribute is produced by a typed smart constructor.

  ## Lean 4 Dependent-Type Guarantees (compile-time, zero-cost)

  - **Content model:** the `Category` index on `Html` rules out nesting
    that violates HTML5's content model for the tags modelled here (e.g.
    `p`'s children are `Html .phrasing`, so a block-level `div` cannot be
    placed inside a `p`).
  - **Void elements:** `img`/`br`/`input` simply have no children
    parameter — there is no way to pass children to them, at any type.
  - **List-item / table-cell scoping:** `li` (`Html .listItem`) and `tr`/
    `td` (`Html .tableRow`/`.tableCell`) can only be produced where their
    parent (`ul`/`ol`, `table`, `tr`) actually expects that category.
  - **Attribute provenance:** `Attr`'s `private` constructor means every
    attribute in application code came from `class_`, `href`, `style`, …
-/

import Linen.Web.Css

namespace Web.Html

-- ── Escaping ──

private def escapeTextChar (c : Char) : String :=
  match c with
  | '&' => "&amp;"
  | '<' => "&lt;"
  | '>' => "&gt;"
  | _ => c.toString

/-- Escape text for use between HTML tags. -/
def escapeText (s : String) : String := String.join (s.toList.map escapeTextChar)

private def escapeAttrChar (c : Char) : String :=
  match c with
  | '&' => "&amp;"
  | '<' => "&lt;"
  | '>' => "&gt;"
  | '"' => "&quot;"
  | _ => c.toString

/-- Escape text for use inside a double-quoted attribute value. -/
def escapeAttr (s : String) : String := String.join (s.toList.map escapeAttrChar)

-- ── Attributes ──

/-- A single `name="value"` attribute. The constructor is private: every
    `Attr` in application code is produced by a typed smart constructor
    below (`class_`, `href`, `style`, …), so an attribute can never be
    fabricated with an arbitrary name. -/
structure Attr where
  private mk ::
  attrName : String
  attrValue : String
deriving Repr, BEq

/-- Render an `Attr` as ` name="value"` (with a leading space). -/
def Attr.render (a : Attr) : String := " " ++ a.attrName ++ "=\"" ++ escapeAttr a.attrValue ++ "\""

def class_ (name : String) : Attr := ⟨"class", name⟩
def id_ (name : String) : Attr := ⟨"id", name⟩
def href (url : String) : Attr := ⟨"href", url⟩
def src (url : String) : Attr := ⟨"src", url⟩
def alt (text : String) : Attr := ⟨"alt", text⟩
def type_ (t : String) : Attr := ⟨"type", t⟩
def name_ (n : String) : Attr := ⟨"name", n⟩
def value_ (v : String) : Attr := ⟨"value", v⟩
def placeholder (p : String) : Attr := ⟨"placeholder", p⟩
def for_ (targetId : String) : Attr := ⟨"for", targetId⟩
def action (url : String) : Attr := ⟨"action", url⟩
def method_ (m : String) : Attr := ⟨"method", m⟩
def checked : Attr := ⟨"checked", "checked"⟩

/-- An inline `style` attribute built from typed `Web.Css.Declaration`s. -/
def style (decls : List Web.Css.Declaration) : Attr :=
  ⟨"style", decls.map Web.Css.Declaration.render |>.foldl (init := "") fun acc d =>
    if acc.isEmpty then d else acc ++ " " ++ d⟩

-- ── Content model ──

/-- Where an `Html` node may legally appear. -/
inductive Category where
  /-- Block-level "flow" content: `div`, `p`, `ul`, `form`, `table`, … -/
  | flow
  /-- Inline "phrasing" content: text, `span`, `a`, `img`, `input`, … -/
  | phrasing
  /-- A `<li>`, only valid inside `<ul>`/`<ol>`. -/
  | listItem
  /-- A `<tr>`, only valid inside `<table>`. -/
  | tableRow
  /-- A `<td>`, only valid inside `<tr>`. -/
  | tableCell
deriving Repr, BEq

/-- An HTML5 node, indexed by the `Category` describing where it may
    legally be placed. -/
inductive Html : Category → Type where
  | text (s : String) : Html .phrasing
  | span (attrs : List Attr) (children : List (Html .phrasing)) : Html .phrasing
  | a (attrs : List Attr) (children : List (Html .phrasing)) : Html .phrasing
  | label (attrs : List Attr) (children : List (Html .phrasing)) : Html .phrasing
  | button (attrs : List Attr) (children : List (Html .phrasing)) : Html .phrasing
  | img (attrs : List Attr) : Html .phrasing
  | br : Html .phrasing
  | input (attrs : List Attr) : Html .phrasing
  | div (attrs : List Attr) (children : List (Html .flow)) : Html .flow
  | p (attrs : List Attr) (children : List (Html .phrasing)) : Html .flow
  | h1 (attrs : List Attr) (children : List (Html .phrasing)) : Html .flow
  | h2 (attrs : List Attr) (children : List (Html .phrasing)) : Html .flow
  | h3 (attrs : List Attr) (children : List (Html .phrasing)) : Html .flow
  | form (attrs : List Attr) (children : List (Html .flow)) : Html .flow
  | ul (attrs : List Attr) (items : List (Html .listItem)) : Html .flow
  | ol (attrs : List Attr) (items : List (Html .listItem)) : Html .flow
  | li (attrs : List Attr) (children : List (Html .flow)) : Html .listItem
  | table (attrs : List Attr) (rows : List (Html .tableRow)) : Html .flow
  | tr (attrs : List Attr) (cells : List (Html .tableCell)) : Html .tableRow
  | td (attrs : List Attr) (children : List (Html .flow)) : Html .tableCell
  /-- An embedded stylesheet, e.g. `styleSheet (Web.Css.Stylesheet.render
      ss)` — only ever valid as flow content (in practice, `<head>`). -/
  | styleSheet (css : String) : Html .flow
  /-- Phrasing content is also flow content — every inline element can
      appear wherever block-level content can. -/
  | fromPhrasing (h : Html .phrasing) : Html .flow

/-- Phrasing content coerces to flow content, so e.g. `div [] [text "hi"]`
    needs no explicit `fromPhrasing`. -/
instance : Coe (Html .phrasing) (Html .flow) := ⟨Html.fromPhrasing⟩

-- ── Rendering ──

private def renderAttrs (attrs : List Attr) : String :=
  attrs.foldl (fun acc a => acc ++ a.render) ""

private def wrap (tag : String) (attrs : List Attr) (inner : String) : String :=
  "<" ++ tag ++ renderAttrs attrs ++ ">" ++ inner ++ "</" ++ tag ++ ">"

private def selfClosing (tag : String) (attrs : List Attr) : String :=
  "<" ++ tag ++ renderAttrs attrs ++ ">"

/-- Render an `Html` node to an HTML5 string. -/
def Html.render {cat : Category} : Html cat → String
  | .text s => escapeText s
  | .span attrs children => wrap "span" attrs (String.join (children.map render))
  | .a attrs children => wrap "a" attrs (String.join (children.map render))
  | .label attrs children => wrap "label" attrs (String.join (children.map render))
  | .button attrs children => wrap "button" attrs (String.join (children.map render))
  | .img attrs => selfClosing "img" attrs
  | .br => selfClosing "br" []
  | .input attrs => selfClosing "input" attrs
  | .div attrs children => wrap "div" attrs (String.join (children.map render))
  | .p attrs children => wrap "p" attrs (String.join (children.map render))
  | .h1 attrs children => wrap "h1" attrs (String.join (children.map render))
  | .h2 attrs children => wrap "h2" attrs (String.join (children.map render))
  | .h3 attrs children => wrap "h3" attrs (String.join (children.map render))
  | .form attrs children => wrap "form" attrs (String.join (children.map render))
  | .ul attrs items => wrap "ul" attrs (String.join (items.map render))
  | .ol attrs items => wrap "ol" attrs (String.join (items.map render))
  | .li attrs children => wrap "li" attrs (String.join (children.map render))
  | .table attrs rows => wrap "table" attrs (String.join (rows.map render))
  | .tr attrs cells => wrap "tr" attrs (String.join (cells.map render))
  | .td attrs children => wrap "td" attrs (String.join (children.map render))
  | .styleSheet css => "<style>" ++ css ++ "</style>"
  | .fromPhrasing h => render h

/-- Render a full HTML5 document (`<!DOCTYPE html>` + `<html>…</html>`). -/
def Html.renderDocument (titleText : String) (head : List (Html .flow)) (body : List (Html .flow)) : String :=
  "<!DOCTYPE html>\n<html><head><title>" ++ escapeText titleText ++ "</title>" ++
    String.join (head.map Html.render) ++ "</head><body>" ++
    String.join (body.map Html.render) ++ "</body></html>"

-- ── Syntax ──

/-- `elem! tag [attrs] [children]` sugar for `tag [attrs] [children]`, and
    `elem! tag [attrs]` for void elements (no children slot), e.g.

    ```
    elem! div [class_ "todo"] [elem! h1 [] [text "TODO"], elem! img [src "x.png"]]
    ```

    Beyond brevity, the macro reads as a flat tag/attrs/children triple
    while every expansion still goes through the ordinary typed
    constructors, so illegal nesting is caught exactly as it would be
    without the macro. -/
syntax "elem!" ident "[" term,* "]" "[" term,* "]" : term
syntax "elem!" ident "[" term,* "]" : term

macro_rules
  | `(elem! $tag:ident [$attrs,*] [$children,*]) => `($tag [$attrs,*] [$children,*])
  | `(elem! $tag:ident [$attrs,*]) => `($tag [$attrs,*])

end Web.Html
