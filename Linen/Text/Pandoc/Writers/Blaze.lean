/-
  `Linen.Text.Pandoc.Writers.Blaze` — the HTML→`Doc` layout shim.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.Blaze` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Writers/Blaze.hs`).

  Upstream exports the single function `layoutMarkup :: Markup -> Doc Text`,
  which walks a `blaze-html` `MarkupM` tree and renders it into a
  `Text.DocLayout` `Doc Text`, turning HTML whitespace into *breakable* layout
  spaces/newlines (`toChunks`) so the pretty-printer can reflow it, while
  keeping the contents of `code`/`pre`/`style`/`script`/`textarea` unbreakable.

  ### Deviations from upstream (retargeted onto `Linen.Web.Html`)

  Per `docs/imports/pandoc/dependencies.md`'s blaze-substitution note, `linen`
  already provides a typed HTML-construction library (`Linen.Web.Html`), which
  the precedence rule elects over importing `blaze-html`.  So instead of
  dispatching on `blaze-html`'s `MarkupM` constructors
  (`Parent`/`Leaf`/`CustomParent`/`CustomLeaf`/`Content`/`Comment`/`Append`/
  `AddAttribute`/`Empty`), `layoutMarkup` here traverses the typed
  `Web.Html.Html` tree.  The *rendering* behaviour is preserved faithfully:

  * text content is emitted through `withWrap`/`toChunks`, so runs of spaces
    become a single breakable `space` and newline runs become `cr`, letting the
    `Doc` reflow to `writerColumns`;
  * element attributes are rendered as unbreakable ` name="value"` runs with
    XML-escaped values (`Web.Html.escapeAttr`), and text is entity-escaped
    (`escapeMarkupEntities`), matching blaze's `fromChoiceString`;
  * `Web.Html`'s `styleSheet` (a `<style>` block) is emitted wrap-disabled and
    `flush`ed, exactly as blaze special-cases `style`/`pre`/… ;
  * void elements (`img`/`br`/`input`) render as a single open tag with no
    children, as blaze's `Leaf`/`CustomLeaf` do.

  `Web.Html` models a fixed tag set, so the `code`/`pre`/`script`/`textarea`
  wrap-suppression only applies through `styleSheet` here; the other tags all
  reflow (their inline text was already wrappable in blaze too).
-/

import Linen.Web.Html
import Linen.Text.DocLayout

namespace Linen.Text.Pandoc.Writers.Blaze

open _root_.Text.DocLayout
  (Doc literal space cr char flush hcatList)

/- ── Entity escaping ───────────────────────────────────────────────────── -/

/-- Escape the HTML metacharacters `< > & " '` (upstream `escapeMarkupEntities`). -/
def escapeMarkupEntities (s : String) : String :=
  String.join <| s.toList.map fun c =>
    match c with
    | '<' => "&lt;"
    | '>' => "&gt;"
    | '&' => "&amp;"
    | '"' => "&quot;"
    | '\'' => "&#39;"
    | _ => c.toString

/- ── Whitespace chunking ───────────────────────────────────────────────── -/

/-- The layout status of a character: `0` = a space, `1` = a newline, `2` =
    other.  Consecutive equal-status characters group together. -/
private def status (c : Char) : Nat :=
  if c == ' ' then 0 else if c == '\n' then 1 else 2

/-- Group a character list into maximal same-`status` runs (upstream's
    `T.groupBy sameStatus`), preserving order. -/
private def groupByStatus : List Char → List (List Char)
  | [] => []
  | c :: cs =>
      match groupByStatus cs with
      | (g@(g0 :: _)) :: gs => if status c == status g0 then (c :: g) :: gs else [c] :: g :: gs
      | _ => [[c]]

/-- Turn text into layout chunks: a run of spaces becomes one breakable
    `space`, a run of newlines becomes `cr`, any other run becomes a
    `literal` (upstream `toChunks`). -/
def toChunks (s : String) : List (Doc String) :=
  (groupByStatus s.toList).map fun g =>
    match g with
    | ' ' :: _ => space
    | '\n' :: _ => cr
    | _ => literal (String.ofList g)

/-- Emit text, breaking on whitespace when `wrap` is set, otherwise as one
    unbreakable `literal` (upstream `withWrap`). -/
def withWrap (wrap : Bool) (s : String) : Doc String :=
  if wrap then hcatList (toChunks s) else literal s

/- ── Attribute rendering ───────────────────────────────────────────────── -/

/-- Render one attribute as an unbreakable ` name="value"` run (value
    XML-escaped). -/
def attrDoc (a : Web.Html.Attr) : Doc String :=
  literal (" " ++ a.attrName ++ "=\"" ++ Web.Html.escapeAttr a.attrValue ++ "\"")

/-- Render an attribute list. -/
def attrsDoc (attrs : List Web.Html.Attr) : Doc String :=
  hcatList (attrs.map attrDoc)

/- ── The layout traversal ──────────────────────────────────────────────── -/

/-- Render an open/close element `<tag …>children</tag>`. -/
private def parent (tag : String) (attrs : List Web.Html.Attr)
    (children : Doc String) : Doc String :=
  literal ("<" ++ tag) ++ attrsDoc attrs ++ literal ">" ++ children
    ++ literal ("</" ++ tag ++ ">")

/-- Render a void element `<tag …>` (no children). -/
private def leaf (tag : String) (attrs : List Web.Html.Attr) : Doc String :=
  literal ("<" ++ tag) ++ attrsDoc attrs ++ literal ">"

/-- Lay out a typed `Web.Html.Html` node as a breakable `Doc` (upstream
    `layoutMarkup`, retargeted; `wrap` propagates the reflow permission). -/
def go {cat : Web.Html.Category} (wrap : Bool) : Web.Html.Html cat → Doc String
  | .text s => withWrap wrap (escapeMarkupEntities s)
  | .span attrs children => parent "span" attrs (hcatList (children.map (go wrap)))
  | .a attrs children => parent "a" attrs (hcatList (children.map (go wrap)))
  | .label attrs children => parent "label" attrs (hcatList (children.map (go wrap)))
  | .button attrs children => parent "button" attrs (hcatList (children.map (go wrap)))
  | .img attrs => leaf "img" attrs
  | .br => leaf "br" []
  | .input attrs => leaf "input" attrs
  | .div attrs children => parent "div" attrs (hcatList (children.map (go wrap)))
  | .p attrs children => parent "p" attrs (hcatList (children.map (go wrap)))
  | .h1 attrs children => parent "h1" attrs (hcatList (children.map (go wrap)))
  | .h2 attrs children => parent "h2" attrs (hcatList (children.map (go wrap)))
  | .h3 attrs children => parent "h3" attrs (hcatList (children.map (go wrap)))
  | .form attrs children => parent "form" attrs (hcatList (children.map (go wrap)))
  | .ul attrs items => parent "ul" attrs (hcatList (items.map (go wrap)))
  | .ol attrs items => parent "ol" attrs (hcatList (items.map (go wrap)))
  | .li attrs children => parent "li" attrs (hcatList (children.map (go wrap)))
  | .table attrs rows => parent "table" attrs (hcatList (rows.map (go wrap)))
  | .tr attrs cells => parent "tr" attrs (hcatList (cells.map (go wrap)))
  | .td attrs children => parent "td" attrs (hcatList (children.map (go wrap)))
  | .styleSheet css => flush (literal ("<style>" ++ css ++ "</style>"))
  | .fromPhrasing h => go wrap h

/-- Render a `Web.Html.Html` node into a breakable layout `Doc` (upstream
    `layoutMarkup`).  Wrapping is enabled at the top level. -/
def layoutMarkup {cat : Web.Html.Category} (h : Web.Html.Html cat) : Doc String :=
  go true h

end Linen.Text.Pandoc.Writers.Blaze
