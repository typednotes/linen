/-
  Tests for `Linen.Text.Pandoc.Writers.HtmlLayout` (the HTML→`Doc` layout shim,
  retargeted onto `Linen.Web.Html`).
-/
import Linen.Text.Pandoc.Writers.HtmlLayout

namespace Tests.Linen.Text.Pandoc.Writers.HtmlLayout

open _root_.Linen.Text.Pandoc.Writers.HtmlLayout
open _root_.Web.Html
open _root_.Text.DocLayout (render)

-- ── Entity escaping ───────────────────────────────────────────────────

#guard escapeMarkupEntities "a<b>c&d\"e'f" == "a&lt;b&gt;c&amp;d&quot;e&#39;f"
#guard escapeMarkupEntities "plain" == "plain"

-- ── Whitespace chunking ───────────────────────────────────────────────

-- "a b" → [literal "a", space, literal "b"]
#guard (toChunks "a b").length == 3
-- a run of spaces collapses to a single breakable space
#guard (toChunks "a   b").length == 3
#guard (toChunks "abc").length == 1

-- ── layoutMarkup: elements and attributes ─────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (render none (layoutMarkup (Html.p [] [Html.text "hi"])) : String) == "<p>hi</p>"

/-- info: true -/
#guard_msgs in
#eval (render none (layoutMarkup (Html.div [class_ "note"] [Html.text "a"])) : String)
  == "<div class=\"note\">a</div>"

-- text content is entity-escaped
/-- info: true -/
#guard_msgs in
#eval (render none (layoutMarkup (Html.span [] [Html.text "x<y"])) : String)
  == "<span>x&lt;y</span>"

-- void elements render as a single open tag
/-- info: true -/
#guard_msgs in
#eval (render none (layoutMarkup (Html.img [src "a.png", alt "cap"])) : String)
  == "<img src=\"a.png\" alt=\"cap\">"

-- whitespace stays a single space when the width is generous
/-- info: true -/
#guard_msgs in
#eval (render none (layoutMarkup (Html.p [] [Html.text "hello world"])) : String)
  == "<p>hello world</p>"

end Tests.Linen.Text.Pandoc.Writers.HtmlLayout
