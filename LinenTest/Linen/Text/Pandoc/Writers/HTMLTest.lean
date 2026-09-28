/-
  Tests for `Linen.Text.Pandoc.Writers.HTML`.

  `writeHtmlString` is backed by an `unsafe` renderer (see the module note), so
  the checks use `#eval` + `#guard_msgs`, the pattern this codebase uses for
  the other `unsafe`-backed writers (e.g. `Writers.Shared.gridTable`).
-/
import Linen.Text.Pandoc.Writers.HTML

namespace Tests.Linen.Text.Pandoc.Writers.HTML

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Writers.HTML

private def w (bs : List Block) : String := writeHtmlString ({} : WriterOptions) ⟨nullMeta, bs⟩

-- ── Pure attribute helpers ────────────────────────────────────────────

#guard renderAttrs ("i", ["a", "b"], [("k", "v")]) == " id=\"i\" class=\"a b\" k=\"v\""
#guard renderAttrs nullAttr == ""
#guard headerTag 2 == "h2"
#guard headerTag 9 == "p"
#guard olTypeAttr .LowerAlpha == " type=\"a\""

-- ── Blocks ────────────────────────────────────────────────────────────

/-- info: "<p>hi</p>" -/
#guard_msgs in #eval w [.Para [.Str "hi"]]

/-- info: "<h1 id=\"t\">Title</h1>" -/
#guard_msgs in #eval w [.Header 1 ("t", [], []) [.Str "Title"]]

/-- info: "<hr />" -/
#guard_msgs in #eval w [.HorizontalRule]

/-- info: "<pre><code>x = 1</code></pre>" -/
#guard_msgs in #eval w [.CodeBlock nullAttr "x = 1"]

/-- info: "<ul>\n<li>a</li>\n<li>b</li>\n</ul>" -/
#guard_msgs in #eval w [.BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]]]

/-- info: "<ol start=\"3\" type=\"a\">\n<li>x</li>\n</ol>" -/
#guard_msgs in #eval w [.OrderedList (3, .LowerAlpha, .Period) [[.Plain [.Str "x"]]]]

/-- info: "<blockquote>\n<p>q</p>\n</blockquote>" -/
#guard_msgs in #eval w [.BlockQuote [.Para [.Str "q"]]]

-- ── Inlines ───────────────────────────────────────────────────────────

/-- info: "<p><em>a</em> <strong>b</strong></p>" -/
#guard_msgs in #eval w [.Para [.Emph [.Str "a"], .Space, .Strong [.Str "b"]]]

/-- info: "<p><a href=\"http://x\" title=\"t\">link</a></p>" -/
#guard_msgs in #eval w [.Para [.Link nullAttr [.Str "link"] ("http://x", "t")]]

/-- info: "<p><code>f x</code></p>" -/
#guard_msgs in #eval w [.Para [.Code nullAttr "f x"]]

-- `&`/`<` in Str are XML-escaped
/-- info: "<p>a &amp; b &lt; c</p>" -/
#guard_msgs in #eval w [.Para [.Str "a & b < c"]]

-- a Note produces a footnote reference plus a footnote section
private def noteHtml : String := w [.Para [.Str "x", .Note [.Para [.Str "note"]]]]

/-- info: true -/
#guard_msgs in
#eval (noteHtml.splitOn "class=\"footnote-ref\"").length == 2
  && (noteHtml.splitOn "class=\"footnotes\"").length == 2
  && (noteHtml.splitOn "<p>note</p>").length == 2
  && (noteHtml.splitOn "href=\"#fn1\"").length == 2

-- raw html passes through; raw non-html is dropped
/-- info: "<p><b>raw</b></p>" -/
#guard_msgs in #eval w [.Para [.RawInline ⟨"html"⟩ "<b>raw</b>"]]

/-- info: "<p></p>" -/
#guard_msgs in #eval w [.Para [.RawInline ⟨"latex"⟩ "\\emph{x}"]]

-- monadic wrapper agrees
/-- info: "<p>hi</p>" -/
#guard_msgs in #eval Id.run (writeHtml5 (m := Id) ({} : WriterOptions) ⟨nullMeta, [.Para [.Str "hi"]]⟩)

end Tests.Linen.Text.Pandoc.Writers.HTML
