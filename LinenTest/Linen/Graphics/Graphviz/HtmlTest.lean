/-
  Tests for `Linen.Graphics.Graphviz.Html`.

  Covers the embedded bundle (its size, and the compile-time proof that it
  cannot close its `<script>`), and the page: its structure, escaping of the
  title, the DOT embedded as a JSON string that hostile labels cannot break
  out of, and that it contains exactly the three scripts it should.

  That the page actually renders is checked in a browser, not here: `#guard`
  cannot run WebAssembly.
-/
import Linen.Graphics.Graphviz.Html

open Graphics.Graphviz Web.Html

namespace Tests.Graphics.Graphviz.Html

/-- `s` contains `sub`. -/
def has (s sub : String) : Bool := (s.splitOn sub).length > 1
/-- The number of occurrences of `sub` in `s`. -/
def count (s sub : String) : Nat := (s.splitOn sub).length - 1

-- ── The bundle ──────────────────────────────────────────────────────────────

#guard _root_.Graphics.Graphviz.Html.bundleVersion == "1.29.1"
#guard _root_.Graphics.Graphviz.Html.bundleSource.utf8ByteSize == 819284
#guard count _root_.Graphics.Graphviz.Html.bundleSource "</script" == 0 && count _root_.Graphics.Graphviz.Html.bundleSource "<!--" == 0
example : RawText.Safe .script _root_.Graphics.Graphviz.Html.bundleSource := _root_.Graphics.Graphviz.Html.bundle_safe

-- ── The page ────────────────────────────────────────────────────────────────

/-- A graph whose label tries to close the script and inject markup. -/
def hostile : Graph .directed :=
  { name := "g"
    nodes := #[{ attrs := [label "</script><script>alert(1)</script> & <!--"] }] }

def thePage : String := _root_.Graphics.Graphviz.Html.page hostile "a <title> & \"quotes\""

#guard thePage.startsWith "<!DOCTYPE html>\n<html><head><title>a &lt;title&gt; &amp; \"quotes\"</title>"
#guard has thePage "<meta charset=\"utf-8\">"
#guard has thePage "<script type=\"text/plain\" id=\"graphviz-wasm\">"
#guard has thePage "<script type=\"application/json\" id=\"graphviz-dot\">"
#guard has thePage "<script type=\"module\">"
#guard has thePage "<div id=\"graphviz-graph\"></div><p id=\"graphviz-status\">Rendering…</p>"
-- Exactly three scripts: the hostile label opens and closes none …
#guard count thePage "<script" == 3 && count thePage "</script>" == 3
#guard count thePage "<!--" == 0
-- … because the DOT is embedded as a JSON string with `<` escaped.
#guard has thePage (RawText.jsonString (tag := .script) hostile.render).text
#guard has thePage "\\u003c/script\\u003e\\u003cscript\\u003ealert(1)\\u003c/script\\u003e \\u0026 \\u003c!--"

end Tests.Graphics.Graphviz.Html
