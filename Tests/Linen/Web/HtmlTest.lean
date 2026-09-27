/-
  Tests for `Linen.Web.Html` — a typed HTML5 construction library.
-/
import Linen.Web.Html

open Web.Html Web.Html.Html

namespace Tests.Web.Html

/-! ### Escaping -/

#guard escapeText "<b>&\"x\"</b>" == "&lt;b&gt;&amp;\"x\"&lt;/b&gt;"
#guard escapeAttr "a\"b" == "a&quot;b"

/-! ### Attributes -/

#guard (class_ "todo").render == " class=\"todo\""
#guard (href "/x?a=1&b=2").render == " href=\"/x?a=1&amp;b=2\""
#guard checked.render == " checked=\"checked\""
#guard (style [Web.Css.color (.named "red")]).render == " style=\"color: red;\""

/-! ### Rendering -/

#guard (text "hi").render == "hi"
#guard (br).render == "<br>"
#guard (img [src "x.png", alt "x"]).render == "<img src=\"x.png\" alt=\"x\">"
#guard (span [] [text "hi"]).render == "<span>hi</span>"
#guard (div [class_ "box"] [p [] [text "hello"]]).render == "<div class=\"box\"><p>hello</p></div>"
#guard (ul [] [li [] [text "a"], li [] [text "b"]]).render == "<ul><li>a</li><li>b</li></ul>"
#guard (table [] [tr [] [td [] [text "a"]]]).render == "<table><tr><td>a</td></tr></table>"

-- Phrasing content coerces to flow content, so `text`/`span` can appear
-- directly among a `div`'s `Html .flow` children without `fromPhrasing`.
#guard (div [] [text "hi", span [] [text "!"]]).render == "<div>hi<span>!</span></div>"

#guard Html.renderDocument "T" [] [p [] [text "hi"]] ==
  "<!DOCTYPE html>\n<html><head><title>T</title></head><body><p>hi</p></body></html>"

#guard (styleSheet (raw! "body { margin: 0; }")).render == "<style>body { margin: 0; }</style>"
#guard Html.renderDocument "T" [styleSheet (raw! "body{margin:0}")] [] ==
  "<!DOCTYPE html>\n<html><head><title>T</title><style>body{margin:0}</style></head><body></body></html>"

/-! ### Raw text: `<script>` / `<style>` bodies -/

-- A runtime check rejects anything that would close the element, in any case …
#guard (RawText.ofString? (tag := .script) "let x = 1;").isSome
#guard (RawText.ofString? (tag := .script) "a</script>b").isNone
#guard (RawText.ofString? (tag := .script) "a</SCRIPT >b").isNone
#guard (RawText.ofString? (tag := .script) "a</ScRiPt").isNone
-- … and, for a script, `<!--` (the "script data escaped" state).
#guard (RawText.ofString? (tag := .script) "x <!-- y").isNone
-- The hazards are per element: `</style>` is harmless in a script, and
-- `</script>` or `<!--` in a style sheet.
#guard (RawText.ofString? (tag := .script) "'</style>'").isSome
#guard (RawText.ofString? (tag := .style) "a</style>").isNone
#guard (RawText.ofString? (tag := .style) "/* </script> <!-- */").isSome
-- `<` on its own, or opening tags, are fine.
#guard (RawText.ofString? (tag := .script) "if (a < b) { '<script>' }").isSome

-- `jsonString` makes any text safe, proven once for all inputs.
#guard (RawText.jsonString (tag := .script) "</script><!-- & \"q\" \\").text ==
  "\"\\u003c/script\\u003e\\u003c!-- \\u0026 \\\"q\\\" \\\\\""
#guard (RawText.jsonString (tag := .script) "a\nb\u0001").text == "\"a\\u000ab\\u0001\""
#guard (RawText.jsonString (tag := .script) (String.ofList ['\u2028'])).text == "\"\\u2028\""
example (s : String) : RawText.Safe .style (RawText.jsonString (tag := .style) s).text :=
  (RawText.jsonString s).safe
example : breaksOut .script "no angle brackets here".toList = false :=
  breaksOut_of_lt_not_mem .script _ (by decide)

-- `raw!` literals are checked by the kernel at compile time.
#guard (raw! "body{}" : RawText .style).text == "body{}"

/-! ### `<script>` and `<meta>` -/

#guard (script [type_ "module"] (raw! "go();")).render == "<script type=\"module\">go();</script>"
#guard (script [src "x.js"] RawText.empty).render == "<script src=\"x.js\"></script>"
#guard (meta_ [charset "utf-8"]).render == "<meta charset=\"utf-8\">"
#guard Html.renderDocument "T" [meta_ [charset "utf-8"]] [] ==
  "<!DOCTYPE html>\n<html><head><title>T</title><meta charset=\"utf-8\"></head><body></body></html>"

/-! ### The `elem!` macro -/

#guard (elem! div [class_ "todo"] [text "hi"]).render == (div [class_ "todo"] [text "hi"]).render
#guard (elem! img [src "x.png"]).render == (img [src "x.png"]).render
#guard (elem! ul [] [li [] [text "a"]]).render == (ul [] [li [] [text "a"]]).render

end Tests.Web.Html
