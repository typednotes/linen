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

#guard (styleSheet "body { margin: 0; }").render == "<style>body { margin: 0; }</style>"
#guard Html.renderDocument "T" [styleSheet "body{margin:0}"] [] ==
  "<!DOCTYPE html>\n<html><head><title>T</title><style>body{margin:0}</style></head><body></body></html>"

/-! ### The `elem!` macro -/

#guard (elem! div [class_ "todo"] [text "hi"]).render == (div [class_ "todo"] [text "hi"]).render
#guard (elem! img [src "x.png"]).render == (img [src "x.png"]).render
#guard (elem! ul [] [li [] [text "a"]]).render == (ul [] [li [] [text "a"]]).render

end Tests.Web.Html
