/-
  Tests for `Linen.Web.Css` — a typed CSS construction library.
-/
import Linen.Web.Css

open Web.Css

namespace Tests.Web.Css

/-! ### Length / Color rendering -/

#guard (Length.px 8).render == "8px"
#guard (Length.pct 100).render == "100%"
#guard Length.auto.render == "auto"
#guard Length.zero.render == "0"
#guard (BoxSides.all (.px 8)).render == "8px 8px 8px 8px"
#guard ({ top := .px 4, right := .px 8, bottom := .px 4, left := .px 8 } : BoxSides).render == "4px 8px 4px 8px"

#guard (Color.named "crimson").render == "crimson"
#guard (Color.hex "ff0000").render == "#ff0000"
#guard (Color.rgb 255 0 0).render == "rgb(255, 0, 0)"

/-! ### Declarations -/

#guard (color (.named "crimson")).render == "color: crimson;"
#guard (backgroundColor (.hex "1a1a1a")).render == "background-color: #1a1a1a;"
#guard (margin (.px 8)).render == "margin: 8px;"
#guard (marginBox (BoxSides.all .zero)).render == "margin: 0 0 0 0;"
#guard (display .flex).render == "display: flex;"
#guard (fontFamily ["system-ui", "sans-serif"]).render == "font-family: system-ui, sans-serif;"
#guard (border (.px 1) .solid (.named "black")).render == "border: 1px solid black;"
#guard (boxSizing true).render == "box-sizing: border-box;"

/-! ### FontWeight — bounded numeric values -/

#guard FontWeight.normal.render == "normal"
#guard FontWeight.bold.render == "bold"
#guard (FontWeight.numeric 600).render == "600"
-- `FontWeight.numeric 450` does not compile: 450 % 100 ≠ 0, so `by decide` fails,
-- illustrating the bounded-value guarantee at compile time (not tested here since
-- `#guard` cannot express "this term must fail to elaborate").

/-! ### Selectors -/

#guard (Selector.tag "div").render == "div"
#guard (Selector.class_ "todo").render == ".todo"
#guard (Selector.id_ "app").render == "#app"
#guard (Selector.and (.tag "div") (.class_ "todo")).render == "div.todo"
#guard (Selector.descendant (.tag "ul") (.tag "li")).render == "ul li"
#guard (Selector.child (.tag "ul") (.tag "li")).render == "ul > li"
#guard (Selector.hover (.class_ "btn")).render == ".btn:hover"

/-! ### Rules, stylesheets, and the `rule!` macro -/

#guard (Rule.mk (.class_ "todo") [color (.named "crimson"), padding (.px 8)]).render ==
  ".todo {\n  color: crimson;\n  padding: 8px;\n}"

#guard (rule! (.class_ "todo") { color (.named "crimson"), padding (.px 8) }).render ==
  ".todo {\n  color: crimson;\n  padding: 8px;\n}"

#guard (rule! (.tag "body") { margin (.zero) }) ==
  (Rule.mk (.tag "body") [margin .zero])

#guard
  let ss : Stylesheet := [
    rule! (.tag "body") { margin (.zero) },
    rule! (.class_ "todo") { color (.named "black") }
  ]
  ss.render ==
    "body {\n  margin: 0;\n}\n\n.todo {\n  color: black;\n}"

end Tests.Web.Css
