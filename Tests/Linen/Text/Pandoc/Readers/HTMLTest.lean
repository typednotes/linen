/-
  Tests for `Linen.Text.Pandoc.Readers.HTML`.

  `readHtml`/`tokenize` are backed by `unsafe` recursive descent (see the
  module note), so behavioural checks use `#eval` + `#guard_msgs`.
-/
import Linen.Text.Pandoc.Readers.HTML

namespace Tests.Linen.Text.Pandoc.Readers.HTML

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Readers.HTML

private def blocksOf (s : String) : List Block :=
  match readHtml {} s with
  | .ok d => d.blocks
  | .error _ => []

-- ── Tokenizer ─────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval tokenize "<p class=\"x\">hi</p>"
  == [.TagOpen "p" [("class", "x")] false, .TagText "hi", .TagClose "p"]

/-- info: true -/
#guard_msgs in
#eval tokenize "<br/><img src=\"a.png\">"
  == [.TagOpen "br" [] true, .TagOpen "img" [("src", "a.png")] false]

-- comments and doctype are dropped to comment tokens
/-- info: true -/
#guard_msgs in
#eval tokenize "<!DOCTYPE html><!-- c -->x"
  == [.TagComment "", .TagComment " c ", .TagText "x"]

-- ── Block parsing ─────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "<p>hello world</p>" == [Block.Para [.Str "hello", .Space, .Str "world"]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<h2 id=\"s\">Head</h2>" == [Block.Header 2 ("s", [], []) [.Str "Head"]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<ul><li>a</li><li>b</li></ul>"
  == [Block.BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<blockquote><p>q</p></blockquote>"
  == [Block.BlockQuote [.Para [.Str "q"]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<hr>" == [Block.HorizontalRule]

-- ordered list reads start / type
/-- info: true -/
#guard_msgs in
#eval blocksOf "<ol start=\"2\" type=\"a\"><li>x</li></ol>"
  == [Block.OrderedList (2, .LowerAlpha, .DefaultDelim) [[.Plain [.Str "x"]]]]

-- ── Inline parsing ────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "<p><em>a</em> <strong>b</strong></p>"
  == [Block.Para [.Emph [.Str "a"], .Space, .Strong [.Str "b"]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<p><a href=\"u\" title=\"t\">L</a></p>"
  == [Block.Para [.Link ("", [], []) [.Str "L"] ("u", "t")]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "<p>x<br>y</p>" == [Block.Para [.Str "x", .LineBreak, .Str "y"]]

-- entities decoded in text
/-- info: true -/
#guard_msgs in
#eval blocksOf "<p>a &amp; b</p>" == [Block.Para [.Str "a", .Space, .Str "&", .Space, .Str "b"]]

-- div with attributes
/-- info: true -/
#guard_msgs in
#eval blocksOf "<div class=\"note\"><p>hi</p></div>"
  == [Block.Div ("", ["note"], []) [.Para [.Str "hi"]]]

-- a simple table (th header row + td body row)
/-- info: true -/
#guard_msgs in
#eval (match blocksOf "<table><tr><th>H</th></tr><tr><td>c</td></tr></table>" with
       | [.Table _ _ specs (.TableHead _ hrows) [.TableBody _ _ _ brows] _] =>
           specs.length == 1 && hrows.length == 1 && brows.length == 1
       | _ => false)

end Tests.Linen.Text.Pandoc.Readers.HTML
