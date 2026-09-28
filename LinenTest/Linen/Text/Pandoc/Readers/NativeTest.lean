/-
  Tests for `Linen.Text.Pandoc.Readers.Native`.
-/
import Linen.Text.Pandoc.Readers.Native
import Linen.Text.Pandoc.Writers.Native

namespace Tests.Linen.Text.Pandoc.Readers.Native

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Readers.Native
open _root_.Linen.Text.Pandoc.Writers.Native

/-- Parse and return the block list, or `[]` on error. -/
private def blocksOf (s : String) : List Block :=
  match readNative {} s with
  | .ok d => d.blocks
  | .error _ => []

-- ── Direct parses ─────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "[Para [Str \"hi\"]]" == [Block.Para [.Str "hi"]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "[Header 1 (\"\",[],[]) [Str \"T\"], HorizontalRule]"
  == [Block.Header 1 nullAttr [.Str "T"], Block.HorizontalRule]

-- a bare Block cascades into a one-element block list
/-- info: true -/
#guard_msgs in
#eval blocksOf "Para [Str \"x\"]" == [Block.Para [.Str "x"]]

-- a bare inline list cascades into a Plain paragraph
/-- info: true -/
#guard_msgs in
#eval blocksOf "[Str \"hi\", Space, Str \"there\"]"
  == [Block.Plain [.Str "hi", .Space, .Str "there"]]

-- a bare single inline cascades all the way down
/-- info: true -/
#guard_msgs in
#eval blocksOf "Str \"lonely\"" == [Block.Plain [.Str "lonely"]]

-- escapes round-trip through the string literal
/-- info: true -/
#guard_msgs in
#eval blocksOf "[Para [Str \"a\\nb\"]]" == [Block.Para [.Str "a\nb"]]

-- unreadable input is an error
/-- info: true -/
#guard_msgs in
#eval (match readNative {} "@@@" with | .error _ => true | .ok _ => false)

-- ── Writer/reader round-trip ──────────────────────────────────────────

private def sample : List Block :=
  [ .Header 2 ("h", ["sec"], [("k", "v")]) [.Str "Title"]
  , .Para [.Emph [.Str "hi"], .Space, .Strong [.Str "world"], .Str "!"]
  , .BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]]
  , .OrderedList (1, .Decimal, .Period) [[.Plain [.Str "one"]]]
  , .CodeBlock ("", ["lean"], []) "def x := 1"
  , .BlockQuote [.Para [.Str "quoted"]]
  , .Para [.Link nullAttr [.Str "text"] ("http://x", "t"), .RawInline ⟨"html"⟩ "<b>"]
  , .HorizontalRule ]

/-- info: true -/
#guard_msgs in
#eval blocksOf (writeNativeString ({} : WriterOptions) ⟨nullMeta, sample⟩) == sample

end Tests.Linen.Text.Pandoc.Readers.Native
