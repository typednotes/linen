/-
  Tests for `Linen.Text.Pandoc.Readers.Markdown`.

  `readMarkdown` is backed by `unsafe` recursive descent (see the module note),
  so behavioural checks use `#eval` + `#guard_msgs`.
-/
import Linen.Text.Pandoc.Readers.Markdown

namespace Tests.Linen.Text.Pandoc.Readers.Markdown

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Readers.Markdown

private def mdOpts : ReaderOptions := { readerExtensions := pandocExtensions }

private def blocksOf (s : String) : List Block :=
  match readMarkdown mdOpts s with
  | .ok d => d.blocks
  | .error _ => []

private def docOf (s : String) : Pandoc :=
  match readMarkdown mdOpts s with
  | .ok d => d
  | .error _ => ⟨nullMeta, []⟩

-- ── Headers ────────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "# Hello" == [Block.Header 1 nullAttr [.Str "Hello"]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "### A B" == [Block.Header 3 nullAttr [.Str "A", .Space, .Str "B"]]

-- setext level 2
/-- info: true -/
#guard_msgs in
#eval blocksOf "Title\n---" == [Block.Header 2 nullAttr [.Str "Title"]]

-- ── Paragraphs and inline emphasis ─────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "a *b* **c**"
  == [Block.Para [.Str "a", .Space, .Emph [.Str "b"], .Space, .Strong [.Str "c"]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "text with `code` span"
  == [Block.Para [.Str "text", .Space, .Str "with", .Space, .Code nullAttr "code", .Space, .Str "span"]]

-- ── Links and images ───────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "[t](http://x.com \"ti\")"
  == [Block.Para [.Link nullAttr [.Str "t"] ("http://x.com", "ti")]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "![alt](img.png)"
  == [Block.Para [.Image nullAttr [.Str "alt"] ("img.png", "")]]

-- reference link
/-- info: true -/
#guard_msgs in
#eval blocksOf "see [ref][1]\n\n[1]: http://y.com \"T\""
  == [Block.Para [.Str "see", .Space, .Link nullAttr [.Str "ref"] ("http://y.com", "T")]]

-- ── Lists ───────────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "- a\n- b"
  == [Block.BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "1. x\n2. y"
  == [Block.OrderedList (1, .Decimal, .Period) [[.Plain [.Str "x"]], [.Plain [.Str "y"]]]]

-- ── Code blocks ─────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "```haskell\nfoo\n```"
  == [Block.CodeBlock ("", ["haskell"], []) "foo"]

/-- info: true -/
#guard_msgs in
#eval blocksOf "    indented\n    code"
  == [Block.CodeBlock nullAttr "indented\ncode"]

-- ── Blockquote and horizontal rule ──────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "> quoted" == [Block.BlockQuote [.Para [.Str "quoted"]]]

/-- info: true -/
#guard_msgs in
#eval blocksOf "---" == [Block.HorizontalRule]

-- ── Pipe table ──────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (match blocksOf "| a | b |\n|:--|--:|\n| 1 | 2 |" with
       | [.Table _ _ specs (.TableHead _ [_]) [.TableBody _ _ _ brows] _] =>
           specs.length == 2 && brows.length == 1 && specs.map (fun (s : ColSpec) => s.1) == [Alignment.AlignLeft, .AlignRight]
       | _ => false)

-- ── Footnotes ───────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (match blocksOf "text[^n]\n\n[^n]: note body" with
       | [.Para [.Str "text", .Note [.Para [.Str "note", .Space, .Str "body"]]]] => true
       | _ => false)

-- ── Raw HTML block ──────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval blocksOf "<div class=\"n\"><p>hi</p></div>"
  == [Block.Div ("", ["n"], []) [.Para [.Str "hi"]]]

-- ── YAML front matter ───────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (docTitle (docOf "---\ntitle: My Title\n---\n\nbody").docMeta) == [Inline.Str "My Title"]

/-- info: true -/
#guard_msgs in
#eval (match lookupMeta "tags" (docOf "---\ntags:\n  - x\n  - y\n---\n\nbody").docMeta with
       | some (.MetaList [.MetaString "x", .MetaString "y"]) => true
       | _ => false)

end Tests.Linen.Text.Pandoc.Readers.Markdown
