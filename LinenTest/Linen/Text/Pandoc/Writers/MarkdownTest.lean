/-
  Tests for `Linen.Text.Pandoc.Writers.Markdown`.

  `writeMarkdownString` is backed by `unsafe` renderers (see the module note),
  so behavioural checks use `#eval` + `#guard_msgs`.
-/
import Linen.Text.Pandoc.Writers.Markdown

namespace Tests.Linen.Text.Pandoc.Writers.Markdown

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Writers.Markdown

private def wr (bs : List Block) : String := writeMarkdownString {} ⟨nullMeta, bs⟩

-- ── Pure helpers ────────────────────────────────────────────────────────

#guard escapeMarkdown "a*b_c" == "a\\*b\\_c"
#guard escapeMarkdown "plain text" == "plain text"
#guard renderCode "x" == "`x`"
#guard renderCode "a`b" == "`` a`b ``"
#guard attrMD ("id", ["c"], []) == "{#id .c}"
#guard attrMD nullAttr == ""

-- ── Inline rendering ────────────────────────────────────────────────────

/-- info: "a *b* **c**" -/
#guard_msgs in
#eval wr [.Para [.Str "a", .Space, .Emph [.Str "b"], .Space, .Strong [.Str "c"]]]

/-- info: "text `code` here" -/
#guard_msgs in
#eval wr [.Para [.Str "text", .Space, .Code nullAttr "code", .Space, .Str "here"]]

/-- info: "[t](http://x.com \"ti\")" -/
#guard_msgs in
#eval wr [.Para [.Link nullAttr [.Str "t"] ("http://x.com", "ti")]]

/-- info: "![alt](img.png)" -/
#guard_msgs in
#eval wr [.Para [.Image nullAttr [.Str "alt"] ("img.png", "")]]

-- ── Block rendering ─────────────────────────────────────────────────────

/-- info: "# Hello" -/
#guard_msgs in
#eval wr [.Header 1 nullAttr [.Str "Hello"]]

/-- info: "> quoted" -/
#guard_msgs in
#eval wr [.BlockQuote [.Para [.Str "quoted"]]]

/-- info: "```haskell\nfoo\n```" -/
#guard_msgs in
#eval wr [.CodeBlock ("", ["haskell"], []) "foo"]

/-- info: "---" -/
#guard_msgs in
#eval wr [.HorizontalRule]

/-- info: "-   a\n-   b" -/
#guard_msgs in
#eval wr [.BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]]]

/-- info: "1. x\n2. y" -/
#guard_msgs in
#eval wr [.OrderedList (1, .Decimal, .Period) [[.Plain [.Str "x"]], [.Plain [.Str "y"]]]]

-- ── Footnotes ───────────────────────────────────────────────────────────

/-- info: "see[^1]\n\n[^1]: a note" -/
#guard_msgs in
#eval wr [.Para [.Str "see", .Note [.Para [.Str "a", .Space, .Str "note"]]]]

-- ── Table (pipe) ────────────────────────────────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (wr [.Table nullAttr (.Caption none [])
            [(.AlignLeft, .ColWidthDefault), (.AlignRight, .ColWidthDefault)]
            (.TableHead nullAttr [.Row nullAttr
              [.Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "h1"]],
               .Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "h2"]]]])
            [.TableBody nullAttr 0 [] [.Row nullAttr
              [.Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "a"]],
               .Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "b"]]]]]
            (.TableFoot nullAttr [])]).toList.any (fun c => c == '|')

end Tests.Linen.Text.Pandoc.Writers.Markdown
