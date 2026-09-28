/-
  Tests for `Linen.Text.DocLayout`.

  The `render`/`offset`/`height` engine is `unsafe` (see the module note), so
  its results are asserted with `#guard_msgs in #eval` (interpreter-based)
  rather than `#guard` (kernel-based).  The pure width / structural helpers are
  checked with ordinary `#guard`.
-/
import Linen.Text.DocLayout

namespace Tests.Linen.Text.DocLayout

open _root_.Text.DocLayout

-- ── Display width (`charWidth` / `realLength`) ───

#guard charWidth 'a' = 1
#guard charWidth ' ' = 1
-- East-Asian wide characters take two columns
#guard charWidth '中' = 2
-- combining marks are zero width
#guard charWidth (Char.ofNat 0x0301) = 0
#guard realLength (IsString.fromString "abc" : String) = 3
#guard realLength (IsString.fromString "中文" : String) = 4

-- ── Emoji-cluster character predicates ───────────

#guard isZWJ (Char.ofNat 0x200D) = true
#guard isSkinToneModifier (Char.ofNat 0x1F3FB) = true
#guard isEmojiVariation (Char.ofNat 0xFE0F) = true
#guard isEmojiVariation 'a' = false

-- ── Structural helpers (`isEmpty` / `unfoldD`) ───

#guard isEmpty (empty : Doc String) = true
#guard isEmpty (text "x" : Doc String) = false
#guard isEmpty ((empty : Doc String) ++ empty) = true
-- `<+>` and `$$` drop empty operands
#guard isEmpty (besideSp (empty : Doc String) empty) = true
#guard (unfoldD (text "a" ++ text "b" ++ text "c" : Doc String)).length = 3

-- ── `flatten` / `normalize` ──────────────────────

-- `a\nb` flattens to text / newline / text
#guard (flatten (text "a\nb" : Doc String)).length = 3
-- adjacent breaking spaces collapse and a trailing one is dropped
#guard (normalize ([.FBreakingSpace, .FBreakingSpace] : List (FlatDoc String))).length = 0
-- adjacent blank-line runs merge into one carriage return
#guard (normalize ([.FBlankLines 1, .FBlankLines 3] : List (FlatDoc String))).length = 1

-- ── Rendering: line wrapping ─────────────────────

-- With a line length of 10, `hsep` breaks the flow to keep lines ≤ 10 wide.
/-- info: true -/
#guard_msgs in
#eval (render (some 10) (hsep [text "hello", text "world", text "foo", text "bar"]) : String)
  == "hello\nworld foo\nbar"

-- No wrapping when the width is generous.
/-- info: true -/
#guard_msgs in
#eval (render (some 80) (hsep [text "hello", text "world", text "foo", text "bar"]) : String)
  == "hello world foo bar"

-- ── Rendering: nesting / indentation ─────────────

/-- info: true -/
#guard_msgs in
#eval (render none (nest 2 (vcat [text "a", text "b"])) : String) == "  a\n  b"

-- `hang` indents the continuation lines to the hanging column.
/-- info: true -/
#guard_msgs in
#eval (render (some 20) (hang 4 (text "foo") (vcat [text "l1", text "l2"])) : String)
  == "fool1\n    l2"

-- Prefixing every line.
/-- info: true -/
#guard_msgs in
#eval (render none (prefixed "> " (vcat [text "a", text "b"])) : String) == "> a\n> b"

-- ── Rendering: blocks side by side ───────────────

-- Two left-blocks placed beside each other pad to a common width/height.
/-- info: true -/
#guard_msgs in
#eval (render none (lblock 3 (vcat [text "a", text "bb"]) ++ lblock 2 (text "X")) : String)
  == "a  X\nbb "

-- ── Rendering: ANSI styling ──────────────────────

-- Bold text is wrapped in SGR codes and reset (plus the OSC-8 link close).
/-- info: true -/
#guard_msgs in
#eval (renderANSI none (bold (text "hi" : Doc String)))
  == "\x1b[1m\x1b[23m\x1b[39m\x1b[49m\x1b[24m\x1b[29mhi\x1b[0m\x1b]8;;\x1b\\"

-- Plain render ignores styling.
/-- info: true -/
#guard_msgs in
#eval (render none (bold (text "hi" : Doc String)) : String) == "hi"

-- ── Queries (`offset` / `minOffset` / `height`) ──

/-- info: true -/
#guard_msgs in
#eval offset (text "hello" : Doc String) == 5

-- Longest word when broken at every space.
/-- info: true -/
#guard_msgs in
#eval minOffset (hsep [text "hi", text "there"] : Doc String) == 5

/-- info: true -/
#guard_msgs in
#eval height (vcat [text "a", text "b", text "c"] : Doc String) == 3

end Tests.Linen.Text.DocLayout
