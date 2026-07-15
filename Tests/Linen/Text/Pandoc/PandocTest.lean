/-
  Tests for the top-level `Linen.Text.Pandoc` facade (format dispatch and the
  re-exported readers/writers).
-/
import Linen.Text.Pandoc

namespace Tests.Linen.Text.Pandoc.Facade

open _root_.Linen.Text.Pandoc

-- ── Registry dispatch ───────────────────────────────────────────────────

#guard (getReader "markdown").isSome
#guard (getReader "html").isSome
#guard (getReader "native").isSome
#guard (getReader "latex").isNone
#guard (getWriter "markdown").isSome
#guard (getWriter "html").isSome
#guard (getWriter "native").isSome
#guard (getWriter "docx").isNone

-- ── Re-exported readers/writers round-trip ──────────────────────────────

/-- info: true -/
#guard_msgs in
#eval (match readMarkdown {} "# Hi" with
       | .ok d => d.blocks == [Block.Header 1 nullAttr [.Str "Hi"]]
       | .error _ => false)

/-- info: "# Hi" -/
#guard_msgs in
#eval writeMarkdown {} ⟨nullMeta, [.Header 1 nullAttr [.Str "Hi"]]⟩

/-- info: true -/
#guard_msgs in
#eval (writeHtml {} ⟨nullMeta, [.Para [.Str "hi"]]⟩) == "<p>hi</p>"

/-- info: true -/
#guard_msgs in
#eval (writeNative {} ⟨nullMeta, [.HorizontalRule]⟩) == "[HorizontalRule]"

-- ── convert: markdown → html via the registry ───────────────────────────

/-- info: true -/
#guard_msgs in
#eval (match convert "markdown" "html" "*hi*" with
       | some (.ok s) => s == "<p><em>hi</em></p>"
       | _ => false)

-- ── convert: markdown → native round-trips a heading ────────────────────

/-- info: true -/
#guard_msgs in
#eval (match convert "markdown" "native" "## Head" with
       | some (.ok s) => s == "[Header 2 (\"\",[],[]) [Str \"Head\"]]"
       | _ => false)

-- out-of-scope format pairs yield none
/-- info: true -/
#guard_msgs in
#eval (convert "latex" "html" "x").isNone

end Tests.Linen.Text.Pandoc.Facade
