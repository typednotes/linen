/-
  Tests for `Linen.Text.Pandoc.Templates` (template-free scope).
-/
import Linen.Text.Pandoc.Templates

namespace Tests.Linen.Text.Pandoc.Templates

open _root_.Linen.Text.Pandoc
open _root_.Text.DocLayout (render)

-- ── compileTemplate always succeeds and keeps the raw text ────────────

#guard (compileTemplate "name" "hello").toOption.map (·.raw) == some "hello"
#guard (compileTemplate "n" "").toOption.map (·.raw) == some ""

-- ── renderTemplate is a passthrough of the raw template text ──────────

/-- info: true -/
#guard_msgs in
#eval (render none (renderTemplate ⟨"hi there"⟩ ([] : Context String)) : String)
  == "hi there"

-- context is ignored in the template-free path
/-- info: true -/
#guard_msgs in
#eval (render none (renderTemplate ⟨"body"⟩ [("x", "y")]) : String) == "body"

-- ── default-template lookup / format aliasing ─────────────────────────

#guard defaultTemplateFormat "html" == "html5"
#guard defaultTemplateFormat "gfm" == "commonmark"
#guard defaultTemplateFormat "docx" == "openxml"
#guard defaultTemplateFormat "markdown_github" == "markdown"
#guard defaultTemplateFormat "latex" == "latex"

-- no bundled data files are ported, so every default template is empty
#guard getDefaultTemplate "html5" == ""
#guard getDefaultTemplate "native" == ""

end Tests.Linen.Text.Pandoc.Templates
