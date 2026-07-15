/-
  Tests for `Linen.Text.Pandoc.Writers.Native`.
-/
import Linen.Text.Pandoc.Writers.Native

namespace Tests.Linen.Text.Pandoc.Writers.Native

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Writers.Native

-- ── Leaf renderers ────────────────────────────────────────────────────

#guard showStr "hi" == "\"hi\""
#guard showStr "a\"b" == "\"a\\\"b\""
#guard showStr "l1\nl2" == "\"l1\\nl2\""

#guard showInline (.Str "hi") == "Str \"hi\""
#guard showInline .Space == "Space"
#guard showInline .SoftBreak == "SoftBreak"
#guard showInline (.Emph [.Str "x"]) == "Emph [Str \"x\"]"
#guard showInline (.Strong [.Str "a", .Space, .Str "b"]) == "Strong [Str \"a\",Space,Str \"b\"]"

-- ── Blocks ────────────────────────────────────────────────────────────

#guard showBlock (.Para [.Str "hi"]) == "Para [Str \"hi\"]"
#guard showBlock .HorizontalRule == "HorizontalRule"
#guard showBlock (.Header 1 nullAttr [.Str "T"]) == "Header 1 (\"\",[],[]) [Str \"T\"]"
#guard showBlock (.CodeBlock nullAttr "x = 1") == "CodeBlock (\"\",[],[]) \"x = 1\""

-- ── Top level (template-free = block list only) ───────────────────────

#guard writeNativeString ({} : WriterOptions) ⟨nullMeta, [.Para [.Str "hi"]]⟩
  == "[Para [Str \"hi\"]]"

#guard writeNativeString ({} : WriterOptions) ⟨nullMeta, []⟩ == "[]"

-- with a template set, the whole Pandoc (meta included) is printed
#guard (writeNativeString
    ({ writerTemplate := some ⟨""⟩ } : WriterOptions) ⟨nullMeta, [.Plain [.Str "z"]]⟩).startsWith
    "Pandoc Meta {unMeta = fromList []}"

-- monadic wrapper agrees
#guard Id.run (writeNative (m := Id) ({} : WriterOptions) ⟨nullMeta, [.Para [.Str "hi"]]⟩)
  == "[Para [Str \"hi\"]]"

end Tests.Linen.Text.Pandoc.Writers.Native
