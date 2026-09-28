/-
  Tests for `Linen.Text.Pandoc.JSON`: the AST ↔ JSON bridge.
-/
import Linen.Text.Pandoc.JSON

namespace Tests.Linen.Text.Pandoc.JSON

open _root_.Linen.Text.Pandoc

private def sampleDoc : Pandoc := ⟨nullMeta, [.Para [.Str "hi"]]⟩

-- ── Round-tripping through JSON ────────────────────────────────────────

#guard (readPandoc (writePandoc sampleDoc)).toOption == some sampleDoc

-- a malformed document fails to decode
#guard (readPandoc "not json").isOk == false

-- ── `toJSONFilter` (via `Walkable`, exercised without stdin/stdout) ─────

#guard walk (fun (i : Inline) => match i with
  | .Str s => .Str (s ++ "!")
  | i => i) sampleDoc == (⟨nullMeta, [.Para [.Str "hi!"]]⟩ : Pandoc)

end Tests.Linen.Text.Pandoc.JSON
