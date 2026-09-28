/-
  Tests for `Linen.Text.DocLayout.Attributed`.
-/
import Linen.Text.DocLayout.Attributed
import Linen.Data.Foldable
import Linen.Data.Traversable

namespace Tests.Linen.Text.DocLayout.Attributed

open _root_.Text.DocLayout
open Data (Foldable Traversable)

-- ── `IsString` construction ──────────────────────

#guard (IsString.fromString "hi" : Attr String).value = "hi"
#guard (IsString.fromString "hi" : Attr String).font = baseFont
#guard ((IsString.fromString "hi" : Attributed String).chunks.map (·.value)) = ["hi"]

-- ── `Append` keeps the left link/font, appends payloads ──

#guard ((⟨none, baseFont ~> .RWeight .Bold, "ab"⟩ : Attr String)
        ++ ⟨none, baseFont, "cd"⟩).value = "abcd"
#guard ((⟨none, baseFont ~> .RWeight .Bold, "ab"⟩ : Attr String)
        ++ ⟨none, baseFont, "cd"⟩).font = baseFont ~> .RWeight .Bold

#guard (((⟨[⟨none, baseFont, "a"⟩]⟩ : Attributed String)
        ++ ⟨[⟨none, baseFont, "b"⟩]⟩).chunks.map (·.value)) = ["a", "b"]

-- ── `Functor` / `Foldable` ───────────────────────

#guard ((fun s => s ++ "!") <$> (⟨none, baseFont, "hi"⟩ : Attr String)).value = "hi!"
#guard Foldable.toList (⟨none, baseFont, "x"⟩ : Attr String) = ["x"]
#guard Foldable.toList (⟨[⟨none, baseFont, "a"⟩, ⟨none, baseFont, "b"⟩]⟩ : Attributed String)
       = ["a", "b"]
#guard (((fun s => s ++ "!") <$> (⟨[⟨none, baseFont, "a"⟩, ⟨none, baseFont, "b"⟩]⟩
        : Attributed String)).chunks.map (·.value)) = ["a!", "b!"]

-- ── `Traversable` (via `Option`) ────────────────

#guard (Traversable.traverse (fun s => if s == "" then none else some s)
        (⟨[⟨none, baseFont, "a"⟩, ⟨none, baseFont, "b"⟩]⟩ : Attributed String)
        |>.map (fun r => r.chunks.map (·.value))) = some ["a", "b"]
#guard (Traversable.traverse (fun s => if s == "" then none else some s)
        (⟨[⟨none, baseFont, "a"⟩, ⟨none, baseFont, ""⟩]⟩ : Attributed String)
        |>.map (fun r => r.chunks.map (·.value))) = none

end Tests.Linen.Text.DocLayout.Attributed
