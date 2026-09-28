/-
  Tests for `Linen.Text.DocLayout.HasChars`.
-/
import Linen.Text.DocLayout.HasChars

namespace Tests.Linen.Text.DocLayout.HasChars

open _root_.Text.DocLayout

-- ── `String` instance ────────────────────────────

#guard HasChars.foldrChar (fun c acc => c :: acc) [] "abc" = ['a', 'b', 'c']
#guard HasChars.foldlChar (fun acc c => acc ++ [c]) [] "abc" = ['a', 'b', 'c']
#guard (HasChars.replicateChar 3 'x' : String) = "xxx"
#guard HasChars.isNull "" = true
#guard HasChars.isNull "a" = false
#guard HasChars.build "hello" = "hello"

-- `splitLines` == `lines . (++ "\n")`
#guard HasChars.splitLines "a\nb\nc" = ["a", "b", "c"]
#guard HasChars.splitLines "a\n\nb" = ["a", "", "b"]
#guard HasChars.splitLines "" = [""]
#guard HasChars.splitLines "a\n" = ["a", ""]

-- ── `Attr` instance delegates to the payload ─────

#guard HasChars.isNull (⟨none, baseFont, ""⟩ : Attr String) = true
#guard HasChars.isNull (⟨none, baseFont, "x"⟩ : Attr String) = false
#guard HasChars.build (⟨none, baseFont, "hi"⟩ : Attr String) = "hi"
-- `splitLines` re-attaches the chunk's font to each line
#guard ((HasChars.splitLines (⟨none, baseFont ~> .RWeight .Bold, "a\nb"⟩ : Attr String)).map
        (fun c => (c.value, c.font == baseFont ~> .RWeight .Bold)))
       = [("a", true), ("b", true)]

-- ── `Attributed` instance ────────────────────────

#guard HasChars.build (⟨[⟨none, baseFont, "ab"⟩, ⟨none, baseFont, "cd"⟩]⟩ : Attributed String)
       = "abcd"
#guard HasChars.isNull (⟨[⟨none, baseFont, ""⟩]⟩ : Attributed String) = true
#guard HasChars.foldrChar (fun c acc => c :: acc) []
        (⟨[⟨none, baseFont, "ab"⟩, ⟨none, baseFont, "c"⟩]⟩ : Attributed String)
       = ['a', 'b', 'c']

-- `splitLines` over `Attributed`: a newline inside a chunk splits the run,
-- merging boundary pieces with adjacent chunks (payloads shown per line)
#guard ((HasChars.splitLines
          (⟨[⟨none, baseFont, "ab"⟩, ⟨none, baseFont, "c\nd"⟩, ⟨none, baseFont, "ef"⟩]⟩
            : Attributed String)).map (fun ln => ln.chunks.map (·.value)))
       = [["ab", "c"], ["d", "ef"]]

end Tests.Linen.Text.DocLayout.HasChars
