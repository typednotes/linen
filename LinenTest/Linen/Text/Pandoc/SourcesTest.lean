/-
  Tests for `Linen.Text.Pandoc.Sources`.
-/
import Linen.Text.Pandoc.Sources

namespace Tests.Linen.Text.Pandoc.Sources

open _root_.Linen.Text.Pandoc

-- ── toSources / sourcesToText ─────────────────────────────────────────

#guard (ToSources.toSources "abc" : Sources).sourcesToText == "abc"
-- carriage returns are stripped
#guard (ToSources.toSources "a\r\nb" : Sources).sourcesToText == "a\nb"
-- multi-file input: each chunk gets a trailing newline and a source name
#guard (ToSources.toSources [("f1", "a"), ("f2", "b")] : Sources).sourcesToText == "a\nb\n"
#guard (ToSources.toSources [("f1", "a")] : Sources).initialSourceName == "f1"
#guard (ToSources.toSources "abc" : Sources).initialSourceName == ""

-- ── ensureFinalNewlines ───────────────────────────────────────────────

#guard ((ToSources.toSources "abc" : Sources).ensureFinalNewlines 2).sourcesToText == "abc\n\n"
#guard ((ToSources.toSources "abc\n\n" : Sources).ensureFinalNewlines 2).sourcesToText == "abc\n\n"
#guard ((⟨[]⟩ : Sources).ensureFinalNewlines 3).sourcesToText == "\n\n\n"

-- ── append ────────────────────────────────────────────────────────────

#guard ((ToSources.toSources "ab" : Sources) ++ (ToSources.toSources "cd" : Sources)).sourcesToText == "abcd"

end Tests.Linen.Text.Pandoc.Sources
