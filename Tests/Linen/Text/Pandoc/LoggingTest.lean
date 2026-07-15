/-
  Tests for `Linen.Text.Pandoc.Logging`.
-/
import Linen.Text.Pandoc.Logging

namespace Tests.Linen.Text.Pandoc.Logging

open _root_.Linen.Text.Pandoc
open Verbosity LogMessage

private def pos : SourcePos := { name := "doc.md", line := 3, column := 5 }

-- ── messageVerbosity ──────────────────────────────────────────────────

#guard (messageVerbosity (SkippedContent "x" pos) == INFO) == true
#guard (messageVerbosity (DuplicateIdentifier "id" pos) == WARNING) == true
#guard (messageVerbosity (Fetching "http://x") == INFO) == true
-- `.sty` include file is INFO, others WARNING
#guard (messageVerbosity (CouldNotLoadIncludeFile "a.sty" pos) == INFO) == true
#guard (messageVerbosity (CouldNotLoadIncludeFile "a.tex" pos) == WARNING) == true

-- ── showLogMessage ────────────────────────────────────────────────────

#guard showPos pos == "doc.md line 3 column 5"
#guard showLogMessage (SkippedContent "junk" pos) == "Skipped 'junk' at doc.md line 3 column 5"
#guard showLogMessage (IgnoredElement "table") == "Ignored element table"
#guard showLogMessage (CouldNotDetermineMimeType "a.xyz") == "Could not determine mime type for 'a.xyz'"
#guard showLogMessage (Fetching "u") == "Fetching u..."
#guard showLogMessage (CiteprocWarning "oops") == "Citeproc: oops"
-- optional-suffix helper: empty extra vs nonempty
#guard showLogMessage (CouldNotFetchResource "u" "") == "Could not fetch resource 'u'"
#guard showLogMessage (CouldNotFetchResource "u" "timeout") == "Could not fetch resource 'u': timeout"

end Tests.Linen.Text.Pandoc.Logging
