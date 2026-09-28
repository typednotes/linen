/-
  Tests for `Linen.Text.Pandoc.Options`.
-/
import Linen.Text.Pandoc.Options

namespace Tests.Linen.Text.Pandoc.Options

open _root_.Linen.Text.Pandoc
open Extension TrackChanges WrapOption HTMLMathMethod ReferenceLocation

-- ── Reader option defaults ────────────────────────────────────────────

private def ro : ReaderOptions := {}

#guard ro.readerColumns == 80
#guard ro.readerTabStop == 4
#guard ro.readerStandalone == false
#guard (ro.readerTrackChanges == AcceptChanges) == true
#guard ro.readerAbbreviations.contains "Mr." == true
#guard ro.readerAbbreviations.length == 28

-- ── Writer option defaults ────────────────────────────────────────────

private def wo : WriterOptions := {}

#guard wo.writerTabStop == 4
#guard wo.writerColumns == 72
#guard wo.writerDpi == 96
#guard wo.writerTemplate.isNone == true
#guard (wo.writerWrapText == WrapAuto) == true
#guard (wo.writerHTMLMathMethod == PlainMath) == true
#guard (wo.writerReferenceLocation == EndOfDocument) == true
#guard wo.writerNumberOffset == [0, 0, 0, 0, 0, 0]
#guard wo.writerEpubSubdirectory == "EPUB"
#guard wo.writerChunkTemplate == "%s-%i.html"

-- ── isEnabled through HasSyntaxExtensions ─────────────────────────────

private def roWithExt : ReaderOptions := { readerExtensions := pandocExtensions }
private def woWithExt : WriterOptions := { writerExtensions := githubMarkdownExtensions }

#guard isEnabled Ext_footnotes roWithExt == true
#guard isEnabled Ext_hard_line_breaks roWithExt == false
#guard isEnabled Ext_emoji woWithExt == true

end Tests.Linen.Text.Pandoc.Options
