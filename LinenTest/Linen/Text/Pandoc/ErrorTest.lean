/-
  Tests for `Linen.Text.Pandoc.Error`.
-/
import Linen.Text.Pandoc.Error

namespace Tests.Linen.Text.Pandoc.Error

open _root_.Linen.Text.Pandoc
open PandocError

-- ── renderError ───────────────────────────────────────────────────────

#guard renderError (PandocSomeError "boom") == "boom"
#guard renderError PandocFailOnWarningError == "Failing because there were warnings."
#guard renderError (PandocParseError "bad syntax") == "bad syntax"
#guard renderError (PandocResourceNotFound "img.png") == "File img.png not found in resource path"
#guard renderError (PandocMacroLoop "\\foo") == "Loop encountered in expanding macro '\\foo'"
#guard (renderError (PandocHttpError "http://x" "404")).startsWith "Could not fetch http://x" == true
#guard (renderError (PandocUnknownReaderError "doc")).startsWith "Unknown input format doc" == true

-- ── exitCode ──────────────────────────────────────────────────────────

#guard exitCode (PandocIOError "" "") == 1
#guard exitCode PandocFailOnWarningError == 3
#guard exitCode (PandocOptionError "x") == 6
#guard exitCode (PandocParseError "x") == 64
#guard exitCode (PandocResourceNotFound "x") == 99

end Tests.Linen.Text.Pandoc.Error
