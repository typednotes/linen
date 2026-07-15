/-
  Tests for `Linen.Text.Pandoc.Asciify`.
-/
import Linen.Text.Pandoc.Asciify

namespace Tests.Linen.Text.Pandoc.Asciify

open _root_.Linen.Text.Pandoc

-- ── toAsciiChar ───────────────────────────────────────────────────────

#guard Asciify.toAsciiChar 'a' == some 'a'
#guard Asciify.toAsciiChar 'é' == some 'e'
#guard Asciify.toAsciiChar 'ñ' == some 'n'
#guard Asciify.toAsciiChar 'ü' == some 'u'
#guard Asciify.toAsciiChar 'ı' == some 'i'   -- Turkish dotless i special case
-- letters that do not NFD-decompose to an ASCII base have no ASCII form
#guard Asciify.toAsciiChar 'ø' == none
#guard Asciify.toAsciiChar 'æ' == none

-- ── toAsciiText ───────────────────────────────────────────────────────

#guard Asciify.toAsciiText "café" == "cafe"
#guard Asciify.toAsciiText "naïve" == "naive"
#guard Asciify.toAsciiText "Žluťoučký" == "Zlutoucky"
-- undecomposable letters are dropped
#guard Asciify.toAsciiText "søster" == "sster"

end Tests.Linen.Text.Pandoc.Asciify
