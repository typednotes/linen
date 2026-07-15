/-
  Tests for `Linen.Text.Pandoc.Shared`.
-/
import Linen.Text.Pandoc.Shared

namespace Tests.Linen.Text.Pandoc.Shared

open _root_.Linen.Text.Pandoc

-- ── List / text processing ────────────────────────────────────────────

#guard Shared.splitTextBy (· == ',') "a,b,c" == ["a", "b", "c"]
#guard Shared.trim "  hi  " == "hi"
#guard Shared.triml "  hi  " == "hi  "
#guard Shared.trimr "  hi  " == "  hi"
#guard Shared.stripTrailingNewlines "abc\n\n" == "abc"
#guard Shared.camelCaseToHyphenated "camelCase" == "camel-case"
#guard Shared.tabFilter 4 "a\tb" == "a   b"

-- ── Roman numerals ────────────────────────────────────────────────────

#guard Shared.toRomanNumeral 4 == "IV"
#guard Shared.toRomanNumeral 9 == "IX"
#guard Shared.toRomanNumeral 1990 == "MCMXC"
#guard Shared.toRomanNumeral 2024 == "MMXXIV"

-- ── Ordered-list markers ──────────────────────────────────────────────

#guard Shared.orderedListMarkersN 3 (1, ListNumberStyle.Decimal, ListNumberDelim.Period) == ["1.", "2.", "3."]
#guard Shared.orderedListMarkersN 3 (1, ListNumberStyle.LowerAlpha, ListNumberDelim.OneParen) == ["a)", "b)", "c)"]
#guard Shared.orderedListMarkersN 3 (1, ListNumberStyle.UpperRoman, ListNumberDelim.Period) == ["I.", "II.", "III."]

-- ── AST helpers ───────────────────────────────────────────────────────

#guard Shared.stringify ([Inline.Str "a", Inline.Space, Inline.Str "b"] : List Inline) == "a b"
#guard Shared.stringify ([Inline.Emph [Inline.Str "hi"], Inline.Note []] : List Inline) == "hi"
#guard Shared.isTightList [[Block.Plain []]] == true
#guard Shared.isTightList [[Block.Para []]] == false
#guard Shared.isHeaderBlock (Block.Header 1 nullAttr []) == true

-- ── Identifiers ───────────────────────────────────────────────────────

#guard Shared.textToIdentifier emptyExtensions "Hello World!" == "hello-world"
#guard Shared.uniqueIdent emptyExtensions [Inline.Str "Intro"] [] == "intro"
#guard Shared.uniqueIdent emptyExtensions [Inline.Str "Intro"] ["intro"] == "intro-1"

-- ── File paths ────────────────────────────────────────────────────────

#guard Shared.collapseFilePath "a/./b/../c" == "a/c"
#guard Shared.collapseFilePath "./foo" == "foo"

-- ── combineAttr ───────────────────────────────────────────────────────

#guard Shared.combineAttr ("x", ["a"], []) ("y", ["b"], []) == ("x", ["a", "b"], [])

end Tests.Linen.Text.Pandoc.Shared
