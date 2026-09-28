/-
  Tests for `Linen.Text.Pandoc.Parsing`.
-/
import Linen.Text.Pandoc.Parsing

namespace Tests.Linen.Text.Pandoc.Parsing

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Parsing

-- ── character / line primitives ───────────────────────────────────────

#guard (readWith anyChar "abc").toOption == some 'a'
#guard (readWith anyLine "hello\nworld").toOption == some "hello"
#guard (readWith (many1Char (satisfyC Char.isDigit)) "123abc").toOption == some "123"
#guard (readWith blanklines "\n\n x").toOption == some "\n\n"

-- ── repetition combinators ────────────────────────────────────────────

#guard ((readWith (count 3 anyChar) "abcd").toOption.map (·.toList)) == some ['a', 'b', 'c']
#guard ((readWith (manyTillChar anyChar (char '!')) "abc!").toOption) == some "abc"
#guard ((readWith (sepBy1 (many1Char (satisfyC Char.isAlpha)) (char ',')) "a,bb,ccc").toOption.map (·.toList))
        == some ["a", "bb", "ccc"]

-- ── string matching ───────────────────────────────────────────────────

#guard (readWith (oneOfStrings ["foo", "foobar"]) "foobar").toOption == some "foobar"
#guard (readWith (stringAnyCase "abc") "AbC").toOption == some "AbC"
#guard (readWith (oneOfStringsCI ["http", "https"]) "HTTPS://x").toOption == some "HTTPS"

-- ── character references ──────────────────────────────────────────────

#guard (readWith characterReference "&amp;").toOption == some "&"
#guard (readWith characterReference "&#65;").toOption == some "A"

-- ── roman numerals ────────────────────────────────────────────────────

#guard (readWith (romanNumeral true) "XIV").toOption == some (14 : Int)
#guard (readWith (romanNumeral false) "mmxxiv").toOption == some (2024 : Int)

-- ── uri / email ───────────────────────────────────────────────────────

#guard ((readWith uri "https://example.com/x.").toOption.map (·.1)) == some "https://example.com/x"
#guard ((readWith emailAddress "a@b.com").toOption.map (·.2)) == some "mailto:a@b.com"

-- ── keys, positions, attributes ───────────────────────────────────────

#guard (toKey "[Foo  Bar]").unKey == "foo bar"
#guard (posFromByteOffset "ab\ncd" 4).line == 2
#guard (posFromByteOffset "ab\ncd" 4).column == 2
#guard extractIdClass ("", [], [("id", "x"), ("class", "a b"), ("k", "v")]) == ("x", ["a", "b"], [("k", "v")])

end Tests.Linen.Text.Pandoc.Parsing
