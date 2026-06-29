/-
  Tests for `Linen.Data.ByteString.Char8` — the Latin-1 `Char` view of
  `ByteString`.
-/
import Linen.Data.ByteString.Char8

open Data
open Data.ByteString

namespace Tests.Data.ByteString.Char8

/-! ### pack / unpack round-trips -/

#guard Char8.unpack (Char8.pack "Hello, World!") == "Hello, World!"
#guard Char8.unpack (Char8.pack "") == ""
#guard (Char8.pack "AB") == ByteString.pack [65, 66]      -- Latin-1 bytes

/-! ### cons / snoc / head / last -/

#guard Char8.unpack (Char8.cons 'A' (Char8.pack "BC")) == "ABC"
#guard Char8.unpack (Char8.snoc (Char8.pack "AB") 'C') == "ABC"
#guard Char8.head? (Char8.pack "Hi") == some 'H'
#guard Char8.head? ByteString.empty == (none : Option Char)
#guard Char8.head (Char8.pack "Hi") (by decide) == 'H'
#guard Char8.last (Char8.pack "Hi") (by decide) == 'i'

/-! ### map / filter / folds -/

#guard Char8.unpack (Char8.map Char.toUpper (Char8.pack "abc")) == "ABC"
#guard Char8.unpack (Char8.filter (· != ' ') (Char8.pack "a b c")) == "abc"
#guard Char8.foldl (fun (acc : String) c => acc.push c) "" (Char8.pack "Hi") == "Hi"
#guard Char8.foldr (fun c (acc : String) => acc.push c) "" (Char8.pack "Hi") == "iH"

/-! ### search -/

#guard Char8.elem 'b' (Char8.pack "abc") == true
#guard Char8.elem 'z' (Char8.pack "abc") == false
#guard Char8.find (· == 'b') (Char8.pack "abc") == some 'b'

/-! ### takeWhile / dropWhile / span / break -/

#guard Char8.unpack (Char8.takeWhile (· != ' ') (Char8.pack "ab cd")) == "ab"
#guard Char8.unpack (Char8.dropWhile (· != ' ') (Char8.pack "ab cd")) == " cd"
#guard Char8.unpack (Char8.span (· != ' ') (Char8.pack "ab cd")).1 == "ab"
#guard Char8.unpack (Char8.break (· == ' ') (Char8.pack "ab cd")).1 == "ab"

/-! ### lines (trailing newline does not add an empty line) -/

#guard (Char8.lines (Char8.pack "a\nb\nc")).map Char8.unpack == ["a", "b", "c"]
#guard (Char8.lines (Char8.pack "a\nb\n")).map Char8.unpack == ["a", "b"]
#guard (Char8.lines (Char8.pack "")).map Char8.unpack == ([] : List String)
#guard (Char8.lines (Char8.pack "\n")).map Char8.unpack == [""]

/-! ### words (whitespace runs collapse; empties dropped) -/

#guard (Char8.words (Char8.pack "  hello   world  ")).map Char8.unpack == ["hello", "world"]
#guard (Char8.words (Char8.pack "one\ttwo\nthree")).map Char8.unpack == ["one", "two", "three"]
#guard (Char8.words (Char8.pack "   ")).map Char8.unpack == ([] : List String)

/-! ### unlines / unwords -/

#guard Char8.unpack (Char8.unlines [Char8.pack "a", Char8.pack "b"]) == "a\nb\n"
#guard Char8.unpack (Char8.unwords [Char8.pack "a", Char8.pack "b", Char8.pack "c"]) == "a b c"

/-! ### lines / unlines round-trip on newline-terminated text -/

#guard Char8.unpack (Char8.unlines (Char8.lines (Char8.pack "x\ny\n"))) == "x\ny\n"

end Tests.Data.ByteString.Char8
