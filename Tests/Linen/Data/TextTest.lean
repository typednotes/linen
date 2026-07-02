/-
  Tests for `Linen.Data.Text` — Haskell `Data.Text` API on `String`.
-/
import Linen.Data.Text

open Data

namespace Tests.Data.Text

/-! ### Construction -/

#guard Text.pack ['h', 'i'] == "hi"
#guard Text.unpack "hi" == ['h', 'i']
#guard Text.singleton 'x' == "x"
#guard Text.empty == ""

/-! ### Basic interface -/

#guard Text.null "" == true
#guard Text.null "x" == false
#guard Text.length "hello" == 5
#guard Text.compareLength "hello" 5 == Ordering.eq
#guard Text.compareLength "hello" 3 == Ordering.gt
#guard Text.compareLength "hi" 5 == Ordering.lt

/-! ### Transformations -/

#guard Text.map Char.toUpper "abc" == "ABC"
#guard Text.intercalate ", " ["a", "b", "c"] == "a, b, c"
#guard Text.intersperse ',' "abc" == "a,b,c"
#guard Text.transpose ["ab", "cd"] == ["ac", "bd"]
#guard Text.transpose ["abc", "de"] == ["ad", "be", "c"]
#guard Text.reverse "abc" == "cba"
#guard Text.replace "a" "X" "banana" == "bXnXnX"

/-! ### Case conversion -/

#guard Text.toLower "ABC" == "abc"
#guard Text.toUpper "abc" == "ABC"
#guard Text.toTitle "hello world" == "Hello world"
#guard Text.toCaseFold "ABC" == "abc"

/-! ### Justification -/

#guard Text.justifyLeft 5 '-' "ab" == "ab---"
#guard Text.justifyRight 5 '-' "ab" == "---ab"
#guard Text.center 6 '-' "ab" == "--ab--"
#guard Text.justifyLeft 1 '-' "abc" == "abc"

/-! ### Folds -/

#guard Text.foldl (fun acc c => acc ++ toString c) "" "abc" == "abc"
#guard Text.foldl' (· + ·.toNat) 0 "AB" == 'A'.toNat + 'B'.toNat
#guard Text.foldr (fun c acc => toString c ++ acc) "" "abc" == "abc"

/-! ### Special folds -/

#guard Text.concat ["a", "b", "c"] == "abc"
#guard Text.concatMap (fun c => Text.singleton c ++ Text.singleton c) "ab" == "aabb"
#guard Text.any (· == 'b') "abc" == true
#guard Text.all (· == 'a') "aaa" == true
#guard Text.all (· == 'a') "aab" == false
#guard Text.maximum "bca" == 'c'
#guard Text.minimum "bca" == 'a'

/-! ### Substrings -/

#guard Text.head "abc" == 'a'
#guard Text.last "abc" == 'c'
#guard Text.tail "abc" == "bc"
#guard Text.init "abc" == "ab"
#guard Text.cons 'x' "yz" == "xyz"
#guard Text.snoc "xy" 'z' == "xyz"
#guard Text.append "ab" "cd" == "abcd"
#guard Text.uncons "abc" == some ('a', "bc")
#guard Text.uncons "" == none
#guard Text.unsnoc "abc" == some ("ab", 'c')
#guard Text.unsnoc "" == none

/-! ### Cutting -/

#guard Text.take 2 "hello" == "he"
#guard Text.drop 2 "hello" == "llo"
#guard Text.takeWhile Char.isAlpha "ab12" == "ab"
#guard Text.dropWhile Char.isAlpha "ab12" == "12"
#guard Text.dropWhileEnd Char.isDigit "ab12" == "ab"
#guard Text.dropAround Char.isWhitespace "  hi  " == "hi"
#guard Text.strip "  hi  " == "hi"
#guard Text.stripStart "  hi" == "hi"
#guard Text.stripEnd "hi  " == "hi"
#guard Text.splitAt 2 "hello" == ("he", "llo")
#guard Text.breakOn "," "a,b,c" == ("a", ",b,c")
#guard Text.breakOn "," "abc" == ("abc", "")
#guard Text.breakOnEnd "," "a,b,c" == ("a,b,", "c")
#guard Text.break_ (· == 'c') "abcd" == ("ab", "cd")
#guard Text.span Char.isAlpha "ab12" == ("ab", "12")

/-! ### Grouping -/

#guard Text.group "aabbbca" == ["aa", "bbb", "c", "a"]
#guard Text.groupBy (· == ·) "aabbbca" == ["aa", "bbb", "c", "a"]

/-! ### Prefixes/suffixes -/

#guard Text.inits "abc" == ["", "a", "ab", "abc"]
#guard Text.tails "abc" == ["abc", "bc", "c", ""]

/-! ### Splitting -/

#guard Text.splitOn "," "a,b,c" == ["a", "b", "c"]
#guard Text.split (· == ',') "a,b,c" == ["a", "b", "c"]
#guard Text.chunksOf 2 "abcde" == ["ab", "cd", "e"]
#guard Text.chunksOf 0 "abc" == ["abc"]

/-! ### Lines and words -/

#guard Text.lines "a\nb\nc" == ["a", "b", "c"]
#guard Text.words "hello   world" == ["hello", "world"]
#guard Text.unlines ["a", "b"] == "a\nb\n"
#guard Text.unwords ["hello", "world"] == "hello world"

/-! ### Predicates -/

#guard Text.isPrefixOf "ab" "abc" == true
#guard Text.isPrefixOf "bc" "abc" == false
#guard Text.isSuffixOf "bc" "abc" == true
#guard Text.isInfixOf "bc" "abcd" == true
#guard Text.isInfixOf "xy" "abcd" == false
#guard Text.isInfixOf "" "abcd" == true
#guard Text.stripPrefix "ab" "abc" == some "c"
#guard Text.stripPrefix "x" "abc" == none
#guard Text.stripSuffix "bc" "abc" == some "a"
#guard Text.stripSuffix "x" "abc" == none

/-! ### Search -/

#guard Text.elem 'b' "abc" == true
#guard Text.elem 'z' "abc" == false
#guard Text.find (· == 'b') "abc" == some 'b'
#guard Text.filter Char.isAlpha "ab12" == "ab"
#guard Text.partition Char.isAlpha "ab12" == ("ab", "12")

/-! ### Indexing -/

#guard Text.index "abc" 1 == some 'b'
#guard Text.index "abc" 9 == none
#guard Text.count "a" "banana" == 3
#guard Text.count "" "abc" == 4

/-! ### Zipping -/

#guard Text.zip "abc" "xyz" == [('a', 'x'), ('b', 'y'), ('c', 'z')]
#guard Text.zipWith (fun a b => a) "abc" "xy" == "ab"

/-! ### Laws -/

example : Text.pack (Text.unpack "abc") = "abc" := Text.pack_unpack _
example : Text.unpack (Text.pack ['a', 'b']) = ['a', 'b'] := Text.unpack_pack _
example : Text.null Text.empty = true := Text.null_empty
example : Text.length Text.empty = 0 := Text.length_empty
example (c : Char) : Text.length (Text.singleton c) = 1 := Text.length_singleton c
example (c : Char) (t : Text) : Text.length (Text.cons c t) = Text.length t + 1 :=
  Text.length_cons c t
example (t : Text) : Text.append Text.empty t = t := Text.append_empty_left t
example (t : Text) : Text.append t Text.empty = t := Text.append_empty_right t
example : Text.reverse Text.empty = Text.empty := Text.reverse_empty
example (t : Text) : Text.reverse (Text.reverse t) = t := Text.reverse_reverse t

end Tests.Data.Text
