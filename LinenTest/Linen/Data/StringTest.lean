/-
  Tests for `Linen.Data.String` — the `IsString` class and `words`/`unwords`/
  `unlines` tokenising/joining (`lines` is core `String.splitOn "\n"`).
-/
import Linen.Data.String

namespace Tests.Data.String

/-! ### IsString -/

#guard (IsString.fromString "hi" : String) == "hi"

/-! ### words — split on whitespace runs, drop empties -/

#guard String.words "hello world" == ["hello", "world"]
#guard String.words "  leading and   multiple   spaces  " == ["leading", "and", "multiple", "spaces"]
#guard String.words "" == []
#guard String.words "single" == ["single"]
#guard String.words "a\tb\nc" == ["a", "b", "c"]      -- tabs/newlines are whitespace

/-! ### unwords / unlines -/

#guard String.unwords ["hello", "world"] == "hello world"
#guard String.unwords [] == ""
#guard String.unwords ["solo"] == "solo"
#guard String.unlines ["a", "b"] == "a\nb\n"          -- trailing newline
#guard String.unlines [] == ""
#guard String.unlines ["x"] == "x\n"

/-! ### roundtrip on single-spaced text -/

#guard String.unwords (String.words "round trip here") == "round trip here"

/-! ### `lines` is core `splitOn "\n"` (documenting the mapping) -/

#guard "a\nb\nc".splitOn "\n" == ["a", "b", "c"]

end Tests.Data.String
