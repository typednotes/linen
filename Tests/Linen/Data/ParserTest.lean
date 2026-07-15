/-
  Tests for `Data.Parser` (the combinators + list driver).

  The list driver (`parseList`/`parseBreakList`) is `unsafe` (backtracking makes
  it non-well-founded), so checks run inside `#eval show IO Unit from do …`, like
  the `Data.Stream.Eliminate` tests.
-/
import Linen.Data.Parser
import Linen.Data.Fold.Type

open Data.Parser
open Data.Fold (Fold)

namespace Tests.Data.Parser

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Parser test failed: {name}")

/-- Run a parser over a list (in `Id`) and get the result. -/
private unsafe def run (p : Parser a Id b) (xs : List a) : Except String b :=
  Id.run (parseList p xs)

/-- Match an `Except.ok` result against an expected value. -/
private def okEq [BEq b] : Except String b → b → Bool
  | .ok x, v => x == v
  | .error _, _ => false

/-- Was the parse an error? -/
private def isErr : Except String b → Bool
  | .error _ => true
  | .ok _ => false

#eval show IO Unit from do
  -- satisfy / one / oneEq
  check "satisfy pass" (okEq (run (satisfy (· == 1)) [1, 0, 1]) 1)
  check "satisfy fail" (isErr (run (satisfy (· == 9)) [1, 0, 1]))
  check "satisfy eof" (isErr (run (satisfy (· == 1)) ([] : List Nat)))
  check "one" (okEq (run (one : Parser Nat Id Nat) [7, 8]) 7)
  check "oneEq" (okEq (run (oneEq 3) [3, 4]) 3)
  check "oneNotEq" (okEq (run (oneNotEq 3) [4, 3]) 4)
  check "oneOf" (okEq (run (oneOf [2, 3, 4]) [3, 9]) 3)
  check "noneOf" (okEq (run (noneOf [2, 3, 4]) [9, 3]) 9)
  -- peek / eof
  check "peek" (okEq (run (peek : Parser Nat Id Nat) [5, 6]) 5)
  check "eof fail" (isErr (run (eof : Parser Nat Id PUnit) [1]))
  check "eof pass" (okEq (run (eof : Parser Nat Id PUnit) ([] : List Nat)) ⟨⟩)
  -- either / maybe
  check "either ok" (okEq (run (either (fun x => if x > 0 then .ok (x * 10) else .error "neg")) [3]) 30)
  check "either err" (isErr (run (either (fun x : Nat => if x > 0 then .ok x else .error "neg")) [0]))
  check "maybe some" (okEq (run (maybe (fun x => if x > 0 then some (x + 1) else none)) [4]) 5)
  check "maybe none" (isErr (run (maybe (fun x : Nat => if x > 0 then some x else none)) [0]))
  -- takeWhile / takeWhile1 / dropWhile
  check "takeWhile" (okEq (run (takeWhile (· == 0) Data.Fold.toList) [0, 0, 1, 0]) [0, 0])
  check "takeWhile none" (okEq (run (takeWhile (· == 0) Data.Fold.toList) [1, 2]) ([] : List Nat))
  check "takeWhile1 ok" (okEq (run (takeWhile1 (· == 0) Data.Fold.toList) [0, 0, 1]) [0, 0])
  check "takeWhile1 fail" (isErr (run (takeWhile1 (· == 0) Data.Fold.toList) [1, 0]))
  check "dropWhile" (okEq (run (dropWhile (· == 0)) [0, 0, 1]) ⟨⟩)
  -- takeEQ / takeGE / takeBetween
  check "takeEQ ok" (okEq (run (takeEQ 2 Data.Fold.toList) [1, 0, 1]) [1, 0])
  check "takeEQ short" (isErr (run (takeEQ 4 Data.Fold.toList) [1, 0, 1]))
  check "takeGE ok" (okEq (run (takeGE 4 Data.Fold.toList) [1, 0, 1, 0, 1]) [1, 0, 1, 0, 1])
  check "takeGE short" (isErr (run (takeGE 4 Data.Fold.toList) [1, 0, 1]))
  check "takeBetween ok" (okEq (run (takeBetween 2 4 Data.Fold.toList) [1, 2, 3, 4, 5]) [1, 2, 3, 4])
  check "takeBetween short" (isErr (run (takeBetween 2 4 Data.Fold.toList) [1]))
  check "takeBetween exact" (okEq (run (takeBetween 2 4 Data.Fold.toList) [1, 2]) [1, 2])
  -- fromFold / fromFoldMaybe
  check "fromFold" (okEq (run (fromFold Data.Fold.toList) [1, 2, 3]) [1, 2, 3])
  check "fromFoldMaybe ok"
    (okEq (run (fromFoldMaybe "bad" ((fun (l : List Nat) => l.head?) <$> Data.Fold.toList)) [7, 8]) 7)
  check "fromFoldMaybe none"
    (isErr (run (fromFoldMaybe "bad" ((fun (l : List Nat) => l.head?) <$> Data.Fold.toList)) ([] : List Nat)))
  -- Applicative: splitWith / split_
  check "splitWith" (okEq (run ((fun x y => x + y) <$> satisfy (· > 0) <*> satisfy (· > 0)) [3, 4]) 7)
  check "split_ (*>)" (okEq (run (satisfy (· > 0) *> satisfy (· > 0)) [3, 4]) 4)
  -- Alternative: alt
  check "alt left" (okEq (run (alt (satisfy (· == 1)) (satisfy (· == 2))) [1]) 1)
  check "alt right" (okEq (run (alt (satisfy (· == 1)) (satisfy (· == 2))) [2]) 2)
  check "alt both fail" (isErr (run (alt (satisfy (· == 1)) (satisfy (· == 2))) [3]))
  -- many / some
  check "many" (okEq (run (many (satisfy (· == 0))) [0, 0, 0, 1]) [0, 0, 0])
  check "many empty" (okEq (run (many (satisfy (· == 0))) [1, 2]) ([] : List Nat))
  check "some ok" (okEq (run (some (satisfy (· == 0))) [0, 0, 1]) [0, 0])
  check "some fail" (isErr (run (some (satisfy (· == 0))) [1, 2]))
  -- Functor + leftover
  check "fmap" (okEq (run ((· * 100) <$> satisfy (· > 0)) [3]) 300)
  let (r, rest) := Id.run (parseBreakList (satisfy (· == 1)) [1, 2, 3])
  check "leftover result" (okEq r 1)
  check "leftover rest" (rest == [2, 3])

end Tests.Data.Parser
