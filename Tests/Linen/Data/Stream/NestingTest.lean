/-
  Tests for `Data.Stream.Nesting`.

  Interleave/merge combinators are total, but the `toList` driver is `unsafe`,
  so checks run inside `#eval show IO Unit from do …`.
-/
import Linen.Data.Stream.Nesting

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream.Nesting

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Nesting test failed: {name}")

private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  -- interleave drains both streams
  check "interleave equal"
    (runList (interleave (fromList [1, 3, 5]) (fromList [2, 4, 6]) : Stream Id Nat)
      == [1, 2, 3, 4, 5, 6])
  check "interleave uneven"
    (runList (interleave (fromList [1, 2, 3]) (fromList [10, 20]) : Stream Id Nat)
      == [1, 10, 2, 20, 3])
  -- interleaveMin stops with the shorter stream
  check "interleaveMin"
    (runList (interleaveMin (fromList [1, 2, 3]) (fromList [10, 20]) : Stream Id Nat)
      == [1, 10, 2, 20, 3])
  -- mergeBy merges two ascending streams
  check "mergeBy"
    (runList (mergeBy compare (fromList [1, 3, 5]) (fromList [2, 4, 6, 8]) : Stream Id Nat)
      == [1, 2, 3, 4, 5, 6, 8])
  check "mergeBy one empty"
    (runList (mergeBy compare (fromList [1, 2, 3]) (nil : Stream Id Nat)) == [1, 2, 3])
  check "mergeBy ties favour first"
    (runList (mergeBy (fun (x : Nat × Bool) y => compare x.1 y.1)
      (fromList [(1, true), (2, true)]) (fromList [(1, false)])) == [(1, true), (1, false), (2, true)])

end Tests.Data.Stream.Nesting
