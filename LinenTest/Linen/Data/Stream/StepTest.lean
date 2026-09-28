import Linen.Data.Stream.Step

open Data.Stream Data.Stream.Step

-- `map` transforms the yielded value and preserves `Skip`/`Stop`.
#guard (match Step.map (· + 1) (Step.Yield 4 "s") with
          | .Yield x st => (x, st) == (5, "s") | _ => false)
#guard (match Step.map (· + 1) (Step.Skip "s") with | .Skip st => st == "s" | _ => false)
#guard (match (Step.Stop : Step String Nat).map (· + 1) with | .Stop => true | _ => false)

-- The `Functor` instance agrees with `map`.
#guard (match (· + 1) <$> (Step.Yield 4 "s") with | .Yield x _ => x == 5 | _ => false)
