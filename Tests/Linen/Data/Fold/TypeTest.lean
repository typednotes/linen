/-
  Tests for `Data.Fold.Type` — driving folds over `Id` to their final result,
  honouring early `Done`.
-/
import Linen.Data.Fold.Type

open Data.Fold

namespace Tests.Data.Fold

/-- Feed a list through a fold, returning the final result. -/
def run [Monad m] (fld : Fold m a b) (xs : List a) : m b := do
  match ← fld.initial with
  | .Done b => pure b
  | .Partial s0 => go s0 xs
where
  go (s : fld.s) : List a → m b
    | [] => fld.final s
    | x :: rest => do
      match ← fld.step s x with
      | .Done b => pure b
      | .Partial s' => go s' rest

-- `foldl'` sums; `foldlM'` over `Id`.
#guard Id.run (run (foldl' (· + ·) 0) [1, 2, 3, 4]) == 10
#guard Id.run (run (foldlM' (m := Id) (fun s a => pure (s + a)) (pure 0)) [1, 2, 3]) == 6

-- `toList` / `toListRev`.
#guard Id.run (run toList [1, 2, 3]) == [1, 2, 3]
#guard Id.run (run toListRev [1, 2, 3]) == [3, 2, 1]

-- `fromPure` ignores all input.
#guard Id.run (run (fromPure 42) [1, 2, 3]) == 42

-- `Functor` maps the output.
#guard Id.run (run ((· * 2) <$> foldl' (· + ·) 0) [1, 2, 3]) == 12

-- `splitWith`: first `take 2` folds two, then the rest; combine with a pair.
#guard Id.run (run (splitWith Prod.mk (take 2 toList) toList) [1, 2, 3, 4]) == ([1, 2], [3, 4])

-- Applicative `<*>` (splitWith id): feed the header to the first, rest to second.
#guard Id.run (run ((Prod.mk <$> take 2 toList) <*> toList) [1, 2, 3, 4]) == ([1, 2], [3, 4])

-- `split_` / `*>` runs both serially, keeping the second.
#guard Id.run (run (split_ (take 2 toList) toList) [1, 2, 3, 4]) == [3, 4]

-- `teeWith` distributes the input to both folds.
#guard Id.run (run (teeWith Prod.mk (foldl' (· + ·) 0) toList) [1, 2, 3]) == (6, [1, 2, 3])

-- `lmap` / `filter` / `take` on the input.
#guard Id.run (run (lmap (· * 2) (foldl' (· + ·) 0)) [1, 2, 3]) == 12
#guard Id.run (run (filter (· % 2 == 0) (foldl' (· + ·) 0)) [1, 2, 3, 4]) == 6
#guard Id.run (run (take 2 (foldl' (· + ·) 0)) [1, 2, 3, 4]) == 3

-- `drain`.
#guard Id.run (run drain [1, 2, 3]) == ⟨⟩

end Tests.Data.Fold
