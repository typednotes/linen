/-
  Tests for `Linen.Graphics.Image.Utils` — the composition combinators,
  `swapIx`, and the bounded loop primitives.
-/
import Linen.Graphics.Image.Utils

open Graphics.Image.Utils

-- ── compose₂ / compose₂! ──

#guard compose₂ (· + 1) (· * ·) 3 4 == 13
#guard compose₂! (· + 1) (· * ·) 3 4 == 13
#guard compose₂ (· ++ "!") (fun (a b : Nat) => toString (a + b)) 2 3 == "5!"

-- ── swapIx ──

#guard swapIx (1, "a") == ("a", 1)
#guard swapIx ((true, 2) : Bool × Nat) == (2, true)

-- ── loop ──

-- Sum of indices 0..4.
#guard loop 0 5 0 (fun i acc => acc + i) == 10

-- Loop over an empty range leaves the accumulator untouched.
#guard loop 3 0 42 (fun i acc => acc + i) == 42

-- Loop starting at a non-zero offset visits `start, start+1, …`.
#guard loop 2 3 [] (fun i acc => acc ++ [i]) == [2, 3, 4]

-- ── loopM_ ──

private def imgUtilsCollect (start len : Nat) : List Nat :=
  ((loopM_ start len (fun i => modify (· ++ [i])) : StateM (List Nat) PUnit).run []).snd

#guard imgUtilsCollect 0 3 == [0, 1, 2]
#guard imgUtilsCollect 5 0 == []
