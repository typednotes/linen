/-
  Tests for `Data.Stream.Lift`.

  Inner-monad transforms are total, but the `toList` driver is `unsafe`, so
  checks run inside `#eval show IO Unit from do …`.
-/
import Linen.Data.Stream.Lift

open Data.Stream Data.Stream.Stream

namespace Tests.Data.Stream.Lift

private unsafe def check (name : String) (cond : Bool) : IO Unit :=
  unless cond do throw (IO.userError s!"Lift test failed: {name}")

private unsafe def runList (t : Stream Id a) : List a := Id.run (toList t)

#eval show IO Unit from do
  -- generalizeInner: Id → any monad (here back to Id)
  check "generalizeInner"
    (runList (generalizeInner (fromList [1, 2, 3]) : Stream Id Nat) == [1, 2, 3])
  -- liftInner then runReaderT round-trips a plain stream
  check "liftInner+runReaderT"
    (runList (runReaderT (pure 0)
      (liftInner (fromList [1, 2, 3] : Stream Id Nat) : Stream (ReaderT Nat Id) Nat)) == [1, 2, 3])
  -- runReaderT reads the environment inside the stream's effects
  let sr : Stream (ReaderT Nat Id) Nat :=
    mapM (fun x => (do let e ← read; pure (x + e) : ReaderT Nat Id Nat))
      (fromList [1, 2, 3] : Stream (ReaderT Nat Id) Nat)
  check "runReaderT" (runList (runReaderT (pure 10) sr) == [11, 12, 13])
  -- evalStateT threads and mutates state across steps
  let ss : Stream (StateT Nat Id) Nat :=
    mapM (fun x => (do modify (· + 1); let c ← get; pure (x * 100 + c) : StateT Nat Id Nat))
      (fromList [1, 2, 3] : Stream (StateT Nat Id) Nat)
  check "evalStateT" (runList (evalStateT (pure 0) ss) == [101, 202, 303])
  check "runStateT" (runList (runStateT (pure 0) ss) == [(1, 101), (2, 202), (3, 303)])
  -- foldlT: fold into a ReaderT accumulator
  let r : ReaderT Nat Id Nat :=
    foldlT (fun acc x => do let a ← acc; let e ← read; pure (a + x + e)) (pure 0)
      (fromList [1, 2, 3, 4] : Stream Id Nat)
  check "foldlT" (Id.run (r.run 0) == 10)

end Tests.Data.Stream.Lift
