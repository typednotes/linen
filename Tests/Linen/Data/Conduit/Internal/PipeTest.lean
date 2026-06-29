/-
  Tests for `Linen.Data.Conduit.Internal.Pipe`.

  `Pipe` has function-typed fields, so values can't be compared with `BEq`.
  Instead we observe an `Id`-effect, input-free pipe with `drain`, which
  collects the emitted outputs and the final result.
-/
import Linen.Data.Conduit.Internal.Pipe

open Data.Conduit.Internal

namespace Tests.Data.Conduit.Internal.Pipe

/-- A concrete pipe: `Unit` leftovers/input/upstream, `Nat` outputs, over `Id`. -/
abbrev P (r : Type) := Pipe Unit Unit Nat Unit Id r

/-- Run an input-free `Id`-pipe: collect its outputs and final result.
    `needInput` is treated as stuck. Structural recursion on the pipe. -/
private def drain {l i o u r : Type} : Pipe l i o u Id r → List o × Option r
  | .done r => ([], some r)
  | .haveOutput next out => let (os, res) := drain next; (out :: os, res)
  | .pipeM act k => drain (k (Id.run act))
  | .leftover next _ => drain next
  | .needInput _ _ => ([], none)

/-! ### done / outputs -/

#guard drain (Pipe.done 5 : P Nat) == ([], some 5)
#guard drain (Pipe.haveOutput (Pipe.haveOutput (Pipe.done 0) 20) 10 : P Nat) == ([10, 20], some 0)
#guard drain (Pipe.leftover (Pipe.done 3) () : P Nat) == ([], some 3)

/-! ### Functor: `map` rewrites the result, not the outputs -/

#guard drain ((· + 1) <$> (Pipe.haveOutput (Pipe.done 0) 10 : P Nat)) == ([10], some 1)

/-! ### Bind: substitutes at `done`, preserving the spine -/

#guard drain ((Pipe.haveOutput (Pipe.done 5) 99 : P Nat) >>= fun n => Pipe.done (n + 1)) == ([99], some 6)

/-! ### pipeM runs the effect, then continues -/

#guard drain (Pipe.pipeM (pure 7 : Id Nat) (fun n => Pipe.done (n * 2)) : P Nat) == ([], some 14)

/-! ### do-notation (the Monad) -/

#guard drain (do let a ← (Pipe.done 5 : P Nat); let b ← (Pipe.done 10 : P Nat); pure (a + b)) == ([], some 15)
#guard drain (do
    let _ ← (Pipe.haveOutput (Pipe.done 1) 100 : P Nat)
    Pipe.haveOutput (Pipe.done 2) 200) == ([100, 200], some 2)

/-! ### computation rules (compile-time) -/

example (a : Nat) (f : Nat → P Nat) : (pure a : P Nat) >>= f = f a := Pipe.pure_bind a f
example (a : Nat) (f : Nat → P Nat) : Pipe.bind f (Pipe.done a) = f a := Pipe.bind_done f a

end Tests.Data.Conduit.Internal.Pipe
