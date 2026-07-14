/-
  Tests for `Linen.Control.Exception.Lens`.

  `IO.Error` derives no `DecidableEq` (nor does Lean core's `Except`), so
  every equality below that involves an `IO.Error` value (directly, or
  nested inside a `Sum`/`Except`) is checked with `example ... := rfl`
  (definitional equality, no `Decidable` instance needed) rather than
  `#guard`; equalities between plain `String`/`Nat`/`Option String × UInt32
  × String` payloads still use `#guard`.
-/
import Linen.Control.Exception.Lens

open Control.Lens Control.Exception.Lens

namespace Tests.Linen.Control.Exception.Lens

-- Each per-constructor prism round-trips its own case and rejects others.

example : withPrism _UserError (fun bt _ => bt "boom") = IO.Error.userError "boom" := rfl
example : withPrism _UserError (fun _ seta => seta (IO.Error.userError "boom")) = Sum.inr "boom" := rfl
example : withPrism _UserError (fun _ seta => seta IO.Error.unexpectedEof)
    = Sum.inl IO.Error.unexpectedEof := rfl

example : withPrism _UnexpectedEof (fun bt _ => bt ()) = IO.Error.unexpectedEof := rfl
example : withPrism _UnexpectedEof (fun _ seta => seta IO.Error.unexpectedEof) = Sum.inr () := rfl
example : withPrism _UnexpectedEof (fun _ seta => seta (IO.Error.userError "x"))
    = Sum.inl (IO.Error.userError "x") := rfl

example : withPrism _NoSuchThing (fun bt _ => bt (some "f", 2, "d"))
    = IO.Error.noSuchThing (some "f") 2 "d" := rfl
example : withPrism _NoSuchThing (fun _ seta => seta (IO.Error.noSuchThing (some "f") 2 "d"))
    = Sum.inr (some "f", 2, "d") := rfl

example : withPrism _Interrupted (fun bt _ => bt ("f", 3, "d")) = IO.Error.interrupted "f" 3 "d" := rfl
example : withPrism _Interrupted (fun _ seta => seta (IO.Error.interrupted "f" 3 "d"))
    = Sum.inr ("f", (3 : UInt32), "d") := rfl

example : withPrism _OtherError (fun bt _ => bt (1, "x")) = IO.Error.otherError 1 "x" := rfl
example : withPrism _AlreadyExists (fun bt _ => bt (none, 1, "x"))
    = IO.Error.alreadyExists none 1 "x" := rfl

-- `throwing`/`catching`/`trying` are `Control.Monad.Error.Lens`'s
-- `MonadExcept`-generic combinators, re-exported here specialized to
-- `IO.Error`/`IO`.

example : (Except.error (IO.Error.userError "boom") : Except IO.Error Nat)
    = throwing _UserError "boom" := rfl

example : catching _UserError (throw (IO.Error.userError "boom") : Except IO.Error Nat)
    (fun (m : String) => pure m.length) = Except.ok 4 := rfl

example : trying _UserError (throw (IO.Error.userError "boom") : Except IO.Error Nat)
    = Except.ok (Except.error "boom") := rfl

end Tests.Linen.Control.Exception.Lens
