/-
  Tests for `Linen.Data.PDF.Core.Exception`.

  `sure`/`message` are `IO`-returning, so their behaviour is checked with
  `#eval` (a mismatch or an unexpectedly-successful/failing action throws,
  which fails the build), following the pattern used elsewhere for `IO`-based
  ports (e.g. `Tests/Linen/Crypto/Zlib/FFITest.lean`). The pure `render`
  helper is checked with plain `#guard`s.
-/
import Linen.Data.PDF.Core.Exception

open Data.PDF.Core.Exception

namespace Tests.Data.PDF.Core.Exception

-- `render` includes the tag and message, with no parenthesised details when
-- `details` is empty.
#guard render ⟨.corrupted, "bad xref", []⟩ == "Corrupted: bad xref"

-- `render` appends any accumulated `details`, semicolon-separated.
#guard render ⟨.unexpected, "invariant broken", ["ctx1", "ctx2"]⟩ ==
  "Unexpected: invariant broken (ctx1; ctx2)"

-- `sure` on `.ok` just returns the value.
#eval show IO Unit from do
  let v ← sure (.ok (42 : Nat))
  unless v == 42 do
    throw (IO.userError s!"sure .ok mismatch: got {v}")

-- `sure` on `.error` throws a `corrupted`-rendered `userError`.
#eval show IO Unit from do
  let result ← try
      let _ ← sure (.error "boom" : Except String Nat)
      pure (some "no error")
    catch e =>
      match e with
      | .userError s => pure (if s == "Corrupted: boom" then none else some s)
      | _ => pure (some "wrong error kind")
  match result with
  | none => pure ()
  | some msg => throw (IO.userError s!"sure .error mismatch: {msg}")

-- `message` prepends context to a rethrown error.
#eval show IO Unit from do
  let action : IO Unit := throw (corrupted "inner failure")
  let result ← try
      message "outer context" action
      pure (some "no error")
    catch e =>
      match e with
      | .userError s =>
        pure (if s == "outer context: Corrupted: inner failure" then none else some s)
      | _ => pure (some "wrong error kind")
  match result with
  | none => pure ()
  | some msg => throw (IO.userError s!"message mismatch: {msg}")

-- `message` doesn't interfere with a successful action.
#eval show IO Unit from do
  let v ← message "ctx" (pure (7 : Nat))
  unless v == 7 do
    throw (IO.userError s!"message success-path mismatch: got {v}")

end Tests.Data.PDF.Core.Exception
