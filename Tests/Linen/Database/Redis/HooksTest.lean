/-
  Tests for `Linen.Database.Redis.Hooks`.
-/
import Linen.Database.Redis.Hooks

open Database.Redis.Hooks
open Database.Redis.Protocol (Reply)

private def expectEq [BEq α] (expected actual : α) : IO Unit :=
  if actual == expected then
    pure ()
  else
    throw (IO.userError "expectEq: values differ")

-- The default hooks are the identity function, so running a request
-- through `sendRequestHook` just runs the underlying action unchanged.
#eval show IO Unit from do
  let action : List ByteArray → IO Reply := fun _ => pure (Reply.integer 1)
  let r ← defaultHooks.sendRequestHook action ["PING".toUTF8]
  expectEq (Reply.integer 1) r

-- A hook can wrap the underlying action, e.g. to count invocations.
private def countingRequestHook (calls : IO.Ref Nat) : SendRequestHook :=
  fun action args => do
    calls.modify (· + 1)
    action args

#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let action : List ByteArray → IO Reply := fun _ => pure (Reply.singleLine "OK".toUTF8)
  let _ ← (countingRequestHook calls) action ["SET".toUTF8]
  let _ ← (countingRequestHook calls) action ["GET".toUTF8]
  let n ← calls.get
  expectEq 2 n

-- `sendHook` and `receiveHook` default to the identity function too.
#eval show IO Unit from do
  let sent ← IO.mkRef (ByteArray.mk #[])
  let baseline : ByteArray → IO Unit := fun bytes => sent.set bytes
  defaultHooks.sendHook baseline "PING\r\n".toUTF8
  let got ← sent.get
  expectEq "PING\r\n".toUTF8 got

#eval show IO Unit from do
  let r ← defaultHooks.receiveHook (pure (Reply.integer 7))
  expectEq (Reply.integer 7) r
