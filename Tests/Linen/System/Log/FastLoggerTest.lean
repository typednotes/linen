/-
  Tests for `Linen.System.Log.FastLogger`.

  The logger is IO; behaviour is checked with `#eval` (a thrown error fails the
  build) by routing output through a `callback` that captures the flushed bytes
  into an `IO.Ref`.
-/
import Linen.System.Log.FastLogger

open System.Log.FastLogger

namespace Tests.System.Log.FastLogger

/-! ### explicit flush combines buffered messages -/

#eval show IO Unit from do
  let captured ← IO.mkRef (#[] : Array ByteArray)
  let logger ← newLoggerSet (.callback fun b => captured.modify (·.push b))
  pushLogStr logger "a"
  pushLogStr logger "b"
  -- below the default 4096 buffer: nothing flushed yet
  unless (← captured.get).isEmpty do throw (IO.userError "should not have flushed before the buffer fills")
  flushLogStr logger
  let out ← captured.get
  unless out.size == 1 do throw (IO.userError s!"expected one flush, got {out.size}")
  unless out[0]!.toList == "ab".toUTF8.toList do throw (IO.userError "flush should concatenate buffered messages")
  -- a second flush with an empty buffer does nothing
  flushLogStr logger
  unless (← captured.get).size == 1 do throw (IO.userError "flushing an empty buffer should be a no-op")

/-! ### auto-flush when the buffer fills -/

#eval show IO Unit from do
  let captured ← IO.mkRef (#[] : Array ByteArray)
  let logger ← newLoggerSet (.callback fun b => captured.modify (·.push b)) (bufSize := 2)
  pushLogStr logger "x"
  unless (← captured.get).isEmpty do throw (IO.userError "size 1 < 2: no flush yet")
  pushLogStr logger "y"     -- buffer reaches 2 ⇒ auto-flush
  let out ← captured.get
  unless out.size == 1 do throw (IO.userError "should have auto-flushed at the buffer limit")
  unless out[0]!.toList == "xy".toUTF8.toList do throw (IO.userError "auto-flush should emit xy")

/-! ### withFastLogger flushes on exit -/

#eval show IO Unit from do
  let captured ← IO.mkRef (#[] : Array ByteArray)
  withFastLogger (.callback fun b => captured.modify (·.push b)) fun lg => do
    pushLogStr lg "hello"
  -- rmLoggerSet ran on exit, flushing the remaining message
  let out ← captured.get
  unless out.size == 1 do throw (IO.userError "withFastLogger should flush on exit")
  unless out[0]!.toList == "hello".toUTF8.toList do throw (IO.userError "expected 'hello'")

end Tests.System.Log.FastLogger
