/-
  Linen.System.Log.FastLogger — buffered thread-safe logger

  Logs are buffered in a `Std.Mutex`-protected array and flushed on a full
  buffer or on close, to stdout/stderr/a file/or a callback. Mirrors Haskell's
  `System.Log.FastLogger` (the `LoggerSet` handle is opaque — its constructor is
  private, so it can only be built via `newLoggerSet`).
-/

import Std.Sync.Mutex

namespace System.Log.FastLogger

/-- Where log output goes. -/
inductive LogType where
  | stdout
  | stderr
  | file (path : String)
  | callback (write : ByteArray → IO Unit)

/-- A log message. -/
abbrev LogStr := String

/-- An opaque handle to a running logger. -/
structure LoggerSet where
  private mk ::
  /-- The mutex-protected buffer. -/
  buffer : Std.Mutex (Array String)
  /-- Writes buffered content to the destination. -/
  flushAction : Array String → IO Unit
  /-- Maximum buffer size before an automatic flush. -/
  bufSize : Nat

/-- Flush all buffered messages to the destination. -/
def flushLogStr (logger : LoggerSet) : IO Unit := do
  let msgs ← logger.buffer.atomically do
    let current ← get
    set (#[] : Array String)
    return current
  unless msgs.isEmpty do
    logger.flushAction msgs

/-- Create a `LoggerSet` writing to `logType`. -/
def newLoggerSet (logType : LogType) (bufSize : Nat := 4096) : IO LoggerSet := do
  let flushAction : Array String → IO Unit := fun msgs => do
    let combined := String.join msgs.toList
    match logType with
    | .stdout         => IO.print combined
    | .stderr         => IO.eprint combined
    | .file path      => (← IO.FS.Handle.mk path .append).write combined.toUTF8
    | .callback write => write combined.toUTF8
  return ⟨← Std.Mutex.new (#[] : Array String), flushAction, bufSize⟩

/-- Append a message, auto-flushing if the buffer reaches `bufSize`. -/
def pushLogStr (logger : LoggerSet) (msg : LogStr) : IO Unit := do
  let shouldFlush ← logger.buffer.atomically do
    modify (· ++ #[msg])
    return decide ((← get).size ≥ logger.bufSize)
  if shouldFlush then
    flushLogStr logger

/-- Close the logger, flushing remaining messages. -/
def rmLoggerSet (logger : LoggerSet) : IO Unit :=
  flushLogStr logger

/-- Run `action` with a fresh logger, flushing it on exit (even on error). -/
def withFastLogger (logType : LogType) (action : LoggerSet → IO α) : IO α := do
  let logger ← newLoggerSet logType
  try action logger finally rmLoggerSet logger

end System.Log.FastLogger
