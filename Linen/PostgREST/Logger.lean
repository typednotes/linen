/-
  PostgREST.Logger — Structured logging

  Logging facilities for PostgREST, using the application's observer
  pattern and standard IO output.

  ## Haskell source
  - `PostgREST.Logger` (postgrest package)
-/

namespace PostgREST.Logger

/-- Log levels matching PostgREST convention. -/
inductive LogLevel where
  | crit | error | warn | info | debug
  deriving BEq, Repr, Inhabited

instance : Ord LogLevel where
  compare a b := compare (toNat a) (toNat b)
where
  toNat : LogLevel → Nat
    | .crit => 0 | .error => 1 | .warn => 2 | .info => 3 | .debug => 4

instance : ToString LogLevel where
  toString
    | .crit => "CRT" | .error => "ERR" | .warn => "WRN"
    | .info => "INF" | .debug => "DBG"

/-- Log a message at the given level. -/
def log (level : LogLevel) (configLevel : LogLevel) (msg : String) : IO Unit := do
  if compare level configLevel != .gt then
    let timestamp ← IO.monoMsNow
    IO.eprintln s!"[{level}] {timestamp} {msg}"

/-- Log a critical message. -/
def logCrit (configLevel : LogLevel) (msg : String) : IO Unit :=
  log .crit configLevel msg

/-- Log an error message. -/
def logError (configLevel : LogLevel) (msg : String) : IO Unit :=
  log .error configLevel msg

/-- Log a warning. -/
def logWarn (configLevel : LogLevel) (msg : String) : IO Unit :=
  log .warn configLevel msg

/-- Log an info message. -/
def logInfo (configLevel : LogLevel) (msg : String) : IO Unit :=
  log .info configLevel msg

/-- Log a debug message. -/
def logDebug (configLevel : LogLevel) (msg : String) : IO Unit :=
  log .debug configLevel msg

end PostgREST.Logger
