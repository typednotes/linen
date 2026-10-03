import Linen.System.Worker
open System.Worker
#guard (Line.check "{}").isOk
#guard (Line.check "{}\n{}").toOption.isNone
example (line : Line) : line.value.utf8ByteSize ≤ 64 * 1024 * 1024 := line.bounded
example (line : Line) : line.value.contains '\n' = false := line.noDelimiter

private def check (b : Bool) (message : String) : IO Unit :=
  unless b do throw (IO.userError message)

-- A single real process answers successive frames; stderr never corrupts stdout.
#eval show IO Unit from do
  let w ← spawn "sh" #["-c", "while IFS= read -r line; do printf '%s\\n' \"$line\"; printf diagnostic >&2; done"]
  try
    for value in ["first", "second", "Unicode: λ", "".pushn 'x' 10000] do
      let line ← IO.ofExcept ((Line.check value).mapError IO.userError)
      check ((← w.call line 10000) == value) "worker mixed up frames"
  finally w.stop
  check (!(← w.isAlive)) "stopped worker remained reusable"

-- Deadlines cover hung processes and their descendants, and prevent reuse.
#eval show IO Unit from do
  let w ← spawn "sh" #["-c", "sleep 30 & wait"]
  let line ← IO.ofExcept ((Line.check "request").mapError IO.userError)
  let start ← IO.monoMsNow
  let result ← (w.call line 100).toBaseIO
  check result.toOption.isNone "hung worker returned successfully"
  check ((← IO.monoMsNow) - start < 5000) "worker timeout failed to reap its group"
  check (!(← w.isAlive)) "timed-out worker remained reusable"

#eval show IO Unit from do
  let w ← spawn "printf" #["\\377\\n"]
  let line ← IO.ofExcept ((Line.check "request").mapError IO.userError)
  let result ← (w.call line 5000).toBaseIO
  check result.toOption.isNone "invalid UTF-8 reply was accepted"
  check (!(← w.isAlive)) "malformed worker remained reusable"
