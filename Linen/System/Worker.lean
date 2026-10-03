/- Bounded reusable line-protocol processes. Requests are serialized, deadlines
   include pipe writes/reads, and failures destroy the entire process group. -/
import Linen.System.Process
import Std.Sync.Mutex

namespace System.Worker

abbrev stdio : IO.Process.StdioConfig := {stdin := .piped, stdout := .piped, stderr := .null}

/-- The caller owns the protocol/context; this object retains only a process. -/
structure Worker where
  private mk ::
  child : IO.Process.Child stdio
  lock : Std.Mutex Unit
  private alive : Std.Mutex Bool

/-- Private validated transport line: no embedded frame delimiter, bounded UTF-8. -/
structure Line where
  private mk ::
  value : String
  bounded : value.utf8ByteSize ≤ 64 * 1024 * 1024
  noDelimiter : value.contains '\n' = false

def Line.check (value : String) : Except String Line :=
  if hb : value.utf8ByteSize ≤ 64 * 1024 * 1024 then
    if hn : value.contains '\n' = false then .ok ⟨value,hb,hn⟩
    else .error "worker frame contains a newline"
  else .error "worker frame exceeds 64 MiB"

def spawn (cmd : String) (args : Array String := #[])
    (env : Array (String × Option String) := #[]) : IO Worker := do
  return {child := ← IO.Process.spawn {stdio with cmd,args,env,setsid := true}, lock := ← Std.Mutex.new (), alive := ← Std.Mutex.new true}

def Worker.stop (w : Worker) : IO Unit :=
  w.alive.atomically (m := IO) do
    if ← get then
      set false
      -- Never signal a cached PID after a naturally exited child was reaped.
      if (← w.child.tryWait).isNone then
        Process.killGroup w.child.pid
        try w.child.kill catch _ => pure ()
      let _ ← w.child.wait

private def readLine (h : IO.FS.Handle) : IO String := do
  let mut bytes := ByteArray.empty
  repeat
    -- Handle.read uses fread and may wait to fill a requested block on a live
    -- pipe. A byte read uses libc's buffering without waiting for another frame.
    let b ← h.read 1
    if b.isEmpty then throw (IO.userError "worker exited before its reply")
    if b[0]! == 10 then break
    unless bytes.size < 64 * 1024 * 1024 do throw (IO.userError "worker reply exceeds 64 MiB")
    bytes := bytes.push b[0]!
  match String.fromUTF8? bytes with
  | some value => return value
  | none => throw (IO.userError "worker reply is not UTF-8")

/-- Consume validated framing; stop/reap on timeout, malformed output or IO error.
    The caller must never reuse a failed worker for another request. -/
def Worker.call (w : Worker) (line : Line) (timeoutMs : Nat) : IO String :=
  w.lock.atomically (m := IO) do
    unless ← w.alive.atomically (m := IO) get do throw (IO.userError "worker is closed")
    let done ← IO.mkRef (none : Option (Except IO.Error String))
    let task ← IO.asTask (do
      let result ← (do
        w.child.stdin.putStrLn line.value
        w.child.stdin.flush
        readLine w.child.stdout).toBaseIO
      done.set (some result)) .dedicated
    let deadline := (← IO.monoMsNow) + timeoutMs
    repeat
      if let some answer ← done.get then
        let _ ← IO.wait task
        match answer with
        | .ok value => return value
        | .error error => w.stop; throw error
      if (← IO.monoMsNow) ≥ deadline then
        w.stop
        let _ ← IO.wait task
        throw (IO.userError "worker request timed out")
      IO.sleep 1

/-- Process identity for supervising/debugging a worker, without exposing pipes. -/
def Worker.pid (w : Worker) : UInt32 := w.child.pid

/-- A stopped worker cannot be returned to a cache. -/
def Worker.isAlive (w : Worker) : IO Bool := w.alive.atomically (m := IO) get

end System.Worker
