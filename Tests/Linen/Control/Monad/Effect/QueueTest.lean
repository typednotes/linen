/-
  Tests for `Control.Monad.Effect.Queue`.

  The distinctions worth pinning here are the ones a type-level effect row
  cannot draw: publish-but-never-consume, consume-but-never-purge, and read
  one queue while writing another.
-/
import Linen.Control.Monad.Effect.Queue

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.Queue

namespace Tests.Control.Monad.Effect.Queue

-- ── Capabilities ────────────────────────────────────────────────────────────

#guard (producer "jobs").consistent
#guard (consumer "jobs").consistent
#guard (pipeline "inbox" "outbox").consistent

example : (producer "jobs").canSend = true := rfl
example : (producer "jobs").canReceive = false := rfl
example : (consumer "jobs").canPurge = false := rfl

example : CanSend (producer "jobs") := inferInstance
example : CanReceive (consumer "jobs") := inferInstance
example : CanAck (consumer "jobs") := inferInstance
example : CanExtend (consumer "jobs") := inferInstance

/- `scopes := []` grants nothing, as in `Effect.ObjectStore`. -/
example : ({ canSend := true } : Capability).permits .send "anything" = false := rfl

-- ── Permitted calls elaborate ───────────────────────────────────────────────

example : Eff [Queue (producer "jobs")] (Except Cloud.Error (List String)) :=
  sendOne "jobs" "work item"

example : Eff [Queue (consumer "jobs")] (Except Cloud.Error (List Cloud.Message)) :=
  receive "jobs"

example : Eff [Queue (consumer "jobs")] (Except Cloud.Error Unit) :=
  ack "jobs" [⟨"rcpt"⟩]

example : Eff [Queue (pipeline "inbox" "outbox")] (Except Cloud.Error Unit) := do
  let _ ← receive "inbox"
  let _ ← sendOne "outbox" "result"
  return .ok ()

-- ── Withheld permissions do not elaborate ───────────────────────────────────

/- **A producer cannot drain the queue it writes to.** This is the split that
   keeps a publisher from consuming its own work, and it is invisible to a row
   that can only say "may use a queue". -/
/--
error: failed to synthesize instance of type class
  CanReceive (producer "jobs")

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  (producer "jobs").permits Op.receive "jobs" = true
is false
-/
#guard_msgs in
#check (receive (effs := [Queue (producer "jobs")]) "jobs")

/- **A consumer cannot purge.** `purge` is the destructive operation and no
   convenience capability grants it — a worker that can discard the backlog it
   was only meant to read is a whole class of incident. -/
/--
error: failed to synthesize instance of type class
  CanPurge (consumer "jobs")

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  (consumer "jobs").permits Op.purge "jobs" = true
is false
-/
#guard_msgs in
#check (purge (effs := [Queue (consumer "jobs")]) "jobs")

#guard (sendReceive "jobs").canPurge == false

-- ── Scope escapes, as theorems ──────────────────────────────────────────────

/-- Another queue is not covered, however similar its name. -/
theorem no_other_queue :
    (consumer "jobs").permits .receive "jobs-retry" ≠ true := by decide

/-- Queue names are matched by equality, so a prefix is not a wildcard — there
    is no way to accidentally cover a queue created next week. -/
theorem no_prefix_wildcard :
    (consumer "jobs").permits .receive "jobs2" ≠ true := by decide

/-- **A pipeline reads its inbox and writes its outbox, and not the reverse.**
    The restriction is on (operation, queue) *pairs*, which is what lets one
    capability describe a worker without letting it re-read its own output. -/
theorem pipeline_cannot_read_outbox :
    (pipeline "inbox" "outbox").permits .receive "outbox" ≠ true := by decide

theorem pipeline_cannot_write_inbox :
    (pipeline "inbox" "outbox").permits .send "inbox" ≠ true := by decide

theorem pipeline_reads_inbox :
    (pipeline "inbox" "outbox").permits .receive "inbox" = true := by decide

theorem pipeline_writes_outbox :
    (pipeline "inbox" "outbox").permits .send "outbox" = true := by decide

-- ── Unions ──────────────────────────────────────────────────────────────────

instance : CanSend ((producer "a").union (consumer "b")) := .of
instance : CanReceive ((producer "a").union (consumer "b")) := .of

example : ((producer "a").union (consumer "b")).permits .send "a" = true :=
  permits_union_left (by decide)

example : ((producer "a").union (consumer "b")).permits .receive "b" = true :=
  permits_union_right (by decide)

/-- A union still grants nothing on a third queue. -/
theorem union_grants_no_third_queue :
    ((producer "a").union (consumer "b")).permits .send "c" ≠ true := by decide

-- ── Runtime-validated names ─────────────────────────────────────────────────

#guard (ScopedQueue.check? (consumer "jobs") .receive "jobs").isSome
#guard (ScopedQueue.check? (consumer "jobs") .purge "jobs").isNone
#guard (ScopedQueue.check? (consumer "jobs") .receive "other").isNone

-- ── End to end, purely ──────────────────────────────────────────────────────

def worker : Eff [Queue (pipeline "inbox" "outbox")] (Except Cloud.Error Nat) := do
  let _ ← receive "inbox" { maxMessages := 5 }
  let _ ← sendOne "outbox" "done"
  let _ ← ack "inbox" [⟨"rcpt-1"⟩]
  return .ok 0

/-- info: ["receive inbox", "send outbox (1)", "ack inbox (1)"] -/
#guard_msgs in
#eval (dryRun worker).2

-- ── End to end, against a backend ───────────────────────────────────────────

/- The same program runs against any backend; here the in-memory one. -/
/-- info: (some "work item", 0, 0) -/
#guard_msgs in
#eval show IO (Option String × Nat × Nat) from do
  let (q, depth) ← Cloud.Queue.inMemoryInspectable
  let program : Eff [Queue (sendReceive "jobs")]
      (Except Cloud.Error (Option String)) := do
    let _ ← sendOne "jobs" "work item"
    match ← receive "jobs" { maxMessages := 1 } with
    | .error e => return .error e
    | .ok msgs =>
      match msgs.head? with
      | none   => return .ok none
      | some m => do
        let _ ← ack "jobs" [m.receipt]
        return .ok (some m.body)
  let result ← runQueue (sendReceive "jobs") q program
  let (waiting, inFlight) ← depth
  return ((result.toOption).getD none, waiting, inFlight)

/- The handler checks nothing, so what remains is provider behaviour: an
   in-memory queue with nothing in it answers empty, and that is not an
   error. -/
/-- info: 0 -/
#guard_msgs in
#eval show IO Nat from do
  let q ← Cloud.Queue.inMemory
  let program : Eff [Queue (consumer "jobs")] (Except Cloud.Error Nat) := do
    match ← receive "jobs" { maxMessages := 10 } with
    | .error e => return .error e
    | .ok msgs => return .ok msgs.length
  match ← runQueue (consumer "jobs") q program with
  | .ok n => return n
  | .error _ => return 99

end Tests.Control.Monad.Effect.Queue
