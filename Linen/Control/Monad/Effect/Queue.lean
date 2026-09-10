/-
  `Control.Monad.Effect.Queue` — a capability-restricted message queue

  ## Not a Haskell port

  `linen`-original, the fifth instance of the pattern `Effect.FileSystem`
  established. See `docs/imports/FreerSimple/dependencies.md`.

  ## What this adds over the row

  A row can say "may use a queue". It cannot say *which* queue, nor "may
  publish but never consume", nor "may consume but never purge" — and those are
  the distinctions that matter. A worker that can drain a queue it was only
  meant to read from is a whole class of incident.

  Here the capability is a value, so

      send to jobs
      receive and acknowledge from jobs-retry
      purge nothing

  is one capability, and `purge "jobs"` under it does not elaborate.

  ## Names are flat, and that is not a simplification

  Unlike `Effect.ObjectStore`'s keys, a queue name has no structure to scope by
  on any of the three clouds: SQS names are flat, and Pub/Sub's short topic and
  subscription names likewise. So `Scope.queue` is a `String` matched by
  equality, and there is no prefix to get subtly wrong. String *equality* does
  reduce under `decide`; it is a fold of them over a long list that does not,
  so keep scope lists short.

  ## `canAck` is separate, but do not withhold it alone

  Acknowledgement has its own bit because it is a distinct authority — a
  consumer that can read but not acknowledge cannot destroy anything. It is
  also, in isolation, a mistake: such a consumer receives the same messages
  forever and never makes progress, turning a queue into an infinite loop. So
  `consumer` grants receive, acknowledge and extend together, and the split
  exists for the auditor rather than for daily use.

  `canPurge` is the genuinely destructive one and is never included in a
  convenience capability.

  ## What the handler does *not* do

  It does not check anything: the proofs are carried by the constructors. It
  also does not paper over the clouds' differences — `purge` on Pub/Sub answers
  `unsupported` from the backend, as `Cloud.Provider.supports` records, and no
  amount of static permission changes that. A capability grants what a
  *program* may attempt, not what a *provider* implements.
-/
import Linen.Control.Monad.Effect
import Linen.Cloud.Queue

namespace Control.Monad.Effect.Queue

open Data.OpenUnion Control.Monad.Effect

-- ── Operations ──────────────────────────────────────────────────────────────

/-- The kind of thing a request does to a queue. -/
inductive Op
  /-- Publish a message. -/
  | send
  /-- Read messages, taking a lease on them. -/
  | receive
  /-- Acknowledge messages, removing them permanently. -/
  | ack
  /-- Change how long a lease has left, including releasing it. -/
  | extend
  /-- Discard every message. The destructive one. -/
  | purge
  deriving DecidableEq, BEq, Repr

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- One queue a capability opens up, and what may be done to it.

    `ops := []` means every operation the capability's own bits allow. -/
structure Scope where
  /-- Operations allowed on this queue; `[]` means every one the capability
      has. -/
  ops   : List Op := []
  /-- The queue's name, matched exactly. Flat on all three clouds, so there is
      no prefix and no wildcard. -/
  queue : String
  deriving DecidableEq, Repr

/-- What a computation is permitted to do to which queues.

    As in `Effect.ObjectStore` and unlike `Effect.FileSystem`, **`scopes := []`
    permits nothing**: a queue capability always names its queues. -/
structure Capability where
  /-- May publish. -/
  canSend    : Bool := false
  /-- May read messages. -/
  canReceive : Bool := false
  /-- May acknowledge messages. -/
  canAck     : Bool := false
  /-- May change a lease's remaining time, including releasing it. -/
  canExtend  : Bool := false
  /-- May discard every message. -/
  canPurge   : Bool := false
  /-- The queues this capability opens up. Empty grants nothing. -/
  scopes     : List Scope := []
  deriving DecidableEq, Repr

/-- Does this capability's global permission set include `op`? -/
def Capability.allows (cap : Capability) : Op → Bool
  | .send    => cap.canSend
  | .receive => cap.canReceive
  | .ack     => cap.canAck
  | .extend  => cap.canExtend
  | .purge   => cap.canPurge

/-- Does `s` cover `op` on `queue`? -/
def Scope.covers (s : Scope) (op : Op) (queue : String) : Bool :=
  (s.ops.isEmpty || s.ops.contains op) && s.queue == queue

/-- Does this capability allow `op` on `queue`? -/
def Capability.permits (cap : Capability) (op : Op) (queue : String) : Bool :=
  cap.scopes.any (fun s => s.covers op queue)

/-- `cap` grants publishing. -/
class CanSend (cap : Capability) : Prop where
  /-- Evidence that the send bit is set. -/
  proof : cap.canSend = true

/-- `cap` grants reading. -/
class CanReceive (cap : Capability) : Prop where
  /-- Evidence that the receive bit is set. -/
  proof : cap.canReceive = true

/-- `cap` grants acknowledgement. -/
class CanAck (cap : Capability) : Prop where
  /-- Evidence that the acknowledge bit is set. -/
  proof : cap.canAck = true

/-- `cap` grants lease changes. -/
class CanExtend (cap : Capability) : Prop where
  /-- Evidence that the extend bit is set. -/
  proof : cap.canExtend = true

/-- `cap` grants purging. -/
class CanPurge (cap : Capability) : Prop where
  /-- Evidence that the purge bit is set. -/
  proof : cap.canPurge = true

instance instCanSend {r a e p : Bool} {ss : List Scope} :
    CanSend ⟨true, r, a, e, p, ss⟩ := ⟨rfl⟩
instance instCanReceive {s a e p : Bool} {ss : List Scope} :
    CanReceive ⟨s, true, a, e, p, ss⟩ := ⟨rfl⟩
instance instCanAck {s r e p : Bool} {ss : List Scope} :
    CanAck ⟨s, r, true, e, p, ss⟩ := ⟨rfl⟩
instance instCanExtend {s r a p : Bool} {ss : List Scope} :
    CanExtend ⟨s, r, a, true, p, ss⟩ := ⟨rfl⟩
instance instCanPurge {s r a e : Bool} {ss : List Scope} :
    CanPurge ⟨s, r, a, e, true, ss⟩ := ⟨rfl⟩

/-- Permission evidence for a capability whose bits are computed — a
    `Capability.union`, say — since instance resolution matches bits
    syntactically. -/
theorem CanSend.of {cap : Capability} (h : cap.canSend = true := by decide) :
    CanSend cap := ⟨h⟩

/-- `CanSend.of` for reading. -/
theorem CanReceive.of {cap : Capability} (h : cap.canReceive = true := by decide) :
    CanReceive cap := ⟨h⟩

/-- `CanSend.of` for acknowledgement. -/
theorem CanAck.of {cap : Capability} (h : cap.canAck = true := by decide) :
    CanAck cap := ⟨h⟩

/-- `CanSend.of` for lease changes. -/
theorem CanExtend.of {cap : Capability} (h : cap.canExtend = true := by decide) :
    CanExtend cap := ⟨h⟩

/-- `CanSend.of` for purging. -/
theorem CanPurge.of {cap : Capability} (h : cap.canPurge = true := by decide) :
    CanPurge cap := ⟨h⟩

-- ── Building capabilities ───────────────────────────────────────────────────

/-- The scope covering `queue`, for `ops` (or for every operation the
    capability grants, when `ops` is empty). -/
abbrev on (queue : String) (ops : List Op := []) : Scope := { ops, queue }

/-- Do the global bits cover every operation the scopes name? Assert
    `#guard cap.consistent` beside a capability definition. -/
def Capability.consistent (cap : Capability) : Bool :=
  cap.scopes.all (fun s => s.ops.all cap.allows)

/-- Combine two capabilities: everything either one allows. -/
def Capability.union (a b : Capability) : Capability :=
  { canSend    := a.canSend    || b.canSend
  , canReceive := a.canReceive || b.canReceive
  , canAck     := a.canAck     || b.canAck
  , canExtend  := a.canExtend  || b.canExtend
  , canPurge   := a.canPurge   || b.canPurge
  , scopes     := a.scopes ++ b.scopes }

/-- A union grants everything its left operand granted. -/
theorem allows_union_left {a b : Capability} {op : Op} (h : a.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union grants everything its right operand granted. -/
theorem allows_union_right {a b : Capability} {op : Op} (h : b.allows op = true) :
    (a.union b).allows op = true := by
  cases op <;> simp_all [Capability.allows, Capability.union]

/-- A union admits every (operation, queue) pair its left operand admitted. -/
theorem permits_union_left {a b : Capability} {op : Op} {q : String}
    (h : a.permits op q = true) : (a.union b).permits op q = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

/-- A union admits every pair its right operand admitted. -/
theorem permits_union_right {a b : Capability} {op : Op} {q : String}
    (h : b.permits op q = true) : (a.union b).permits op q = true := by
  simp only [Capability.permits, Capability.union, List.any_append]
  simp only [Capability.permits] at h
  simp [h]

-- ── Scoped queues, for names not known statically ───────────────────────────

/-- A queue name together with a proof that `cap` allows `op` on it.

    For names known at compile time the obligation is discharged by `decide`
    and this type is not needed; it exists for a name read from
    configuration. -/
structure ScopedQueue (cap : Capability) (op : Op) where
  /-- The queue's name. -/
  queue   : String
  /-- Evidence that `cap` permits `op` on it. -/
  inScope : cap.permits op queue = true

/-- Validate a runtime queue name against `cap` for one operation. -/
def ScopedQueue.check? (cap : Capability) (op : Op) (queue : String) :
    Option (ScopedQueue cap op) :=
  if h : cap.permits op queue = true then some ⟨queue, h⟩ else none

-- ── The effect ──────────────────────────────────────────────────────────────

/-- Queue operations available under the capability `cap`. -/
inductive Queue (cap : Capability) : Type → Type where
  /-- Publish messages. -/
  | send    (hp : cap.canSend = true) (queue : String)
      (hs : cap.permits .send queue = true) (msgs : List Cloud.OutgoingMessage) :
      Queue cap (Except Cloud.Error (List String))
  /-- Receive messages. -/
  | receive (hp : cap.canReceive = true) (queue : String)
      (hs : cap.permits .receive queue = true) (params : Cloud.ReceiveParams) :
      Queue cap (Except Cloud.Error (List Cloud.Message))
  /-- Acknowledge messages. -/
  | ack     (hp : cap.canAck = true) (queue : String)
      (hs : cap.permits .ack queue = true) (receipts : List Cloud.Receipt) :
      Queue cap (Except Cloud.Error Unit)
  /-- Change how long leases have left. -/
  | extend  (hp : cap.canExtend = true) (queue : String)
      (hs : cap.permits .extend queue = true) (receipts : List Cloud.Receipt)
      (seconds : Nat) : Queue cap (Except Cloud.Error Unit)
  /-- Discard every message. -/
  | purge   (hp : cap.canPurge = true) (queue : String)
      (hs : cap.permits .purge queue = true) : Queue cap (Except Cloud.Error Unit)

/-- Locates a `Queue` effect in the row and recovers which capability it
    carries. `cap` is an `outParam`, which is what keeps the obligations
    solvable inside `do`-notation. -/
class HasQueue (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject a queue request into the row. -/
  inject : {α : Type} → Queue cap α → Union effs α

/-- The queue effect is the row's head. -/
instance instHasQueueHere {cap : Capability} {effs : List (Type → Type)} :
    HasQueue (Queue cap :: effs) cap where
  inject e := .here e

/-- The queue effect is somewhere in the row's tail. -/
instance instHasQueueThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasQueue effs cap] :
    HasQueue (eff :: effs) cap where
  inject e := .there (HasQueue.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Publish messages to a queue. -/
def send {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanSend cap] (queue : String)
    (msgs : List Cloud.OutgoingMessage)
    (hs : cap.permits .send queue = true := by decide) :
    Eff effs (Except Cloud.Error (List String)) :=
  .impure (h.inject (.send perm.proof queue hs msgs)) .protect

/-- Publish one message. -/
def sendOne {effs : List (Type → Type)} {cap : Capability}
    [HasQueue effs cap] [CanSend cap] (queue body : String)
    (attributes : List (String × String) := [])
    (hs : cap.permits .send queue = true := by decide) :
    Eff effs (Except Cloud.Error (List String)) :=
  send queue [{ body, attributes }] hs

/-- Receive messages from a queue.

    Under a send-only capability this does not elaborate — the split that keeps
    a publisher from draining the queue it writes to. -/
def receive {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanReceive cap] (queue : String)
    (params : Cloud.ReceiveParams := {})
    (hs : cap.permits .receive queue = true := by decide) :
    Eff effs (Except Cloud.Error (List Cloud.Message)) :=
  .impure (h.inject (.receive perm.proof queue hs params)) .protect

/-- Acknowledge messages, removing them permanently. -/
def ack {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanAck cap] (queue : String)
    (receipts : List Cloud.Receipt)
    (hs : cap.permits .ack queue = true := by decide) :
    Eff effs (Except Cloud.Error Unit) :=
  .impure (h.inject (.ack perm.proof queue hs receipts)) .protect

/-- Change how long leases have left. `0` releases them. -/
def extend {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanExtend cap] (queue : String)
    (receipts : List Cloud.Receipt) (seconds : Nat)
    (hs : cap.permits .extend queue = true := by decide) :
    Eff effs (Except Cloud.Error Unit) :=
  .impure (h.inject (.extend perm.proof queue hs receipts seconds)) .protect

/-- Release messages back to the queue for another consumer. -/
def release {effs : List (Type → Type)} {cap : Capability}
    [HasQueue effs cap] [CanExtend cap] (queue : String)
    (receipts : List Cloud.Receipt)
    (hs : cap.permits .extend queue = true := by decide) :
    Eff effs (Except Cloud.Error Unit) :=
  extend queue receipts 0 hs

/-- Discard every message in a queue.

    The destructive operation, and the one no convenience capability grants:
    `purge` under `consumer` or `producer` does not elaborate. -/
def purge {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanPurge cap] (queue : String)
    (hs : cap.permits .purge queue = true := by decide) :
    Eff effs (Except Cloud.Error Unit) :=
  .impure (h.inject (.purge perm.proof queue hs)) .protect

-- ── Operations on runtime-validated names ───────────────────────────────────

/-- Publish to a queue named at runtime. -/
def sendAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanSend cap] (sq : ScopedQueue cap .send)
    (msgs : List Cloud.OutgoingMessage) :
    Eff effs (Except Cloud.Error (List String)) :=
  .impure (h.inject (.send perm.proof sq.queue sq.inScope msgs)) .protect

/-- Receive from a queue named at runtime. -/
def receiveAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasQueue effs cap] [perm : CanReceive cap] (sq : ScopedQueue cap .receive)
    (params : Cloud.ReceiveParams := {}) :
    Eff effs (Except Cloud.Error (List Cloud.Message)) :=
  .impure (h.inject (.receive perm.proof sq.queue sq.inScope params)) .protect

-- ── Handler ─────────────────────────────────────────────────────────────────

/-- Run a queue computation against a backend.

    `backend` maps a queue name to a `Cloud.Queue`, because a capability may
    span several. Note that the backend still decides what the *provider*
    supports — `purge` on Pub/Sub answers `unsupported` however the capability
    is written. A capability governs what a program may attempt. -/
def runQueueWith (cap : Capability) {α : Type} (backend : String → Cloud.Queue) :
    Eff [Queue cap] α → IO α :=
  interpretM fun
    | .send _ q _ msgs        => (backend q).producer.send msgs
    | .receive _ q _ params   => (backend q).consumer.receive params
    | .ack _ q _ receipts     => (backend q).consumer.ack receipts
    | .extend _ q _ rs secs   => (backend q).consumer.extendLease rs secs
    | .purge _ q _            => (backend q).consumer.purge

/-- Run against a single queue. -/
def runQueue (cap : Capability) {α : Type} (queue : Cloud.Queue) :
    Eff [Queue cap] α → IO α :=
  runQueueWith cap (fun _ => queue)

/-- Interpret purely, into the operations the computation would perform.

    No `IO`, so a test asserts the request sequence with `#guard`. Receives
    answer empty, which is the interesting case for a consumer loop. -/
def dryRun {cap : Capability} {α : Type} : Eff [Queue cap] α → α × List String :=
  go []
where
  /-- The accumulator carries the operations seen so far, most recent first. -/
  go (acc : List String) : Eff [Queue cap] α → α × List String
    | .protect a  => (a, acc.reverse)
    | .impure u k => match u with
      | .here e => match e with
        | .send _ q _ msgs =>
            go (s!"send {q} ({msgs.length})" :: acc)
              (k (.ok (msgs.zipIdx.map (fun (_, i) => s!"msg-{i}"))))
        | .receive _ q _ _ => go (s!"receive {q}" :: acc) (k (.ok []))
        | .ack _ q _ rs    => go (s!"ack {q} ({rs.length})" :: acc) (k (.ok ()))
        | .extend _ q _ rs secs =>
            go (s!"extend {q} ({rs.length}) to {secs}s" :: acc) (k (.ok ()))
        | .purge _ q _     => go (s!"purge {q}" :: acc) (k (.ok ()))
      | .there u' => u'.elim0

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Publish only. Cannot read the queue it writes to, which is the split that
    keeps a producer from draining its own work. -/
abbrev producer (queue : String) : Capability :=
  { canSend := true, scopes := [on queue [.send]] }

/-- Receive, acknowledge and extend — a working consumer.

    All three together deliberately: a consumer that can receive but not
    acknowledge sees the same messages forever, so granting `receive` alone is
    a mistake rather than a restriction. Does **not** include `purge`. -/
abbrev consumer (queue : String) : Capability :=
  { canReceive := true, canAck := true, canExtend := true
  , scopes := [on queue [.receive, .ack, .extend]] }

/-- Publish and consume one queue. Still no `purge`. -/
abbrev sendReceive (queue : String) : Capability :=
  { canSend := true, canReceive := true, canAck := true, canExtend := true
  , scopes := [on queue] }

/-- Publish to one queue and consume another — the shape of a worker that
    reads work and reports results, and which must not read its own output. -/
abbrev pipeline (inbox outbox : String) : Capability :=
  { canSend := true, canReceive := true, canAck := true, canExtend := true
  , scopes := [ on inbox [.receive, .ack, .extend], on outbox [.send] ] }

end Control.Monad.Effect.Queue
