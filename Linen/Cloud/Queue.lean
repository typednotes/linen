/-
  `Cloud.Queue` — one message-queue interface over two different ideas

  ## The seam this module is built around

  SQS — on AWS, and on Scaleway, which is SQS-compatible — is a **queue**: one
  name, you send to it and you receive from it, and a received message carries a
  *receipt handle* you later use to delete it.

  Google Pub/Sub is not a queue. It is a **topic** you publish to, plus one or
  more **subscriptions** you pull from, and a pulled message carries an *ack
  id*. A topic with no subscription silently discards everything published to
  it. The two models are genuinely different, not two spellings of one thing.

  A portable interface has to choose how to be honest about that. The choice
  here is to **split the record in two**:

  - `Producer` — where messages go in. An SQS queue, or a Pub/Sub topic.
  - `Consumer` — where messages come out. The same SQS queue, or a Pub/Sub
    *subscription*.

  So a program that only publishes takes a `Producer` and never has to name a
  subscription; and on Pub/Sub a `Consumer` **cannot be constructed** without
  naming one, because `Consumer.pubSub` asks for it. Nothing raises, nothing
  returns `unsupported`, and no caller discovers at runtime that the queue it
  was handed cannot be read from.

  This is the same move `Control.Monad.Effect.PostgreSQL` makes with its
  connection target: put the thing in the type, and there is no obligation left
  to check because no value could name an alternative. The alternative — one
  `Queue` record whose `receive` fails on Pub/Sub unless a subscription was
  configured — pushes a static fact into a runtime failure for no gain.

  ## What does not survive the crossing

  Two fields are honoured on SQS and **ignored on Pub/Sub**, and say so in
  their own doc-comments rather than being quietly dropped:

  - `ReceiveParams.waitSeconds` — SQS's long poll. Pub/Sub's pull blocks by its
    own rules and takes no such parameter.
  - `ReceiveParams.leaseSeconds` — SQS's per-request visibility timeout. On
    Pub/Sub the ack deadline is a property of the *subscription*, so there is
    nothing to set per pull.

  `Cloud.Provider.supports` records both, and `purge` besides.

  Deliberately out of scope for the portable interface, and reachable from the
  provider modules: FIFO ordering and message groups, dead-letter policy,
  delivery delay, and Pub/Sub ordering keys.

  ## Receipts are opaque and short-lived

  A `Receipt` wraps an SQS receipt handle or a Pub/Sub ack id. It is valid only
  for the receive that produced it, and only against the `Consumer` that issued
  it — a receipt is not a message identifier and cannot be stored and used
  later. `Message.id` is the identifier.

  ## Provenance

  New code. The sibling `typednotes/infra` manages queues but sends no
  messages: its SQS client has no `SendMessage`/`ReceiveMessage`, and its
  Pub/Sub client manages **topics only**, with no subscription support at all.
  So the endpoints came from there and every operation here is new.
-/
import Linen.Cloud.Transport

namespace Cloud

-- ── Messages ────────────────────────────────────────────────────────────────

/-- A message to publish. -/
structure OutgoingMessage where
  /-- The payload. Text rather than bytes: SQS's message body is a string, and
      Pub/Sub base64-encodes bytes but is universally used for text. A caller
      with binary data should encode it itself, so that the encoding is visible
      rather than assumed. -/
  body       : String
  /-- Key-value metadata travelling beside the body — SQS message attributes,
      Pub/Sub attributes. String-valued only: SQS also has binary and numeric
      attribute types, which Pub/Sub has no equivalent for. -/
  attributes : List (String × String) := []
  deriving Repr, DecidableEq, Inhabited

/-- A lease on one received message.

    Opaque: an SQS receipt handle, or a Pub/Sub ack id. Valid only for the
    receive that produced it and only against the `Consumer` that issued it —
    **not** a message identifier, and not storable for later use. -/
structure Receipt where
  /-- The provider's handle, exactly as sent. -/
  handle : String
  deriving Repr, DecidableEq, BEq, Inhabited

/-- A received message. -/
structure Message where
  /-- The provider's message identifier. Stable across redeliveries on both
      systems, and the thing to log or deduplicate on — unlike `receipt`. -/
  id           : String
  /-- The payload. -/
  body         : String
  /-- The lease to `ack` with. -/
  receipt      : Receipt
  /-- Metadata sent with the message. -/
  attributes   : List (String × String) := []
  /-- How many times this message has been delivered, when the provider says.
      SQS reports it as `ApproximateReceiveCount`; Pub/Sub as
      `deliveryAttempt`, and only when a dead-letter policy is configured. The
      number to watch for a poison message. -/
  receiveCount : Option Nat := none
  deriving Repr, DecidableEq, Inhabited

/-- How to receive. -/
structure ReceiveParams where
  /-- How many messages to ask for. Both systems treat this as a maximum and
      may return fewer, **including zero when messages exist** — neither
      guarantees a non-empty result, so a consumer loop must tolerate an empty
      answer rather than treating it as "the queue is empty". -/
  maxMessages  : Nat := 1
  /-- How long to wait for a message before answering empty — SQS's long poll.

      **Ignored on Pub/Sub**, whose pull blocks by its own rules and takes no
      such parameter. See `Provider.supports .queueLongPoll`. -/
  waitSeconds  : Option Nat := none
  /-- How long this receive's messages stay invisible to other consumers —
      SQS's per-request visibility timeout.

      **Ignored on Pub/Sub**, where the ack deadline belongs to the
      subscription rather than the pull. See
      `Provider.supports .queuePerReceiveLease`. -/
  leaseSeconds : Option Nat := none
  deriving Repr, DecidableEq, Inhabited

-- ── The two halves ──────────────────────────────────────────────────────────

/-- Where messages go in: an SQS queue, or a Pub/Sub topic. -/
structure Producer where
  /-- Publish messages, answering their provider-assigned identifiers in the
      same order. -/
  send   : List OutgoingMessage → IO (Except Error (List String))
  /-- Where this publishes to, for diagnostics. Never a credential. -/
  target : String := "queue"

/-- Where messages come out: an SQS queue, or a Pub/Sub **subscription**.

    On Pub/Sub this cannot be built without naming a subscription, which is the
    whole point — see the module header. -/
structure Consumer where
  /-- Receive up to `maxMessages` messages. May answer empty even when
      messages exist. -/
  receive     : ReceiveParams → IO (Except Error (List Message))
  /-- Acknowledge messages, removing them permanently. -/
  ack         : List Receipt → IO (Except Error Unit)
  /-- Change how long the messages' leases have left.

      `0` makes them immediately available to other consumers, which is how
      both systems spell "I could not process this, give it to someone else" —
      there is no separate negative acknowledgement on either. -/
  extendLease : List Receipt → Nat → IO (Except Error Unit)
  /-- Discard every message.

      Not available on every cloud: see `Provider.supports .queuePurge`, and
      expect `unsupported` on GCP. -/
  purge       : IO (Except Error Unit)
  /-- Where this reads from, for diagnostics. -/
  source      : String := "queue"

/-- Both halves, for a program that publishes and consumes. -/
structure Queue where
  /-- Where messages go in. -/
  producer : Producer
  /-- Where messages come out. -/
  consumer : Consumer

-- ── Derived operations ──────────────────────────────────────────────────────

/-- Publish one message. -/
def Producer.sendOne (p : Producer) (body : String)
    (attributes : List (String × String) := []) : IO (Except Error String) := do
  match ← p.send [{ body, attributes }] with
  | .error e    => return .error e
  | .ok (id :: _) => return .ok id
  | .ok []      =>
    return .error (Error.protocol "the provider accepted a message and returned no id")

/-- Receive at most one message, or `none`. -/
def Consumer.receiveOne (c : Consumer) (waitSeconds : Option Nat := none) :
    IO (Except Error (Option Message)) := do
  match ← c.receive { maxMessages := 1, waitSeconds } with
  | .error e => return .error e
  | .ok msgs => return .ok msgs.head?

/-- Acknowledge one message. -/
def Consumer.ackOne (c : Consumer) (m : Message) : IO (Except Error Unit) :=
  c.ack [m.receipt]

/-- Return a message to the queue immediately, for another consumer to try.

    Both systems spell this as "set the remaining lease to zero"; neither has a
    separate negative acknowledgement. -/
def Consumer.release (c : Consumer) (m : Message) : IO (Except Error Unit) :=
  c.extendLease [m.receipt] 0

/-- Receive one message, process it, and acknowledge it only if processing
    succeeded.

    On failure the message is released rather than left to time out, so a
    transient error is retried promptly instead of after the lease expires.
    `none` means there was nothing to receive. -/
def Consumer.withMessage {α : Type} (c : Consumer) (handle : Message → IO (Except Error α))
    (waitSeconds : Option Nat := none) : IO (Except Error (Option α)) := do
  match ← c.receiveOne waitSeconds with
  | .error e      => return .error e
  | .ok none      => return .ok none
  | .ok (some m)  =>
    match ← handle m with
    | .error e => do
      -- Give it back rather than holding the lease to expiry.
      let _ ← c.release m
      return .error e
    | .ok a => do
      match ← c.ackOne m with
      | .error e => return .error e
      | .ok _    => return .ok (some a)

-- ── A local backend ─────────────────────────────────────────────────────────

/-- The in-memory queue's state: messages waiting, and messages leased out. -/
structure MemQueue where
  /-- Waiting to be received, oldest first. -/
  waiting  : List Message := []
  /-- Received and not yet acknowledged, by receipt handle. -/
  inFlight : List (String × Message) := []
  /-- Counter for generating identifiers. -/
  counter  : Nat := 0

/-- A queue held in memory, for local development, debugging and tests.

    Faithful in the ways that catch bugs:

    - FIFO, oldest first, as SQS is in practice for a standard queue.
    - A received message becomes **invisible** to further receives until it is
      acknowledged or released, so a consumer that forgets to `ack` sees the
      backlog stop moving — the same symptom as in production.
    - `extendLease … 0` returns messages to the front of the queue, which is
      how both real systems spell a negative acknowledgement.
    - `receiveCount` increments on each delivery, so poison-message handling
      can be exercised.
    - Identifiers and receipts are distinct, and a receipt is only valid until
      it is used, so code that confuses them fails here too.

    What it does not do: expire leases on a timer. There is no clock, so a
    message leased and never acknowledged stays leased forever rather than
    reappearing after `leaseSeconds`. A test for redelivery-on-timeout has to
    call `extendLease … 0` explicitly.

    ```
    let q ← Queue.inMemory
    let _ ← q.producer.sendOne "work item"
    match ← q.consumer.receiveOne with
    | .ok (some m) => do IO.println m.body; discard <| q.consumer.ackOne m
    | _ => pure ()
    ``` -/
def Queue.inMemoryOf (ref : IO.Ref MemQueue) (name : String) : IO Queue := do
  let producer : Producer :=
    { target := s!"{name} (in memory)"
    , send := fun msgs => do
        let ids ← msgs.foldlM (init := ([] : List String)) fun acc m => do
          let n ← ref.modifyGet fun q => (q.counter, { q with counter := q.counter + 1 })
          let id := s!"msg-{n}"
          ref.modify fun q =>
            { q with waiting := q.waiting ++
                [{ id, body := m.body, attributes := m.attributes
                 , receipt := ⟨""⟩, receiveCount := some 0 }] }
          return id :: acc
        return .ok ids.reverse }
  let consumer : Consumer :=
    { source := s!"{name} (in memory)"
    , receive := fun params => do
        let taken ← ref.modifyGet fun q =>
          let take := q.waiting.take params.maxMessages
          let rest := q.waiting.drop params.maxMessages
          -- A receipt is fresh per delivery, as it is on both real systems: a
          -- handle from an earlier receive is not usable after a release.
          let leased := take.zipIdx.map fun (m, i) =>
            let handle := s!"rcpt-{q.counter + i}"
            { m with receipt := ⟨handle⟩
                   , receiveCount := m.receiveCount.map (· + 1) }
          ( leased
          , { q with waiting := rest
                   , counter := q.counter + take.length
                   , inFlight := q.inFlight ++ leased.map (fun m => (m.receipt.handle, m)) } )
        return .ok taken
    , ack := fun receipts => do
        let handles := receipts.map (·.handle)
        ref.modify fun q =>
          { q with inFlight := q.inFlight.filter (fun kv => !handles.contains kv.1) }
        return .ok ()
    , extendLease := fun receipts seconds => do
        if seconds == 0 then
          -- Return them to the front, so a released message is retried
          -- promptly rather than after everything else.
          let handles := receipts.map (·.handle)
          ref.modify fun q =>
            let returning := (q.inFlight.filter (fun kv => handles.contains kv.1)).map (·.2)
            { q with
                inFlight := q.inFlight.filter (fun kv => !handles.contains kv.1)
              , waiting := returning.map (fun m => { m with receipt := ⟨""⟩ }) ++ q.waiting }
          return .ok ()
        else
          -- No clock, so a longer lease is already what happens.
          return .ok ()
    , purge := do
        ref.modify fun q => { q with waiting := [], inFlight := [] }
        return .ok () }
  return { producer, consumer }

/-- A queue held in memory. See `Queue.inMemoryOf` for what it does and does
    not emulate. -/
def Queue.inMemory (name : String := "in-memory") : IO Queue := do
  Queue.inMemoryOf (← IO.mkRef ({} : MemQueue)) name

/-- How many messages an in-memory queue holds: waiting, and leased out.

    Deliberately **not** a field of `Consumer`. No real provider can answer
    this exactly — SQS's `ApproximateNumberOfMessages` is in the name — so
    putting it on the portable interface would invite code that works locally
    and misleads in production. It is returned alongside the queue instead, so
    reaching for it is visibly a local-only affordance.

    ```
    let (q, depth) ← Queue.inMemoryInspectable
    let _ ← q.producer.sendOne "a"
    IO.println (← depth)      -- (1, 0)
    ``` -/
def Queue.inMemoryInspectable (name : String := "in-memory") :
    IO (Queue × IO (Nat × Nat)) := do
  let ref ← IO.mkRef ({} : MemQueue)
  let q ← Queue.inMemoryOf ref name
  return (q, do let s ← ref.get; return (s.waiting.length, s.inFlight.length))

end Cloud
