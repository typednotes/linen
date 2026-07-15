/-
  Linen.Database.Redis.PubSub.Types — the pure Pub/Sub value types
  (`Message`, `PubSub`, `Cmd`) and the subscription-change algebra

  ## Haskell source
  These types come from `Database.Redis.PubSub`
  (https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/PubSub.hs),
  module #15 of the `hedis` import (see `docs/imports/hedis/dependencies.md`).
  They are split into this small dependency-free module purely to break an
  import cycle, exactly as upstream does.

  ## Why a separate module (the `{-# SOURCE #-}` substitution)
  Upstream's `Database.Redis.Hooks` has two hook fields — `sendPubSubHook`
  and `callbackHook` — whose types mention `Message`/`PubSub`, which are
  *defined* in `Database.Redis.PubSub`. But `PubSub` in turn imports `Hooks`
  (through `Core`/`ProtocolPipelining`), so upstream breaks the cycle with a
  GHC boot file: `Hooks.hs` does `import {-# SOURCE #-} Database.Redis.PubSub
  (Message, PubSub)`. Lean has no `.hs-boot` mechanism, so the faithful
  equivalent is to place the shared value types in a lower module that both
  `Hooks` and the main `PubSub` module import — this file. The main
  `Database.Redis.PubSub` module re-exports everything defined here, so the
  public surface is identical to upstream's single `PubSub` module.

  ## Deviation: no phantom `Cmd a b` type; two explicit `append`s instead
  Upstream's `Cmd a b` carries two phantom type parameters (`Subscribe`/
  `Unsubscribe`, `Channel`/`Pattern`) solely to select, via GHC's overlapping
  `Semigroup (Cmd Subscribe a)` vs. `Semigroup (Cmd Unsubscribe a)` instances,
  whether an empty unsubscribe list ("unsubscribe from everything") absorbs
  its neighbour. Lean's typeclass resolution has no phantom-keyed instance
  selection, so `Cmd` is a plain single-parameter-free inductive and the two
  monoid behaviours are two named functions, `Cmd.appendSub` and
  `Cmd.appendUnsub`; `PubSub.append` applies the right one to each field. The
  observable behaviour is identical.
-/

namespace Database.Redis.PubSub

-- ── Messages ──────────────────────────────────────────────────────────

/-- A message received on a subscribed channel or pattern channel. Mirrors
    upstream's `Message`. `message` is a plain `SUBSCRIBE` delivery
    (`msgChannel`, `msgMessage`); `pmessage` is a `PSUBSCRIBE` delivery,
    additionally carrying the `msgPattern` that matched. -/
inductive Message where
  /-- A message on a plainly-subscribed channel. -/
  | message (msgChannel msgMessage : ByteArray)
  /-- A message on a pattern-subscribed channel. -/
  | pmessage (msgPattern msgChannel msgMessage : ByteArray)
  deriving BEq, Inhabited

/-- The channel a message was delivered on (total; works for both message
    kinds). Mirrors upstream's `msgChannel` record selector. -/
def Message.msgChannel : Message → ByteArray
  | .message c _ => c
  | .pmessage _ c _ => c

/-- The message payload (total; works for both message kinds). Mirrors
    upstream's `msgMessage` record selector. -/
def Message.msgMessage : Message → ByteArray
  | .message _ m => m
  | .pmessage _ _ m => m

/-- The pattern that matched, for a pattern-message; `none` for a plain
    message. Upstream's `msgPattern` selector is partial (it crashes on a
    plain `Message`); here it is total, returning `none`. -/
def Message.msgPattern : Message → Option ByteArray
  | .message _ _ => none
  | .pmessage p _ _ => some p

-- ── Subscription-change commands ──────────────────────────────────────

/-- One subscription-change command: either nothing to do, or a
    `SUBSCRIBE`/`UNSUBSCRIBE`/`PSUBSCRIBE`/`PUNSUBSCRIBE` carrying the list of
    channels/patterns it applies to. Mirrors upstream's `Cmd a b` (minus the
    phantom type parameters — see the module doc-comment). -/
inductive Cmd where
  /-- No command. Mirrors upstream's `DoNothing`. -/
  | doNothing
  /-- A command over the given channels/patterns. Mirrors upstream's
      `Cmd { changes }`. -/
  | cmd (changes : List ByteArray)
  deriving BEq, Inhabited

/-- The channel/pattern list of a command (`[]` for `doNothing`). -/
def Cmd.changes : Cmd → List ByteArray
  | .doNothing => []
  | .cmd cs => cs

/-- Monoid append for *subscribe* commands. Mirrors upstream's
    `Semigroup (Cmd Subscribe a)`: `doNothing` is the identity, and two
    commands concatenate their channel lists. -/
def Cmd.appendSub : Cmd → Cmd → Cmd
  | .doNothing, x => x
  | x, .doNothing => x
  | .cmd xs, .cmd ys => .cmd (xs ++ ys)

/-- Monoid append for *unsubscribe* commands. Mirrors upstream's
    `Semigroup (Cmd Unsubscribe a)`: like `appendSub`, except an empty
    unsubscribe list (`cmd []`, meaning "unsubscribe from *all* channels")
    absorbs its neighbour. -/
def Cmd.appendUnsub : Cmd → Cmd → Cmd
  | .doNothing, x => x
  | x, .doNothing => x
  | .cmd [], _ => .cmd []
  | _, .cmd [] => .cmd []
  | .cmd xs, .cmd ys => .cmd (xs ++ ys)

/-- The `Monoid` unit for both `Cmd` append operations. Mirrors upstream's
    `mempty = DoNothing`. -/
def Cmd.empty : Cmd := .doNothing

-- ── PubSub (a batch of subscription changes) ──────────────────────────

/-- Encapsulates a batch of subscription changes. Build with `subscribe`,
    `unsubscribe`, `psubscribe`, `punsubscribe`, or `PubSub.empty`, and
    combine with `PubSub.append` (`· ++ ·`). Mirrors upstream's `PubSub`
    record. -/
structure PubSub where
  /-- Channels to `SUBSCRIBE`. -/
  subs : Cmd := .doNothing
  /-- Channels to `UNSUBSCRIBE`. -/
  unsubs : Cmd := .doNothing
  /-- Patterns to `PSUBSCRIBE`. -/
  psubs : Cmd := .doNothing
  /-- Patterns to `PUNSUBSCRIBE`. -/
  punsubs : Cmd := .doNothing
  deriving BEq, Inhabited

/-- The empty batch (no subscription changes). Mirrors upstream's
    `mempty :: PubSub`. -/
def PubSub.empty : PubSub := {}

/-- Combine two batches field-wise, using the subscribe/unsubscribe append
    rules for the respective fields. Mirrors upstream's `Semigroup`/`Monoid
    PubSub` instance. -/
def PubSub.append (p1 p2 : PubSub) : PubSub where
  subs := Cmd.appendSub p1.subs p2.subs
  unsubs := Cmd.appendUnsub p1.unsubs p2.unsubs
  psubs := Cmd.appendSub p1.psubs p2.psubs
  punsubs := Cmd.appendUnsub p1.punsubs p2.punsubs

instance : Append PubSub := ⟨PubSub.append⟩

-- ── Smart constructors ────────────────────────────────────────────────

/-- Listen for messages published to the given channels
    (<http://redis.io/commands/subscribe>). Mirrors upstream's `subscribe`
    (an empty list yields `PubSub.empty`). -/
def subscribe : List ByteArray → PubSub
  | [] => PubSub.empty
  | cs => { PubSub.empty with subs := .cmd cs }

/-- Stop listening for messages posted to the given channels
    (<http://redis.io/commands/unsubscribe>). Mirrors upstream's
    `unsubscribe`: an empty list means "unsubscribe from *all* channels". -/
def unsubscribe (cs : List ByteArray) : PubSub :=
  { PubSub.empty with unsubs := .cmd cs }

/-- Like `unsubscribe`, but an empty list is a no-op rather than
    "unsubscribe from all". Mirrors upstream's `unsubscribe1`. -/
def unsubscribe1 : List ByteArray → PubSub
  | [] => PubSub.empty
  | cs => { PubSub.empty with unsubs := .cmd cs }

/-- Listen for messages published to channels matching the given patterns
    (<http://redis.io/commands/psubscribe>). Mirrors upstream's `psubscribe`
    (an empty list yields `PubSub.empty`). -/
def psubscribe : List ByteArray → PubSub
  | [] => PubSub.empty
  | ps => { PubSub.empty with psubs := .cmd ps }

/-- Stop listening for messages posted to channels matching the given
    patterns (<http://redis.io/commands/punsubscribe>). Mirrors upstream's
    `punsubscribe`: an empty list means "unsubscribe from *all* patterns". -/
def punsubscribe (ps : List ByteArray) : PubSub :=
  { PubSub.empty with punsubs := .cmd ps }

/-- Like `punsubscribe`, but an empty list is a no-op. Mirrors upstream's
    `punsubscribe1`. -/
def punsubscribe1 : List ByteArray → PubSub
  | [] => PubSub.empty
  | ps => { PubSub.empty with punsubs := .cmd ps }

end Database.Redis.PubSub
