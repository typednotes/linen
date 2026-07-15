/-
  Linen.Database.Redis.PubSub — Redis Pub/Sub

  ## Haskell source
  `Database.Redis.PubSub` from
  https://hackage.haskell.org/package/hedis-0.16.1/src/src/Database/Redis/PubSub.hs
  (module #15 of the `hedis` import, see `docs/imports/hedis/dependencies.md`).
  Exposes `publish`; the single-threaded `pubSub` interface (`Message`,
  `PubSub`, `subscribe`/`unsubscribe`/`psubscribe`/`punsubscribe`); the
  continuous `pubSubForever` controller (`PubSubController`,
  `newPubSubController`, `addChannels`/`removeChannels`, …); and the
  short-lived `withPubSub` interface.

  The pure value types (`Message`, `PubSub`, `Cmd`, and the subscription-change
  algebra) live in `Database.Redis.PubSub.Types` and are re-exported here, so
  the public surface matches upstream's single module (see that module's
  doc-comment for why the split is necessary — it is the Lean equivalent of
  upstream's `{-# SOURCE #-}` boot-file break of the `Hooks`↔`PubSub` cycle).

  ## Substitutions (per `docs/imports/hedis/dependencies.md`)

  - **`async` → native `Task` concurrency.** Upstream races a listener thread
    against a sender/callback thread with `Control.Concurrent.Async`'s
    `withAsync`/`waitEitherCatch`/`waitEitherCatchSTM`/`concurrently`. There is
    no `async` package in `linen`; those four call sites are re-expressed
    directly over Lean's native `IO.asTask`/`IO.wait`/`IO.waitAny`/
    `IO.hasFinished`. `pubSubForeverOnConn` starts the listen and send threads
    as `Task`s, then waits (via `IO.waitAny`) for whichever of {a thread dies,
    the initial subscription completes} happens first — the "pending
    subscriptions empty" wait being an ordinary blocking `atomically` (STM
    `retry`) launched as its own `Task`, which is exactly upstream's
    `orElse (waitEitherCatchSTM …) (retry-until-pending-empty)`.
    `withPubSubOnConn`'s `concurrently` becomes: run the listener as a `Task`
    and the callback in the current thread inside `try … finally`, then join
    the listener and re-throw any error it raised.

  - **`stm` → `Linen.Control.Concurrent.STM`.** `TVar`/`atomically`/`retry` are
    used as-is. Upstream's *bounded* `TBQueue` (capacity 10) is replaced by
    `linen`'s unbounded `TQueue`: the bound is a soft back-pressure limit on
    how many un-flushed subscription-change batches may queue, not a
    correctness requirement, and `linen`'s STM ships only an unbounded queue.
    Because the sender thread drains the queue continuously this is observably
    equivalent for every use in this module.

  - **`unliftio-core` → direct IO.** Upstream's `sendCmd` uses
    `withRunInIO $ \runInIO -> hook (runInIO . Core.send) …` to run the hook's
    wrapped `Core.send` back in `IO`. `linen`'s `Redis` is already
    `ReaderT RedisEnv IO`, so once the raw `ProtocolPipelining.Connection` is
    read out of the environment, `Core.send` on it is literally
    `ppSend conn ∘ renderRequest` — an `IO` action needing no unlift wrapper.
    That collapse is captured once in `rawSendCmd`, shared by both the
    single-threaded and controller send paths.

  ## Deviations
  - **`UnregisterHandle` is a `Nat`, not an arbitrary-precision `Integer`.**
    Upstream's handle is a `newtype … Integer` incremented from 0; it is only
    ever a fresh non-negative counter value compared for equality, so a `Nat`
    is exact.
  - **`ChannelData` is generic only over its callback type, with the channel
    fixed to `ByteArray`.** Upstream's `ChannelData channel callback` is
    generic over the channel too, but both instantiations use
    `RedisChannel = RedisPChannel = ByteString`; specialising the channel to
    `ByteArray` (which has the required `BEq`/`Hashable`) removes a phantom
    generic parameter with no behavioural change.
  - **`decodeMsg` returns `Except Reply _` instead of crashing.** Upstream's
    `decodeMsg` calls `error` (a crash) on a reply that is not a well-formed
    pub/sub message. Per AGENTS.md (no introduced crashes), it returns
    `Except.error` here, which the IO loops turn into a descriptive
    `IO.userError` — the same "upstream crash → safe failure" treatment used
    throughout this import (see `Core`/`Types`).
  - **`TChan Message` → `TQueue Message`.** `withPubSub`'s single-consumer
    message channel is a `TChan` upstream; `linen`'s STM has no `TChan`, and a
    single-reader `TQueue` is observably identical.
-/
import Linen.Control.Concurrent.STM.TQueue
import Linen.Control.Monad.Reader
import Linen.Database.Redis.Cluster
import Linen.Database.Redis.Connection
import Linen.Database.Redis.Core
import Linen.Database.Redis.ProtocolPipelining
import Linen.Database.Redis.PubSub.Types
import Linen.Database.Redis.Types
import Std.Data.HashMap
import Std.Data.HashSet

namespace Database.Redis.PubSub

open Control.Monad (STM atomically)
open Control.Monad.Reader (ask)
open Control.Concurrent.STM (TVar TQueue)
open Control.Concurrent.STM.TVar (newTVar newTVarIO readTVar writeTVar modifyTVar')
open Database.Redis.Protocol (Reply renderRequest)
open Database.Redis.Types (decode)
open Database.Redis.Core.Internal (RedisEnv Redis)
open Database.Redis.ProtocolPipelining
  renaming send → ppSend, recv → ppRecv, flush → ppFlush,
    beginReceiving → ppBeginReceiving, fromCtxWithHooks → ppFromCtxWithHooks

/-- Short name for a raw pipelined connection (the public value types of this
    module already live in this namespace, imported from
    `Database.Redis.PubSub.Types`, so they need no re-export). -/
private abbrev PPConn := Database.Redis.ProtocolPipelining.Connection

/-- A Redis channel name. Mirrors upstream's `type RedisChannel = ByteString`. -/
abbrev RedisChannel := ByteArray

/-- A Redis pattern-channel name. Mirrors upstream's
    `type RedisPChannel = ByteString`. -/
abbrev RedisPChannel := ByteArray

-- ── Public: PUBLISH ───────────────────────────────────────────────────

/-- Post a message to a channel (<http://redis.io/commands/publish>). Returns
    the number of clients that received the message. Mirrors upstream's
    `publish`. -/
def publish {m : Type → Type} {f : Type → Type} [Monad m] [Database.Redis.Core.MonadRedis m]
    [Database.Redis.Core.RedisCtx m f] (channel message : ByteArray) : m (f Int) :=
  Database.Redis.Core.sendRequest ["PUBLISH".toUTF8, channel, message]

-- ── Decoding pub/sub replies ──────────────────────────────────────────

/-- A decoded pub/sub reply. Mirrors upstream's `PubSubReply`. -/
inductive PubSubReply where
  /-- A `subscribe` acknowledgement for the given channel. -/
  | subscribed (chan : RedisChannel)
  /-- A `psubscribe` acknowledgement for the given pattern. -/
  | psubscribed (chan : RedisPChannel)
  /-- An `unsubscribe` acknowledgement, with the remaining subscription
      count. -/
  | unsubscribed (chan : RedisChannel) (cnt : Int)
  /-- A `punsubscribe` acknowledgement, with the remaining subscription
      count. -/
  | punsubscribed (chan : RedisPChannel) (cnt : Int)
  /-- A delivered message. -/
  | msg (m : Message)
  deriving BEq

/-- Decode a raw multi-bulk reply into a `PubSubReply`. Mirrors upstream's
    `decodeMsg`, but returns `Except.error` (carrying the offending reply)
    instead of crashing on a malformed reply — see the module doc-comment. -/
def decodeMsg (r : Reply) : Except Reply PubSubReply :=
  match r with
  | .multiBulk (some (r0 :: r1 :: r2 :: rest)) => do
    let kind : ByteArray ← decode r0
    if kind == "message".toUTF8 then
      pure (.msg (.message (← decode r1) (← decode r2)))
    else if kind == "pmessage".toUTF8 then
      match rest with
      | r3 :: _ => pure (.msg (.pmessage (← decode r1) (← decode r2) (← decode r3)))
      | [] => throw r
    else if kind == "subscribe".toUTF8 then
      pure (.subscribed (← decode r1))
    else if kind == "psubscribe".toUTF8 then
      pure (.psubscribed (← decode r1))
    else if kind == "unsubscribe".toUTF8 then
      pure (.unsubscribed (← decode r1) (← decode r2))
    else if kind == "punsubscribe".toUTF8 then
      pure (.punsubscribed (← decode r1) (← decode r2))
    else throw r
  | _ => throw r

/-- `decodeMsg`, turning a decode failure into a descriptive `IO.userError`
    (the safe-failure replacement for upstream's crashing `errMsg`). -/
def decodeMsgIO (r : Reply) : IO PubSubReply :=
  match decodeMsg r with
  | .ok m => pure m
  | .error _ => throw (IO.userError "Redis.PubSub: expected a pub/sub message but got a different reply")

-- ── Sending subscription-change commands ──────────────────────────────

/-- Send one subscription-change command over a raw connection, through the
    connection's `sendPubSubHook`. A `doNothing` command sends nothing.
    Mirrors upstream's `rawSendCmd` (and, via the `unliftio` collapse
    described in the module doc-comment, the body of `sendCmd` too): the hook
    wraps `ppSend conn ∘ renderRequest`. -/
def rawSendCmd (conn : PPConn) (name : ByteArray) : Cmd → IO Unit
  | .doNothing => pure ()
  | .cmd changes =>
    conn.hooks.sendPubSubHook (fun bytes => ppSend conn (renderRequest bytes)) (name :: changes)

/-- Send all four subscription-change commands of a `PubSub` batch over a raw
    connection, returning how much the "pending acknowledgements" counter
    should grow (the number of newly-subscribed channels/patterns — upstream's
    `updatePending`: `SUBSCRIBE`/`PSUBSCRIBE` add their channel count,
    `UNSUBSCRIBE`/`PUNSUBSCRIBE` add nothing). -/
def rawSendPubSub (conn : PPConn) (ps : PubSub) : IO Nat := do
  rawSendCmd conn "SUBSCRIBE".toUTF8 ps.subs
  rawSendCmd conn "UNSUBSCRIBE".toUTF8 ps.unsubs
  rawSendCmd conn "PSUBSCRIBE".toUTF8 ps.psubs
  rawSendCmd conn "PUNSUBSCRIBE".toUTF8 ps.punsubs
  pure (ps.subs.changes.length + ps.psubs.changes.length)

-- ── Single-threaded Pub/Sub (`pubSub`) ────────────────────────────────

/-- Read the (non-clustered) raw connection out of the `Redis` environment.
    Upstream reads `envConn`, a selector defined only on the non-clustered
    constructor, so a clustered environment crashes; here it throws a
    descriptive `IO.userError` (the same treatment as `Core.recv`/`Core.send`). -/
private def pubSubConn : Redis PPConn := do
  match ← ask with
  | .nonClustered conn _ => pure conn
  | .clustered .. =>
    (throw (IO.userError "Redis.PubSub: pubSub requires a non-clustered connection") : IO PPConn)

/-- Listen to published messages on subscribed channels and pattern channels.
    The `callback` runs for each received message and returns a `PubSub` of
    subscription changes to apply (return `PubSub.empty` to keep subscriptions
    unchanged). Returns once every subscription has been unsubscribed (the
    subscription count and the pending-acknowledgement count both reach zero).
    Mirrors upstream's `pubSub`.

    Termination: the receive loop is a genuine unbounded server loop (it runs
    until Redis acknowledges that no subscriptions remain, or forever if the
    caller never unsubscribes), so it is written as `while true do …` with an
    early `return` — the same non-recursive infinite-loop idiom used by
    `ProtocolPipelining.parseOneReply`, requiring no termination proof and no
    iteration-count bound. -/
def pubSub (initial : PubSub) (callback : Message → IO PubSub) : Redis Unit := do
  if initial == PubSub.empty then return ()
  let conn ← pubSubConn
  let mut pending : Int := (← (rawSendPubSub conn initial : Redis Nat))
  let mut subCnt : Int := 0
  while true do
    let reply ← (Database.Redis.Core.recv : Redis Reply)
    match ← (decodeMsgIO reply : Redis PubSubReply) with
    | .msg m =>
      let newPS ← (conn.hooks.callbackHook callback m : Redis PubSub)
      pending := pending + (← (rawSendPubSub conn newPS : Redis Nat))
    | .subscribed _ => pending := pending - 1
    | .psubscribed _ => pending := pending - 1
    | .unsubscribed _ n =>
      subCnt := n
      if subCnt == 0 && pending == 0 then return ()
    | .punsubscribed _ n =>
      subCnt := n
      if subCnt == 0 && pending == 0 then return ()

-- ── Continuous Pub/Sub controller (`pubSubForever`) ───────────────────

/-- A callback for a message from a subscribed channel: it receives the
    message payload. Mirrors upstream's `MessageCallback`. -/
abbrev MessageCallback := ByteArray → IO Unit

/-- A callback for a message from a pattern-subscribed channel: it receives
    the channel the message arrived on and the payload. Mirrors upstream's
    `PMessageCallback`. -/
abbrev PMessageCallback := RedisChannel → ByteArray → IO Unit

/-- An action that unregisters the callbacks registered by `addChannels`/
    `addChannelsAndWait`; typically used with a bracket. Mirrors upstream's
    `UnregisterCallbacksAction`. -/
abbrev UnregisterCallbacksAction := IO Unit

/-- A fresh, monotonically-increasing identifier for a batch of registered
    callbacks. Mirrors upstream's `UnregisterHandle` (a `Nat` here — see the
    module doc-comment). -/
abbrev UnregisterHandle := Nat

/-- The channels currently subscribed, pending subscription, and pending
    removal, for one channel kind. Generic over the callback type only (the
    channel is `ByteArray` — see the module doc-comment). Mirrors upstream's
    `ChannelData channel callback`. -/
structure ChannelData (callback : Type) where
  /-- Subscribed channels, each mapped to its list of (handle, callback). -/
  cdSubscribedChannels : TVar (Std.HashMap ByteArray (List (UnregisterHandle × callback)))
  /-- Channels whose subscription has been requested but not yet
      acknowledged. -/
  cdChannelsPendingSubscription : TVar (Std.HashSet ByteArray)
  /-- Channels whose removal has been requested but not yet acknowledged. -/
  cdChannelsPendingRemoval : TVar (Std.HashSet ByteArray)

/-- A controller tracking a set of channels, pattern channels, and their
    callbacks, allowing subscriptions to be altered at any time while a
    `pubSubForever` loop is running. Mirrors upstream's `PubSubController`
    (its `TBQueue` is an unbounded `TQueue` here — see the module
    doc-comment). -/
structure PubSubController where
  /-- Queue of pending subscription-change batches for the send thread. -/
  sendChanges : TQueue PubSub
  /-- Channel subscriptions and their callbacks. -/
  pscChannelData : ChannelData MessageCallback
  /-- Pattern-channel subscriptions and their callbacks. -/
  pscPChannelData : ChannelData PMessageCallback
  /-- The last-issued callback-batch identifier. -/
  lastUsedCallbackId : TVar UnregisterHandle

/-- Build a fresh `ChannelData` from an initial list of (channel, callback)
    pairs (all assigned handle `0`, duplicate channels' callbacks appended).
    Mirrors upstream's `newChannelData`. -/
def newChannelData (initialSubs : List (ByteArray × callback)) :
    STM (ChannelData callback) := do
  let initMap : Std.HashMap ByteArray (List (UnregisterHandle × callback)) :=
    initialSubs.foldl (init := ∅) fun m (ch, cb) =>
      match m.get? ch with
      | some xs => m.insert ch (xs ++ [(0, cb)])
      | none => m.insert ch [(0, cb)]
  let subscribed ← newTVar initMap
  let pendingSub ← newTVar (∅ : Std.HashSet ByteArray)
  let pendingRem ← newTVar (∅ : Std.HashSet ByteArray)
  pure ⟨subscribed, pendingSub, pendingRem⟩

/-- Create a new `PubSubController`. This does not subscribe to anything; the
    subscriptions happen once `pubSubForever` is called. Mirrors upstream's
    `newPubSubController`. -/
def newPubSubController (initialSubs : List (RedisChannel × MessageCallback))
    (initialPSubs : List (RedisPChannel × PMessageCallback)) : IO PubSubController :=
  atomically do
    let q ← TQueue.newTQueue
    let lastId ← newTVar (0 : UnregisterHandle)
    let cd ← newChannelData initialSubs
    let pcd ← newChannelData initialPSubs
    pure ⟨q, cd, pcd, lastId⟩

/-- The channels currently in the controller. WARNING: may lag the server's
    actual subscriptions. Mirrors upstream's `currentChannels`. -/
def currentChannels (ctrl : PubSubController) : IO (List RedisChannel) := do
  pure (← atomically (readTVar ctrl.pscChannelData.cdSubscribedChannels)).keys

/-- The pattern channels currently in the controller. WARNING: may lag the
    server. Mirrors upstream's `currentPChannels`. -/
def currentPChannels (ctrl : PubSubController) : IO (List RedisPChannel) := do
  pure (← atomically (readTVar ctrl.pscPChannelData.cdSubscribedChannels)).keys

/-- Channels whose subscription is pending acknowledgement. Mirrors upstream's
    `pendingChannels`. -/
def pendingChannels (ctrl : PubSubController) : IO (Std.HashSet RedisChannel) :=
  atomically (readTVar ctrl.pscChannelData.cdChannelsPendingSubscription)

/-- Pattern channels whose subscription is pending acknowledgement. Mirrors
    upstream's `pendingPatternChannels`. -/
def pendingPatternChannels (ctrl : PubSubController) : IO (Std.HashSet RedisPChannel) :=
  atomically (readTVar ctrl.pscPChannelData.cdChannelsPendingSubscription)

/-- Is `k` a key of the map or a member of the set? Mirrors upstream's
    `memberMapOrSet`. -/
private def memberMapOrSet (m : Std.HashMap ByteArray α) (s : Std.HashSet ByteArray)
    (k : ByteArray) : Bool :=
  m.contains k || s.contains k

/-- Register `newChans` under handle `ident` in one `ChannelData`, returning
    the channels that were not already subscribed or pending (i.e. the ones a
    subscribe command must now be sent for). Mirrors upstream's
    `addChannelsOfType`. -/
def addChannelsOfType (ident : UnregisterHandle) (newChans : List (ByteArray × callback))
    (cd : ChannelData callback) : STM (List ByteArray) := do
  let callbacks ← readTVar cd.cdSubscribedChannels
  let pendingCallbacks ← readTVar cd.cdChannelsPendingSubscription
  let newChans' := (newChans.map Prod.fst).filter
    (fun ch => ! memberMapOrSet callbacks pendingCallbacks ch)
  -- `HM.unionWith (++) callbacks (fromList newChans mapped to [(ident, cb)])`.
  let newMap : Std.HashMap ByteArray (List (UnregisterHandle × callback)) :=
    newChans.foldl (init := ∅) fun m (ch, cb) => m.insert ch [(ident, cb)]
  let merged := newMap.fold (init := callbacks) fun acc ch newVal =>
    match acc.get? ch with
    | some old => acc.insert ch (old ++ newVal)
    | none => acc.insert ch newVal
  writeTVar cd.cdSubscribedChannels merged
  writeTVar cd.cdChannelsPendingSubscription
    (newChans'.foldl (init := pendingCallbacks) (·.insert ·))
  pure newChans'

/-- Add channels (and pattern channels) to the controller and enqueue the
    resulting subscribe commands. Returns an `UnregisterCallbacksAction` that
    removes exactly these callbacks. Does not wait for acknowledgement — see
    `addChannelsAndWait`. Mirrors upstream's `addChannels`. -/
def addChannels (ctrl : PubSubController)
    (newChans : List (RedisChannel × MessageCallback))
    (newPChans : List (RedisPChannel × PMessageCallback)) :
    IO UnregisterCallbacksAction := do
  match newChans, newPChans with
  | [], [] => pure (pure ())
  | _, _ =>
    let ident ← atomically do
      modifyTVar' ctrl.lastUsedCallbackId (· + 1)
      let ident ← readTVar ctrl.lastUsedCallbackId
      let newChannels ← addChannelsOfType ident newChans ctrl.pscChannelData
      let newPChannels ← addChannelsOfType ident newPChans ctrl.pscPChannelData
      TQueue.writeTQueue ctrl.sendChanges (subscribe newChannels ++ psubscribe newPChannels)
      pure ident
    pure (unsubChannels ctrl (newChans.map Prod.fst) (newPChans.map Prod.fst) ident)
where
  /-- Unsubscribe only the callbacks matching `ident` (defined mutually so
      `addChannels` can return it). Mirrors upstream's `unsubChannels`. -/
  unsubChannels (ctrl : PubSubController) (chans : List RedisChannel)
      (pchans : List RedisPChannel) (ident : UnregisterHandle) : IO Unit :=
    atomically do
      let channelsToDrop ← unregisterHandles ctrl.pscChannelData chans ident
      let pChannelsToDrop ← unregisterHandles ctrl.pscPChannelData pchans ident
      TQueue.writeTQueue ctrl.sendChanges
        (unsubscribe1 channelsToDrop ++ punsubscribe1 pChannelsToDrop)
  /-- Remove the callbacks registered under handle `h` for the given channels,
      returning the channels that thereby became fully unsubscribed. Mirrors
      upstream's `unregisterHandles`. -/
  unregisterHandles {callback : Type} (cd : ChannelData callback) (remChansParam : List ByteArray)
      (h : UnregisterHandle) : STM (List ByteArray) := do
    let callbacks ← readTVar cd.cdSubscribedChannels
    let remChans := remChansParam.filter (callbacks.contains ·)
    let removeHandles (m : Std.HashMap ByteArray (List (UnregisterHandle × callback)))
        (k : ByteArray) : Std.HashMap ByteArray (List (UnregisterHandle × callback)) :=
      match m.get? k with
      | none => m
      | some lst => match lst.filter (fun x => x.1 != h) with
        | [] => m.erase k
        | xs => m.insert k xs
    let callbacks' := remChans.foldl removeHandles callbacks
    let remChans' := remChans.filter
      (fun chan => callbacks.contains chan && ! callbacks'.contains chan)
    writeTVar cd.cdSubscribedChannels callbacks'
    unless remChans'.isEmpty do
      modifyTVar' cd.cdChannelsPendingSubscription
        (fun s => remChans'.foldl (·.erase ·) s)
    pure remChans'

/-- Wait (blocking) until none of the listed channels remain in their
    respective pending-set `TVar`s. Mirrors upstream's `waitUntilAbsent`. -/
def waitUntilAbsent (pending : List (TVar (Std.HashSet ByteArray) × List ByteArray)) :
    IO Unit :=
  atomically do
    for (tPendingChannels, channels) in pending do
      unless channels.isEmpty do
        let pendingChannels' ← readTVar tPendingChannels
        if channels.any (fun ch => pendingChannels'.contains ch) then STM.retry

/-- Call `addChannels` and then wait for Redis to acknowledge the new
    subscriptions. Mirrors upstream's `addChannelsAndWait`. -/
def addChannelsAndWait (ctrl : PubSubController)
    (newChans : List (RedisChannel × MessageCallback))
    (newPChans : List (RedisPChannel × PMessageCallback)) :
    IO UnregisterCallbacksAction := do
  match newChans, newPChans with
  | [], [] => pure (pure ())
  | _, _ =>
    let unreg ← addChannels ctrl newChans newPChans
    waitUntilAbsent
      [ (ctrl.pscChannelData.cdChannelsPendingSubscription, newChans.map Prod.fst)
      , (ctrl.pscPChannelData.cdChannelsPendingSubscription, newPChans.map Prod.fst) ]
    pure unreg

/-- Remove channels from one `ChannelData`, moving them to the pending-removal
    set, and return the channels actually removed. Mirrors upstream's
    `removeChannels'`. -/
def removeChannels' (cd : ChannelData callback) (remChannels : List ByteArray) :
    STM (List ByteArray) := do
  let subbedChannels ← readTVar cd.cdSubscribedChannels
  let pendingChannelSubs ← readTVar cd.cdChannelsPendingSubscription
  let remChannels' := remChannels.filter (memberMapOrSet subbedChannels pendingChannelSubs)
  writeTVar cd.cdSubscribedChannels (remChannels'.foldl (·.erase ·) subbedChannels)
  writeTVar cd.cdChannelsPendingSubscription (remChannels'.foldl (·.erase ·) pendingChannelSubs)
  modifyTVar' cd.cdChannelsPendingRemoval (fun s => remChannels'.foldl (·.insert ·) s)
  pure remChannels'

/-- Remove channels (and pattern channels) from the controller and enqueue the
    resulting unsubscribe commands. As soon as this returns, no more callbacks
    fire for the removed channels. Mirrors upstream's `removeChannels`. -/
def removeChannels (ctrl : PubSubController) (remChans : List RedisChannel)
    (remPChans : List RedisPChannel) : IO Unit := do
  match remChans, remPChans with
  | [], [] => pure ()
  | _, _ =>
    atomically do
      let remChans' ← removeChannels' ctrl.pscChannelData remChans
      let remPChans' ← removeChannels' ctrl.pscPChannelData remPChans
      TQueue.writeTQueue ctrl.sendChanges
        (unsubscribe1 remChans' ++ punsubscribe1 remPChans')

/-- Call `removeChannels` and then wait for the removals to be acknowledged by
    Redis. Mirrors upstream's `removeChannelsAndWait`. -/
def removeChannelsAndWait (ctrl : PubSubController) (remChannels : List RedisChannel)
    (remPChannels : List RedisPChannel) : IO Unit := do
  let (remChans', remPChans') ← atomically do
    let remChans' ← removeChannels' ctrl.pscChannelData remChannels
    let remPChans' ← removeChannels' ctrl.pscPChannelData remPChannels
    TQueue.writeTQueue ctrl.sendChanges
      (unsubscribe1 remChans' ++ punsubscribe1 remPChans')
    pure (remChans', remPChans')
  waitUntilAbsent
    [ (ctrl.pscChannelData.cdChannelsPendingRemoval, remChans')
    , (ctrl.pscPChannelData.cdChannelsPendingRemoval, remPChans') ]

-- ── Listener/sender threads ───────────────────────────────────────────

/-- The listener thread: the only thread that receives on the raw connection.
    It decodes each reply and either dispatches callbacks (through the
    connection's `callbackHook`) or updates the pending-subscription/removal
    sets. Mirrors upstream's `listenThread` (a `forever` loop, written here as
    the non-recursive `while true do …` idiom — see `pubSub`'s termination
    note; it exits only by throwing). -/
def listenThread (ctrl : PubSubController) (rawConn : PPConn) : IO Unit := do
  while true do
    let msg ← ppRecv rawConn
    match ← decodeMsgIO msg with
    | .msg (.message channel mmsg) =>
      let cm ← atomically (readTVar ctrl.pscChannelData.cdSubscribedChannels)
      match cm.get? channel with
      | some cbs =>
        let _ ← rawConn.hooks.callbackHook
          (fun m => do cbs.forM (fun (_, x) => x m.msgMessage); pure PubSub.empty)
          (.message channel mmsg)
        pure ()
      | none => pure ()
    | .msg (.pmessage pat channel mmsg) =>
      let pm ← atomically (readTVar ctrl.pscPChannelData.cdSubscribedChannels)
      match pm.get? pat with
      | some cbs =>
        let _ ← rawConn.hooks.callbackHook
          (fun m => do cbs.forM (fun (_, x) => x m.msgChannel m.msgMessage); pure PubSub.empty)
          (.pmessage pat channel mmsg)
        pure ()
      | none => pure ()
    | .subscribed chan =>
      atomically (modifyTVar' ctrl.pscChannelData.cdChannelsPendingSubscription (·.erase chan))
    | .psubscribed chan =>
      atomically (modifyTVar' ctrl.pscPChannelData.cdChannelsPendingSubscription (·.erase chan))
    | .unsubscribed chan _ =>
      atomically (modifyTVar' ctrl.pscChannelData.cdChannelsPendingRemoval (·.erase chan))
    | .punsubscribed chan _ =>
      atomically (modifyTVar' ctrl.pscPChannelData.cdChannelsPendingRemoval (·.erase chan))

/-- The sender thread: the only thread that sends on the raw connection. It
    dequeues subscription-change batches and writes them. Mirrors upstream's
    `sendThread` (same `forever` → `while true` treatment as `listenThread`). -/
def sendThread (ctrl : PubSubController) (rawConn : PPConn) : IO Unit := do
  while true do
    let ps ← atomically (TQueue.readTQueue ctrl.sendChanges)
    rawSendCmd rawConn "SUBSCRIBE".toUTF8 ps.subs
    rawSendCmd rawConn "UNSUBSCRIBE".toUTF8 ps.unsubs
    rawSendCmd rawConn "PSUBSCRIBE".toUTF8 ps.psubs
    rawSendCmd rawConn "PUNSUBSCRIBE".toUTF8 ps.punsubs
    -- normally the socket is flushed during `recv`, but `recv` may currently
    -- be blocked on a message.
    ppFlush rawConn

/-- Wait (blocking, via STM `retry`) until both pending-subscription sets are
    empty — the "initial subscription complete" condition of
    `pubSubForeverOnConn`. -/
private def waitInitialSubscribed (ctrl : PubSubController) : STM Unit := do
  let a ← readTVar ctrl.pscChannelData.cdChannelsPendingSubscription
  STM.check a.isEmpty
  let b ← readTVar ctrl.pscPChannelData.cdChannelsPendingSubscription
  STM.check b.isEmpty

/-- Register all of the controller's channels, then run the listen and send
    threads forever, calling `onInitialLoad` once Redis acknowledges every
    initial subscription. Exits only when a thread raises (e.g. the connection
    dies), re-raising that error. Mirrors upstream's `pubSubForeverOnConn`;
    the `async` race is re-expressed over `IO.asTask`/`IO.waitAny` — see the
    module doc-comment. -/
def pubSubForeverOnConn (rawConn : PPConn) (ctrl : PubSubController)
    (onInitialLoad : IO Unit) : IO Unit := do
  -- Drain any stale queued changes (no threads are running yet).
  let mut draining := true
  while draining do
    if (← atomically (TQueue.tryReadTQueue ctrl.sendChanges)).isNone then draining := false
  -- Seed the queue and pending sets with the controller's current channels.
  atomically do
    let channels := (← readTVar ctrl.pscChannelData.cdSubscribedChannels).keys
    let patternChannels := (← readTVar ctrl.pscPChannelData.cdSubscribedChannels).keys
    TQueue.writeTQueue ctrl.sendChanges (subscribe channels ++ psubscribe patternChannels)
    writeTVar ctrl.pscChannelData.cdChannelsPendingSubscription
      (channels.foldl (·.insert ·) ∅)
    writeTVar ctrl.pscPChannelData.cdChannelsPendingSubscription
      (patternChannels.foldl (·.insert ·) ∅)
  -- Start both worker threads and an "initial subscription complete" waiter.
  let listenT ← IO.asTask (prio := .dedicated) (listenThread ctrl rawConn)
  let sendT ← IO.asTask (prio := .dedicated) (sendThread ctrl rawConn)
  let initT ← IO.asTask (atomically (waitInitialSubscribed ctrl))
  -- Wait for whichever happens first: a thread dies, or initial load done.
  let _ ← IO.waitAny [listenT, sendT, initT]
  -- If neither worker finished, the waiter did → initial load complete.
  unless (← IO.hasFinished listenT) || (← IO.hasFinished sendT) do
    onInitialLoad
  -- Wait for a worker to end (only ever with an error) and re-raise it.
  match ← IO.waitAny [listenT, sendT] with
  | .error e => throw e
  | .ok _ => pure ()

/-- Open a connection from the pool, register all of the controller's
    channels, and process messages and subscription changes forever. Mirrors
    upstream's `pubSubForever`. -/
def pubSubForever (conn : Database.Redis.Connection.Connection) (ctrl : PubSubController)
    (onInitialLoad : IO Unit) : IO Unit :=
  match conn with
  | .nonClustered pool =>
    pool.withResource fun rawConn => pubSubForeverOnConn rawConn ctrl onInitialLoad
  | .clustered _ pool =>
    pool.withResource fun clusterConn => do
      let rawConn ← masterRawConn clusterConn "pubSubForever"
      pubSubForeverOnConn rawConn ctrl onInitialLoad
where
  /-- Build a raw pipelined connection to the first master node of a clustered
      connection. Shared by `pubSubForever` and `withPubSub`. Mirrors the
      clustered branch of upstream's `pubSubForever`/`withPubSub`. -/
  masterRawConn (clusterConn : Database.Redis.Cluster.Connection) (who : String) :
      IO PPConn := do
    match ← Database.Redis.Cluster.masterNodes clusterConn with
    | [] => throw (IO.userError s!"Hedis: clustered {who} requires at least one master node")
    | nodeConn :: _ =>
      let rawConn ← ppFromCtxWithHooks nodeConn.conn.connCtx clusterConn.hooks
      ppBeginReceiving rawConn
      pure rawConn

-- ── Short-lived Pub/Sub (`withPubSub`) ────────────────────────────────

/-- Send `SUBSCRIBE`/`PSUBSCRIBE` for the (non-empty) channel/pattern lists,
    then flush. Mirrors `withPubSubOnConn`'s `subscribeAll`. -/
private def withPubSubSubscribe (rawConn : PPConn) (chans pchans : List ByteArray) :
    IO Unit := do
  match chans with
  | [] => pure ()
  | c :: cs => ppSend rawConn (renderRequest ("SUBSCRIBE".toUTF8 :: c :: cs))
  match pchans with
  | [] => pure ()
  | p :: ps => ppSend rawConn (renderRequest ("PSUBSCRIBE".toUTF8 :: p :: ps))
  ppFlush rawConn

/-- Send `UNSUBSCRIBE`/`PUNSUBSCRIBE` for the (non-empty) channel/pattern
    lists, then flush. Mirrors `withPubSubOnConn`'s `unsubscribeAll`. -/
private def withPubSubUnsubscribe (rawConn : PPConn) (chans pchans : List ByteArray) :
    IO Unit := do
  match chans with
  | [] => pure ()
  | c :: cs => ppSend rawConn (renderRequest ("UNSUBSCRIBE".toUTF8 :: c :: cs))
  match pchans with
  | [] => pure ()
  | p :: ps => ppSend rawConn (renderRequest ("PUNSUBSCRIBE".toUTF8 :: p :: ps))
  ppFlush rawConn

/-- The listener loop of `withPubSubOnConn`: forward each message to the queue
    and stop once the final unsubscribe (count `0`) is acknowledged. Mirrors
    `withPubSubOnConn`'s `lThread` (`fix next` → the `while true` idiom). -/
private def withPubSubListen (messageChan : TQueue Message) (rawConn : PPConn) : IO Unit := do
  while true do
    let msg ← ppRecv rawConn
    match ← decodeMsgIO msg with
    | .msg m => atomically (TQueue.writeTQueue messageChan m)
    | .unsubscribed _ n => if n == 0 then return ()
    | .punsubscribed _ n => if n == 0 then return ()
    | _ => pure ()

/-- Subscribe, run `f` (handing it a blocking `STM Message` read), unsubscribe
    on exit, and join the listener. Mirrors upstream's `withPubSubOnConn`; the
    `concurrently` is re-expressed over `IO.asTask` — see the module
    doc-comment. -/
def withPubSubOnConn {r : Type} (messageChan : TQueue Message) (chans pchans : List ByteArray)
    (rawConn : PPConn) (f : STM Message → IO r) : IO r := do
  withPubSubSubscribe rawConn chans pchans
  let lt ← IO.asTask (prio := .dedicated) (withPubSubListen messageChan rawConn)
  let result ← (try
      f (TQueue.readTQueue messageChan)
    finally
      withPubSubUnsubscribe rawConn chans pchans)
  match ← IO.wait lt with
  | .error e => throw e
  | .ok _ => pure ()
  pure result

/-- Create a short-lived Pub/Sub subscription, automatically unsubscribing
    when the callback returns. Does not support changing subscriptions while
    running and makes no attempt to handle connection loss. Mirrors upstream's
    `withPubSub`. -/
def withPubSub {r : Type} (conn : Database.Redis.Connection.Connection)
    (chans pchans : List ByteArray) (f : STM Message → IO r) : IO r :=
  match conn with
  | .nonClustered pool =>
    pool.withResource fun rawConn => do
      let messageChan ← TQueue.newTQueueIO
      withPubSubOnConn messageChan chans pchans rawConn f
  | .clustered _ pool =>
    pool.withResource fun clusterConn => do
      let rawConn ← pubSubForever.masterRawConn clusterConn "withPubSub"
      let messageChan ← TQueue.newTQueueIO
      withPubSubOnConn messageChan chans pchans rawConn f

end Database.Redis.PubSub
