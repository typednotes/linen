/-
  Linen.Database.Redis.Hooks — before/after-request instrumentation hooks

  ## Haskell source
  `Database.Redis.Hooks` from https://hackage.haskell.org/package/hedis
  (module 4 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Hooks.hs`.

  ## Scope
  Upstream's `Hooks` has five fields; two of them (`sendPubSubHook`,
  `callbackHook`) are typed in terms of `Database.Redis.PubSub`'s `Message`
  and `PubSub` types. Those types now live in `Database.Redis.PubSub.Types`
  (a small dependency-free module split out precisely to break the import
  cycle upstream breaks with a `{-# SOURCE #-}` boot import — see that
  module's doc-comment), so all five hooks are ported here in full.
-/
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.PubSub.Types

namespace Database.Redis.Hooks

open Database.Redis.Protocol (Reply)
open Database.Redis.PubSub (Message PubSub)

/-- A hook for sending a request (a list of RESP arguments) to the server
    and receiving the raw `Reply`, wrapping the underlying action. -/
abbrev SendRequestHook : Type :=
  (List ByteArray → IO Reply) → List ByteArray → IO Reply

/-- A hook for sending raw bytes to the server, wrapping the underlying
    action. -/
abbrev SendHook : Type :=
  (ByteArray → IO Unit) → ByteArray → IO Unit

/-- A hook for receiving a raw `Reply` from the server, wrapping the
    underlying action. -/
abbrev ReceiveHook : Type :=
  IO Reply → IO Reply

/-- A hook for sending pub/sub subscription-change commands to the server,
    wrapping the underlying action. Mirrors upstream's `SendPubSubHook`. -/
abbrev SendPubSubHook : Type :=
  (List ByteArray → IO Unit) → List ByteArray → IO Unit

/-- A hook for invoking a pub/sub message callback, wrapping the underlying
    action. Mirrors upstream's `CallbackHook`. -/
abbrev CallbackHook : Type :=
  (Message → IO PubSub) → Message → IO PubSub

/-- Instrumentation hooks threaded through `Database.Redis.Core`'s request
    dispatch and `Database.Redis.PubSub`'s send/listen loops. Mirrors
    upstream's five-field `Hooks` record. -/
structure Hooks where
  /-- Wraps sending a request and receiving its reply. -/
  sendRequestHook : SendRequestHook
  /-- Wraps sending pub/sub subscription-change commands. -/
  sendPubSubHook : SendPubSubHook
  /-- Wraps invoking a pub/sub message callback. -/
  callbackHook : CallbackHook
  /-- Wraps sending raw bytes to the server. -/
  sendHook : SendHook
  /-- Wraps receiving a raw reply from the server. -/
  receiveHook : ReceiveHook

/-- The default hooks: every hook is the identity function, i.e. no
    instrumentation. -/
def defaultHooks : Hooks where
  sendRequestHook := id
  sendPubSubHook := id
  callbackHook := id
  sendHook := id
  receiveHook := id

end Database.Redis.Hooks
