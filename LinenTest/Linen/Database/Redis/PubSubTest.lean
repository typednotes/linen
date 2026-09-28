/-
  Tests for `Linen.Database.Redis.PubSub`.

  `decodeMsg` and the public API shape are pure/compile-time checks. The three
  Pub/Sub interfaces (`pubSub`, `withPubSub`, `pubSubForever`) are exercised
  end-to-end against canned RESP2 loopback "servers", following the pattern of
  `ProtocolPipeliningTest`/`ConnectionTest`.
-/
import Linen.Database.Redis.PubSub
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.PubSub
open Database.Redis.Connection (Connection ConnectInfo defaultConnectInfo connect disconnect runRedis)
open Database.Redis.Protocol (Reply)
open Control.Monad (STM atomically)

namespace Tests.Database.Redis.PubSub

/-! ### Compile-time: public API shape -/

example : ByteArray → ByteArray →
    Database.Redis.Core.Redis (Except Reply Int) := publish
example : PubSub → (Message → IO PubSub) → Database.Redis.Core.Redis Unit := pubSub
example : List (RedisChannel × MessageCallback) → List (RedisPChannel × PMessageCallback) →
    IO PubSubController := newPubSubController
example : Connection → PubSubController → IO Unit → IO Unit := pubSubForever
example {r} : Connection → List ByteArray → List ByteArray → (STM Message → IO r) → IO r :=
  withPubSub
example : PubSubController → IO (List RedisChannel) := currentChannels

/-! ### decodeMsg (pure) -/

private def bulk (s : String) : Reply := .bulk (some s.toUTF8)

#guard (decodeMsg (.multiBulk (some [bulk "message", bulk "news", bulk "hello"]))).toOption
  == some (.msg (.message "news".toUTF8 "hello".toUTF8))
#guard (decodeMsg (.multiBulk (some [bulk "pmessage", bulk "n.*", bulk "news", bulk "hi"]))).toOption
  == some (.msg (.pmessage "n.*".toUTF8 "news".toUTF8 "hi".toUTF8))
#guard (decodeMsg (.multiBulk (some [bulk "subscribe", bulk "news", .integer 1]))).toOption
  == some (.subscribed "news".toUTF8)
#guard (decodeMsg (.multiBulk (some [bulk "psubscribe", bulk "n.*", .integer 1]))).toOption
  == some (.psubscribed "n.*".toUTF8)
#guard (decodeMsg (.multiBulk (some [bulk "unsubscribe", bulk "news", .integer 0]))).toOption
  == some (.unsubscribed "news".toUTF8 0)
#guard (decodeMsg (.multiBulk (some [bulk "punsubscribe", bulk "n.*", .integer 2]))).toOption
  == some (.punsubscribed "n.*".toUTF8 2)
-- A non-pub/sub reply, or a truncated pmessage, is an error (not a crash).
#guard (decodeMsg (.integer 5)).toOption == none
#guard (decodeMsg (.multiBulk (some [bulk "pmessage", bulk "n.*", bulk "news"]))).toOption == none

/-! ### RESP2 fragments used by the loopback servers -/

private def subAck : ByteArray := "*3\r\n$9\r\nsubscribe\r\n$4\r\nnews\r\n:1\r\n".toUTF8
private def newsMsg : ByteArray := "*3\r\n$7\r\nmessage\r\n$4\r\nnews\r\n$5\r\nhello\r\n".toUTF8
private def unsubAck : ByteArray := "*3\r\n$11\r\nunsubscribe\r\n$4\r\nnews\r\n:0\r\n".toUTF8

private def waitTask {α} (t : Task (Except IO.Error α)) : IO Unit := do
  let mut done := false
  for _ in [0:300] do
    if ← IO.hasFinished t then done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server task did not finish in time")

private def connectTo (addr : Network.Socket.SockAddr) : IO Connection :=
  connect { defaultConnectInfo with connectAddr := .hostPort addr.host addr.port }

/-! ### `pubSub` — single-threaded, subscribe then unsubscribe on first message -/

#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (c, _) ← Network.Socket.Blocking.accept server
    -- Read SUBSCRIBE, reply with the ack and one message.
    let _ ← Network.Socket.Blocking.recv c 256
    Network.Socket.sendAll c (subAck ++ newsMsg)
    -- Read UNSUBSCRIBE, reply with the ack.
    let _ ← Network.Socket.Blocking.recv c 256
    Network.Socket.sendAll c unsubAck
    Network.Socket.close c
  let conn ← connectTo addr
  let got ← IO.mkRef (ByteArray.empty)
  runRedis conn <| pubSub (subscribe ["news".toUTF8]) fun m => do
    got.set m.msgMessage
    pure (unsubscribe ["news".toUTF8])
  let payload ← got.get
  unless payload == "hello".toUTF8 do
    throw (IO.userError s!"expected 'hello', got {String.fromUTF8! payload}")
  disconnect conn
  waitTask serverTask
  let _ ← Network.Socket.close server

/-! ### `withPubSub` — receive one message, then auto-unsubscribe on return -/

#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (c, _) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv c 256        -- SUBSCRIBE
    Network.Socket.sendAll c (subAck ++ newsMsg)
    let _ ← Network.Socket.Blocking.recv c 256        -- UNSUBSCRIBE (from the `finally`)
    Network.Socket.sendAll c unsubAck
    Network.Socket.close c
  let conn ← connectTo addr
  let payload ← withPubSub conn ["news".toUTF8] [] fun waitMsg => do
    let m ← atomically waitMsg
    pure m.msgMessage
  unless payload == "hello".toUTF8 do
    throw (IO.userError s!"expected 'hello', got {String.fromUTF8! payload}")
  disconnect conn
  waitTask serverTask
  let _ ← Network.Socket.close server

/-! ### `pubSubForever` — controller subscribes, delivers a message, then the
     server closing the connection makes the loop raise -/

#eval show IO Unit from do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (c, _) ← Network.Socket.Blocking.accept server
    let _ ← Network.Socket.Blocking.recv c 256        -- SUBSCRIBE (from the send thread)
    Network.Socket.sendAll c subAck
    Network.Socket.sendAll c newsMsg
    IO.sleep 100                                       -- let callbacks run
    Network.Socket.close c
  let conn ← connectTo addr
  let got ← IO.mkRef ByteArray.empty
  let loaded ← IO.mkRef false
  let ctrl ← newPubSubController [("news".toUTF8, fun msg => got.set msg)] []
  -- The loop runs until the connection dies; the server closes it after the
  -- message, so `pubSubForever` is expected to raise.
  try
    pubSubForever conn ctrl (loaded.set true)
    throw (IO.userError "expected pubSubForever to raise when the connection closed")
  catch _ => pure ()
  let payload ← got.get
  unless payload == "hello".toUTF8 do
    throw (IO.userError s!"expected 'hello', got {String.fromUTF8! payload}")
  unless (← loaded.get) do
    throw (IO.userError "expected onInitialLoad to have run")
  disconnect conn
  waitTask serverTask
  let _ ← Network.Socket.close server

/-! ### Controller bookkeeping (no network) -/

#eval show IO Unit from do
  let ctrl ← newPubSubController [("a".toUTF8, fun _ => pure ())] [("p.*".toUTF8, fun _ _ => pure ())]
  -- Initial channels/pattern-channels are tracked.
  unless (← currentChannels ctrl) == ["a".toUTF8] do
    throw (IO.userError "expected initial channel 'a'")
  unless (← currentPChannels ctrl) == ["p.*".toUTF8] do
    throw (IO.userError "expected initial pattern 'p.*'")
  -- addChannels registers a new channel and returns an unregister action.
  let unreg ← addChannels ctrl [("b".toUTF8, fun _ => pure ())] []
  let chans ← currentChannels ctrl
  unless chans.contains "b".toUTF8 do throw (IO.userError "expected 'b' after addChannels")
  -- unregister removes exactly the channel added under that handle.
  unreg
  let chans2 ← currentChannels ctrl
  unless ! chans2.contains "b".toUTF8 do throw (IO.userError "expected 'b' gone after unregister")
  unless chans2.contains "a".toUTF8 do throw (IO.userError "expected 'a' to remain")
  -- removeChannels drops a channel.
  removeChannels ctrl ["a".toUTF8] []
  let chans3 ← currentChannels ctrl
  unless ! chans3.contains "a".toUTF8 do throw (IO.userError "expected 'a' gone after removeChannels")

end Tests.Database.Redis.PubSub
