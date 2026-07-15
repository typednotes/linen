/-
  Tests for `Linen.Database.Redis.Cluster`.

  Covers the pure redirect-parsing helpers (`moved`/`askingRedirection`) and
  the `ShardMap`/dedup helpers directly, then exercises the routing and
  `MOVED`/`ASK` retry machinery end-to-end against a small two-node "cluster"
  built out of real loopback TCP listeners playing the role of dumb Redis
  nodes.
-/
import Linen.Database.Redis.Cluster
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Cluster
open Database.Redis.Cluster.Command (CommandInfo AritySpec Flag LastKeyPositionSpec)
open Database.Redis.Protocol (Reply)

namespace Tests.Database.Redis.Cluster

/-! ### Compile-time: public API shape -/

example : Option ByteArray → Option ByteArray → List CommandInfo → ShardMap → Database.Redis.Hooks.Hooks →
    IO Connection := connectWith
example : Connection → IO Unit := disconnect
example : NodeConnection → List (List ByteArray) → IO (List Reply) := requestNode
example : NodeConnection → List ByteArray → IO Reply := requestNode1
example : Connection → ShardMap → Option (List NodeConnection) := allMasterNodes
example : Database.Redis.Cluster.Command.InfoMap → List ByteArray → IO (List ByteArray) := requestKeys
example : List (List ByteArray) → List ByteArray → IO Database.Redis.Cluster.HashSlot.HashSlot :=
  hashSlotForKeys
example : Connection → Database.Redis.Cluster.HashSlot.HashSlot → IO NodeConnection := nodeConnForHashSlot
example : Connection → ShardMap → List ByteArray → IO (List NodeConnection) := nodeConnectionForCommand
example : Reply → Bool := moved
example : Reply → Option (Host × Port) := askingRedirection
example : Connection → ShardMap → Host → Port → Option NodeConnection := nodeConnWithHostAndPort
example : Connection → IO ShardMap → List (List ByteArray) → List Reply → IO (List Reply) := retryBatch
example : Connection → IO ShardMap → List (List ByteArray) → IO (List Reply) := evaluateBatch
example : IO ShardMap → Connection → List ByteArray → IO Reply := requestPipelined
example : Connection → IO (List NodeConnection) := masterNodes
example : Connection → List ByteArray → IO (List Reply) := requestMasterNodes
example : NodeConnection → Connection → Option NodeConnection := getRandomConnection

/-! ### Pure: `moved` / `askingRedirection` -/

#guard moved (Reply.error "MOVED 0 127.0.0.1:7000".toUTF8) == true
#guard moved (Reply.error "ASK 0 127.0.0.1:7000".toUTF8) == false
#guard moved (Reply.singleLine "OK".toUTF8) == false
#guard moved (Reply.error "ERR unknown".toUTF8) == false

#guard askingRedirection (Reply.error "ASK 0 127.0.0.1:7000".toUTF8) == some ("127.0.0.1", 7000)
#guard askingRedirection (Reply.error "MOVED 0 127.0.0.1:7000".toUTF8) == none
#guard askingRedirection (Reply.singleLine "OK".toUTF8) == none
#guard askingRedirection (Reply.error "ASK notaslot 127.0.0.1:7000".toUTF8) == some ("127.0.0.1", 7000)

/-! ### Pure: `ShardMap.nodes` / `nodeWithHostAndPort` -/

private def nodeA : Node := { id := "nodeA".toUTF8, role := .master, host := "127.0.0.1", port := 7000 }
private def nodeB : Node := { id := "nodeB".toUTF8, role := .master, host := "127.0.0.1", port := 7001 }
private def nodeBSlave : Node := { id := "nodeBSlave".toUTF8, role := .slave, host := "127.0.0.1", port := 7002 }

private def sampleShardMap : ShardMap :=
  { slots := ((∅ : Std.HashMap Nat Shard).insert 0 { master := nodeA, slaves := [] })
      |>.insert 1 { master := nodeB, slaves := [nodeBSlave] } }

#guard sampleShardMap.nodes.length == 3
#guard sampleShardMap.nodes.contains nodeA
#guard sampleShardMap.nodes.contains nodeB
#guard sampleShardMap.nodes.contains nodeBSlave

#guard sampleShardMap.nodeWithHostAndPort "127.0.0.1" 7000 == some nodeA
#guard sampleShardMap.nodeWithHostAndPort "127.0.0.1" 7002 == some nodeBSlave
#guard sampleShardMap.nodeWithHostAndPort "127.0.0.1" 9999 == none

/-! ### Runtime: routing + redirect handling against a fake two-node cluster

    Each "node" is a real loopback TCP listener. A dumb background task
    accepts exactly one connection, reads whatever the client sent in one
    `recv`, and replies with canned RESP2 bytes — enough to drive
    `connectWith`/`requestPipelined`/`evaluateBatch` through real routing,
    `MOVED`, and `ASK` handling without a real Redis server. -/

/- Accept one connection, drain one `recv`, reply with `replyBytes`, close. -/
private def fakeNodeOnce (listener : Network.Socket.Socket .listening) (replyBytes : ByteArray) :
    IO Unit := do
  let (accepted, _peer) ← Network.Socket.Blocking.accept listener
  let _ ← Network.Socket.Blocking.recv accepted 4096
  Network.Socket.sendAll accepted replyBytes
  let _ ← Network.Socket.close accepted
  pure ()

private def waitForTask (t : Task (Except IO.Error Unit)) : IO Unit := do
  let mut done := false
  for _ in [0:200] do
    if ← IO.hasFinished t then done := true; break
    IO.sleep 10
  unless done do
    throw (IO.userError "server task did not finish within ~2s")
  match t.get with
  | .error e => throw e
  | .ok _ => pure ()

/-- A fabricated `COMMAND`-reply entry for `PING`, so `requestKeys` (via the
    `InfoMap`) resolves it to zero keys without needing a real Redis server
    to `COMMAND`-introspect. -/
private def pingInfo : CommandInfo :=
  { name := "ping".toUTF8
    arity := AritySpec.required 1
    flags := [Flag.readOnly]
    firstKeyPosition := 0
    lastKeyPosition := LastKeyPositionSpec.lastKeyPosition 0
    stepCount := 0 }

/- Basic routing: a keyless command (`PING`) is sent to whichever node owns
    hash slot `0` — here, node A. Node B is part of the cluster's `ShardMap`
    (so `connectWith` opens a connection to it too) but never receives any
    traffic in this sub-test. -/
#eval show IO Unit from do
  let listenerA ← Network.Socket.listenTCP "127.0.0.1" 0
  let listenerB ← Network.Socket.listenTCP "127.0.0.1" 0
  let addrA ← Network.Socket.getSockName listenerA
  let addrB ← Network.Socket.getSockName listenerB
  let nA : Node := { id := "a".toUTF8, role := .master, host := addrA.host, port := addrA.port.toNat }
  let nB : Node := { id := "b".toUTF8, role := .master, host := addrB.host, port := addrB.port.toNat }
  let shardMap : ShardMap :=
    { slots := ((∅ : Std.HashMap Nat Shard).insert 0 { master := nA, slaves := [] })
        |>.insert 1 { master := nB, slaves := [] } }
  let serverTaskA ← IO.asTask (prio := .dedicated) (fakeNodeOnce listenerA "+PONG\r\n".toUTF8)
  let conn ← connectWith none none [pingInfo] shardMap Database.Redis.Hooks.defaultHooks
  let reply ← requestPipelined (pure shardMap) conn ["PING".toUTF8]
  match reply with
  | .singleLine s => unless s == "PONG".toUTF8 do throw (IO.userError "expected PONG")
  | _ => throw (IO.userError "expected a singleLine reply")
  waitForTask serverTaskA
  disconnect conn
  let _ ← Network.Socket.close listenerA
  let _ ← Network.Socket.close listenerB
  pure ()

/- `MOVED` redirect: node A reports that slot `0` moved to node B; on
    retry, `requestPipelined`'s caller-supplied `refreshAction` returns an
    updated `ShardMap` (slot `0` now owned by node B), and the retried
    request reaches node B. -/
#eval show IO Unit from do
  let listenerA ← Network.Socket.listenTCP "127.0.0.1" 0
  let listenerB ← Network.Socket.listenTCP "127.0.0.1" 0
  let addrA ← Network.Socket.getSockName listenerA
  let addrB ← Network.Socket.getSockName listenerB
  let nA : Node := { id := "a".toUTF8, role := .master, host := addrA.host, port := addrA.port.toNat }
  let nB : Node := { id := "b".toUTF8, role := .master, host := addrB.host, port := addrB.port.toNat }
  let shardA : Shard := { master := nA, slaves := [] }
  let shardB : Shard := { master := nB, slaves := [] }
  let initialMap : ShardMap := { slots := ((∅ : Std.HashMap Nat Shard).insert 0 shardA).insert 1 shardB }
  let swappedMap : ShardMap := { slots := ((∅ : Std.HashMap Nat Shard).insert 0 shardB).insert 1 shardA }
  let movedReply := s!"-MOVED 0 {addrB.host}:{addrB.port}\r\n".toUTF8
  let serverTaskA ← IO.asTask (prio := .dedicated) (fakeNodeOnce listenerA movedReply)
  let serverTaskB ← IO.asTask (prio := .dedicated) (fakeNodeOnce listenerB "+PONG\r\n".toUTF8)
  let conn ← connectWith none none [pingInfo] initialMap Database.Redis.Hooks.defaultHooks
  let reply ← requestPipelined (pure swappedMap) conn ["PING".toUTF8]
  match reply with
  | .singleLine s => unless s == "PONG".toUTF8 do throw (IO.userError "expected PONG after MOVED redirect")
  | _ => throw (IO.userError "expected a singleLine reply after MOVED redirect")
  waitForTask serverTaskA
  waitForTask serverTaskB
  disconnect conn
  let _ ← Network.Socket.close listenerA
  let _ ← Network.Socket.close listenerB
  pure ()

/- `ASK` redirect: node A reports an `ASK` redirect to node B, which is
    already connected (`connectWith` opened it up front), so the retry
    finds it directly via `nodeConnWithHostAndPort` without needing to
    consult `refreshAction` at all — sending `ASKING` immediately before
    the retried command, pipelined, and dropping `ASKING`'s own reply. -/
#eval show IO Unit from do
  let listenerA ← Network.Socket.listenTCP "127.0.0.1" 0
  let listenerB ← Network.Socket.listenTCP "127.0.0.1" 0
  let addrA ← Network.Socket.getSockName listenerA
  let addrB ← Network.Socket.getSockName listenerB
  let nA : Node := { id := "a".toUTF8, role := .master, host := addrA.host, port := addrA.port.toNat }
  let nB : Node := { id := "b".toUTF8, role := .master, host := addrB.host, port := addrB.port.toNat }
  let initialMap : ShardMap :=
    { slots := ((∅ : Std.HashMap Nat Shard).insert 0 { master := nA, slaves := [] })
        |>.insert 1 { master := nB, slaves := [] } }
  let askReply := s!"-ASK 0 {addrB.host}:{addrB.port}\r\n".toUTF8
  let serverTaskA ← IO.asTask (prio := .dedicated) (fakeNodeOnce listenerA askReply)
  let serverTaskB ← IO.asTask (prio := .dedicated)
    (fakeNodeOnce listenerB ("+OK\r\n".toUTF8 ++ "+PONG\r\n".toUTF8))
  let conn ← connectWith none none [pingInfo] initialMap Database.Redis.Hooks.defaultHooks
  let refreshAction : IO ShardMap :=
    throw (IO.userError "refresh should not be needed for an already-connected ASK target")
  let reply ← requestPipelined refreshAction conn ["PING".toUTF8]
  match reply with
  | .singleLine s => unless s == "PONG".toUTF8 do throw (IO.userError "expected PONG after ASK redirect")
  | _ => throw (IO.userError "expected a singleLine reply after ASK redirect")
  waitForTask serverTaskA
  waitForTask serverTaskB
  disconnect conn
  let _ ← Network.Socket.close listenerA
  let _ ← Network.Socket.close listenerB
  pure ()

/- `getRandomConnection` picks a *different* node when the cluster has more
    than one; `masterNodes`/`allMasterNodes` see both connected masters. -/
#eval show IO Unit from do
  let listenerA ← Network.Socket.listenTCP "127.0.0.1" 0
  let listenerB ← Network.Socket.listenTCP "127.0.0.1" 0
  let addrA ← Network.Socket.getSockName listenerA
  let addrB ← Network.Socket.getSockName listenerB
  let nA : Node := { id := "a".toUTF8, role := .master, host := addrA.host, port := addrA.port.toNat }
  let nB : Node := { id := "b".toUTF8, role := .master, host := addrB.host, port := addrB.port.toNat }
  let shardMap : ShardMap :=
    { slots := ((∅ : Std.HashMap Nat Shard).insert 0 { master := nA, slaves := [] })
        |>.insert 1 { master := nB, slaves := [] } }
  let conn ← connectWith none none [pingInfo] shardMap Database.Redis.Hooks.defaultHooks
  let masters ← masterNodes conn
  unless masters.length == 2 do throw (IO.userError s!"expected 2 master connections, got {masters.length}")
  match allMasterNodes conn shardMap with
  | none => throw (IO.userError "expected allMasterNodes to succeed")
  | some ms => unless ms.length == 2 do throw (IO.userError "expected 2 nodes from allMasterNodes")
  match conn.nodes.get? "a".toUTF8 with
  | none => throw (IO.userError "expected node a to be connected")
  | some ncA =>
    match getRandomConnection ncA conn with
    | none => throw (IO.userError "expected getRandomConnection to find a node")
    | some other => unless !(NodeConnection.sameNode ncA other) do
        throw (IO.userError "expected getRandomConnection to pick a *different* node")
  disconnect conn
  let _ ← Network.Socket.close listenerA
  let _ ← Network.Socket.close listenerB
  pure ()

end Tests.Database.Redis.Cluster
