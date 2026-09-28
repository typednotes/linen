/-
  Tests for `Linen.Database.Redis.Sentinel`.

  `SentinelConnectInfo` construction and `noSentinelsError` are pure and
  checked with `#guard`. `updateMaster`/`connect`/`runRedis` are exercised
  end-to-end against a pair of canned RESP2 loopback servers — a fake Sentinel
  that answers `SENTINEL get-master-addr-by-name` with a master address, and a
  fake master that answers `PING` with `+PONG` — mirroring the loopback-server
  style of `ConnectionTest`. Failover is exercised by flipping the
  `rcCheckFailover` flag and confirming the next `runRedis` re-queries the
  Sentinel and (same master) reuses the connection.
-/
import Linen.Database.Redis.Sentinel
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Database.Redis.Sentinel
open Database.Redis.Core (Redis)
open Database.Redis.Protocol (Reply)
open Data.List (NonEmpty)

namespace Tests.Database.Redis.Sentinel

/-! ### Compile-time: public API shape -/

example : SentinelConnectInfo → IO SentinelConnection := connect
example {α} : SentinelConnection → Redis (Except Reply α) → IO (Except Reply α) := runRedis
example : SentinelConnectInfo → IO (SentinelConnectInfo × Database.Redis.Connection.ConnectInfo) :=
  updateMaster
example : NonEmpty (String × UInt16) → IO.Error := noSentinelsError

/-! ### noSentinelsError -/

-- The error text names each unreachable sentinel as `host:port`.
private def errText : String := toString (noSentinelsError ⟨("127.0.0.1", 26379), [("h2", 111)]⟩)
#guard (errText.splitOn "127.0.0.1:26379").length == 2
#guard (errText.splitOn "h2:111").length == 2

/-! ### SentinelConnectInfo construction -/

private def sampleSci : SentinelConnectInfo :=
  { connectSentinels := ⟨("127.0.0.1", 26379), []⟩
    connectMasterName := "mymaster".toUTF8
    connectBaseInfo := Database.Redis.Connection.defaultConnectInfo }

#guard sampleSci.connectMasterName == "mymaster".toUTF8
#guard sampleSci.connectSentinels.head == ("127.0.0.1", 26379)
#guard sampleSci.connectSentinels.toList.length == 1

/-! ### updateMaster with no reachable sentinel throws NoSentinels -/

-- Point at a port with nothing listening (grab and immediately close one) so
-- the connect is refused; `updateMaster` must then throw `noSentinelsError`.
#eval show IO Unit from do
  let probe ← Network.Socket.listenTCP "127.0.0.1" 0
  let deadAddr ← Network.Socket.getSockName probe
  let _ ← Network.Socket.close probe
  let sci : SentinelConnectInfo :=
    { connectSentinels := ⟨(deadAddr.host, deadAddr.port), []⟩
      connectMasterName := "mymaster".toUTF8
      connectBaseInfo := Database.Redis.Connection.defaultConnectInfo }
  let threw ← try (do let _ ← updateMaster sci; pure false) catch _ => pure true
  unless threw do throw (IO.userError "expected updateMaster to throw NoSentinels")

/-! ### connect / runRedis against fake Sentinel + master loopback servers -/

/-- Encode `s` as a single RESP bulk string. -/
private def respBulk (s : String) : String :=
  s!"${s.toUTF8.size}\r\n{s}\r\n"

-- Full happy path: `connect` discovers the master via the Sentinel, then two
-- `runRedis` PINGs both reach the master — the second after a forced failover
-- re-query (same master → connection reused).
#eval show IO Unit from do
  -- Fake master: one socket, answers each request with +PONG (two PINGs).
  let masterSrv ← Network.Socket.listenTCP "127.0.0.1" 0
  let masterAddr ← Network.Socket.getSockName masterSrv
  let masterTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept masterSrv
    for _ in [0:2] do
      let _ ← Network.Socket.Blocking.recv accepted 256
      Network.Socket.sendAll accepted "+PONG\r\n".toUTF8
    Network.Socket.close accepted

  -- Fake Sentinel: answers two get-master-addr-by-name queries with the master
  -- address (one accepted connection per query).
  let sentinelSrv ← Network.Socket.listenTCP "127.0.0.1" 0
  let sentinelAddr ← Network.Socket.getSockName sentinelSrv
  let masterReply :=
    "*2\r\n" ++ respBulk masterAddr.host ++ respBulk (toString masterAddr.port)
  let sentinelTask ← IO.asTask (prio := .dedicated) do
    for _ in [0:2] do
      let (accepted, _peer) ← Network.Socket.Blocking.accept sentinelSrv
      let _ ← Network.Socket.Blocking.recv accepted 256
      Network.Socket.sendAll accepted masterReply.toUTF8
      let _ ← Network.Socket.close accepted
      pure ()

  let sci : SentinelConnectInfo :=
    { connectSentinels := ⟨(sentinelAddr.host, sentinelAddr.port), []⟩
      connectMasterName := "mymaster".toUTF8
      connectBaseInfo := Database.Redis.Connection.defaultConnectInfo }

  let conn ← connect sci
  -- The discovered master matches the fake master's address.
  let st ← conn.ref.get
  match st.rcMasterConnectInfo.connectAddr with
  | .hostPort h p =>
    unless h == masterAddr.host && p == masterAddr.port do
      throw (IO.userError s!"discovered master {h}:{p} ≠ {masterAddr.host}:{masterAddr.port}")
  | _ => throw (IO.userError "expected a hostPort master address")

  -- First PING reaches the master.
  match ← runRedis conn Database.Redis.ManualCommands.ping with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "expected first PING to succeed")

  -- Force a failover re-check; the next call re-queries the Sentinel.
  conn.ref.modify fun s => { s with rcCheckFailover := true }
  match ← runRedis conn Database.Redis.ManualCommands.ping with
  | .ok _ => pure ()
  | .error _ => throw (IO.userError "expected second PING (post-failover) to succeed")
  -- Same master → connection reused, and the failover flag is cleared.
  let st2 ← conn.ref.get
  unless st2.rcCheckFailover == false do
    throw (IO.userError "expected rcCheckFailover cleared after re-query")

  Database.Redis.Connection.disconnect st2.rcBaseConnection

  -- Both server tasks should complete.
  let mut done := false
  for _ in [0:200] do
    if (← IO.hasFinished masterTask) && (← IO.hasFinished sentinelTask) then
      done := true; break
    IO.sleep 10
  unless done do throw (IO.userError "server tasks did not finish within ~2s")
  match masterTask.get, sentinelTask.get with
  | .ok _, .ok _ => pure ()
  | .error e, _ => throw e
  | _, .error e => throw e
  let _ ← Network.Socket.close masterSrv
  let _ ← Network.Socket.close sentinelSrv
  pure ()

end Tests.Database.Redis.Sentinel
