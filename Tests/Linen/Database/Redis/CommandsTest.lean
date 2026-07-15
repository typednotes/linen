/-
  Tests for `Linen.Database.Redis.Commands`.

  Each command is a `sendRequest [...]` one-liner: the interesting, testable
  part is the RESP request byte-string it builds. We exercise a representative
  sample across every category (connection, keys, hashes, HyperLogLog, lists,
  scripting, server, sets, sorted sets, strings) by running the command via
  `runRedisInternal` against a loopback TCP server that captures the bytes it
  receives, then comparing them to `Database.Redis.Protocol.renderRequest` of
  the expected argument list. This follows the live-loopback `#eval` pattern
  used by `CoreTest`/`ProtocolPipeliningTest` (a `Redis` action needs a real
  connection to run, so a pure `#guard` cannot capture what it sends).

  The server always replies with a nil bulk (`$-1\r\n`); the reply need not
  decode for a given command's result type — `sendRequest` sends the request
  regardless, and the request bytes are what we assert on.
-/
import Linen.Database.Redis.Commands
import Linen.Network.Socket
import Linen.Network.Socket.Blocking

open Data.List (NonEmpty)
open Database.Redis.Commands
open Database.Redis.Core (Redis runRedisInternal)
open Database.Redis.Protocol (renderRequest)

namespace Tests.Database.Redis.Commands

/-! ### Compile-time: representative public API shapes -/

example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    ByteArray → m (f (Option ByteArray)) := get
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    ByteArray → NonEmpty ByteArray → m (f Int) := sadd
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    ByteArray → m (f (KeyValueReply ByteArray ByteArray)) := hgetall
example [Monad m] [Database.Redis.Core.MonadRedis m] [Database.Redis.Core.RedisCtx m f] :
    m (f (Int × Int)) := time

/-! ### Runtime helper: capture the request bytes a command sends -/

/-- Run `action` against a loopback server, returning the raw request bytes
    the server received. The server replies with a nil bulk so the pipeline
    completes. -/
def captureRequest (action : Redis α) : IO ByteArray := do
  let server ← Network.Socket.listenTCP "127.0.0.1" 0
  let addr ← Network.Socket.getSockName server
  let capTask ← IO.asTask (prio := .dedicated) do
    let (accepted, _peer) ← Network.Socket.Blocking.accept server
    let bytes ← Network.Socket.Blocking.recv accepted 4096
    Network.Socket.sendAll accepted "$-1\r\n".toUTF8
    let _ ← Network.Socket.close accepted
    pure bytes
  let conn ← Database.Redis.ProtocolPipelining.connect (.hostPort addr.host addr.port)
  let _ ← runRedisInternal conn action
  let mut result : Option ByteArray := none
  for _ in [0:200] do
    if ← IO.hasFinished capTask then
      match capTask.get with
      | .error e => throw e
      | .ok bytes => result := some bytes
      break
    IO.sleep 10
  Database.Redis.ProtocolPipelining.disconnect conn
  let _ ← Network.Socket.close server
  match result with
  | some bytes => pure bytes
  | none => throw (IO.userError "server task did not capture a request within ~2s")

/-- Assert that running `action` sends exactly `renderRequest expected`. -/
def checkRequest (action : Redis (Except Database.Redis.Protocol.Reply α)) (expected : List ByteArray) :
    IO Unit := do
  let got ← captureRequest action
  unless got == renderRequest expected do
    throw (IO.userError s!"request mismatch: got {got.toList}, expected {(renderRequest expected).toList}")

-- Local helpers to keep the samples terse.
private def b (s : String) : ByteArray := s.toUTF8
private def ne (x : ByteArray) (xs : List ByteArray) : NonEmpty ByteArray := ⟨x, xs⟩
private def ne_pair (x : ByteArray × ByteArray) (xs : List (ByteArray × ByteArray)) :
    NonEmpty (ByteArray × ByteArray) := ⟨x, xs⟩

/-! ### Request-shape checks, one representative per category -/

-- Connection
#eval checkRequest (echo (b "hi")) [b "ECHO", b "hi"]
#eval checkRequest (quit) [b "QUIT"]

-- Keys
#eval checkRequest (del (ne (b "k1") [b "k2"])) [b "DEL", b "k1", b "k2"]
#eval checkRequest (expire (b "k") 60) [b "EXPIRE", b "k", b "60"]
#eval checkRequest (ttl (b "k")) [b "TTL", b "k"]
#eval checkRequest (rename (b "a") (b "z")) [b "RENAME", b "a", b "z"]
#eval checkRequest (wait 2 100) [b "WAIT", b "2", b "100"]

-- Hashes
#eval checkRequest (hget (b "h") (b "fld")) [b "HGET", b "h", b "fld"]
#eval checkRequest (hset (b "h") ⟨(b "f1", b "v1"), [(b "f2", b "v2")]⟩)
  [b "HSET", b "h", b "f1", b "v1", b "f2", b "v2"]
#eval checkRequest (hgetall (b "h")) [b "HGETALL", b "h"]
#eval checkRequest (hincrby (b "h") (b "f") 3) [b "HINCRBY", b "h", b "f", b "3"]

-- HyperLogLog
#eval checkRequest (pfadd (b "hll") (ne (b "e1") [b "e2"])) [b "PFADD", b "hll", b "e1", b "e2"]
#eval checkRequest (pfmerge (b "dst") [b "s1", b "s2"]) [b "PFMERGE", b "dst", b "s1", b "s2"]

-- Lists
#eval checkRequest (lpush (b "l") (ne (b "x") [b "y"])) [b "LPUSH", b "l", b "x", b "y"]
#eval checkRequest (lrange (b "l") 0 (-1)) [b "LRANGE", b "l", b "0", b "-1"]
#eval checkRequest (blpop [b "l1", b "l2"] 5) [b "BLPOP", b "l1", b "l2", b "5"]
#eval checkRequest (brpop (ne (b "l") []) 5) [b "BRPOP", b "l", b "5"]

-- Scripting
#eval checkRequest (scriptLoad (b "return 1")) [b "SCRIPT", b "LOAD", b "return 1"]
#eval checkRequest (scriptExists (ne (b "sha1") [b "sha2"]))
  [b "SCRIPT", b "EXISTS", b "sha1", b "sha2"]

-- Server
#eval checkRequest (dbsize) [b "DBSIZE"]
#eval checkRequest (configSet (b "maxmemory") (b "100mb")) [b "CONFIG", b "SET", b "maxmemory", b "100mb"]
#eval checkRequest (configGet (ne (b "maxmemory") [])) [b "CONFIG", b "GET", b "maxmemory"]
#eval checkRequest (clientSetname (b "conn1")) [b "CLIENT", b "SETNAME", b "conn1"]
#eval checkRequest (time) [b "TIME"]

-- Sets
#eval checkRequest (sadd (b "s") (ne (b "m1") [b "m2"])) [b "SADD", b "s", b "m1", b "m2"]
#eval checkRequest (smove (b "src") (b "dst") (b "m")) [b "SMOVE", b "src", b "dst", b "m"]
#eval checkRequest (sinterstore (b "d") (ne (b "s1") [b "s2"])) [b "SINTERSTORE", b "d", b "s1", b "s2"]

-- Sorted Sets
#eval checkRequest (zcount (b "z") 1.0 5.0)
  [b "ZCOUNT", b "z", Database.Redis.Types.encode (1.0 : Float), Database.Redis.Types.encode (5.0 : Float)]
#eval checkRequest (zrank (b "z") (b "m")) [b "ZRANK", b "z", b "m"]
#eval checkRequest (zrankWithScore (b "z") (b "m")) [b "ZRANK", b "z", b "m", b "WITHSCORE"]
#eval checkRequest (zrem (b "z") (ne (b "m1") [b "m2"])) [b "ZREM", b "z", b "m1", b "m2"]

-- Strings
#eval checkRequest (get (b "k")) [b "GET", b "k"]
#eval checkRequest (setnx (b "k") (b "v")) [b "SETNX", b "k", b "v"]
#eval checkRequest (incrby (b "k") 7) [b "INCRBY", b "k", b "7"]
#eval checkRequest (mset (ne_pair (b "a", b "1") [(b "b", b "2")]))
  [b "MSET", b "a", b "1", b "b", b "2"]
#eval checkRequest (getrange (b "k") 0 3) [b "GETRANGE", b "k", b "0", b "3"]

end Tests.Database.Redis.Commands
