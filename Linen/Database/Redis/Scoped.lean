import Linen.Control.Resource
import Linen.Database.Redis.Connection

/-! # Scoped Redis connections
Run commands through the connection owner. A command receives a request
and a decoded reply, so the reusable Redis environment stays private.
-/

namespace Database.Redis
open Control.Resource
open Database.Redis.Connection (ConnectInfo)
open Database.Redis.Protocol (Reply)

private opaque redisAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- An action over a private Redis connection and its pool. -/
abbrev ConnectionM := Owned redisAccess.val Connection.Connection

namespace ConnectionM

/-- Open a Redis connection pool for the program. -/
def run (info : ConnectInfo) (action : ConnectionM .open_ post α) : IO α :=
  Owned.run redisAccess.property (Connection.connect info) Connection.disconnect action

/-- Open a TLS Redis connection pool for the program. -/
def runTLS (info : ConnectInfo) (ctx : Network.TLS.TLSContext)
    (action : ConnectionM .open_ post α) : IO α :=
  Owned.run redisAccess.property (Connection.connectTLS info ctx) Connection.disconnect action

/-- Send a Redis command with arguments and decode its reply. -/
def command [Types.RedisResult α] (args : List ByteArray) :
    ConnectionM .open_ .open_ (Except Reply α) :=
  Owned.use redisAccess.property fun conn => Connection.runRedis conn (Core.sendRequest args)

/-- Disconnect the owned Redis connection. -/
def close : ConnectionM .open_ .closed Unit :=
  Owned.close redisAccess.property Connection.disconnect

end ConnectionM
end Database.Redis
