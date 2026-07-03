/-
  Examples.Main — common entrypoint for all `linen` example programs.

  Dispatches to a named example:

      lake exe examples <name> [args...]

  With no name, it lists the available examples. Each example exposes a
  `run : List String → IO Unit`; register it in `examples` below.
-/
import Examples.Echo
import Examples.Bench
import Examples.PostgREST
import Examples.QUIC
import Examples.Recv
import Examples.ResourceT
import Examples.Conduit
import Examples.STM
import Examples.StreamingCommons
import Examples.TLS
import Examples.HTTPClient
import Examples.HTTPConduit
import Examples.Req
import Examples.WebApp
import Examples.WebAppStatic
import Examples.Vault
import Examples.Vector

/-- Registry of runnable examples: `name`, one-line description, entry point.

    `unsafe` because the `conduit` entry's `Examples.Conduit.run` is `unsafe`
    (see that module's docstring: `ConduitT` has no proof of termination for
    `awaitForever`'s corecursion, so the whole layer is `unsafe`) — this
    propagates to the registry, `usage`, and `main` below, which is fine for a
    compiled executable's entry point. -/
unsafe def examples : List (String × String × (List String → IO Unit)) :=
  [ ("echo",
       "green-threaded echo server — self-checking demo; `echo serve <port>` runs it forever",
       Examples.Echo.run),
    ("bench",
       "network round-trips with a few-ms server delay: Green vs blocking pool, same threads  [args: C delayMs total]",
       Examples.Bench.run),
    ("postgrest",
       "in-memory PostgREST request handling + OpenAPI spec generation — self-checking demo; `postgrest spec` prints just the OpenAPI spec, `postgrest live [connstr]` connects to a real Postgres",
       Examples.PostgREST.run),
    ("quic",
       "QUIC types/config + HTTP/3 QPACK/frame wire round trip — self-checking demo; also verifies Client.connect/Server.run/accept fail with the documented \"not yet implemented\" errors (no TLS 1.3 FFI backend yet)",
       Examples.QUIC.run),
    ("recv",
       "Network.Socket.Blocking accept/connect/send/recv round trip — self-checking demo",
       Examples.Recv.run),
    ("resourcet",
       "Control.Monad.Trans.Resource LIFO cleanup over real scratch files — self-checking demo",
       Examples.ResourceT.run),
    ("conduit",
       "Data.Conduit / Data.Conduit.Combinators pipelines (pure, effectful, and bracketP/runConduitRes resource-safe streaming) — self-checking demo",
       Examples.Conduit.run),
    ("stm",
       "Control.Monad.STM + Concurrent.STM.{TVar,TMVar,TQueue}: concurrent counter, producer/consumer, FIFO order, orElse/check — self-checking demo",
       Examples.STM.run),
    ("streaming-commons",
       "Data.Streaming.Network: bindPortTCP/getSocketTCP/acceptSafe/AppData round trip — self-checking demo; `streaming-commons serve <port>` runs it forever",
       Examples.StreamingCommons.run),
    ("tls",
       "Network.TLS.Context: full TLS 1.2/1.3 handshake over loopback against a self-signed cert, trusted via createClientContextWithCA — self-checking demo",
       Examples.TLS.run),
    ("httpclient",
       "Network.HTTP.Client: connectPlain/performRequest and execute's redirect-following, against a hand-rolled loopback HTTP/1.1 server — self-checking demo",
       Examples.HTTPClient.run),
    ("httpconduit",
       "Network.HTTP.Client.Conduit / Network.HTTP.Simple: parseUrl!/httpBS, withResponse, and httpSource streamed through `.| sinkList` — self-checking demo",
       Examples.HTTPConduit.run),
    ("req",
       "Network.HTTP.Req: type-safe req/runReq — GET/NoReqBody and POST/ReqBodyBs, both admitted by HttpBodyAllowed — against a loopback server — self-checking demo",
       Examples.Req.run),
    ("webapp",
       "Network.WebApp: Application/Middleware/AppM (composeMiddleware/ifRequest/modifyResponse) over a hand-rolled loopback HTTP/1.1 server — self-checking demo",
       Examples.WebApp.run),
    ("webappstatic",
       "Network.WebApp.Static: staticApp/static + defaultFileServerSettings over a real scratch directory — file hit, index redirect, 404, 403 dotfile rejection — self-checking demo",
       Examples.WebAppStatic.run),
    ("vault",
       "Data.Vault: Key.new-minted typed keys coexisting in one map, plus adjust/delete/union — self-checking demo",
       Examples.Vault.run),
    ("vector",
       "Data.Vector-derived Array combinators: generate/ifilter, foldl1'/foldr1/ifoldl'/ifoldr, and/or/product/notElem, backpermute/slice — self-checking demo",
       Examples.Vector.run) ]

/-- Print usage and the list of available examples. -/
unsafe def usage : IO Unit := do
  IO.println "usage: lake exe examples <name> [args...]\n"
  IO.println "examples:"
  for (name, desc, _) in examples do
    IO.println s!"  {name}\t{desc}"

unsafe def main (args : List String) : IO Unit := do
  match args with
  | [] => usage
  | name :: rest =>
    match examples.find? (·.1 == name) with
    | some (_, _, run) => run rest
    | none =>
      IO.eprintln s!"unknown example: {name}\n"
      usage
      IO.Process.exit 1
