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

/-- Registry of runnable examples: `name`, one-line description, entry point. -/
def examples : List (String × String × (List String → IO Unit)) :=
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
       Examples.QUIC.run) ]

/-- Print usage and the list of available examples. -/
def usage : IO Unit := do
  IO.println "usage: lake exe examples <name> [args...]\n"
  IO.println "examples:"
  for (name, desc, _) in examples do
    IO.println s!"  {name}\t{desc}"

def main (args : List String) : IO Unit := do
  match args with
  | [] => usage
  | name :: rest =>
    match examples.find? (·.1 == name) with
    | some (_, _, run) => run rest
    | none =>
      IO.eprintln s!"unknown example: {name}\n"
      usage
      IO.Process.exit 1
