/-
  Tests for `Linen.Data.Streaming.Network`.

  Exercises `bindPortTCP`/`getSocketTCP`/`acceptSafe`/`mkAppData` end-to-end
  over a real loopback TCP connection, and `runTCPServer` against a real
  client connecting from a background task.
-/
import Linen.Data.Streaming.Network

open Network.Socket
open Data.Streaming.Network

namespace Tests.Data.Streaming.Network

/-! ### bindPortTCP / acceptSafe / mkAppData / getSocketTCP round trip -/

#eval show IO Unit from do
  let server ← bindPortTCP 0
  let addr ← getSockName server
  let serverTask ← IO.asTask (prio := .dedicated) do
    let (client, peer) ← acceptSafe server
    let appData := mkAppData client peer
    let bytes ← appData.appRead
    appData.appWrite bytes
    appData.appClose
    pure bytes
  let (conn, peer) ← getSocketTCP "127.0.0.1" addr.port
  unless peer.port == addr.port do
    throw (IO.userError s!"getSocketTCP returned port {peer.port}, expected {addr.port}")
  Blocking.sendAll conn "ping".toUTF8
  let echoed ← Blocking.recv conn
  let _ ← close conn
  let _ ← close server
  let received ←
    match serverTask.get with
    | .ok bytes => pure bytes
    | .error e => throw e
  unless received == "ping".toUTF8 do
    throw (IO.userError s!"appRead expected 'ping', got {String.fromUTF8! received}")
  unless echoed == "ping".toUTF8 do
    throw (IO.userError s!"appWrite echo expected 'ping', got {String.fromUTF8! echoed}")

/-! ### runTCPServer handles a real client in the background -/

#eval show IO Unit from do
  let port : UInt16 := 41777
  let serverTask ← IO.asTask (prio := .dedicated) do
    runTCPServer port (fun appData => do
      let bytes ← appData.appRead
      appData.appWrite (bytes ++ "!".toUTF8))
  -- give the server a moment to bind before connecting
  IO.sleep 50
  let (conn, _addr) ← getSocketTCP "127.0.0.1" port
  Blocking.sendAll conn "hi".toUTF8
  let reply ← Blocking.recv conn
  let _ ← close conn
  unless reply == "hi!".toUTF8 do
    throw (IO.userError s!"runTCPServer echo expected 'hi!', got {String.fromUTF8! reply}")
  -- runTCPServer loops forever; the background task is left running for the
  -- process lifetime (there is no `stop`), matching Hale's original design.
  unless (← IO.hasFinished serverTask) == false do
    throw (IO.userError "runTCPServer's accept loop should not have exited")

end Tests.Data.Streaming.Network
