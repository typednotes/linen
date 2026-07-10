/-
  Linen.Network.WebApp.Server.SendFile — File sending abstraction

  Dispatches file sending through the `Connection`'s `connSendFile`, which may
  use sendfile(2) or read+send depending on the transport.

  Ports `Network.Wai.Handler.Warp.SendFile`.
-/
import Linen.Network.WebApp.Server.Types

namespace Network.WebApp.Server

/-- Send a file over a connection, with optional header prepending.
    $$\text{sendFileWithConn} : \text{Connection} \to \text{String} \to \text{Nat} \to \text{Nat} \to \text{IO Unit} \to \text{List ByteArray} \to \text{IO Unit}$$ -/
def sendFileWithConn (conn : Connection) (path : String) (offset count : Nat)
    (hook : IO Unit) (hdrs : List ByteArray) : IO Unit := do
  -- Send headers first
  for hdr in hdrs do
    conn.connSendAll hdr
  -- Send file content
  conn.connSendFile path offset count hook hdrs

/-- Read a file and send it via the connection's send function.
    Fallback when sendfile(2) is not available.
    $$\text{readSendFile} : \text{Connection} \to \text{String} \to \text{Nat} \to \text{Nat} \to \text{IO Unit}$$ -/
def readSendFile (conn : Connection) (path : String) (offset count : Nat) : IO Unit := do
  let content ← IO.FS.readBinFile path
  let slice := content.extract offset (offset + count)
  conn.connSendAll slice

end Network.WebApp.Server
