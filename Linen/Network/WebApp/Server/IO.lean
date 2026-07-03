/-
  Linen.Network.WebApp.Server.IO — Low-level I/O helpers

  Ports Hale's `Network.Wai.Handler.Warp.IO`.
-/
import Linen.Network.WebApp.Server.Types

namespace Network.WebApp.Server

/-- Send a ByteArray through the connection.
    $$\text{connSendByteArray} : \text{Connection} \to \text{ByteArray} \to \text{IO Unit}$$ -/
@[inline] def connSendByteArray (conn : Connection) (bs : ByteArray) : IO Unit :=
  conn.connSendAll bs

/-- Send multiple ByteArrays through the connection.
    $$\text{connSendByteArrays} : \text{Connection} \to \text{List ByteArray} \to \text{IO Unit}$$ -/
def connSendByteArrays (conn : Connection) (chunks : List ByteArray) : IO Unit :=
  conn.connSendMany chunks

end Network.WebApp.Server
