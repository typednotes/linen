/-
  Tests for `Linen.Network.WebApp.Server.Types`.
-/
import Linen.Network.WebApp.Server.Types

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.Types

/-! ### `InvalidRequest` messages -/

#guard toString (InvalidRequest.nonHttp) == "Server: Request line specified a non-HTTP request"
#guard toString (InvalidRequest.badFirstLine "GET") == "Server: Invalid first line of request: GET"
#guard toString InvalidRequest.payloadTooLarge == "Payload too large"

/-! ### `Transport` security -/

#guard Transport.tcp.isSecure == false
#guard (Transport.tls 1 2 none 0).isSecure == true
#guard (Transport.quic none 0).isSecure == true
#guard Transport.tcp.isQUIC == false
#guard (Transport.quic (some "h3") 0).isQUIC == true
#guard (Transport.tls 1 2 none 0).isQUIC == false

/-! ### `Source` (IO, leftover buffering) -/

#eval show IO Unit from do
  let n ← IO.mkRef 0
  let src ← Source.mk' (do n.modify (· + 1); pure (ByteArray.mk #[UInt8.ofNat (← n.get)]))
  src.leftover (ByteArray.mk #[42])
  let first ← src.read
  assert! first == ByteArray.mk #[42]
  let leftover ← src.readLeftover
  assert! leftover.isEmpty

/-! ### `Connection.getHTTP2`/`setHTTP2` (IO) -/

#eval show IO Unit from do
  let http2Ref ← IO.mkRef false
  let wbRef ← IO.mkRef (none : Option WriteBuffer)
  let conn : Connection := {
    connSendMany := fun _ => pure ()
    connSendAll := fun _ => pure ()
    connSendFile := fun _ _ _ act _ => act
    connClose := pure ()
    connRecv := pure ByteArray.empty
    connWriteBuffer := wbRef
    connHTTP2 := http2Ref
    connMySockAddr := { host := "127.0.0.1", port := 0 }
  }
  assert! (← conn.getHTTP2) == false
  conn.setHTTP2 true
  assert! (← conn.getHTTP2) == true

end Tests.Network.WebApp.Server.Types
