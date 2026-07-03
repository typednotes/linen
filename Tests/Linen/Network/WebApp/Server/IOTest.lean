/-
  Tests for `Linen.Network.WebApp.Server.IO`.
-/
import Linen.Network.WebApp.Server.IO

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.IO

/-- A `Connection` whose `connSendAll`/`connSendMany` record every call. -/
private def mkRecordingConnection : IO (Connection × IO.Ref (List ByteArray)) := do
  let sent ← IO.mkRef []
  let writeBuf ← IO.mkRef none
  let http2 ← IO.mkRef false
  let conn : Connection := {
    connSendMany := fun chunks => sent.modify (· ++ chunks)
    connSendAll := fun bs => sent.modify (· ++ [bs])
    connSendFile := fun _ _ _ hook _ => hook
    connClose := pure ()
    connRecv := pure ByteArray.empty
    connWriteBuffer := writeBuf
    connHTTP2 := http2
    connMySockAddr := { host := "127.0.0.1", port := 0 }
  }
  return (conn, sent)

#eval show IO Unit from do
  let (conn, sent) ← mkRecordingConnection
  connSendByteArray conn "hello".toUTF8
  let recorded ← sent.get
  assert! recorded == ["hello".toUTF8]

#eval show IO Unit from do
  let (conn, sent) ← mkRecordingConnection
  connSendByteArrays conn ["a".toUTF8, "b".toUTF8, "c".toUTF8]
  let recorded ← sent.get
  assert! recorded == ["a".toUTF8, "b".toUTF8, "c".toUTF8]

end Tests.Network.WebApp.Server.IO
