/-
  Tests for `Linen.Network.WebApp.Server.SendFile`.
-/
import Linen.Network.WebApp.Server.SendFile

open Network.WebApp.Server

namespace Tests.Network.WebApp.Server.SendFile

/-- A `Connection` whose `connSendAll`/`connSendFile` record every call. -/
private def mkRecordingConnection :
    IO (Connection × IO.Ref (List ByteArray) × IO.Ref (Option (String × Nat × Nat))) := do
  let sent ← IO.mkRef []
  let fileCall ← IO.mkRef none
  let writeBuf ← IO.mkRef none
  let http2 ← IO.mkRef false
  let conn : Connection := {
    connSendMany := fun chunks => sent.modify (· ++ chunks)
    connSendAll := fun bs => sent.modify (· ++ [bs])
    connSendFile := fun path offset count hook _ => do
      fileCall.set (some (path, offset, count))
      hook
    connClose := pure ()
    connRecv := pure ByteArray.empty
    connWriteBuffer := writeBuf
    connHTTP2 := http2
    connMySockAddr := { host := "127.0.0.1", port := 0 }
  }
  return (conn, sent, fileCall)

#eval show IO Unit from do
  -- Headers are sent via connSendAll before connSendFile is invoked.
  let (conn, sent, fileCall) ← mkRecordingConnection
  let hookRan ← IO.mkRef false
  sendFileWithConn conn "/tmp/does-not-need-to-exist" 4 6 (hookRan.set true)
    ["h1".toUTF8, "h2".toUTF8]
  let recordedSent ← sent.get
  assert! recordedSent == ["h1".toUTF8, "h2".toUTF8]
  let recordedFile ← fileCall.get
  assert! recordedFile == some ("/tmp/does-not-need-to-exist", 4, 6)
  let ranHook ← hookRan.get
  assert! ranHook

#eval show IO Unit from do
  -- readSendFile reads the requested slice and forwards it via connSendAll.
  let (conn, sent, _) ← mkRecordingConnection
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "0123456789ABCDEF"
  handle.flush
  readSendFile conn path.toString 4 6
  IO.FS.removeFile path
  let recorded ← sent.get
  assert! recorded == ["456789".toUTF8]

end Tests.Network.WebApp.Server.SendFile
