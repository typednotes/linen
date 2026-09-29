import Linen.Network.WebApp.Server.Transport

open Network.WebApp.Server
open Network.HTTP2 (connectionPreface)
open Control.Concurrent.Green (Green)

-- ── Detection preserves buffered input and tolerates fragmentation ──

#guard http2PrefacePrefix ByteArray.empty
#guard http2PrefacePrefix "PRI * HTTP/2.0\r\n\r\n".toUTF8
#guard http2PrefacePrefix connectionPreface
#guard http2PrefacePrefix (connectionPreface ++ "extra".toUTF8)
#guard !http2PrefacePrefix "POST / HTTP/1.1\r\n".toUTF8
#guard !http2PrefacePrefix "PRI * HTTP/2.0\r\n\r\nXX".toUTF8

#eval show IO Unit from do
  for chunks in [["P", "RI * HTTP/2.0\r\n\r\n", "SM\r\n\r\n"],
                 ["GET / HTTP/1.1\r\n\r\n"], ["PRI * HTTP/2.0\r\n\r\n"]] do
    let pending ← IO.mkRef chunks
    let recv : IO ByteArray := do
      let piece ← pending.modifyGet fun
        | [] => ("", [])
        | x :: xs => (x, xs)
      return piece.toUTF8
    let reader ← ByteSource.buffered recv
    let transport : HttpTransport := {
      nextChunk := do return some (← (recv : IO _)), reader,
      sink := {
        send := fun _ => pure (), sendIO := fun _ => pure (),
        sendFile := fun _ _ => pure (), rawRecv := recv, rawSend := fun _ => pure () } }
    let detected ← Green.block transport.detectHttp2 (← Std.CancellationToken.new)
    let expected := if chunks == ["PRI * HTTP/2.0\r\n\r\n"] then none
      else some (chunks == ["P", "RI * HTTP/2.0\r\n\r\n", "SM\r\n\r\n"])
    unless detected == expected do
      throw (IO.userError s!"detection: {chunks}")
    unless (← reader.unread) == (String.join chunks).toUTF8 do
      throw (IO.userError "detection consumed bytes")
