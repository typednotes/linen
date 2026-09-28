/-
  Tests for `Linen.Network.WebSockets.Connection`.

  Drives `mkConnection` with an in-memory `send`/`recv` pair: `send`
  appends to a ref, `recv` pops pre-queued chunks (or returns empty on
  exhaustion, like a closed socket).
-/
import Linen.Network.WebSockets.Connection

open Network.WebSockets

namespace Tests.Network.WebSockets.Connection

/-- Build a connection over an in-memory queue of `recv` chunks, returning
    the connection plus a ref recording every `send`. -/
private def mkTestConnection (recvQueue : List ByteArray) :
    IO (Connection × IO.Ref (List ByteArray)) := do
  let sent ← IO.mkRef []
  let queue ← IO.mkRef recvQueue
  let recv : IO ByteArray := do
    match ← queue.get with
    | [] => pure ByteArray.empty
    | b :: rest => queue.set rest; pure b
  let conn ← mkConnection (fun bs => sent.modify (· ++ [bs])) recv
  return (conn, sent)

#eval show IO Unit from do
  let (conn, sent) ← mkTestConnection []
  conn.sendText "hi"
  let recorded ← sent.get
  assert! recorded == [Frame.encode { fin := true, opcode := .text, mask := none, payload := "hi".toUTF8 }]

#eval show IO Unit from do
  let (conn, sent) ← mkTestConnection []
  conn.sendBinary (ByteArray.mk #[1, 2, 3])
  let recorded ← sent.get
  assert! recorded ==
    [Frame.encode { fin := true, opcode := .binary, mask := none, payload := ByteArray.mk #[1, 2, 3] }]

#eval show IO Unit from do
  let (conn, sent) ← mkTestConnection []
  conn.sendPing (ByteArray.mk #[9])
  let recorded ← sent.get
  assert! recorded ==
    [Frame.encode { fin := true, opcode := .ping, mask := none, payload := ByteArray.mk #[9] }]

#eval show IO Unit from do
  let (conn, sent) ← mkTestConnection []
  conn.sendClose CloseCode.normal "bye"
  let recorded ← sent.get
  let expectedPayload := ByteArray.mk #[0x03, 0xE8] ++ "bye".toUTF8  -- 1000 big-endian ++ "bye"
  assert! recorded ==
    [Frame.encode { fin := true, opcode := .close, mask := none, payload := expectedPayload }]
  let state ← conn.getState
  assert! state == .closing

-- A regular (non-control) frame is returned as its payload.
#eval show IO Unit from do
  let textFrame := Frame.encode { fin := true, opcode := .text, mask := none, payload := "hello".toUTF8 }
  let (conn, _) ← mkTestConnection [textFrame]
  let data ← conn.receiveData
  assert! data == "hello".toUTF8

-- A close frame transitions the connection to `.closed` and yields empty data.
#eval show IO Unit from do
  let closeFrame := Frame.encode { fin := true, opcode := .close, mask := none, payload := ByteArray.empty }
  let (conn, _) ← mkTestConnection [closeFrame]
  let data ← conn.receiveData
  assert! data == ByteArray.empty
  let state ← conn.getState
  assert! state == .closed

-- A ping frame triggers an auto-pong and yields whatever `recv` returns next.
#eval show IO Unit from do
  let pingFrame := Frame.encode { fin := true, opcode := .ping, mask := none, payload := ByteArray.empty }
  let (conn, sent) ← mkTestConnection [pingFrame, "next-chunk".toUTF8]
  let data ← conn.receiveData
  assert! data == "next-chunk".toUTF8
  let recorded ← sent.get
  assert! recorded ==
    [Frame.encode { fin := true, opcode := .pong, mask := none, payload := ByteArray.empty }]

end Tests.Network.WebSockets.Connection
