import Linen.Network.HTTP.Simple
import Linen.Network.Socket.Blocking

open Network.HTTP.Client Network.HTTP.Types Network.Socket Control.Resource

-- S1: a response needs a prior send, and no second send can follow a send.
example : True := by
  fail_if_success
    have _ : ExchangeM .ready .received Response := ExchangeM.receive
  fail_if_success
    have _ : ExchangeM .ready .awaitingResponse Unit :=
      Program.ibind ExchangeM.send fun () => ExchangeM.send
  trivial

-- S2: the normal public entry point runs the exchange and closes its TCP socket.
#eval show IO Unit from do
  withListenTCP "127.0.0.1" 0 fun listener => do
    let addr ← getSockName listener
    let peer ← IO.asTask (prio := .dedicated) do
      let (socket, _) ← Blocking.accept listener 2000
      try
        setRecvTimeout socket 2000
        let mut head := ByteArray.empty
        repeat
          let chunk ← Blocking.recv socket 4096 2000
          if chunk.isEmpty then throw (IO.userError "HTTP request ended early")
          head := head ++ chunk
          if (String.fromUTF8! head).endsWith "\r\n\r\n" then break
          if head.size > 4096 then throw (IO.userError "unexpected HTTP headers")
        unless (String.fromUTF8! head).startsWith "GET /resource HTTP/1.1\r\n" do
          throw (IO.userError "wrong scoped HTTP request")
        Blocking.sendAll socket "HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nyes".toUTF8 2000
        unless (← Blocking.recv socket 1 2000).isEmpty do
          throw (IO.userError "HTTP client left transport open")
      finally
        discard (Network.Socket.close socket)
    let request ← Network.HTTP.Simple.parseUrl! s!"http://127.0.0.1:{addr.port}/resource"
    let response ← Network.HTTP.Simple.httpBS { request with timeoutMillis := 2000 }
    IO.ofExcept peer.get
    unless response.body == "yes".toUTF8 do throw (IO.userError "wrong scoped HTTP response")
