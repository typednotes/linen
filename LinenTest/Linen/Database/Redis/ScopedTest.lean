import Linen.Database.Redis.Scoped
import Linen.Network.Socket.Blocking

open Database.Redis Network.Socket Control.Resource

example : True := by
  fail_if_success
    have _ : ConnectionM .open_ .closed Unit :=
      Program.ibind ConnectionM.close fun () => Program.map (fun _ => ())
        (ConnectionM.command (α := ByteArray) ["PING".toUTF8])
  trivial

#eval show IO Unit from do
  withListenTCP "127.0.0.1" 0 fun listener => do
    let addr ← getSockName listener
    let peer ← IO.asTask (prio := .dedicated) do
      let (socket, _) ← Blocking.accept listener 2000
      try
        setRecvTimeout socket 2000
        let request ← Blocking.recv socket 4096 2000
        unless request == Protocol.renderRequest ["PING".toUTF8] do
          throw (IO.userError "unexpected scoped Redis command")
        Blocking.sendAll socket "$4\r\nPONG\r\n".toUTF8 2000
        unless (← Blocking.recv socket 4096 2000).isEmpty do
          throw (IO.userError "Redis connection did not close")
      finally
        discard (Network.Socket.close socket)
    let info : Connection.ConnectInfo := { connectAddr := .hostPort addr.host addr.port }
    let result ← ConnectionM.run info <|
      Program.ibind (ConnectionM.command (α := ByteArray) ["PING".toUTF8]) fun reply =>
      Program.map (fun () => reply) ConnectionM.close
    IO.ofExcept peer.get
    match result with
    | .ok reply => unless reply == "PONG".toUTF8 do throw (IO.userError "wrong Redis reply")
    | .error _ => throw (IO.userError "could not decode Redis reply")
