import Linen.Network.Socket.Scoped
import LinenTest.Linen.Network.TLS.TestSupport

open Network.Socket Network.TLS Control.Resource

example : True := by
  fail_if_success
    have _ : TLSM .open_ .closed Unit :=
      Program.ibind TLSM.close fun () => TLSM.write ByteArray.empty
  trivial

-- Exercise the actual TLS handshake and I/O, with scoped TLS nested under
-- both TCP sockets. The test certificate is verified against localhost.
#eval Tests.Network.TLS.TestSupport.withTestCert fun certPath keyPath => do
  let serverCtx ← createContext certPath keyPath
  let clientCtx ← createClientContextWithCA certPath
  withListenTCP "127.0.0.1" 0 fun server => do
    let addr ← getSockName server
    let serverTask ← IO.asTask (prio := .dedicated) do
      let (conn, _) ← Blocking.accept server 2000
      try
        TLSM.runServer serverCtx conn.raw (do
          let bytes ← TLSM.read 4096
          TLSM.write (bytes ++ "!".toUTF8))
      finally
        discard (Network.Socket.close conn)
    let response ← SocketM.run .inet .stream <|
      Program.ibind (SocketM.connect addr) fun () =>
      Program.ibind (SocketM.withClientTLS clientCtx "localhost" <|
        Program.ibind (do TLSM.write "ping".toUTF8; TLSM.read 4096) fun bytes =>
          Program.map (fun () => bytes) TLSM.close) fun bytes =>
      Program.map (fun () => bytes) SocketM.close
    IO.ofExcept serverTask.get
    unless response == "ping!".toUTF8 do throw (IO.userError "scoped TLS did not round trip")
