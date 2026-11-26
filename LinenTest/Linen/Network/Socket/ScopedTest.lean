import Linen.Network.Socket.Scoped

open Network.Socket Control.Resource

-- S5: neither close nor use can follow close, and scoped code cannot run a
-- program directly on an alias it supplies.
example (_alias : Socket .fresh) : True := by
  fail_if_success
    have _ : SocketM .fresh .closed Unit :=
      Program.ibind (SocketM.close (state := .fresh)) fun () => SocketM.close
  fail_if_success
    have _ : SocketM .fresh .closed ByteArray :=
      Program.ibind (SocketM.close (state := .fresh)) fun () => SocketM.recvBlocking
  fail_if_success
    have _ := Program.run (by trivial) (SocketM.close (state := .fresh)) _alias
  trivial

private def closeWith (value : α) (valid : state ≠ .closed) : SocketM state .closed α :=
  Program.map (fun () => value) (SocketM.close valid)

-- A non-blocking connect (immediate or in progress) shares an address
-- with a nested server child. The listener remains live after child close.
#eval show IO Unit from do
  let result ← SocketM.run .inet .stream <|
    Program.ibind (SocketM.bindAddress ⟨"127.0.0.1", 0⟩) fun () =>
    Program.ibind SocketM.listen fun () =>
    Program.ibind SocketM.localAddress fun addr =>
    Program.ibind (Program.lift (IO.asTask (prio := .dedicated) do
      SocketM.run .inet .stream <|
        Program.bind (SocketM.connectNB addr) fun status =>
          let sendAndClose : SocketM .connected .closed Unit :=
            Program.ibind (SocketM.sendAll "hello".toUTF8 2000) fun () => SocketM.close
          match status with
          | .connected => sendAndClose
          | .inProgress =>
            Program.ibind (SocketM.poll .write 2000) fun readiness =>
            match readiness with
            | .ready =>
              Program.bind SocketM.connectFinish fun finished =>
                match finished with
                | .connected => sendAndClose
                | .inProgress => Program.lift (throw (IO.userError "connect still pending")) |>.ibind
                    (fun () => SocketM.close)
            | _ => Program.lift (throw (IO.userError "connect did not become writable")) |>.ibind
                (fun () => SocketM.close))) fun client =>
    Program.ibind (SocketM.withAccepted (fun _ =>
      Program.ibind (SocketM.recvBlocking 4096 2000) fun bytes => closeWith bytes (by decide)) 2000)
      fun bytes =>
    Program.ibind SocketM.localAddress fun after =>
    Program.ibind (Program.lift (IO.ofExcept client.get)) fun () =>
    closeWith (bytes, after.port == addr.port) (by decide)
  unless result.1 == "hello".toUTF8 && result.2 do
    throw (IO.userError "nested socket program did not round trip")
