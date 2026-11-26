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

-- S6: EOF and errors from recvOrClose leave no live socket in the handler.
example : True := by
  fail_if_success
    have _ : SocketM .connected .closed Unit :=
      Program.bind (SocketM.recvOrClose 1) fun outcome =>
        match outcome with
        | .eof => SocketM.close
        | .error _ => Program.pure ()
        | .data _ => SocketM.close
        | .wouldBlock => SocketM.close
  trivial

private def waitReadable : SocketM .connected .connected Unit := do
  match ← SocketM.poll .read 2000 with
  | .ready => pure ()
  | .timeout => Program.lift (throw (IO.userError "peer did not become readable"))
  | .error error => Program.lift (throw error)

private def handleReceive (peer : Socket .connected) (outcome : RecvOutcome) :
    SocketM (SocketM.recvOrCloseState outcome) .closed RecvOutcome :=
  match outcome with
  | .data bytes =>
    Program.ibind SocketM.peerAddress fun _ => closeWith (.data bytes) (by decide)
  | .wouldBlock =>
    Program.ibind SocketM.peerAddress fun _ => closeWith .wouldBlock (by decide)
  | .eof => Program.lift do
    -- A peer that only half-closed can still read. It sees our close before
    -- this handler returns, so recvOrClose closed the native socket already.
    unless (← Blocking.recv peer 1 2000).isEmpty do
      throw (IO.userError "expected the client to close before its EOF handler")
    return .eof
  | .error error => Program.pure (.error error)

private def receiveFromPeer (prepare : Option (Socket .connected → IO Unit)) : IO RecvOutcome :=
  withListenTCP "127.0.0.1" 0 fun listener => do
    let addr ← getSockName listener
    let peerRef ← IO.mkRef (none : Option (Socket .connected))
    try
      SocketM.run .inet .stream <|
        Program.ibind (SocketM.connect addr) fun () =>
        Program.ibind (Program.lift (do
          let (peer, _) ← Blocking.accept listener 2000
          peerRef.set (some peer)
          pure peer)) fun peer =>
        let arrange : SocketM .connected .connected Unit :=
          match prepare with
          | none => pure ()
          | some change => Program.ibind (Program.lift (change peer)) fun () => waitReadable
        Program.ibind arrange fun () =>
        Program.bind (SocketM.recvOrClose 1) (handleReceive peer)
    finally
      if let some peer ← peerRef.get then discard (Network.Socket.close peer)

-- S6: The connected client dispatches to each branch using a real peer.
-- Readiness polling bounds the waits after the peer changes the connection.
#eval show IO Unit from do
  unless (← receiveFromPeer none) matches .wouldBlock do
    throw (IO.userError "idle peer should select the wouldBlock branch")
  match ← receiveFromPeer (some fun peer => Blocking.sendAll peer "x".toUTF8 2000) with
  | .data bytes =>
    unless bytes == "x".toUTF8 do throw (IO.userError "unexpected received payload")
  | _ => throw (IO.userError "sending peer should select the data branch")
  unless (← receiveFromPeer (some (Network.Socket.shutdown · .write))) matches .eof do
    throw (IO.userError "peer FIN should select the EOF branch")
  unless (← receiveFromPeer (some fun peer => do
    Network.Socket.setLinger peer true 0
    discard (Network.Socket.close peer))) matches .error _ do
    throw (IO.userError "peer reset should select the error branch")

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
