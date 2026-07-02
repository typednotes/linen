/-
  Data.Streaming.Network — Streaming network utilities

  Thin wrappers around the Network socket API for common patterns.
  Mirrors Haskell's `Data.Streaming.Network`.
-/

import Linen.Network.Socket.Blocking

namespace Data.Streaming.Network

open Network.Socket

/-- Application data for a connected client. -/
structure AppData where
  /-- Read data from the connection. -/
  appRead : IO ByteArray
  /-- Write data to the connection. -/
  appWrite : ByteArray → IO Unit
  /-- The client's address. -/
  appSockAddr : SockAddr
  /-- Close the connection. -/
  appClose : IO Unit

/-- Bind a TCP server socket to a port. Returns a listening socket.
    $$\text{bindPortTCP} : \text{UInt16} \to \text{String} \to \text{IO}(\text{Socket}\ \texttt{.listening})$$ -/
def bindPortTCP (port : UInt16) (host : String := "0.0.0.0") : IO (Socket .listening) :=
  listenTCP host port

/-- Connect to a remote TCP server. Returns a connected socket and the address.
    $$\text{getSocketTCP} : \text{String} \to \text{UInt16} \to \text{IO}(\text{Socket}\ \texttt{.connected} \times \text{SockAddr})$$ -/
def getSocketTCP (host : String) (port : UInt16) : IO (Socket .connected × SockAddr) := do
  let s ← socket .inet .stream
  let s ← Blocking.connect s ⟨host, port⟩
  pure (s, ⟨host, port⟩)

/-- Accept a connection on a listening socket, retrying on transient errors.
    $$\text{acceptSafe} : \text{Socket}\ \texttt{.listening} \to \text{IO}(\text{Socket}\ \texttt{.connected} \times \text{SockAddr})$$ -/
def acceptSafe (serverSock : Socket .listening) : IO (Socket .connected × SockAddr) := do
  while true do
    try
      return (← Blocking.accept serverSock)
    catch _ =>
      IO.sleep 10
  unreachable!

/-- Create AppData from a connected socket. -/
def mkAppData (clientSock : Socket .connected) (addr : SockAddr) : AppData :=
  { appRead := Blocking.recv clientSock 4096
  , appWrite := fun data => Blocking.sendAll clientSock data
  , appSockAddr := addr
  , appClose := do let _ ← close clientSock; pure () }

/-- Run a TCP server: accept connections and handle each in a new task.
    $$\text{runTCPServer} : \text{UInt16} \to (\text{AppData} \to \text{IO}(\text{Unit})) \to \text{IO}(\text{Unit})$$ -/
def runTCPServer (port : UInt16) (handler : AppData → IO Unit) : IO Unit := do
  let server ← bindPortTCP port
  try
    while true do
      let (client, addr) ← acceptSafe server
      let appData := mkAppData client addr
      let _task ← IO.asTask (prio := .dedicated) do
        try handler appData catch _ => pure ()
        appData.appClose
  finally
    let _ ← close server

end Data.Streaming.Network
