/-
  Linen.Network.Socket.Blocking — Blocking convenience wrappers

  Thin wrappers around the non-blocking socket API that loop on `wouldBlock`
  and present the old blocking-style signatures. Intended for tests, scripts,
  and code that doesn't need event-loop integration.

  These functions call the underlying non-blocking FFI and, on EAGAIN, wait
  for readiness with `poll` before retrying. They work on sockets in blocking
  mode (the default after `socket()`) and on non-blocking ones.

  ## Timeouts, and why the retry is a `poll`

  Each retry loop takes a `timeoutMillis` and throws once it elapses, so a
  peer that connects and then stalls cannot hang the caller forever.

  An earlier version retried EAGAIN immediately, with no wait. That busy-spun
  at 100% CPU on a non-blocking socket, and — worse — it made socket-level
  timeouts unusable: `SO_RCVTIMEO` reports expiry *as* EAGAIN, so setting one
  turned a silent block into a hot loop. Waiting on `poll` fixes both, and
  matches what `connectFinishLoop` in this module already did.

  ## No `partial`

  Each retry loop is a `while true do ... ; unreachable!`: the loop body
  `return`s on every terminating outcome, so `unreachable!` is only ever
  reached along a path the type checker — not the termination checker —
  needs satisfied. No self-recursion, so nothing to prove.
-/

import Linen.Network.Socket

namespace Network.Socket.Blocking

open Network.Socket

/-- How long these wrappers wait for readiness before giving up, in
    milliseconds. Matches `connectFinishLoop`'s long-standing connect timeout
    and `Network.HTTP.Client.defaultTimeoutMillis`. -/
def defaultTimeoutMillis : Nat := 30000

/-- Wait for the socket to become ready, or fail saying which operation gave
    up and after how long. `0` waits indefinitely, which is the old behaviour
    and is available for callers that genuinely want it. -/
private def waitReady (s : Socket state) (mode : PollMode) (timeoutMillis : Nat)
    (op : String) : IO Unit := do
  if timeoutMillis == 0 then
    -- No deadline: still poll rather than spin, just without one.
    match ← Network.Socket.poll s mode 0 with
    | .error e => throw e
    | _        => pure ()
  else
    match ← Network.Socket.poll s mode timeoutMillis with
    | .ready   => pure ()
    | .timeout => throw (IO.userError s!"{op} timed out after {timeoutMillis}ms")
    | .error e => throw e

/-- Blocking accept: loops until a connection is accepted or an error occurs.
    $$\text{accept} : \text{Socket}\ \texttt{.listening} \to \text{IO}(\text{Socket}\ \texttt{.connected} \times \text{SockAddr})$$ -/
def accept (s : Socket .listening) (timeoutMillis : Nat := defaultTimeoutMillis) :
    IO (Socket .connected × SockAddr) := do
  while true do
    match ← Network.Socket.accept s with
    | .accepted sock addr => return (sock, addr)
    | .wouldBlock => waitReady s .read timeoutMillis "accept"
    | .error e => throw e
  unreachable!

/-- Wait for a connecting socket to become writable, then finish the
    handshake, looping while it is still in progress. -/
private def connectFinishLoop (s : Socket .connecting) : IO (Socket .connected) := do
  let mut cur := s
  while true do
    -- Wait for the socket to become writable before checking connect status.
    -- getsockopt(SO_ERROR)==0 is ambiguous without a writability check:
    -- it can mean "connected" or "still connecting".
    match ← Network.Socket.poll cur .write 30000 with
    | .timeout => throw (IO.userError "connect timed out")
    | .error e => throw e
    | .ready =>
      match ← Network.Socket.connectFinish cur with
      | .connected sock => return sock
      | .inProgress sock => cur := sock
      | .refused e => throw e
  unreachable!

/-- Blocking connect: loops until connected or an error occurs.

    `Network.Socket.connect` delegates to `socketConnectNB`, which sets
    `O_NONBLOCK` on the file descriptor. After the connect loop finishes
    we restore blocking mode so that subsequent send/recv and TLS
    handshakes (`SSL_connect`) work correctly on a blocking fd.

    $$\text{connect} : \text{Socket}\ \texttt{.fresh} \to \text{SockAddr} \to \text{IO}(\text{Socket}\ \texttt{.connected})$$ -/
def connect (s : Socket .fresh) (addr : SockAddr) : IO (Socket .connected) := do
  let sock ← match ← Network.Socket.connect s addr with
    | .connected sock => pure sock
    | .inProgress sock => connectFinishLoop sock
    | .refused e => throw e
  -- socketConnectNB sets O_NONBLOCK; restore blocking mode
  Network.Socket.setNonBlocking sock false
  pure sock

/-- Blocking send: returns bytes sent, throws on error.
    $$\text{send} : \text{Socket}\ \texttt{.connected} \to \text{ByteArray} \to \text{IO}\ \mathbb{N}$$ -/
def send (s : Socket .connected) (data : ByteArray)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO Nat := do
  while true do
    match ← Network.Socket.send s data with
    | .sent n => return n
    | .wouldBlock => waitReady s .write timeoutMillis "send"
    | .error e => throw e
  unreachable!

/-- Blocking sendAll: sends all bytes, looping on partial writes and wouldBlock.
    $$\text{sendAll} : \text{Socket}\ \texttt{.connected} \to \text{ByteArray} \to \text{IO}(\text{Unit})$$ -/
def sendAll (s : Socket .connected) (data : ByteArray)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO Unit := do
  let mut offset := 0
  while offset < data.size do
    match ← Network.Socket.send s (data.extract offset data.size) with
    | .sent n => offset := offset + n
    | .wouldBlock => waitReady s .write timeoutMillis "send"
    | .error e => throw e

/-- Blocking recv: returns received bytes, throws on error, returns empty on EOF.
    $$\text{recv} : \text{Socket}\ \texttt{.connected} \to \mathbb{N} \to \text{IO ByteArray}$$ -/
def recv (s : Socket .connected) (maxlen : Nat := 4096)
    (timeoutMillis : Nat := defaultTimeoutMillis) : IO ByteArray := do
  while true do
    match ← Network.Socket.recv s maxlen with
    | .data bytes => return bytes
    | .wouldBlock => waitReady s .read timeoutMillis "recv"
    | .eof => return ByteArray.empty
    | .error e => throw e
  unreachable!

end Network.Socket.Blocking
