/-
  Network.Sendfile — Efficient file sending

  Sends a file over a connected socket. On macOS/Linux, uses the sendfile(2)
  syscall for zero-copy transfers. Falls back to read+send if FFI is unavailable.

  ## Design

  For the initial implementation, we use the fallback path (read + send)
  which works on all platforms. A future enhancement can add FFI to
  the platform-specific sendfile(2) syscall.
-/

import Linen.Network.Socket.Blocking

namespace Network.Sendfile

open Network.Socket

/-- A portion of a file to send. -/
structure FilePart where
  /-- Offset in bytes from start of file. -/
  offset : Nat
  /-- Number of bytes to send. 0 means to end of file. -/
  count : Nat
deriving BEq, Repr

/-- Send a file (or the `FilePart` of it) through `send`, in 64 KiB pieces —
    the transport-independent core of `sendFile`, used as-is over TLS, where
    the bytes must go through the session rather than the socket. A
    `FilePart` with `count = 0` sends to the end of the file, as documented
    (through 1.8.0 it sent nothing).
    $$\text{sendFileWith} : (\text{ByteArray} \to \text{IO}()) \to \text{String} \to \text{Option}(\text{FilePart}) \to \text{IO}()$$ -/
def sendFileWith (send : ByteArray → IO Unit) (path : String) (part : Option FilePart := none) :
    IO Unit := do
  let handle ← IO.FS.Handle.mk path .read
  let (offset, count) := match part with
    | some fp => (fp.offset, if fp.count == 0 then none else some fp.count)
    | none => (0, none)
  -- Skip to offset by reading and discarding bytes
  let mut skipped := 0
  while skipped < offset do
    let data ← handle.read (min (offset - skipped) 65536).toUSize
    if data.size == 0 then break
    skipped := skipped + data.size
  -- Read and send in chunks, up to `count` bytes or the end of the file
  let mut remaining := count
  let mut done := remaining == some 0
  while !done do
    let data ← handle.read (min (remaining.getD 65536) 65536).toUSize
    if data.size == 0 then
      done := true
    else
      send data
      remaining := remaining.map (· - data.size)
      done := remaining == some 0

/-- Send a file (or portion thereof) over a connected socket.
    Uses read+send fallback implementation.
    $$\text{sendFile} : \text{Socket}\ \texttt{.connected} \to \text{String} \to \text{Option}(\text{FilePart}) \to \text{IO}(\text{Unit})$$ -/
def sendFile (sock : Socket .connected) (path : String) (part : Option FilePart := none) : IO Unit :=
  sendFileWith (Blocking.sendAll sock) path part

/-- Send an entire file over a connected socket.
    $$\text{sendFileSimple} : \text{Socket}\ \texttt{.connected} \to \text{String} \to \text{IO}(\text{Unit})$$ -/
@[inline] def sendFileSimple (sock : Socket .connected) (path : String) : IO Unit :=
  sendFile sock path

end Network.Sendfile
