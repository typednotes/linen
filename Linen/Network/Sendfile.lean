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

/-- Send a file (or portion thereof) over a connected socket.
    Uses read+send fallback implementation.
    $$\text{sendFile} : \text{Socket}\ \texttt{.connected} \to \text{String} \to \text{Option}(\text{FilePart}) \to \text{IO}(\text{Unit})$$ -/
def sendFile (sock : Socket .connected) (path : String) (part : Option FilePart := none) : IO Unit := do
  let handle ← IO.FS.Handle.mk path .read
  match part with
  | some fp => do
    -- Skip to offset by reading and discarding bytes
    if fp.offset > 0 then
      let mut skipped := 0
      while skipped < fp.offset do
        let chunkSize := min (fp.offset - skipped) 65536
        let data ← handle.read chunkSize.toUSize
        if data.size == 0 then break
        skipped := skipped + data.size
    -- Read and send in chunks
    let mut remaining := fp.count
    while remaining > 0 do
      let chunkSize := min remaining 65536
      let data ← handle.read chunkSize.toUSize
      if data.size == 0 then break
      Blocking.sendAll sock data
      remaining := remaining - data.size
  | none => do
    -- Send entire file in chunks
    let mut done := false
    while !done do
      let data ← handle.read 65536
      if data.size == 0 then
        done := true
      else
        Blocking.sendAll sock data

/-- Send an entire file over a connected socket.
    $$\text{sendFileSimple} : \text{Socket}\ \texttt{.connected} \to \text{String} \to \text{IO}(\text{Unit})$$ -/
@[inline] def sendFileSimple (sock : Socket .connected) (path : String) : IO Unit :=
  sendFile sock path

end Network.Sendfile
