/-
  Linen.Network.WebApp.Server.Conduit — Request body stream handling

  Provides the `ISource` type for reading request bodies with support for
  chunked transfer encoding and known-length bodies. Handles leftover bytes
  from header parsing that belong to the body.

  Ports Hale's `Network.Wai.Handler.Warp.Conduit`.

  ## Design
  - `ISource` wraps a `Source` with remaining byte count tracking
  - For chunked bodies, reads until the stream signals EOF
  - For known-length bodies, reads exactly the specified number of bytes
-/
import Linen.Network.WebApp.Server.Types

namespace Network.WebApp.Server

/-- Input source for request body reading.
    Tracks remaining bytes for known-length bodies.
    $$\text{ISource} = \text{Source} \times \text{IO.Ref}(\text{Option Nat})$$ -/
structure ISource where
  /-- The underlying byte source. -/
  source : Source
  /-- Remaining bytes to read. `none` for chunked encoding. -/
  remaining : IO.Ref (Option Nat)

/-- Create an ISource for a known-length body. -/
def ISource.mkKnown (src : Source) (len : Nat) : IO ISource := do
  let ref ← IO.mkRef (some len)
  return ⟨src, ref⟩

/-- Create an ISource for a chunked body. -/
def ISource.mkChunked (src : Source) : IO ISource := do
  let ref ← IO.mkRef none
  return ⟨src, ref⟩

/-- Read the next chunk from the ISource.
    Returns empty ByteArray when body is exhausted.
    For known-length bodies, limits reads to remaining bytes. -/
def ISource.read (isrc : ISource) : IO ByteArray := do
  let rem ← isrc.remaining.get
  match rem with
  | some 0 => return ByteArray.empty
  | some n =>
    let bs ← isrc.source.read
    if bs.isEmpty then
      isrc.remaining.set (some 0)
      return ByteArray.empty
    else
      let taken := min bs.size n
      let result := bs.extract 0 taken
      isrc.remaining.set (some (n - taken))
      -- If we read more than needed, put leftover back
      if taken < bs.size then
        isrc.source.leftover (bs.extract taken bs.size)
      return result
  | none =>
    -- Chunked: just read from source
    isrc.source.read

end Network.WebApp.Server
