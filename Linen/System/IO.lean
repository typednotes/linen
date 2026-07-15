/-
  Linen.System.IO — default array/chunk buffer sizes for streaming I/O

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.System.IO`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/System/IO.hs),
  module #5 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`). No prior `Linen/System/IO.lean`
  existed, so the direct path is used.

  Provides the default buffer sizes that streamly's array/stream code uses when
  chunking I/O, so an allocation lands on a page-friendly boundary.

  ## Substitutions / deviations

  - **`unsafeInlineIO` dropped.** Upstream's `unsafeInlineIO (IO m) = case m
    realWorld# of …` extracts a pure value from an `IO` action via GHC's
    `realWorld#` primop — GHC-specific machinery with no Lean analogue, and not
    a buffer-size concern. No in-scope module needs it.
  - **`byteArrayOverhead`** upstream is `2 * SIZEOF_HSWORD` (two machine words
    of heap header). Lean's `ByteArray` header layout is not exposed, so this
    is pinned to `2 * 8 = 16` bytes (64-bit word), matching upstream on the
    64-bit platforms this library targets.
-/

namespace System.IO

-- ── Buffer sizes ────────────────────────────────────────────────────────────

/-- Heap-object header overhead of a byte array, in bytes: two machine words
    (`2 * SIZEOF_HSWORD` upstream), fixed to 64-bit words here. -/
def byteArrayOverhead : Nat := 2 * 8

/-- Usable payload size of a byte array allocated with a request of `n` bytes,
    after subtracting the object header overhead (never negative). -/
def arrayPayloadSize (n : Nat) : Nat := n - byteArrayOverhead

/-- Default I/O chunk/buffer size: the payload of a 32 KB request, so the real
    allocation lands at exactly 32 KB including the header. -/
def defaultChunkSize : Nat := arrayPayloadSize (32 * 1024)

end System.IO
