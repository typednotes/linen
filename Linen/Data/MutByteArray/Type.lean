/-
  Linen.Data.MutByteArray.Type — a mutable byte array over Lean's `ByteArray`

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.MutByteArray.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/MutByteArray/Type.hs),
  module #10 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  The fixed-size mutable byte buffer that backs streamly's unboxed arrays and
  `Unbox`-serialized data.

  ## Substitutions / deviations

  - **`MutableByteArray# RealWorld` → Lean `ByteArray`.** Upstream wraps GHC's
    raw pinned/unpinned mutable primitive. Lean has no such primop; the port
    wraps the managed `ByteArray`, which already supports destructive update
    in place when uniquely referenced. Operations are therefore pure functions
    returning the updated array rather than `IO ()` primop calls.
  - **Pinned/unpinned is a flag, not a memory placement.** Lean's GC does not
    expose object pinning, so `pinned` is carried as a boolean tag. `pin`/
    `unpin` set/clear it; `newPinned` constructs with it set. This records
    intent faithfully (`isPinned` reflects it) without a real pinned heap —
    the same "GHC-specific machinery, no Lean analogue" call the plan makes for
    `ghc-prim`. `newAlignedPinned`'s explicit alignment argument is likewise
    not meaningful for a managed array and is dropped.
-/

namespace Data

-- ── The mutable byte array ──────────────────────────────────────────────────

/-- A mutable byte array: Lean's managed `ByteArray` plus a `pinned` intent
    flag (Lean's GC exposes no real pinning — see the module deviations). -/
structure MutByteArray where
  /-- The underlying bytes. -/
  bytes : ByteArray
  /-- Whether the array is (intended as) pinned. -/
  pinned : Bool := false
  deriving Inhabited

namespace MutByteArray

/-- A fresh unpinned array of `n` zero bytes. -/
def new (n : Nat) : MutByteArray := { bytes := ByteArray.mk (Array.replicate n 0) }

/-- A fresh pinned array of `n` zero bytes (`pinnedNew`/`new'` upstream). -/
def newPinned (n : Nat) : MutByteArray :=
  { bytes := ByteArray.mk (Array.replicate n 0), pinned := true }

/-- The size of the array in bytes. -/
def length (a : MutByteArray) : Nat := a.bytes.size

/-- Is the array (intended as) pinned? -/
def isPinned (a : MutByteArray) : Bool := a.pinned

/-- Return the array marked pinned (a real copy-to-pinned upstream). -/
def pin (a : MutByteArray) : MutByteArray := { a with pinned := true }

/-- Return the array marked unpinned (a real copy-to-unpinned upstream). -/
def unpin (a : MutByteArray) : MutByteArray := { a with pinned := false }

/-- The empty (zero-length) array (`empty`/`nil` upstream). -/
def empty : MutByteArray := new 0

end MutByteArray
end Data
