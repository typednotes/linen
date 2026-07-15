/-
  Linen.Data.Unbox — fixed-size (de)serialization to/from a `MutByteArray`

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Unbox`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Unbox.hs),
  module #9 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  An `Unbox a` instance knows the fixed byte `size` of `a` and how to `peekAt`
  (read) and `pokeAt` (write) an `a` at a byte offset in a `MutByteArray`.
  These operations do not check array bounds — that is the caller's
  responsibility, as upstream.

  ## Substitutions / deviations

  - **No Template Haskell.** Upstream generates the primitive instances with a
    `DERIVE_UNBOXED` CPP macro (and user instances via `Unbox.TH`). Lean has no
    TH, so this is a plain type class with hand-written instances — exactly the
    "hand-written-instance substitute for `Unbox.TH`" the plan prescribes. A
    representative set (`UInt8`/`UInt16`/`UInt32`/`UInt64`/`Bool`) is provided,
    not an exhaustive port of every numeric type.
  - **`Proxy`-passed `sizeOf` → a class constant `size`.** Upstream's
    `sizeOf :: Proxy a -> Int` threads a phantom `Proxy`; Lean resolves the
    type by instance, so it is a bare `size : Nat`. Named `size` (not `sizeOf`)
    to avoid shadowing Lean core's `sizeOf`.
  - **GHC primops → explicit little-endian byte splitting.** Upstream reads via
    `readWord8ArrayAsWord16#` etc.; here each multibyte instance composes the
    individual bytes little-endian (the layout streamly's primops give on the
    little-endian platforms this targets).
  - **`peekAt`/`pokeAt` are pure.** `MutByteArray` is a managed value, so
    `peekAt : Nat → MutByteArray → a` and `pokeAt : Nat → MutByteArray → a →
    MutByteArray` (returning the updated array) rather than `IO`.
  - **`peekByteIndex`/`pokeByteIndex` aliases dropped** — upstream marks them
    `DEPRECATED` in favour of `peekAt`/`pokeAt`, which are the only forms here.
-/

import Linen.Data.MutByteArray.Type

namespace Data

open Data (MutByteArray)

-- ── The Unbox class ─────────────────────────────────────────────────────────

/-- Fixed-size unboxed (de)serialization of `a` to/from a `MutByteArray`. -/
class Unbox (a : Type) where
  /-- Byte size of `a` (upstream `sizeOf`; at least 1). -/
  size : Nat
  /-- Read an `a` from the byte offset (no bounds check). -/
  peekAt : Nat → MutByteArray → a
  /-- Write an `a` at the byte offset, returning the updated array. -/
  pokeAt : Nat → MutByteArray → a → MutByteArray

namespace Unbox

-- ── Concrete instances ──────────────────────────────────────────────────────

instance : Unbox UInt8 where
  size := 1
  peekAt i a := a.bytes.get! i
  pokeAt i a v := { a with bytes := a.bytes.set! i v }

instance : Unbox UInt16 where
  size := 2
  peekAt i a :=
    (a.bytes.get! i).toUInt16 ||| ((a.bytes.get! (i + 1)).toUInt16 <<< 8)
  pokeAt i a v :=
    let b := a.bytes.set! i (v &&& 0xff).toUInt8
    let b := b.set! (i + 1) ((v >>> 8) &&& 0xff).toUInt8
    { a with bytes := b }

instance : Unbox UInt32 where
  size := 4
  peekAt i a :=
    (a.bytes.get! i).toUInt32
      ||| ((a.bytes.get! (i + 1)).toUInt32 <<< 8)
      ||| ((a.bytes.get! (i + 2)).toUInt32 <<< 16)
      ||| ((a.bytes.get! (i + 3)).toUInt32 <<< 24)
  pokeAt i a v :=
    let b := a.bytes.set! i (v &&& 0xff).toUInt8
    let b := b.set! (i + 1) ((v >>> 8) &&& 0xff).toUInt8
    let b := b.set! (i + 2) ((v >>> 16) &&& 0xff).toUInt8
    let b := b.set! (i + 3) ((v >>> 24) &&& 0xff).toUInt8
    { a with bytes := b }

instance : Unbox UInt64 where
  size := 8
  peekAt i a :=
    (a.bytes.get! i).toUInt64
      ||| ((a.bytes.get! (i + 1)).toUInt64 <<< 8)
      ||| ((a.bytes.get! (i + 2)).toUInt64 <<< 16)
      ||| ((a.bytes.get! (i + 3)).toUInt64 <<< 24)
      ||| ((a.bytes.get! (i + 4)).toUInt64 <<< 32)
      ||| ((a.bytes.get! (i + 5)).toUInt64 <<< 40)
      ||| ((a.bytes.get! (i + 6)).toUInt64 <<< 48)
      ||| ((a.bytes.get! (i + 7)).toUInt64 <<< 56)
  pokeAt i a v :=
    let b := a.bytes.set! i (v &&& 0xff).toUInt8
    let b := b.set! (i + 1) ((v >>> 8) &&& 0xff).toUInt8
    let b := b.set! (i + 2) ((v >>> 16) &&& 0xff).toUInt8
    let b := b.set! (i + 3) ((v >>> 24) &&& 0xff).toUInt8
    let b := b.set! (i + 4) ((v >>> 32) &&& 0xff).toUInt8
    let b := b.set! (i + 5) ((v >>> 40) &&& 0xff).toUInt8
    let b := b.set! (i + 6) ((v >>> 48) &&& 0xff).toUInt8
    let b := b.set! (i + 7) ((v >>> 56) &&& 0xff).toUInt8
    { a with bytes := b }

/-- `Bool` is stored as a single byte (`0`/`1`), matching upstream's hand-
    written `Int8` encoding. -/
instance : Unbox Bool where
  size := 1
  peekAt i a := a.bytes.get! i != 0
  pokeAt i a v := { a with bytes := a.bytes.set! i (if v then 1 else 0) }

end Unbox
end Data
