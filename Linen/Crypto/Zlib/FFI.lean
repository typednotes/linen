/-
  Crypto.Zlib.FFI — zlib inflate (decompress) FFI

  Opaque handle wrapping zlib's `z_stream *`, via FFI, for the raw
  zlib/RFC 1950 inflate (decompress) direction only — `WindowBits 15`, not
  gzip (`WindowBits 31`), and no compress/deflate.

  Ported from Hackage's `zlib` package, `Codec.Zlib`'s low-level
  `initInflate`/`feedInflate`/`finishInflate` FFI surface (see
  `docs/imports/Zlib/dependencies.md` for the exact scope). Modeled on
  `Linen.Network.TLS.Context`'s OpenSSL-handle pattern: the C shim in
  `ffi/zlib.c` allocates the stream with `lean_alloc_external` and registers
  a GC finalizer that calls `inflateEnd`.

  ## Design
  `Inflate` is a persistent handle around a `z_stream`. `feed` may be called
  any number of times with successive input chunks (streaming decompression);
  `finish` flushes any output still buffered inside zlib and releases the
  stream. A one-shot `decompress` convenience wrapper is provided for callers
  with the whole compressed payload in memory already.

  ## Guarantees
  - The underlying `z_stream` is freed exactly once, either by an explicit
    `finish` call or, if the handle is dropped without one, by the GC
    finalizer (`inflateEnd`).
  - `feed`/`finish` called again after the stream has already reached
    `Z_STREAM_END` (or been finished) simply return an empty `ByteArray`.
-/
namespace Crypto.Zlib

/-- Opaque handle to a zlib inflate (decompress) stream — a persistent
    `z_stream *`. Created by `initInflate`, driven by `feed`, and released
    by `finish` (or, if abandoned, by the GC finalizer). -/
opaque InflateHandle : NonemptyType
def Inflate := InflateHandle.type
instance : Nonempty Inflate := InflateHandle.property

/-- Start a new inflate (decompress) stream, using raw zlib/RFC 1950 framing
    (`WindowBits 15`).
    $$\text{initInflate} : \text{IO Inflate}$$ -/
@[extern "linen_zlib_inflate_init"]
opaque initInflate : IO Inflate

/-- Feed one chunk of compressed input into the stream, returning whatever
    decompressed bytes zlib is able to produce from it so far. May be called
    repeatedly with successive chunks of a larger compressed payload.
    $$\text{feedInflate} : \text{Inflate} \to \text{ByteArray} \to \text{IO ByteArray}$$ -/
@[extern "linen_zlib_inflate_feed"]
opaque feedInflate (handle : @& Inflate) (chunk : @& ByteArray) : IO ByteArray

/-- Signal end of input, flush any output still buffered inside zlib, and
    release the stream (`inflateEnd`). Safe to call at most meaningfully
    once; subsequent calls return an empty `ByteArray`.
    $$\text{finishInflate} : \text{Inflate} \to \text{IO ByteArray}$$ -/
@[extern "linen_zlib_inflate_finish"]
opaque finishInflate (handle : @& Inflate) : IO ByteArray

/-- One-shot convenience wrapper: decompress a complete raw zlib/RFC 1950
    payload (`WindowBits 15`) held entirely in memory, e.g. a PDF stream's
    `FlateDecode`-filtered data.
    $$\text{decompress} : \text{ByteArray} \to \text{IO ByteArray}$$ -/
def decompress (input : ByteArray) : IO ByteArray := do
  let handle ← initInflate
  let head ← feedInflate handle input
  let tail ← finishInflate handle
  return head ++ tail

end Crypto.Zlib
