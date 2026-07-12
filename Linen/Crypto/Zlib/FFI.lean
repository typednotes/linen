/-
  Crypto.Zlib.FFI — zlib inflate (decompress) and deflate (compress) FFI

  Opaque handles wrapping zlib's `z_stream *`, via FFI, for the raw
  zlib/RFC 1950 inflate and deflate directions only — `WindowBits 15`, not
  gzip (`WindowBits 31`).

  Ported from Hackage's `zlib` package, `Codec.Zlib`'s low-level
  `initInflate`/`feedInflate`/`finishInflate` FFI surface, together with its
  deflate mirror `initDeflate`/`feedDeflate`/`finishDeflate` (see
  `docs/imports/Zlib/dependencies.md` for the exact scope). Modeled on
  `Linen.Network.TLS.Context`'s OpenSSL-handle pattern: the C shim in
  `ffi/zlib.c` allocates each stream with `lean_alloc_external` and registers
  a GC finalizer that calls `inflateEnd`/`deflateEnd`.

  Deflate compresses at zlib's `Z_DEFAULT_COMPRESSION` level — this codebase
  has no call site needing a specific speed/ratio trade-off (e.g. the PNG
  encoder in `Linen.Codec.Picture.Png.Internal.Export` just needs *some*
  valid zlib-compressed stream), and the existing inflate side exposes no
  level/parameter choice to mirror, so there is no precedent for anything
  more specific than zlib's own default.

  ## Design
  `Inflate`/`Deflate` are persistent handles around a `z_stream`. `feed` may
  be called any number of times with successive input chunks (streaming
  (de)compression); `finish` flushes any output still buffered inside zlib
  and releases the stream. One-shot `decompress`/`compress` convenience
  wrappers are provided for callers with the whole payload in memory already.

  ## Guarantees
  - The underlying `z_stream` is freed exactly once, either by an explicit
    `finish` call or, if the handle is dropped without one, by the GC
    finalizer (`inflateEnd`/`deflateEnd`).
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

/-- Opaque handle to a zlib deflate (compress) stream — a persistent
    `z_stream *`. Created by `initDeflate`, driven by `feedDeflate`, and
    released by `finishDeflate` (or, if abandoned, by the GC finalizer). -/
opaque DeflateHandle : NonemptyType
def Deflate := DeflateHandle.type
instance : Nonempty Deflate := DeflateHandle.property

/-- Start a new deflate (compress) stream, using raw zlib/RFC 1950 framing
    (`WindowBits 15`) at `Z_DEFAULT_COMPRESSION`.
    $$\text{initDeflate} : \text{IO Deflate}$$ -/
@[extern "linen_zlib_deflate_init"]
opaque initDeflate : IO Deflate

/-- Feed one chunk of uncompressed input into the stream, returning
    whatever compressed bytes zlib is able to produce from it so far. May
    be called repeatedly with successive chunks of a larger payload.
    $$\text{feedDeflate} : \text{Deflate} \to \text{ByteArray} \to \text{IO ByteArray}$$ -/
@[extern "linen_zlib_deflate_feed"]
opaque feedDeflate (handle : @& Deflate) (chunk : @& ByteArray) : IO ByteArray

/-- Signal end of input, flush any output still buffered inside zlib, and
    release the stream (`deflateEnd`). Safe to call at most meaningfully
    once; subsequent calls return an empty `ByteArray`.
    $$\text{finishDeflate} : \text{Deflate} \to \text{IO ByteArray}$$ -/
@[extern "linen_zlib_deflate_finish"]
opaque finishDeflate (handle : @& Deflate) : IO ByteArray

/-- One-shot convenience wrapper: compress a complete byte string into a raw
    zlib/RFC 1950 payload (`WindowBits 15`) held entirely in memory, e.g. a
    PNG encoder's filtered scanline stream before writing `IDAT` chunks.
    $$\text{compress} : \text{ByteArray} \to \text{IO ByteArray}$$ -/
def compress (input : ByteArray) : IO ByteArray := do
  let handle ← initDeflate
  let head ← feedDeflate handle input
  let tail ← finishDeflate handle
  return head ++ tail

end Crypto.Zlib
