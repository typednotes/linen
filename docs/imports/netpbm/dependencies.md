# `netpbm` — dependency plan

Upstream: https://hackage.haskell.org/package/netpbm (version 1.0.4)

## Module list (topological order)

`netpbm` ships a single module, so there is nothing to order:

1. `Graphics.Netpbm` → `Linen.Graphics.Netpbm`

Namespace note: "Netpbm" and "Graphics" name a domain/format (the *Portable
Anymap* family) and a general subject area, not Haskell/GHC itself, so no
Lean-ification rename is needed — unlike, e.g., `repa` → `Shaped`.

## External dependencies

Per the Hackage-import precedence rule in `AGENTS.md`, checked before porting:

- `attoparsec` / `attoparsec-binary` — no Lean stdlib parser-combinator
  package was already present in `linen` from an earlier import, but Lean's
  own standard library ships one: `Std.Internal.Parsec.ByteArray`. It plays
  the same role (byte-level parser combinators over a `ByteArray`) as
  attoparsec plays over a `ByteString`, so it is used directly in place of
  importing anything from Hackage.
- `bytestring` → Lean's `ByteArray`.
- `vector` / `vector-th-unbox` → Lean's `Array`. Upstream's `Storable`-backed
  `Data.Vector.Storable` exists purely to describe the C memory layout of
  pixel types for FFI; Lean's persistent `Array` needs no such layout
  descriptor, so it is used directly and the whole `Storable`/`Unbox`
  machinery (see below) is dropped.
- `storable-record`, `template-haskell` (via `derivingUnbox`) — these exist
  solely to hand-write `Storable`/`Unbox` instances for the pixel types
  (`PpmPixelRGB8`, `PpmPixelRGB16`, `PbmPixel`, `PgmPixel8`, `PgmPixel16`).
  They have no Lean analogue and are dropped entirely, as already anticipated
  in `docs/imports/index.md`.

## Scope and simplifications

- Pixel rasters (`ppmData` in `PPM`) are plain `Array`s of plain structures
  instead of `Data.Vector.Storable` of `Storable`-derived records — the
  `Storable`/`Foreign.Storable.Record` instances and TH `derivingUnbox`
  splices they required are FFI/metaprogramming machinery with no meaning
  for a Lean `Array`, and are dropped.
- The hand-rolled `Show PPM` instance is dropped in favour of `deriving Repr`
  on the underlying structures, following the precedent already set by the
  `colour` port (`docs/imports/colour/dependencies.md`).
- `imagesParser`'s upstream `error "haskell-netpbm bug: ..."` branch, guarding
  against "an ASCII image file produced more than one image", is dropped: it
  is unreachable by construction, since every ASCII body parser here asserts
  `eof` at the end of its own image — matching upstream's own acknowledgement
  of this ("TODO Restructure so that this cannot happen").
- Attoparsec's default backtracking `<|>`/`many` is replicated faithfully on
  top of `Std.Internal.Parsec`'s consumption-sensitive combinators by wrapping
  every composite/multi-byte alternative or repeated sub-parser in `attempt`.
- Attoparsec's `Word8`/`Word16`-instantiated `decimal` silently truncates
  (wraps modulo 256/65536) on overflow rather than failing; parsing into an
  unbounded `Nat` and truncating via `UInt8.ofNat`/`UInt16.ofNat` at the end
  produces an identical result, since truncation commutes with the stepwise
  accumulation `decimal` performs — a non-observable implementation choice,
  not a behavior change.
