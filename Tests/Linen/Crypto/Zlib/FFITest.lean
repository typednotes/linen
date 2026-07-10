/-
  Tests for `Linen.Crypto.Zlib.FFI`.

  These bindings are `@[extern]` IO actions backed by the system zlib, so
  behaviour is checked with `#eval` (a thrown error fails the build), as in
  the other native FFI tests (e.g. `Tests/Linen/Network/Socket/FFITest.lean`).
  Running these requires the `linenffi` native library, which
  `precompileModules` makes available to the interpreter.
-/
import Linen.Crypto.Zlib.FFI

open Crypto.Zlib

namespace Tests.Crypto.Zlib.FFI

/-- The raw zlib/RFC 1950 bytes produced by Python's
    `zlib.compress(b"hello world")` — a 2-byte header, a DEFLATE block, and a
    4-byte Adler-32 checksum trailer. -/
private def helloWorldZlib : ByteArray :=
  ByteArray.mk #[120, 156, 203, 72, 205, 201, 201, 87, 40, 207, 47, 202, 73, 1, 0, 26, 11, 4, 93]

private def helloWorld : ByteArray :=
  String.toUTF8 "hello world"

-- One-shot `decompress` recovers the original bytes.
#eval show IO Unit from do
  let out ← decompress helloWorldZlib
  unless out == helloWorld do
    throw (IO.userError s!"decompress mismatch: got {out.toList}")

-- The same round-trip driven by the streaming handle API directly: feed the
-- whole compressed payload in one chunk, then finish.
#eval show IO Unit from do
  let handle ← initInflate
  let head ← feedInflate handle helloWorldZlib
  let tail ← finishInflate handle
  unless head ++ tail == helloWorld do
    throw (IO.userError s!"streaming round-trip mismatch: got {(head ++ tail).toList}")

-- Feeding the compressed payload split across several small chunks (as a
-- real streaming consumer would) still recovers the exact original bytes.
#eval show IO Unit from do
  let handle ← initInflate
  let chunks := #[helloWorldZlib.extract 0 5, helloWorldZlib.extract 5 12,
                  helloWorldZlib.extract 12 helloWorldZlib.size]
  let mut acc := ByteArray.empty
  for chunk in chunks do
    let out ← feedInflate handle chunk
    acc := acc ++ out
  let tail ← finishInflate handle
  acc := acc ++ tail
  unless acc == helloWorld do
    throw (IO.userError s!"chunked round-trip mismatch: got {acc.toList}")

-- Decompressing empty input (no data fed) after `finish` yields an empty
-- `ByteArray` rather than throwing.
#eval show IO Unit from do
  let handle ← initInflate
  let out ← finishInflate handle
  unless out == ByteArray.empty do
    throw (IO.userError s!"expected empty output for empty stream, got {out.toList}")

-- Feeding malformed (non-zlib) input surfaces a decompression error rather
-- than succeeding silently.
#eval show IO Unit from do
  let handle ← initInflate
  let bogus := String.toUTF8 "not zlib data at all"
  let result ← try
      let _ ← feedInflate handle bogus
      pure true
    catch _ =>
      pure false
  unless !result do
    throw (IO.userError "expected feedInflate on bogus data to fail")

end Tests.Crypto.Zlib.FFI
