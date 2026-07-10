/-
  Tests for `Linen.Data.PDF.Stream`.

  Most of this module is IO-based (streams are mutable, ref-backed values),
  so behaviour is checked with `#eval` (a thrown error fails the build), as
  in `Tests/Linen/Crypto/Zlib/FFITest.lean`. `decompress` depends on the
  `linenffi` native library (`Crypto.Zlib`'s FFI handle), which
  `precompileModules` makes available to the interpreter when building this
  file as part of the `Tests` library.
-/
import Linen.Data.PDF.Stream
import Std.Internal.Parsec.ByteArray

open Data.PDF.Stream

namespace Tests.Data.PDF.Stream

/-- Fail the enclosing `IO` action with `msg` unless `cond` holds. -/
private def check (cond : Bool) (msg : String) : IO Unit :=
  unless cond do throw (IO.userError msg)

-- ── read / unRead round-trip ──

-- Reading back a pushed-back chunk returns exactly that chunk, and the
-- stream then continues with whatever it would otherwise have yielded.
#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "hello")
  let some chunk ← read s | throw (IO.userError "expected a chunk")
  check (chunk == String.toUTF8 "hello") "unexpected first chunk"
  unRead chunk s
  let some again ← read s | throw (IO.userError "expected the pushed-back chunk")
  check (again == chunk) "unRead did not replay the same chunk"
  let .none ← read s | throw (IO.userError "expected EOF after the replayed chunk")
  pure ()

-- ── fromList / toList round-trip ──

#eval show IO Unit from do
  let chunks := [String.toUTF8 "foo", String.toUTF8 "bar", String.toUTF8 "baz"]
  let s ← fromList chunks
  let out ← toList s
  check (out == chunks) s!"fromList/toList round-trip mismatch: got {out.map (·.toList)}"

-- `fromByteString` composed with `toList` yields the whole array back as a
-- single chunk.
#eval show IO Unit from do
  let bytes := String.toUTF8 "round trip me"
  let s ← fromByteString bytes
  let out ← toList s
  check (out == [bytes]) "fromByteString/toList round-trip mismatch"

-- An empty source is immediately exhausted.
#eval show IO Unit from do
  let s ← fromByteString ByteArray.empty
  let out ← toList s
  check (out == []) "empty fromByteString should yield no chunks"

-- ── countInput ──

#eval show IO Unit from do
  let s ← fromList [String.toUTF8 "aaa", String.toUTF8 "bb", String.toUTF8 "c"]
  let (counted, total) ← countInput s
  let some _ ← read counted | throw (IO.userError "expected chunk 1")
  check ((← total) == 3) s!"count after 1st read: {← total}"
  let some _ ← read counted | throw (IO.userError "expected chunk 2")
  check ((← total) == 5) s!"count after 2nd read: {← total}"
  let some _ ← read counted | throw (IO.userError "expected chunk 3")
  check ((← total) == 6) s!"count after 3rd read: {← total}"
  let .none ← read counted | throw (IO.userError "expected EOF")
  check ((← total) == 6) "count unchanged at EOF"

-- Pushing a chunk back through the *counted* wrapper decrements the total
-- again, matching upstream `countInput`'s accounting.
#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "abcdef")
  let (counted, total) ← countInput s
  let some chunk ← read counted | throw (IO.userError "expected a chunk")
  check ((← total) == 6) "count after read"
  unRead chunk counted
  check ((← total) == 0) "count after unRead through the wrapper"

-- ── takeBytes ──

#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "0123456789")
  let capped ← takeBytes 4 s
  let some chunk ← read capped | throw (IO.userError "expected a chunk")
  check (chunk == String.toUTF8 "0123") s!"takeBytes truncation: got {chunk.toList}"
  let .none ← read capped | throw (IO.userError "expected EOF after the cap")
  pure ()

-- The untaken tail is left on the underlying stream, positioned right
-- after the taken prefix.
#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "0123456789")
  let capped ← takeBytes 4 s
  let some _ ← read capped | throw (IO.userError "expected a chunk")
  let some rest ← read s | throw (IO.userError "expected the untaken tail on the source")
  check (rest == String.toUTF8 "456789") s!"unexpected tail: got {rest.toList}"

-- ── readExactly ──

#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "hello world")
  let prefixBytes ← readExactly 5 s
  check (prefixBytes == String.toUTF8 "hello") s!"readExactly prefix: got {prefixBytes.toList}"
  let some rest ← read s | throw (IO.userError "expected the remainder on the source")
  check (rest == String.toUTF8 " world") s!"readExactly did not leave the rest intact: {rest.toList}"

-- A short read throws rather than silently returning fewer bytes.
#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "abc")
  let ok ← try
      let _ ← readExactly 10 s
      pure true
    catch _ =>
      pure false
  check (!ok) "expected readExactly to fail on a short stream"

-- ── parseFromStream ──

/-- Parse a fixed 4-byte ASCII header, returned as a `String`. -/
private def headerParser : Std.Internal.Parsec.ByteArray.Parser String := do
  let slice ← Std.Internal.Parsec.ByteArray.take 4
  return String.fromUTF8! slice.toByteArray

#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "HEADrest of the payload")
  let header ← parseFromStream headerParser s
  check (header == "HEAD") s!"parseFromStream result: {header}"
  let some rest ← read s | throw (IO.userError "expected the unconsumed suffix on the stream")
  check (rest == String.toUTF8 "rest of the payload")
    s!"stream did not resume right after the parsed prefix: {rest.toList}"

-- A failing parse leaves the stream's contents untouched.
#eval show IO Unit from do
  let s ← fromByteString (String.toUTF8 "short")
  let tooLong : Std.Internal.Parsec.ByteArray.Parser String := do
    let slice ← Std.Internal.Parsec.ByteArray.take 100
    return String.fromUTF8! slice.toByteArray
  let ok ← try
      let _ ← parseFromStream tooLong s
      pure true
    catch _ =>
      pure false
  check (!ok) "expected parseFromStream to fail on a too-short buffer"
  let some rest ← read s | throw (IO.userError "expected the original bytes to still be there")
  check (rest == String.toUTF8 "short") s!"stream contents changed after a failed parse: {rest.toList}"

-- ── decompress ──

/-- The raw zlib/RFC 1950 bytes produced by Python's
    `zlib.compress(b"hello world")` — same fixture as
    `Tests/Linen/Crypto/Zlib/FFITest.lean`. -/
private def helloWorldZlib : ByteArray :=
  ByteArray.mk #[120, 156, 203, 72, 205, 201, 201, 87, 40, 207, 47, 202, 73, 1, 0, 26, 11, 4, 93]

#eval show IO Unit from do
  let compressed ← fromByteString helloWorldZlib
  let inflated ← decompress compressed
  let out ← toList inflated
  let flat := out.foldl (· ++ ·) ByteArray.empty
  check (flat == String.toUTF8 "hello world") s!"decompress round-trip mismatch: {flat.toList}"

end Tests.Data.PDF.Stream
