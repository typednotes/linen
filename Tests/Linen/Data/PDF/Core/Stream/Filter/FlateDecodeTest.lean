/-
  Tests for `Linen.Data.PDF.Core.Stream.Filter.FlateDecode`.

  `pngUnfilterRows` is pure, so it gets a plain `#guard`. Everything else
  (`decode`, `flateDecode`) drives real zlib inflate via FFI and returns
  `IO`, so it is checked with `#eval`, following
  `Tests/Linen/Crypto/Zlib/FFITest.lean`'s pattern.
-/
import Linen.Data.PDF.Core.Stream.Filter.FlateDecode

open Data.PDF.Core.Object Data.PDF.Core.Stream.Filter.FlateDecode

namespace Tests.Data.PDF.Core.Stream.Filter.FlateDecode

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

-- `pngUnfilterRows`: two 3-byte rows (each preceded by a discarded
-- filter-type tag byte), `cols = 3`. Row 1's data `[10, 20, 30]` sums with
-- the all-zero initial `prevRow` to itself; row 2's data `[1, 2, 3]` sums
-- with row 1's output to `[11, 22, 33]`.
#guard pngUnfilterRows 3 [0, 0, 0] [2, 10, 20, 30, 2, 1, 2, 3] == [10, 20, 30, 11, 22, 33]

-- An empty input has no rows to unfilter.
#guard pngUnfilterRows 3 [0, 0, 0] [] == ([] : List UInt8)

-- `decode none` (no `/DecodeParms`, hence no predictor) just inflates.
#eval show IO Unit from do
  let compressed := ByteArray.mk
    #[120, 156, 203, 72, 205, 201, 201, 215, 81, 72, 203, 73, 44, 73, 85, 4, 0, 33, 95, 4, 142]
  let is ← Data.PDF.Stream.fromByteString compressed
  let out ← decode none is
  let chunks ← Data.PDF.Stream.toList out
  let bytes := chunks.foldl (· ++ ·) ByteArray.empty
  unless bytes == String.toUTF8 "hello, flate!" do
    throw (IO.userError s!"decode (no predictor) mismatch: got {bytes.toList}")

-- `decode (some parms)` with `/Predictor 12` and `/Columns 3` inflates, then
-- reverses the PNG "Up" predictor over the same two-row layout as the pure
-- `pngUnfilterRows` test above.
#eval show IO Unit from do
  let compressed := ByteArray.mk
    #[120, 156, 99, 226, 18, 145, 99, 98, 100, 98, 6, 0, 1, 126, 0, 71]
  let is ← Data.PDF.Stream.fromByteString compressed
  let parms : Dict := Std.HashMap.ofList
    [(mkName "Predictor", Object.number (Data.Scientific.fromInt 12)),
     (mkName "Columns", Object.number (Data.Scientific.fromInt 3))]
  let out ← decode (some parms) is
  let chunks ← Data.PDF.Stream.toList out
  let bytes := chunks.foldl (· ++ ·) ByteArray.empty
  unless bytes.toList == [10, 20, 30, 11, 22, 33] do
    throw (IO.userError s!"decode (PNG-Up predictor) mismatch: got {bytes.toList}")

-- `flateDecode`'s name is `/FlateDecode`, and it is usable directly as a
-- `StreamFilter` (`filterDecode` is `decode`).
#guard flateDecode.filterName == mkName "FlateDecode"

-- An unsupported predictor value (e.g. TIFF predictor `2`) is rejected with
-- an error rather than silently ignored.
#eval show IO Unit from do
  let compressed := ByteArray.mk
    #[120, 156, 203, 72, 205, 201, 201, 215, 81, 72, 203, 73, 44, 73, 85, 4, 0, 33, 95, 4, 142]
  let is ← Data.PDF.Stream.fromByteString compressed
  let parms : Dict := Std.HashMap.ofList [(mkName "Predictor", Object.number (Data.Scientific.fromInt 2))]
  let result ← try
      let _ ← decode (some parms) is
      pure true
    catch _ =>
      pure false
  unless !result do
    throw (IO.userError "expected decode with predictor 2 to fail")

end Tests.Data.PDF.Core.Stream.Filter.FlateDecode
