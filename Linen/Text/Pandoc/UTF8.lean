/-
  `Linen.Text.Pandoc.UTF8` — UTF-8 aware (de)coding helpers.

  ## Haskell source

  Ported from `Text.Pandoc.UTF8` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/UTF8.hs`).

  Provides UTF-8 decode/encode between `ByteArray` and `String`, matching
  upstream's decode-side behaviour: a leading UTF-8 BOM (`EF BB BF`) is
  dropped and carriage returns (`\r`) are stripped before decoding; encoding
  never re-adds a BOM or CR. Also the newline-native file/handle IO wrappers.

  ### Deviations from upstream

  * `Text`/`String`/lazy variants collapse to Lean `String` (a single Unicode
    string type), and `ByteString` to `ByteArray`, so `toText`/`toString`/
    `toStringLazy` coincide, as do `fromText`/`fromString`.
  * Decoding is lenient (`String.fromUTF8?` with an empty-string fallback),
    matching upstream's use of a lenient decoder.
  * `encodePath`/`decodeArg` are the identity, as upstream.
-/

namespace Linen.Text.Pandoc
namespace UTF8

/-- Drop a leading UTF-8 byte-order mark (`EF BB BF`) if present. -/
def dropBOM (bs : ByteArray) : ByteArray :=
  if bs.size ≥ 3 && bs[0]! == 0xEF && bs[1]! == 0xBB && bs[2]! == 0xBF then
    bs.extract 3 bs.size
  else bs

/-- Strip carriage-return (`\r`, `0x0D`) bytes. -/
def stripCR (bs : ByteArray) : ByteArray :=
  bs.foldl (fun acc b => if b == 0x0D then acc else acc.push b) ByteArray.empty

/-- Decode a UTF-8 `ByteArray` into a `String`, dropping a BOM and stripping
    carriage returns first. -/
def toString (bs : ByteArray) : String :=
  (String.fromUTF8? (stripCR (dropBOM bs))).getD ""

/-- Alias for `toString` (`Text` and `String` coincide in this port). -/
def toText (bs : ByteArray) : String := toString bs

/-- Alias for `toString` (the lazy `ByteString` variant coincides here). -/
def toStringLazy (bs : ByteArray) : String := toString bs

/-- Encode a `String` as a UTF-8 `ByteArray` (no BOM, no CR reinsertion). -/
def fromString (s : String) : ByteArray := s.toUTF8

/-- Alias for `fromString`. -/
def fromText (s : String) : ByteArray := s.toUTF8

/-- No-op path encoder, as upstream. -/
def encodePath (fp : String) : String := fp

/-- Deprecated no-op argument decoder, as upstream. -/
def decodeArg (s : String) : String := s

-- ── File / handle IO (native newlines, UTF-8) ─────────────────────────

/-- Read a file as UTF-8 `String`. -/
def readFile (fp : System.FilePath) : IO String := do
  let bytes ← IO.FS.readBinFile fp
  pure (toString bytes)

/-- Write a `String` to a file as UTF-8. -/
def writeFile (fp : System.FilePath) (s : String) : IO Unit :=
  IO.FS.writeBinFile fp (fromString s)

/-- Read all of stdin as UTF-8 `String`. -/
def getContents : IO String := do
  let bytes ← (← IO.getStdin).readBinToEnd
  pure (toString bytes)

/-- Write a `String` to stdout as UTF-8. -/
def putStr (s : String) : IO Unit := do
  (← IO.getStdout).write (fromString s)

/-- Write a `String` and a newline to stdout as UTF-8. -/
def putStrLn (s : String) : IO Unit := putStr (s ++ "\n")

/-- Write a `String` to a handle as UTF-8. -/
def hPutStr (h : IO.FS.Handle) (s : String) : IO Unit := h.write (fromString s)

/-- Write a `String` and a newline to a handle as UTF-8. -/
def hPutStrLn (h : IO.FS.Handle) (s : String) : IO Unit := hPutStr h (s ++ "\n")

/-- Read a handle's contents as UTF-8 `String`. -/
def hGetContents (h : IO.FS.Handle) : IO String := do
  pure (toString (← h.readBinToEnd))

end UTF8
end Linen.Text.Pandoc
