import Std.Internal.Parsec.ByteArray

/-!
  Port of `Codec.Picture.InternalHelper` from the `JuicyPixels` package
  (see `docs/imports/JuicyPixels/dependencies.md`, module 3 of 29).

  Upstream wraps `Data.Binary.Get`'s `Get` monad (`runGet`/`runGetStrict`) and
  a `Binary a => decode` entry point for running a binary-format parser over
  a `ByteString`. Per `docs/imports/PdfToolboxCore/dependencies.md`'s
  precedent (also followed by `Linen.Data.PDF.Core.Parsers.Util`), the Lean
  analogue of a Haskell binary/attoparsec-style parser monad is
  `Std.Internal.Parsec.ByteArray.Parser`, so this module is just thin
  wrappers around `Parser.run`.

  Upstream's generic `decode :: Binary a => ByteString -> Either String a`
  dispatches on a typeclass instance selecting `get`; Lean has no such
  typeclass-driven binary deserialization, and every call site in JuicyPixels
  actually supplies its own concrete parser for a specific header type rather
  than relying on `decode`'s polymorphism, so it is dropped in favour of
  always calling `runGet`/`runGetStrict` with an explicit parser — no loss of
  functionality, since that is exactly what `decode = runGetStrict get`
  reduces to once `get` is picked.
-/

namespace Codec.Picture

open Std.Internal.Parsec.ByteArray

/-- Run a parser over a `ByteArray`, succeeding even if it leaves input
    unconsumed (matching upstream's `runGetOrFail`, which discards the
    leftover rather than treating it as an error). -/
def runGet (p : Parser α) (input : ByteArray) : Except String α :=
  Parser.run p input

/-- Upstream distinguishes `runGet` (lazy `ByteString`) from `runGetStrict`
    (strict `ByteString`); Lean's `ByteArray` has no such distinction, so
    both are the same function. -/
def runGetStrict (p : Parser α) (input : ByteArray) : Except String α :=
  Parser.run p input

/-- Parse the remainder of the input as raw bytes, consuming it all. -/
def getRemainingBytes : Parser ByteArray := fun it =>
  .success (it.forward it.remainingBytes) (it.array[it.idx...(it.idx + it.remainingBytes)]).toByteArray

end Codec.Picture
