/-
  Tests for `Linen.Data.PDF.Content` (module 13, the package aggregator).

  This module carries no logic of its own — it is a thin `export`-based
  re-export of names already defined (and already tested) in
  `Data.PDF.Content.Ops`, `.Parser`, `.UnicodeCMap`, `.Processor`,
  `.Transform`, `.FontInfo` and `.FontDescriptor` (see that module's
  doc-comment). So there is nothing new to *test* here beyond confirming
  the re-export actually makes those names reachable, unqualified, under
  the plain `Data.PDF.Content` namespace — i.e. that `import
  Linen.Data.PDF.Content` alone (mirroring upstream's `import Pdf.Content`)
  gives the intended surface, with no need to `open`/import every submodule
  individually.
-/
import Linen.Data.PDF.Content
import Linen.Data.Text.Encoding

open Data.PDF.Content

private def bs (s : String) : Data.ByteString := Data.Text.Encoding.encodeUtf8 s

-- `toOp`/`Op`/`Expr`, re-exported from `.Ops`, are reachable directly under
-- `Data.PDF.Content`. (`Op`'s *constructors* still need anonymous
-- constructor notation — `.cm`, not `Op.cm` — since Lean's `export` aliases
-- a type name without teaching dot notation on the alias to find
-- constructors declared under the original namespace; this is a general
-- Lean limitation, not specific to this module.)
#guard toOp (bs "cm") == (.cm : Op)

-- `identity`/`Transform`, re-exported from `.Transform`, are reachable
-- directly under `Data.PDF.Content`.
#guard (identity : Transform Float) == ⟨1, 0, 0, 1, 0, 0⟩

-- `mkProcessor`/`processOp`/`Processor`, re-exported from `.Processor`, are
-- reachable directly under `Data.PDF.Content`.
#guard match processOp ((.q : Op), []) mkProcessor with
  | .ok p => p.prStateStack.length == 1
  | .error _ => false

-- `FontDescriptorFlag`/`flagSet`/`FontDescriptor`, re-exported from
-- `.FontDescriptor`, are reachable directly under `Data.PDF.Content`.
private def testFd : FontDescriptor :=
  { fontName := bs "Test", fontFamily := none, fontStretch := none, fontWeight := none, flags := 4, fontBBox := none, italicAngle := 0, ascent := none, descent := none, leading := none, capHeight := none, xHeight := none, stemV := none, stemH := none, avgWidth := none, maxWidth := none, missingWidth := none, charSet := none }

#guard flagSet testFd .symbolic

-- `parseContent`, re-exported from `.Parser`, is reachable directly under
-- `Data.PDF.Content`.
#guard match Std.Internal.Parsec.ByteArray.Parser.run parseContent (String.toUTF8 "q") with
  | .ok (some (.op .q)) => true
  | _ => false

-- `UnicodeCMap`/`unicodeCMapDecodeGlyph`, re-exported from `.UnicodeCMap`,
-- are reachable directly under `Data.PDF.Content`.
#guard
  let cmap : UnicodeCMap := { codeRanges := [], chars := {}, ranges := [] }
  unicodeCMapDecodeGlyph cmap 65 == none
