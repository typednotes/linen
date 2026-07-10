/-
  Data.PDF.Content — low-level tools for processing a PDF page's content
  stream

  Ports `Pdf.Content` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content.hs`),
  module 13 (the package's own top-level aggregator, and the last module) of
  the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`.

  ## Design: a thin re-export module

  Exactly as `Data.PDF.Core` re-exports `pdf-toolbox-core`'s own top-level
  aggregator module, this module re-exports upstream's `Pdf.Content`, which
  itself carries no code of its own — only a curated re-export of
  `Pdf.Content.Ops`, `Pdf.Content.Parser`, `Pdf.Content.UnicodeCMap`,
  `Pdf.Content.Processor`, `Pdf.Content.Transform`, `Pdf.Content.FontInfo`
  and `Pdf.Content.FontDescriptor` — so a downstream user can
  `import Pdf.Content` alone rather than every submodule individually.

  `Data.PDF.Content` the *namespace* is already the direct parent of each of
  those seven submodules' own namespaces; this module additionally declares
  `namespace Data.PDF.Content` and uses `export` to alias every name each of
  those submodule's own upstream export list names, verbatim, straight into
  that parent namespace (see `Data.PDF.Core`'s doc-comment for the same
  technique and the same non-goal it calls out: this is not a name
  collision with the submodules themselves, since Lean lets a namespace
  hold direct members alongside nested sub-namespaces). Each submodule's own
  export list — read from its Haskell source header — is reproduced here
  exactly:

  - `Pdf.Content.Ops`: `Op`, `Expr`, `Operator`, `toOp`.
  - `Pdf.Content.Parser`: `readNextOperator`, `parseContent`.
  - `Pdf.Content.UnicodeCMap`: `UnicodeCMap`, `parseUnicodeCMap`,
    `unicodeCMapNextGlyph`, `unicodeCMapDecodeGlyph`.
  - `Pdf.Content.Processor`: `Processor`, `GraphicsState`, `GlyphDecoder`,
    `Glyph`, `Span`, `initialGraphicsState`, `mkProcessor`, `processOp`.
  - `Pdf.Content.Transform`: `Transform`, `Vector`, `identity`,
    `translation`, `scale`, `transform`, `translate`, `multiply`.
  - `Pdf.Content.FontInfo`: `FontInfo`, `FISimple`, `FontBaseEncoding`,
    `SimpleFontEncoding`, `FIComposite`, `CIDFontWidths`,
    `makeCIDFontWidths`, `cidFontGetWidth`, `fontInfoDecodeGlyphs`.
  - `Pdf.Content.FontDescriptor`: `FontDescriptor`, `FontDescriptorFlag`,
    `flagSet`.
-/
import Linen.Data.PDF.Content.Ops
import Linen.Data.PDF.Content.Parser
import Linen.Data.PDF.Content.UnicodeCMap
import Linen.Data.PDF.Content.Processor
import Linen.Data.PDF.Content.Transform
import Linen.Data.PDF.Content.FontInfo
import Linen.Data.PDF.Content.FontDescriptor

namespace Data.PDF.Content

export Data.PDF.Content.Ops (Op Expr Operator toOp)
export Data.PDF.Content.Parser (readNextOperator parseContent)
export Data.PDF.Content.UnicodeCMap
  (UnicodeCMap parseUnicodeCMap unicodeCMapNextGlyph unicodeCMapDecodeGlyph)
export Data.PDF.Content.Processor
  (Processor GraphicsState GlyphDecoder Glyph Span initialGraphicsState mkProcessor processOp)
export Data.PDF.Content.Transform
  (Transform Vector identity translation scale transform translate multiply)
export Data.PDF.Content.FontInfo
  (FontInfo FISimple FontBaseEncoding SimpleFontEncoding FIComposite CIDFontWidths
   makeCIDFontWidths cidFontGetWidth fontInfoDecodeGlyphs)
export Data.PDF.Content.FontDescriptor (FontDescriptor FontDescriptorFlag flagSet)

end Data.PDF.Content
