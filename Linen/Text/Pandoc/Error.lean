/-
  `Linen.Text.Pandoc.Error` — the `PandocError` sum type.

  ## Haskell source

  Ported from `Text.Pandoc.Error` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Error.hs`).

  Provides the `PandocError` exception type, the pure `renderError` producing a
  human-readable message, and the `exitCode` mapping used by the CLI.

  ### Deviations from upstream

  * `Text` → `String`; `Word8` → `UInt8`; the wrapped `IOError` becomes its
    rendered `String`; `CiteprocError` (from the deferred `citeproc` subtree)
    becomes a `String` placeholder.
  * The `IO`-performing `handleError :: Either PandocError a -> IO a` belongs to
    the deferred App/CLI layer; only its pure parts are ported here —
    `renderError` (the rendered message) and `exitCode` (the per-constructor
    exit status).
-/

namespace Linen.Text.Pandoc

/-- Errors raised during conversion. -/
inductive PandocError where
  | PandocIOError (msg : String) (ioerr : String)
  | PandocHttpError (url : String) (err : String)
  | PandocShouldNeverHappenError (msg : String)
  | PandocSomeError (msg : String)
  | PandocParseError (msg : String)
  | PandocMakePDFError (msg : String)
  | PandocOptionError (msg : String)
  | PandocSyntaxMapError (msg : String)
  | PandocFailOnWarningError
  | PandocPDFProgramNotFoundError (prog : String)
  | PandocPDFError (log : String)
  | PandocXMLError (context : String) (msg : String)
  | PandocFilterError (filter : String) (msg : String)
  | PandocLuaError (msg : String)
  | PandocNoScriptingEngine
  | PandocCouldNotFindDataFileError (file : String)
  | PandocCouldNotFindMetadataFileError (file : String)
  | PandocResourceNotFound (resource : String)
  | PandocTemplateError (msg : String)
  | PandocNoTemplateError (name : String)
  | PandocAppError (msg : String)
  | PandocEpubSubdirectoryError (msg : String)
  | PandocMacroLoop (macroName : String)
  | PandocUTF8DecodingError (file : String) (offset : Int) (byte : UInt8)
  | PandocIpynbDecodingError (msg : String)
  | PandocUnsupportedCharsetError (charset : String)
  | PandocFormatError (format : String) (msg : String)
  | PandocUnknownReaderError (reader : String)
  | PandocUnknownWriterError (writer : String)
  | PandocUnsupportedExtensionError (ext : String) (format : String)
  | PandocCiteprocError (err : String)
  | PandocBibliographyError (file : String) (msg : String)
  | PandocInputNotTextError (file : String)
  deriving Repr, Inhabited

namespace PandocError

/-- Wrap text in single quotes. -/
private def quote (s : String) : String := "'" ++ s ++ "'"

/-- Render a `PandocError` as a human-readable message. -/
def renderError : PandocError → String
  | PandocIOError _ ioerr => ioerr
  | PandocHttpError u err => s!"Could not fetch {u}\n{err}"
  | PandocShouldNeverHappenError s =>
      "Something we thought was impossible happened!\n"
        ++ s!"Please report this to pandoc's developers: {s}"
  | PandocSomeError s => s
  | PandocParseError s => s
  | PandocMakePDFError s => s
  | PandocOptionError s => s
  | PandocSyntaxMapError s => s
  | PandocFailOnWarningError => "Failing because there were warnings."
  | PandocPDFProgramNotFoundError pdfprog =>
      s!"{pdfprog} not found. Please select a different --pdf-engine or install {pdfprog}"
  | PandocPDFError logmsg => s!"Error producing PDF.\n{logmsg}"
  | PandocXMLError fp logmsg =>
      let loc := if fp == "" then "" else " in " ++ fp
      s!"Invalid XML{loc}:\n{logmsg}"
  | PandocFilterError filtername msg =>
      s!"Error running filter {filtername}:\n{msg}"
  | PandocLuaError msg => s!"Error running Lua:\n{msg}"
  | PandocNoScriptingEngine => "This version of pandoc has no support for scripting."
  | PandocCouldNotFindDataFileError fn => s!"Could not find data file {fn}"
  | PandocCouldNotFindMetadataFileError fn => s!"Could not find metadata file {fn}"
  | PandocResourceNotFound fn => s!"File {fn} not found in resource path"
  | PandocTemplateError s => s!"Error compiling template: {s}"
  | PandocNoTemplateError name => s!"No default template found for {name}"
  | PandocAppError s => s
  | PandocEpubSubdirectoryError s =>
      s!"EPUB subdirectory name '{s}' contains illegal characters"
  | PandocMacroLoop s => s!"Loop encountered in expanding macro {quote s}"
  | PandocUTF8DecodingError f offset w =>
      s!"UTF-8 decoding error in {f} at byte offset {offset} ({w}).\n"
        ++ "The input must be a UTF-8 encoded text."
  | PandocIpynbDecodingError s => s!"ipynb decoding error: {s}"
  | PandocUnsupportedCharsetError charset => s!"Unsupported charset {quote charset}"
  | PandocFormatError format msg => s!"Error parsing {format} format: {msg}"
  | PandocUnknownReaderError r =>
      let hint :=
        if r == "doc" then "\nPandoc can convert from DOCX, but not from DOC."
        else if r == "pdf" then "\nPandoc can convert to PDF, but not from PDF."
        else ""
      s!"Unknown input format {r}{hint}"
  | PandocUnknownWriterError w =>
      let hint :=
        if w == "pdf" then "\nTo create a pdf, use -t latex|beamer|context|ms|html5 and an output file with a .pdf extension."
        else if w == "doc" then "\nPandoc can convert to DOCX, but not to DOC."
        else ""
      s!"Unknown output format {w}{hint}"
  | PandocUnsupportedExtensionError ext f =>
      s!"The extension {ext} is not supported for {f}"
  | PandocCiteprocError e => e
  | PandocBibliographyError file msg =>
      s!"Error reading bibliography file {file}:\n{msg}"
  | PandocInputNotTextError file =>
      s!"File {file} contains binary data, not text.\n"
        ++ "The input must be a UTF-8 encoded text."

/-- The CLI exit code for each error (mirrors the upstream `handleError`
    mapping; the surrounding `IO` action is in the deferred App layer). -/
def exitCode : PandocError → Nat
  | PandocIOError _ _ => 1
  | PandocFailOnWarningError => 3
  | PandocAppError _ => 4
  | PandocTemplateError _ => 5
  | PandocOptionError _ => 6
  | PandocUnknownReaderError _ => 21
  | PandocUnknownWriterError _ => 22
  | PandocUnsupportedExtensionError _ _ => 23
  | PandocCiteprocError _ => 24
  | PandocBibliographyError _ _ => 25
  | PandocEpubSubdirectoryError _ => 31
  | PandocPDFError _ => 43
  | PandocXMLError _ _ => 44
  | PandocPDFProgramNotFoundError _ => 47
  | PandocHttpError _ _ => 61
  | PandocShouldNeverHappenError _ => 62
  | PandocSomeError _ => 63
  | PandocParseError _ => 64
  | PandocMakePDFError _ => 66
  | PandocSyntaxMapError _ => 67
  | PandocFilterError _ _ => 83
  | PandocLuaError _ => 84
  | PandocNoScriptingEngine => 89
  | PandocMacroLoop _ => 91
  | PandocUTF8DecodingError _ _ _ => 92
  | PandocIpynbDecodingError _ => 93
  | PandocUnsupportedCharsetError _ => 94
  | PandocCouldNotFindDataFileError _ => 97
  | PandocCouldNotFindMetadataFileError _ => 98
  | PandocResourceNotFound _ => 99
  | PandocFormatError _ _ => 65
  | PandocNoTemplateError _ => 5
  | PandocInputNotTextError _ => 92

end PandocError
end Linen.Text.Pandoc
