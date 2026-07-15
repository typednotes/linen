/-
  `Linen.Text.Pandoc.Logging` — log messages and verbosity levels.

  ## Haskell source

  Ported from `Text.Pandoc.Logging` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Logging.hs`).

  Provides `Verbosity`, the `LogMessage` sum type, `messageVerbosity` (the
  severity of each message) and `showLogMessage` (its rendered text).

  ### Deviations from upstream

  * `Text` → `String`; `Integer`/`Int` → `Int`/`Nat`; `FilePath` → `String`.
  * `SourcePos` (upstream from `Text.Parsec.Pos`) is modeled by a lightweight
    record here — the parsing layer that produces positions is a later tier
    (`Text.Pandoc.Sources`/`.Parsing`).
  * The `ToJSON` serialization of `LogMessage` is out of scope.
-/

import Linen.Text.Pandoc.Definition

namespace Linen.Text.Pandoc

/-- A source position: file name, 1-based line and column. -/
structure SourcePos where
  /-- Source name (file). -/
  name : String := ""
  /-- 1-based line number. -/
  line : Nat := 1
  /-- 1-based column number. -/
  column : Nat := 1
  deriving DecidableEq, BEq, Repr, Inhabited

/-- Render a source position `name line:col`. -/
def showPos (p : SourcePos) : String :=
  let nm := if p.name == "" then "" else p.name ++ " "
  s!"{nm}line {p.line} column {p.column}"

/-- Log-message severity, ordered `ERROR < WARNING < INFO`. -/
inductive Verbosity where
  | ERROR | WARNING | INFO
  deriving DecidableEq, BEq, Repr, Inhabited, Ord

open Verbosity

/-- Diagnostic messages emitted during conversion. -/
inductive LogMessage where
  | SkippedContent (content : String) (pos : SourcePos)
  | IgnoredElement (element : String)
  | DuplicateLinkReference (ref : String) (pos : SourcePos)
  | DuplicateNoteReference (ref : String) (pos : SourcePos)
  | NoteDefinedButNotUsed (ref : String) (pos : SourcePos)
  | DuplicateIdentifier (ident : String) (pos : SourcePos)
  | ReferenceNotFound (ref : String) (pos : SourcePos)
  | CircularReference (ref : String) (pos : SourcePos)
  | UndefinedToggle (toggle : String) (pos : SourcePos)
  | ParsingUnescaped (content : String) (pos : SourcePos)
  | CouldNotLoadIncludeFile (file : String) (pos : SourcePos)
  | CouldNotParseIncludeFile (file : String) (pos : SourcePos)
  | MacroAlreadyDefined (name : String) (pos : SourcePos)
  | InlineNotRendered (il : Inline)
  | BlockNotRendered (bl : Block)
  | DocxParserWarning (msg : String)
  | PowerpointTemplateWarning (msg : String)
  | IgnoredIOError (msg : String)
  | CouldNotFetchResource (url : String) (msg : String)
  | CouldNotDetermineImageSize (url : String) (msg : String)
  | CouldNotConvertImage (url : String) (msg : String)
  | CouldNotDetermineMimeType (url : String)
  | CouldNotConvertTeXMath (math : String) (msg : String)
  | CouldNotParseCSS (msg : String)
  | Fetching (url : String)
  | Extracting (path : String)
  | LoadedResource (orig : String) (found : String)
  | ScriptingInfo (msg : String) (pos : Option SourcePos)
  | ScriptingWarning (msg : String) (pos : Option SourcePos)
  | NoTitleElement (fallback : String)
  | NoLangSpecified
  | InvalidLang (lang : String)
  | CouldNotHighlight (msg : String)
  | MissingCharacter (msg : String)
  | Deprecated (thing : String) (msg : String)
  | NoTranslation (term : String)
  | CouldNotLoadTranslations (lang : String) (msg : String)
  | UnusualConversion (msg : String)
  | UnexpectedXmlElement (element : String) (parent : String)
  | UnknownOrgExportOption (option : String)
  | CouldNotDeduceFormat (exts : List String) (format : String)
  | RunningFilter (path : String)
  | FilterCompleted (path : String) (milliseconds : Int)
  | CiteprocWarning (msg : String)
  | ATXHeadingInLHS (level : Int) (contents : String)
  | EnvironmentVariableUndefined (var : String)
  | DuplicateAttribute (attr : String) (val : String)
  | NotUTF8Encoded (src : String)
  | MakePDFInfo (context : String) (msg : String)
  | MakePDFWarning (msg : String)
  | UnclosedDiv (openPos : SourcePos) (closePos : SourcePos)
  | UnsupportedCodePage (codepage : Int)
  | YamlWarning (pos : SourcePos) (msg : String)
  | UnsupportedPdfStandard (standard : String)
  deriving Repr, Inhabited

open LogMessage

/-- The severity of a log message. -/
def messageVerbosity : LogMessage → Verbosity
  | SkippedContent _ _ => INFO
  | IgnoredElement _ => INFO
  | ParsingUnescaped _ _ => INFO
  | InlineNotRendered _ => INFO
  | BlockNotRendered _ => INFO
  | Fetching _ => INFO
  | Extracting _ => INFO
  | LoadedResource _ _ => INFO
  | ScriptingInfo _ _ => INFO
  | NoTitleElement _ => INFO
  | NoLangSpecified => INFO
  | RunningFilter _ => INFO
  | FilterCompleted _ _ => INFO
  | MakePDFInfo _ _ => INFO
  -- `.sty` include files are only INFO; everything else WARNING.
  | CouldNotLoadIncludeFile f _ => if f.endsWith ".sty" then INFO else WARNING
  | _ => WARNING

/-- `sep ++ s` when `s` is nonempty, else the empty string. -/
private def optSuffix (sep s : String) : String := if s == "" then "" else sep ++ s

/-- `" at <pos>"` when a position is present, else empty. -/
private def posSuffix : Option SourcePos → String
  | some p => " at " ++ showPos p
  | none => ""

/-- Render a log message as human-readable text. -/
def showLogMessage : LogMessage → String
  | SkippedContent s pos => s!"Skipped '{s}' at {showPos pos}"
  | IgnoredElement s => s!"Ignored element {s}"
  | DuplicateLinkReference s pos => s!"Duplicate link reference '{s}' at {showPos pos}"
  | DuplicateNoteReference s pos => s!"Duplicate note reference '{s}' at {showPos pos}"
  | NoteDefinedButNotUsed s pos => s!"Note with key '{s}' defined at {showPos pos} but not used."
  | DuplicateIdentifier s pos => s!"Duplicate identifier '{s}' at {showPos pos}"
  | ReferenceNotFound s pos => s!"Reference not found for '{s}' at {showPos pos}"
  | CircularReference s pos => s!"Circular reference '{s}' at {showPos pos}"
  | UndefinedToggle s pos => s!"Undefined toggle '{s}' at {showPos pos}"
  | ParsingUnescaped s pos => s!"Parsing unescaped '{s}' at {showPos pos}"
  | CouldNotLoadIncludeFile fp pos => s!"Could not load include file '{fp}' at {showPos pos}"
  | CouldNotParseIncludeFile fp pos => s!"Could not parse include file '{fp}' at {showPos pos}"
  | MacroAlreadyDefined name pos => s!"Macro '{name}' already defined, ignoring at {showPos pos}"
  | InlineNotRendered il => s!"Not rendering {repr il}"
  | BlockNotRendered bl => s!"Not rendering {repr bl}"
  | DocxParserWarning s => s!"Docx parser warning: {s}"
  | PowerpointTemplateWarning s => s!"Powerpoint template warning: {s}"
  | IgnoredIOError s => s!"IO Error (ignored): {s}"
  | CouldNotFetchResource fp s =>
      let extra := optSuffix ": " s
      s!"Could not fetch resource '{fp}'{extra}"
  | CouldNotDetermineImageSize fp s =>
      let extra := optSuffix ": " s
      s!"Could not determine image size for '{fp}'{extra}"
  | CouldNotConvertImage fp s =>
      let extra := optSuffix ": " s
      s!"Could not convert image '{fp}'{extra}"
  | CouldNotDetermineMimeType fp => s!"Could not determine mime type for '{fp}'"
  | CouldNotConvertTeXMath s msg =>
      let extra := optSuffix ":\n" msg
      s!"Could not convert TeX math '{s}', rendering as TeX{extra}"
  | CouldNotParseCSS msg =>
      let extra := optSuffix ": " msg
      s!"Could not parse CSS{extra}"
  | Fetching fp => s!"Fetching {fp}..."
  | Extracting fp => s!"Extracting {fp}..."
  | LoadedResource orig found => s!"Loaded {orig} from {found}"
  | ScriptingInfo msg pos =>
      let extra := posSuffix pos
      s!"{msg}{extra}"
  | ScriptingWarning msg pos =>
      let extra := posSuffix pos
      s!"{msg}{extra}"
  | NoTitleElement fallback =>
      "This document format requires a nonempty <title> element.\n"
        ++ s!"Defaulting to '{fallback}' as the title.\n"
        ++ "To specify a title, use 'title' in metadata or the pagetitle variable."
  | NoLangSpecified =>
      "No value for 'lang' was specified in the metadata.\n"
        ++ "It is recommended that lang be specified for this format."
  | InvalidLang s => s!"Invalid 'lang' value '{s}'. Use an IETF language tag like 'en-US'."
  | CouldNotHighlight msg => s!"Could not highlight code block:\n{msg}"
  | MissingCharacter msg => s!"Missing character: {msg}"
  | Deprecated thing msg =>
      let extra := optSuffix ". " msg
      s!"Deprecated: {thing}{extra}"
  | NoTranslation term => s!"The term {term} has no translation defined."
  | CouldNotLoadTranslations lang msg =>
      let extra := optSuffix "\n" msg
      s!"Could not load translations for {lang}{extra}"
  | UnusualConversion msg => s!"Unusual conversion: {msg}"
  | UnexpectedXmlElement element parent =>
      s!"Unexpected XML element {element} in {parent}"
  | UnknownOrgExportOption option => s!"Ignoring unknown Org export option: {option}"
  | CouldNotDeduceFormat exts format =>
      let joined := ", ".intercalate exts
      s!"Could not deduce format from file extension {joined}\nDefaulting to {format}"
  | RunningFilter fp => s!"Running filter {fp}"
  | FilterCompleted fp ms => s!"Completed filter {fp} in {ms} ms"
  | CiteprocWarning ms => s!"Citeproc: {ms}"
  | ATXHeadingInLHS _ contents =>
      s!"Rendering heading '{contents}' as a setext heading (ATX headings are incompatible with literate Haskell)."
  | EnvironmentVariableUndefined var => s!"Undefined environment variable {var} in defaults file."
  | DuplicateAttribute attr val => s!"Ignoring duplicate attribute {attr}={val}."
  | NotUTF8Encoded src => s!"{src} is not UTF-8 encoded: falling back to latin1."
  | MakePDFInfo context msg =>
      let extra := optSuffix "\n" msg
      s!"[makePDF] {context}{extra}"
  | MakePDFWarning msg => s!"[makePDF] {msg}"
  | UnclosedDiv openpos closepos =>
      s!"Div at {showPos openpos} unclosed, closing implicitly at {showPos closepos}."
  | UnsupportedCodePage cpg => s!"Unsupported code page {cpg}; text may be garbled."
  | YamlWarning pos m => s!"YAML warning ({showPos pos}): {m}"
  | UnsupportedPdfStandard s => s!"LaTeX ignores the PDF standard setting {s}."

end Linen.Text.Pandoc
