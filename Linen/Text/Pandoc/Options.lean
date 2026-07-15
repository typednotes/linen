/-
  `Linen.Text.Pandoc.Options` — reader and writer options.

  ## Haskell source

  Ported from `Text.Pandoc.Options` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Options.hs`).

  Provides the option enumerations (`TrackChanges`, `WrapOption`,
  `TopLevelDivision`, `ReferenceLocation`, `HTMLMathMethod`, `CiteMethod`,
  `ObfuscationMethod`, `HTMLSlideVariant`, `EPUBVersion`, `CaptionPosition`,
  `HighlightMethod`), the `ReaderOptions`/`WriterOptions` records with their
  `Default` values, the `HasSyntaxExtensions` class, and `isEnabled`.

  ### Deviations from upstream

  * Several `WriterOptions` fields reference engines deferred out of this
    import (per `docs/imports/pandoc/dependencies.md`). Their types are
    modeled by lightweight placeholders documented below:
    - `Template α` (doctemplates) — writers run template-free
      (`writerTemplate = none`);
    - `Context α` (doctemplates) — the template variable context;
    - `SyntaxMap`/`HighlightMethod`'s `Style` (skylighting) — syntax
      highlighting is deferred, so `Skylighting` carries `Unit`;
    - `PathTemplate` (`Text.Pandoc.Chunks`) — a bare `String`.
  * `Text` → `String`, `Int` → `Int`, `Double`/`Set`/`Map` map to `Int`/
    `List` as elsewhere in the port.
-/

import Linen.Text.Pandoc.Extensions
import Linen.Data.Default

namespace Linen.Text.Pandoc

open Data (Default)

-- ── Placeholders for deferred engines ─────────────────────────────────

/-- A skylighting `Style` (syntax highlighting deferred). -/
abbrev Style := Unit

/-- A skylighting `SyntaxMap` (syntax highlighting deferred). -/
abbrev SyntaxMap := List String

/-- A compiled doctemplates `Template` (templating engine deferred; writers
    run template-free). Carries the raw template text. -/
structure Template (α : Type) where
  /-- The unparsed template text. -/
  raw : α
  deriving Repr, Inhabited

/-- A doctemplates variable `Context` (templating engine deferred). -/
abbrev Context (α : Type) := List (String × α)

/-- A path template for chunked output (`Text.Pandoc.Chunks`). -/
abbrev PathTemplate := String

-- ── Option enumerations ───────────────────────────────────────────────

/-- How to handle document tracked changes. -/
inductive TrackChanges where
  | AcceptChanges | RejectChanges | AllChanges
  deriving DecidableEq, BEq, Repr, Inhabited

/-- Text-wrapping behaviour. -/
inductive WrapOption where
  | WrapAuto | WrapNone | WrapPreserve
  deriving DecidableEq, BEq, Repr, Inhabited

/-- The top-level document division. -/
inductive TopLevelDivision where
  | TopLevelPart | TopLevelChapter | TopLevelSection | TopLevelDefault
  deriving DecidableEq, BEq, Repr, Inhabited

/-- Where to place footnotes / reference links. -/
inductive ReferenceLocation where
  | EndOfBlock | EndOfSection | EndOfDocument
  deriving DecidableEq, BEq, Repr, Inhabited

/-- How to typeset math in HTML output. -/
inductive HTMLMathMethod where
  | PlainMath
  | WebTeX (url : String)
  | GladTeX
  | MathML
  | MathJax (url : String)
  | KaTeX (url : String)
  deriving DecidableEq, BEq, Repr, Inhabited

/-- How to render citations. -/
inductive CiteMethod where
  | Citeproc | Natbib | Biblatex
  deriving DecidableEq, BEq, Repr, Inhabited

/-- How to obfuscate email addresses. -/
inductive ObfuscationMethod where
  | NoObfuscation | ReferenceObfuscation | JavascriptObfuscation
  deriving DecidableEq, BEq, Repr, Inhabited

/-- Which HTML slide-show framework to target. -/
inductive HTMLSlideVariant where
  | S5Slides | SlidySlides | SlideousSlides | DZSlides | RevealJsSlides | NoSlides
  deriving DecidableEq, BEq, Repr, Inhabited

/-- The EPUB version to produce. -/
inductive EPUBVersion where
  | EPUB2 | EPUB3
  deriving DecidableEq, BEq, Repr, Inhabited

/-- Caption placement for figures and tables. -/
inductive CaptionPosition where
  | CaptionAbove | CaptionBelow
  deriving DecidableEq, BEq, Repr, Inhabited

/-- The syntax-highlighting method. `Skylighting`'s `Style` is deferred
    (represented by `Unit`). -/
inductive HighlightMethod where
  | Skylighting (style : Style)
  | IdiomaticHighlighting
  | DefaultHighlighting
  | NoHighlighting
  deriving Repr, Inhabited

open TrackChanges WrapOption TopLevelDivision ReferenceLocation HTMLMathMethod
open CiteMethod ObfuscationMethod CaptionPosition HighlightMethod

-- ── Default abbreviations ─────────────────────────────────────────────

/-- The default set of abbreviations recognised by the Markdown reader. -/
def defaultAbbrevs : List String :=
  [ "Mr.", "Mrs.", "Ms.", "Capt.", "Dr.", "Prof.", "Gen.", "Gov.", "e.g."
  , "i.e.", "Sgt.", "St.", "vol.", "vs.", "Sen.", "Rep.", "Pres.", "Hon."
  , "Rev.", "Ph.D.", "M.D.", "M.A.", "p.", "pp.", "ch.", "sec.", "cf.", "cp." ]

-- ── Reader options ────────────────────────────────────────────────────

/-- Options for the readers. -/
structure ReaderOptions where
  /-- Syntax extensions. -/
  readerExtensions : Extensions := emptyExtensions
  /-- Parse as standalone document (with metadata). -/
  readerStandalone : Bool := false
  /-- Number of columns in terminal. -/
  readerColumns : Int := 80
  /-- Tab stop width. -/
  readerTabStop : Int := 4
  /-- Classes for indented code blocks. -/
  readerIndentedCodeClasses : List String := []
  /-- Strings to treat as abbreviations. -/
  readerAbbreviations : List String := defaultAbbrevs
  /-- Default extension for images without one. -/
  readerDefaultImageExtension : String := ""
  /-- How to handle tracked changes. -/
  readerTrackChanges : TrackChanges := AcceptChanges
  /-- Strip HTML comments. -/
  readerStripComments : Bool := false
  /-- Inputs for the typst reader. -/
  readerTypstInputs : List (String × String) := []
  deriving Repr, Inhabited

instance : Default ReaderOptions where default := {}

-- ── Writer options ────────────────────────────────────────────────────

/-- Options for the writers. -/
structure WriterOptions where
  /-- Template to use (template-free path: `none`). -/
  writerTemplate : Option (Template String) := none
  /-- Variables to set in the template. -/
  writerVariables : Context String := []
  /-- Tab stop width. -/
  writerTabStop : Int := 4
  /-- Include a table of contents. -/
  writerTableOfContents : Bool := false
  /-- Include a list of figures. -/
  writerListOfFigures : Bool := false
  /-- Include a list of tables. -/
  writerListOfTables : Bool := false
  /-- Incremental slide-show lists. -/
  writerIncremental : Bool := false
  /-- How to typeset math. -/
  writerHTMLMathMethod : HTMLMathMethod := PlainMath
  /-- Number sections in LaTeX. -/
  writerNumberSections : Bool := false
  /-- Starting section numbers. -/
  writerNumberOffset : List Int := [0, 0, 0, 0, 0, 0]
  /-- Wrap sections in `<div>` tags. -/
  writerSectionDivs : Bool := false
  /-- Syntax extensions. -/
  writerExtensions : Extensions := emptyExtensions
  /-- Use reference links in writing markdown/rst. -/
  writerReferenceLinks : Bool := false
  /-- Dpi for pixel to/from inch/cm conversions. -/
  writerDpi : Int := 96
  /-- Option for wrapping text. -/
  writerWrapText : WrapOption := WrapAuto
  /-- Characters in a line (for text wrapping). -/
  writerColumns : Int := 72
  /-- How to obfuscate emails. -/
  writerEmailObfuscation : ObfuscationMethod := NoObfuscation
  /-- Prefix for section and note ids. -/
  writerIdentifierPrefix : String := ""
  /-- How to write citations. -/
  writerCiteMethod : CiteMethod := Citeproc
  /-- Use `<q>` tags for quotes in HTML. -/
  writerHtmlQTags : Bool := false
  /-- Force header level for slides. -/
  writerSlideLevel : Option Int := none
  /-- Type of top-level divisions. -/
  writerTopLevelDivision : TopLevelDivision := TopLevelDefault
  /-- How to highlight code. -/
  writerHighlightMethod : HighlightMethod := DefaultHighlighting
  /-- Use setext headers for levels 1,2 in markdown. -/
  writerSetextHeaders : Bool := false
  /-- Use list tables for RST. -/
  writerListTables : Bool := false
  /-- Subdirectory for epub in OCF. -/
  writerEpubSubdirectory : String := "EPUB"
  /-- Metadata to include in EPUB. -/
  writerEpubMetadata : Option String := none
  /-- Paths to fonts to embed. -/
  writerEpubFonts : List String := []
  /-- Create an epub title page. -/
  writerEpubTitlePage : Bool := true
  /-- Level at which to split into chapters. -/
  writerSplitLevel : Int := 1
  /-- Template for filenames in chunked HTML. -/
  writerChunkTemplate : PathTemplate := "%s-%i.html"
  /-- Number of levels to include in TOC. -/
  writerTOCDepth : Int := 3
  /-- Path of reference doc. -/
  writerReferenceDoc : Option String := none
  /-- Location of footnotes and references. -/
  writerReferenceLocation : ReferenceLocation := EndOfDocument
  /-- Position of figure captions. -/
  writerFigureCaptionPosition : CaptionPosition := CaptionBelow
  /-- Position of table captions. -/
  writerTableCaptionPosition : CaptionPosition := CaptionAbove
  /-- Syntax highlighting definitions. -/
  writerSyntaxMap : SyntaxMap := []
  /-- Prefer ASCII representations of characters when possible. -/
  writerPreferAscii : Bool := false
  /-- Add links to images. -/
  writerLinkImages : Bool := false
  deriving Repr, Inhabited

instance : Default WriterOptions where default := {}

-- ── Extension access ──────────────────────────────────────────────────

/-- Types that carry a set of syntax extensions. -/
class HasSyntaxExtensions (α : Type) where
  /-- The extensions carried by the value. -/
  getExtensions : α → Extensions

instance : HasSyntaxExtensions ReaderOptions where
  getExtensions opts := opts.readerExtensions

instance : HasSyntaxExtensions WriterOptions where
  getExtensions opts := opts.writerExtensions

/-- Test whether an extension is enabled in an options record. -/
def isEnabled {α : Type} [HasSyntaxExtensions α] (ext : Extension) (opts : α) : Bool :=
  extensionEnabled ext (HasSyntaxExtensions.getExtensions opts)

end Linen.Text.Pandoc
