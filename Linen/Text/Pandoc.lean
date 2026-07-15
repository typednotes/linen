/-
  `Linen.Text.Pandoc` — the top-level pandoc facade.

  ## Haskell source

  Ported from the top-level `Text.Pandoc` module in the `pandoc` package
  (v3.10, `src/Text/Pandoc.hs`), which re-exports the library's public surface
  and wires the format-name → reader/writer dispatch (upstream via the `Reader`
  and `Writer` GADTs and the `readers`/`writers` association lists, keyed by
  format name).

  This is the capstone module of the in-scope pandoc import (see
  `docs/imports/pandoc/dependencies.md`).  It:

  * re-exports the in-scope readers and writers under the `Linen.Text.Pandoc`
    namespace (`readMarkdown`/`readHtml`/`readNative`,
    `writeMarkdown`/`writeHtml`/`writeNative`), and
  * provides the `getReader`/`getWriter` registries restricted to the in-scope
    formats (Markdown, HTML, and the AST-native format), mirroring how
    upstream's `Text.Pandoc` dispatches on `Reader`/`Writer` by format name.

  ### Deviations from upstream (documented scope)

  * Upstream's `Reader`/`Writer` GADTs distinguish text readers/writers from
    the binary (`ByteString`) ones (DOCX/EPUB/…) and carry a `PandocMonad`
    constraint.  Every in-scope format is text-valued and pure, so the registry
    models a `Reader` as `ReaderOptions → String → Except PandocError Pandoc`
    and a `Writer` as `WriterOptions → Pandoc → String` — no monad wrapper and
    no binary variant.  The long tail of formats (LaTeX, RST, Org, DOCX, EPUB,
    …), the Lua/filter layer, and the App/CLI dispatch are all deferred (see the
    plan's Scope note).
  * `getDefaultExtensions` (from `Extensions`) is the per-format extension
    preset; a caller building a `ReaderOptions`/`WriterOptions` combines it with
    the format from `getReader`/`getWriter` exactly as upstream's App layer
    does.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Builder
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Extensions
import Linen.Text.Pandoc.Error
import Linen.Text.Pandoc.Readers.HTML
import Linen.Text.Pandoc.Readers.Markdown
import Linen.Text.Pandoc.Readers.Native
import Linen.Text.Pandoc.Writers.HTML
import Linen.Text.Pandoc.Writers.Markdown
import Linen.Text.Pandoc.Writers.Native

namespace Linen.Text.Pandoc

/- ── Re-exported readers ─────────────────────────────────────────────────── -/

/-- Read a Markdown document into the pandoc AST. -/
def readMarkdown (opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  Readers.Markdown.readMarkdown opts input

/-- Read an HTML document into the pandoc AST. -/
def readHtml (opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  Readers.HTML.readHtml opts input

/-- Read a native (AST-literal) document into the pandoc AST. -/
def readNative (opts : ReaderOptions) (input : String) : Except PandocError Pandoc :=
  Readers.Native.readNative opts input

/- ── Re-exported writers ─────────────────────────────────────────────────── -/

/-- Render a document as Markdown. -/
def writeMarkdown (opts : WriterOptions) (doc : Pandoc) : String :=
  Writers.Markdown.writeMarkdownString opts doc

/-- Render a document as an HTML5 fragment. -/
def writeHtml (opts : WriterOptions) (doc : Pandoc) : String :=
  Writers.HTML.writeHtmlString opts doc

/-- Render a document in native (AST-literal) syntax. -/
def writeNative (opts : WriterOptions) (doc : Pandoc) : String :=
  Writers.Native.writeNativeString opts doc

/- ── Reader / writer registries ──────────────────────────────────────────── -/

/-- A (text) reader: parse a string into the AST under some reader options. -/
abbrev Reader := ReaderOptions → String → Except PandocError Pandoc

/-- A (text) writer: render the AST to a string under some writer options. -/
abbrev Writer := WriterOptions → Pandoc → String

/-- Look up the reader for a format name (in-scope formats only). -/
def getReader (fmt : String) : Option Reader :=
  match fmt with
  | "markdown" | "markdown_strict" | "markdown_phpextra" | "markdown_mmd"
  | "markdown_github" | "gfm" | "commonmark" | "commonmark_x" =>
      some readMarkdown
  | "html" | "html4" | "html5" => some readHtml
  | "native" => some readNative
  | _ => none

/-- Look up the writer for a format name (in-scope formats only). -/
def getWriter (fmt : String) : Option Writer :=
  match fmt with
  | "markdown" | "markdown_strict" | "markdown_phpextra" | "markdown_mmd"
  | "markdown_github" | "gfm" | "commonmark" | "commonmark_x" | "plain" =>
      some writeMarkdown
  | "html" | "html4" | "html5" => some writeHtml
  | "native" => some writeNative
  | _ => none

/-- The in-scope reader format names. -/
def readerNames : List String := ["markdown", "gfm", "commonmark", "html", "native"]

/-- The in-scope writer format names. -/
def writerNames : List String := ["markdown", "gfm", "commonmark", "html", "native", "plain"]

/-- Convert a document from one in-scope format to another, returning `none`
    when either format is out of scope.  Reader/writer options carry the
    per-format default extensions (`getDefaultExtensions`). -/
def convert (fromFmt toFmt : String) (input : String) : Option (Except PandocError String) :=
  match getReader fromFmt, getWriter toFmt with
  | some rd, some wr =>
      let ropts : ReaderOptions := { readerExtensions := getDefaultExtensions fromFmt }
      let wopts : WriterOptions := { writerExtensions := getDefaultExtensions toFmt }
      some ((rd ropts input).map (fun d => wr wopts d))
  | _, _ => none

end Linen.Text.Pandoc
