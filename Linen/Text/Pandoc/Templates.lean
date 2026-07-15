/-
  `Linen.Text.Pandoc.Templates` — template loading and rendering.

  ## Haskell source

  Ported from `Text.Pandoc.Templates` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Templates.hs`).

  Upstream this module is a thin layer of pandoc-specific plumbing over the
  external `doctemplates` engine: it re-exports `Template`/`compileTemplate`/
  `renderTemplate` from `Text.DocTemplates` and adds partial-resolution
  (`WithDefaultPartials`/`WithPartials`), template retrieval (`getTemplate`),
  default-template lookup (`getDefaultTemplate`) and compilation
  (`compileDefaultTemplate`).

  ### Deviations from upstream (template-free scope)

  Per `docs/imports/pandoc/dependencies.md`, the full `doctemplates` engine is
  **deferred**: the in-scope writers all run *template-free*
  (`writerTemplate = none`), so `renderTemplate` is only ever reached with no
  template.  Accordingly:

  * `Template α` and `Context α` are the lightweight placeholders already
    declared in `Linen.Text.Pandoc.Options` (a `Template` carries only its raw
    text; a `Context` is a plain `String`-keyed association list).  There is no
    `doctemplates` template language, `Val` sum, or `TemplateTarget`/`ToContext`
    machinery.
  * `compileTemplate` always succeeds, wrapping the raw text unchanged — there
    is no template grammar to parse or reject.
  * `renderTemplate` is the passthrough form: with the templating engine
    deferred there is no variable interpolation, so it emits the template's raw
    text as a `Doc` literal (this is exercised only in the template-free path,
    where writers never call it with a real template).
  * `WithDefaultPartials`/`WithPartials` (the two `TemplateMonad` partial
    resolution strategies) have no analogue without `doctemplates`'
    `TemplateMonad`, so they are omitted.
  * `getTemplate`/`compileDefaultTemplate` fetch bundled data files
    (`Text.Pandoc.Data`, `file-embed`) which are a deferred subtree; only the
    pure format-alias normalisation of `getDefaultTemplate` is kept, and it
    returns `""` (no bundled templates), matching the template-free behaviour.
-/

import Linen.Text.Pandoc.Options
import Linen.Text.DocLayout

namespace Linen.Text.Pandoc

open _root_.Text.DocLayout (Doc literal)

/-- Compile template text into a `Template`.  Template-free scope: this never
    fails (there is no template grammar to parse) and simply wraps the raw
    text; the `name` is accepted for signature compatibility with upstream's
    `compileTemplate :: FilePath -> Text -> m (Either Text (Template Text))`. -/
def compileTemplate (_name : String) (tmpl : String) :
    Except String (Template String) :=
  .ok ⟨tmpl⟩

/-- Render a compiled `Template` against a variable `Context`.  Template-free
    scope: with the `doctemplates` engine deferred there is no interpolation,
    so the template's raw text is emitted verbatim as a `Doc` literal (the
    `context` is accepted but unused). -/
def renderTemplate (t : Template String) (_context : Context String) :
    Doc String :=
  literal t.raw

/-- Normalise a writer name to the format whose default template it shares,
    following upstream's alias table (`docx → openxml`, `html → html5`,
    `gfm → commonmark`, the `markdown_*`/`bbcode_*` variants, …). -/
def defaultTemplateFormat (writer : String) : String :=
  match writer with
  | "docx" | "odt" | "pptx" => "openxml"
  | "html" => "html5"
  | "gfm" => "commonmark"
  | "commonmark_x" => "commonmark"
  | "markdown_github" | "markdown_mmd" | "markdown_phpextra"
  | "markdown_strict" => "markdown"
  | "bbcode_phpbb" | "bbcode_mybb" => "bbcode"
  | w => w

/-- The default template text for a writer.  Formats with no template concept
    (`native`, `csljson`, `json`, `xml`, `fb2`, `pptx`, `ipynb`) have none;
    every other format's template ships as a bundled data file
    (`Text.Pandoc.Data`), a deferred subtree — so with no bundled data this
    returns `""` for every format, matching the template-free scope. -/
def getDefaultTemplate (_writer : String) : String := ""

end Linen.Text.Pandoc
