/-
  `Linen.Text.Pandoc.Extensions` — individually selectable syntax extensions.

  ## Haskell source

  Ported from `Text.Pandoc.Extensions` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Extensions.hs`).

  Provides the `Extension` enumeration of format-feature flags and the
  `Extensions` set that turns them on/off, together with the named presets
  (`pandocExtensions`, `githubMarkdownExtensions`, …) and the
  `getDefaultExtensions` format→preset map.

  ### Deviations from upstream

  * `Extensions` wraps a `List Extension` rather than a `Data.Set.Set`
    (Lean's stdlib has no ordered `Set`; the list is kept duplicate-free by
    `enableExtension`/`extensionsFromList`, giving the same membership
    semantics). `extensionsToList` returns the members in insertion order,
    not sorted.
  * `showExtension`/`readExtension` use an explicit name↔constructor table
    (`extensionNames`) in place of GHC's derived `Show`/`Read` reflection.
  * `getAllExtensions` returns the full universe of named extensions for every
    format (the per-format restriction used upstream only for `--list-extensions`
    spec validation is part of the deferred App layer); `universalExtensions`
    is folded in as upstream does.
-/

namespace Linen.Text.Pandoc

-- ── The extension enumeration ─────────────────────────────────────────

/-- Individually selectable syntax extensions. -/
inductive Extension where
  | Ext_abbreviations
  | Ext_alerts
  | Ext_all_symbols_escapable
  | Ext_amuse
  | Ext_angle_brackets_escapable
  | Ext_ascii_identifiers
  | Ext_attributes
  | Ext_auto_identifiers
  | Ext_autolink_bare_uris
  | Ext_backtick_code_blocks
  | Ext_blank_before_blockquote
  | Ext_blank_before_header
  | Ext_bracketed_spans
  | Ext_citations
  | Ext_definition_lists
  | Ext_east_asian_line_breaks
  | Ext_element_citations
  | Ext_emoji
  | Ext_empty_paragraphs
  | Ext_epub_html_exts
  | Ext_escaped_line_breaks
  | Ext_example_lists
  | Ext_fancy_lists
  | Ext_fenced_code_attributes
  | Ext_fenced_code_blocks
  | Ext_fenced_divs
  | Ext_footnotes
  | Ext_four_space_rule
  | Ext_gfm_auto_identifiers
  | Ext_grid_tables
  | Ext_gutenberg
  | Ext_hard_line_breaks
  | Ext_header_attributes
  | Ext_table_attributes
  | Ext_ignore_line_breaks
  | Ext_implicit_figures
  | Ext_implicit_header_references
  | Ext_inline_code_attributes
  | Ext_inline_notes
  | Ext_intraword_underscores
  | Ext_latex_macros
  | Ext_line_blocks
  | Ext_link_attributes
  | Ext_lists_without_preceding_blankline
  | Ext_literate_haskell
  | Ext_mark
  | Ext_markdown_attribute
  | Ext_markdown_in_html_blocks
  | Ext_mmd_header_identifiers
  | Ext_mmd_link_attributes
  | Ext_mmd_title_block
  | Ext_multiline_tables
  | Ext_native_divs
  | Ext_native_spans
  | Ext_native_numbering
  | Ext_ntb
  | Ext_old_dashes
  | Ext_pandoc_title_block
  | Ext_pipe_tables
  | Ext_raw_attribute
  | Ext_raw_html
  | Ext_raw_tex
  | Ext_raw_markdown
  | Ext_rebase_relative_paths
  | Ext_short_subsuperscripts
  | Ext_shortcut_reference_links
  | Ext_simple_tables
  | Ext_smart
  | Ext_smart_quotes
  | Ext_special_strings
  | Ext_sourcepos
  | Ext_space_in_atx_header
  | Ext_spaced_reference_links
  | Ext_startnum
  | Ext_strikeout
  | Ext_subscript
  | Ext_superscript
  | Ext_styles
  | Ext_tagging
  | Ext_task_lists
  | Ext_table_captions
  | Ext_tex_math_dollars
  | Ext_tex_math_gfm
  | Ext_tex_math_double_backslash
  | Ext_tex_math_single_backslash
  | Ext_wikilinks_title_after_pipe
  | Ext_wikilinks_title_before_pipe
  | Ext_xrefs_name
  | Ext_xrefs_number
  | Ext_yaml_metadata_block
  /-- An extension identified by an arbitrary name. -/
  | CustomExtension (name : String)
  deriving DecidableEq, BEq, Repr, Inhabited

open Extension

-- ── The extension set ─────────────────────────────────────────────────

/-- A set of enabled extensions (kept duplicate-free). -/
structure Extensions where
  /-- The enabled extensions, in insertion order. -/
  exts : List Extension := []
  deriving Repr, Inhabited

/-- The empty extension set. -/
def emptyExtensions : Extensions := ⟨[]⟩

/-- Test whether an extension is enabled. -/
def extensionEnabled (ext : Extension) (es : Extensions) : Bool :=
  es.exts.contains ext

/-- Enable an extension (idempotent). -/
def enableExtension (ext : Extension) (es : Extensions) : Extensions :=
  if es.exts.contains ext then es else ⟨es.exts ++ [ext]⟩

/-- Disable an extension. -/
def disableExtension (ext : Extension) (es : Extensions) : Extensions :=
  ⟨es.exts.filter (· != ext)⟩

/-- Build an extension set from a list. -/
def extensionsFromList (exts : List Extension) : Extensions :=
  exts.foldl (fun acc e => enableExtension e acc) emptyExtensions

/-- The list of enabled extensions. -/
def extensionsToList (es : Extensions) : List Extension := es.exts

/-- `disableExtensions base remove` keeps only those extensions in `base` that
    are not in `remove`. -/
def disableExtensions (remove base : Extensions) : Extensions :=
  ⟨base.exts.filter (fun e => !remove.exts.contains e)⟩

/-- Set membership equality for extension sets (order-insensitive). -/
instance : BEq Extensions where
  beq a b := a.exts.all b.exts.contains && b.exts.all a.exts.contains

-- ── Name ↔ constructor table ──────────────────────────────────────────

/-- The bare (prefix-stripped) name of every named extension, paired with the
    constructor. Backs `showExtension`/`readExtension` and the universe. -/
def extensionNames : List (String × Extension) :=
  [ ("abbreviations", Ext_abbreviations)
  , ("alerts", Ext_alerts)
  , ("all_symbols_escapable", Ext_all_symbols_escapable)
  , ("amuse", Ext_amuse)
  , ("angle_brackets_escapable", Ext_angle_brackets_escapable)
  , ("ascii_identifiers", Ext_ascii_identifiers)
  , ("attributes", Ext_attributes)
  , ("auto_identifiers", Ext_auto_identifiers)
  , ("autolink_bare_uris", Ext_autolink_bare_uris)
  , ("backtick_code_blocks", Ext_backtick_code_blocks)
  , ("blank_before_blockquote", Ext_blank_before_blockquote)
  , ("blank_before_header", Ext_blank_before_header)
  , ("bracketed_spans", Ext_bracketed_spans)
  , ("citations", Ext_citations)
  , ("definition_lists", Ext_definition_lists)
  , ("east_asian_line_breaks", Ext_east_asian_line_breaks)
  , ("element_citations", Ext_element_citations)
  , ("emoji", Ext_emoji)
  , ("empty_paragraphs", Ext_empty_paragraphs)
  , ("epub_html_exts", Ext_epub_html_exts)
  , ("escaped_line_breaks", Ext_escaped_line_breaks)
  , ("example_lists", Ext_example_lists)
  , ("fancy_lists", Ext_fancy_lists)
  , ("fenced_code_attributes", Ext_fenced_code_attributes)
  , ("fenced_code_blocks", Ext_fenced_code_blocks)
  , ("fenced_divs", Ext_fenced_divs)
  , ("footnotes", Ext_footnotes)
  , ("four_space_rule", Ext_four_space_rule)
  , ("gfm_auto_identifiers", Ext_gfm_auto_identifiers)
  , ("grid_tables", Ext_grid_tables)
  , ("gutenberg", Ext_gutenberg)
  , ("hard_line_breaks", Ext_hard_line_breaks)
  , ("header_attributes", Ext_header_attributes)
  , ("table_attributes", Ext_table_attributes)
  , ("ignore_line_breaks", Ext_ignore_line_breaks)
  , ("implicit_figures", Ext_implicit_figures)
  , ("implicit_header_references", Ext_implicit_header_references)
  , ("inline_code_attributes", Ext_inline_code_attributes)
  , ("inline_notes", Ext_inline_notes)
  , ("intraword_underscores", Ext_intraword_underscores)
  , ("latex_macros", Ext_latex_macros)
  , ("line_blocks", Ext_line_blocks)
  , ("link_attributes", Ext_link_attributes)
  , ("lists_without_preceding_blankline", Ext_lists_without_preceding_blankline)
  , ("literate_haskell", Ext_literate_haskell)
  , ("mark", Ext_mark)
  , ("markdown_attribute", Ext_markdown_attribute)
  , ("markdown_in_html_blocks", Ext_markdown_in_html_blocks)
  , ("mmd_header_identifiers", Ext_mmd_header_identifiers)
  , ("mmd_link_attributes", Ext_mmd_link_attributes)
  , ("mmd_title_block", Ext_mmd_title_block)
  , ("multiline_tables", Ext_multiline_tables)
  , ("native_divs", Ext_native_divs)
  , ("native_spans", Ext_native_spans)
  , ("native_numbering", Ext_native_numbering)
  , ("ntb", Ext_ntb)
  , ("old_dashes", Ext_old_dashes)
  , ("pandoc_title_block", Ext_pandoc_title_block)
  , ("pipe_tables", Ext_pipe_tables)
  , ("raw_attribute", Ext_raw_attribute)
  , ("raw_html", Ext_raw_html)
  , ("raw_tex", Ext_raw_tex)
  , ("raw_markdown", Ext_raw_markdown)
  , ("rebase_relative_paths", Ext_rebase_relative_paths)
  , ("short_subsuperscripts", Ext_short_subsuperscripts)
  , ("shortcut_reference_links", Ext_shortcut_reference_links)
  , ("simple_tables", Ext_simple_tables)
  , ("smart", Ext_smart)
  , ("smart_quotes", Ext_smart_quotes)
  , ("special_strings", Ext_special_strings)
  , ("sourcepos", Ext_sourcepos)
  , ("space_in_atx_header", Ext_space_in_atx_header)
  , ("spaced_reference_links", Ext_spaced_reference_links)
  , ("startnum", Ext_startnum)
  , ("strikeout", Ext_strikeout)
  , ("subscript", Ext_subscript)
  , ("superscript", Ext_superscript)
  , ("styles", Ext_styles)
  , ("tagging", Ext_tagging)
  , ("task_lists", Ext_task_lists)
  , ("table_captions", Ext_table_captions)
  , ("tex_math_dollars", Ext_tex_math_dollars)
  , ("tex_math_gfm", Ext_tex_math_gfm)
  , ("tex_math_double_backslash", Ext_tex_math_double_backslash)
  , ("tex_math_single_backslash", Ext_tex_math_single_backslash)
  , ("wikilinks_title_after_pipe", Ext_wikilinks_title_after_pipe)
  , ("wikilinks_title_before_pipe", Ext_wikilinks_title_before_pipe)
  , ("xrefs_name", Ext_xrefs_name)
  , ("xrefs_number", Ext_xrefs_number)
  , ("yaml_metadata_block", Ext_yaml_metadata_block) ]

/-- Render an extension as its (prefix-stripped) name; a `CustomExtension`
    renders as its literal name. -/
def showExtension : Extension → String
  | CustomExtension t => t
  | e => (extensionNames.find? (·.2 == e)).map (·.1) |>.getD ""

/-- Read an extension from a bare name. `"lhs"` is an alias for
    `literate_haskell`; unknown names become a `CustomExtension`. -/
def readExtension (name : String) : Extension :=
  if name == "lhs" then Ext_literate_haskell
  else match extensionNames.find? (·.1 == name) with
       | some (_, e) => e
       | none => CustomExtension name

-- ── Named presets ─────────────────────────────────────────────────────

/-- The full pandoc-flavored Markdown extension set. -/
def pandocExtensions : Extensions := extensionsFromList
  [ Ext_footnotes, Ext_inline_notes, Ext_pandoc_title_block
  , Ext_yaml_metadata_block, Ext_table_captions, Ext_implicit_figures
  , Ext_simple_tables, Ext_multiline_tables, Ext_grid_tables
  , Ext_pipe_tables, Ext_citations, Ext_raw_tex, Ext_raw_html
  , Ext_tex_math_dollars, Ext_latex_macros, Ext_fenced_code_blocks
  , Ext_fenced_code_attributes, Ext_backtick_code_blocks
  , Ext_inline_code_attributes, Ext_raw_attribute
  , Ext_markdown_in_html_blocks, Ext_native_divs, Ext_fenced_divs
  , Ext_native_spans, Ext_bracketed_spans, Ext_escaped_line_breaks
  , Ext_fancy_lists, Ext_startnum, Ext_definition_lists, Ext_example_lists
  , Ext_all_symbols_escapable, Ext_intraword_underscores
  , Ext_blank_before_blockquote, Ext_blank_before_header
  , Ext_space_in_atx_header, Ext_strikeout, Ext_superscript, Ext_subscript
  , Ext_task_lists, Ext_auto_identifiers, Ext_header_attributes
  , Ext_table_attributes, Ext_link_attributes, Ext_implicit_header_references
  , Ext_line_blocks, Ext_shortcut_reference_links, Ext_smart ]

/-- Reduced extension set for plain-text output. -/
def plainExtensions : Extensions := extensionsFromList
  [ Ext_table_captions, Ext_implicit_figures, Ext_simple_tables
  , Ext_multiline_tables, Ext_grid_tables, Ext_latex_macros, Ext_fancy_lists
  , Ext_startnum, Ext_definition_lists, Ext_example_lists
  , Ext_intraword_underscores, Ext_blank_before_blockquote
  , Ext_blank_before_header, Ext_strikeout ]

/-- Minimal strict-Markdown extension set. -/
def strictExtensions : Extensions := extensionsFromList
  [ Ext_raw_html, Ext_shortcut_reference_links, Ext_spaced_reference_links ]

/-- PHP Markdown Extra compatible extension set. -/
def phpMarkdownExtraExtensions : Extensions := extensionsFromList
  [ Ext_footnotes, Ext_pipe_tables, Ext_raw_html, Ext_markdown_attribute
  , Ext_fenced_code_blocks, Ext_definition_lists, Ext_intraword_underscores
  , Ext_header_attributes, Ext_link_attributes, Ext_abbreviations
  , Ext_shortcut_reference_links, Ext_spaced_reference_links ]

/-- GitHub-flavored Markdown extension set. -/
def githubMarkdownExtensions : Extensions := extensionsFromList
  [ Ext_pipe_tables, Ext_raw_html, Ext_auto_identifiers
  , Ext_gfm_auto_identifiers, Ext_autolink_bare_uris, Ext_strikeout
  , Ext_task_lists, Ext_emoji, Ext_fenced_code_blocks
  , Ext_backtick_code_blocks, Ext_footnotes, Ext_alerts ]

/-- MultiMarkdown compatible extension set. -/
def multimarkdownExtensions : Extensions := extensionsFromList
  [ Ext_pipe_tables, Ext_raw_html, Ext_markdown_attribute
  , Ext_mmd_link_attributes, Ext_tex_math_double_backslash
  , Ext_tex_math_dollars, Ext_intraword_underscores, Ext_mmd_title_block
  , Ext_footnotes, Ext_definition_lists, Ext_all_symbols_escapable
  , Ext_implicit_header_references, Ext_shortcut_reference_links
  , Ext_auto_identifiers, Ext_mmd_header_identifiers, Ext_implicit_figures
  , Ext_short_subsuperscripts, Ext_subscript, Ext_superscript
  , Ext_backtick_code_blocks, Ext_spaced_reference_links, Ext_raw_attribute ]

/-- Extensions that apply universally, folded into every format. -/
def universalExtensions : Extensions :=
  extensionsFromList [ Ext_east_asian_line_breaks ]

-- ── Format → default extension set ────────────────────────────────────

/-- The default extension set for a named format. Unknown formats default to
    `Ext_auto_identifiers`. -/
def getDefaultExtensions : String → Extensions
  | "markdown_strict"   => strictExtensions
  | "markdown_phpextra" => phpMarkdownExtraExtensions
  | "markdown_mmd"      => multimarkdownExtensions
  | "markdown_github"   =>
      extensionsFromList (githubMarkdownExtensions.exts ++
        [ Ext_all_symbols_escapable, Ext_backtick_code_blocks
        , Ext_fenced_code_blocks, Ext_space_in_atx_header
        , Ext_intraword_underscores, Ext_lists_without_preceding_blankline
        , Ext_shortcut_reference_links ])
  | "markdown"          => pandocExtensions
  | "ipynb"             => extensionsFromList
      [ Ext_all_symbols_escapable, Ext_pipe_tables, Ext_raw_html
      , Ext_fenced_code_blocks, Ext_auto_identifiers, Ext_gfm_auto_identifiers
      , Ext_backtick_code_blocks, Ext_autolink_bare_uris
      , Ext_space_in_atx_header, Ext_intraword_underscores, Ext_strikeout
      , Ext_task_lists, Ext_lists_without_preceding_blankline
      , Ext_shortcut_reference_links, Ext_tex_math_dollars ]
  | "muse"              => extensionsFromList [ Ext_amuse, Ext_auto_identifiers ]
  | "plain"             => plainExtensions
  | "gfm"               => extensionsFromList
      [ Ext_pipe_tables, Ext_raw_html, Ext_auto_identifiers
      , Ext_gfm_auto_identifiers, Ext_autolink_bare_uris, Ext_strikeout
      , Ext_task_lists, Ext_emoji, Ext_yaml_metadata_block, Ext_footnotes
      , Ext_tex_math_dollars, Ext_tex_math_gfm, Ext_alerts ]
  | "commonmark"        => extensionsFromList [ Ext_raw_html ]
  | "commonmark_x"      => extensionsFromList
      [ Ext_pipe_tables, Ext_raw_html, Ext_gfm_auto_identifiers, Ext_strikeout
      , Ext_task_lists, Ext_emoji, Ext_smart, Ext_tex_math_dollars
      , Ext_superscript, Ext_subscript, Ext_definition_lists, Ext_footnotes
      , Ext_fancy_lists, Ext_fenced_divs, Ext_bracketed_spans
      , Ext_raw_attribute, Ext_implicit_header_references, Ext_attributes
      , Ext_alerts, Ext_yaml_metadata_block ]
  | "org"               => extensionsFromList
      [ Ext_citations, Ext_special_strings, Ext_task_lists, Ext_auto_identifiers ]
  | "html"  | "html4" | "html5" => extensionsFromList
      [ Ext_auto_identifiers, Ext_native_divs, Ext_line_blocks, Ext_native_spans ]
  | "epub"  | "epub2" | "epub3" => extensionsFromList
      [ Ext_raw_html, Ext_native_divs, Ext_native_spans, Ext_epub_html_exts ]
  | "latex"             => extensionsFromList
      [ Ext_smart, Ext_latex_macros, Ext_auto_identifiers ]
  | "beamer"            => extensionsFromList
      [ Ext_smart, Ext_latex_macros, Ext_auto_identifiers ]
  | "context"           => extensionsFromList [ Ext_smart, Ext_auto_identifiers ]
  | "textile"           => extensionsFromList
      [ Ext_old_dashes, Ext_smart, Ext_raw_html, Ext_auto_identifiers ]
  | "jats" | "jats_archiving" | "jats_publishing" | "jats_articleauthoring" =>
      extensionsFromList [ Ext_auto_identifiers ]
  | "opml"              => pandocExtensions
  | "markua"            => emptyExtensions
  | "typst"             => extensionsFromList [ Ext_citations, Ext_smart ]
  | "dokuwiki"          => extensionsFromList [ Ext_smart ]
  | _                   => extensionsFromList [ Ext_auto_identifiers ]

/-- Every named extension, combined with `universalExtensions`.

    Upstream restricts this per format for `--list-extensions` spec
    validation; that per-format table lives in the deferred App layer, so the
    in-scope core returns the full universe of named extensions. -/
def getAllExtensions (_fmt : String) : Extensions :=
  extensionsFromList (universalExtensions.exts ++ extensionNames.map (·.2))

end Linen.Text.Pandoc
