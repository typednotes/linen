/-
  Tests for `Linen.Text.Pandoc.Extensions`.
-/
import Linen.Text.Pandoc.Extensions

namespace Tests.Linen.Text.Pandoc.Extensions

open _root_.Linen.Text.Pandoc
open Extension

-- ── Enable / disable / membership ─────────────────────────────────────

#guard extensionEnabled Ext_footnotes emptyExtensions == false
#guard extensionEnabled Ext_footnotes (enableExtension Ext_footnotes emptyExtensions) == true
-- enabling is idempotent
#guard (enableExtension Ext_footnotes (enableExtension Ext_footnotes emptyExtensions)).exts.length == 1
#guard extensionEnabled Ext_footnotes
  (disableExtension Ext_footnotes (enableExtension Ext_footnotes emptyExtensions)) == false

-- ── Presets ───────────────────────────────────────────────────────────

#guard extensionEnabled Ext_footnotes pandocExtensions == true
#guard extensionEnabled Ext_citations pandocExtensions == true
#guard extensionEnabled Ext_hard_line_breaks pandocExtensions == false
#guard extensionEnabled Ext_task_lists githubMarkdownExtensions == true
#guard extensionEnabled Ext_emoji githubMarkdownExtensions == true
#guard extensionEnabled Ext_raw_html strictExtensions == true
#guard extensionEnabled Ext_footnotes strictExtensions == false

-- ── disableExtensions (set difference) ────────────────────────────────

#guard extensionEnabled Ext_raw_html
  (disableExtensions (extensionsFromList [Ext_raw_html]) pandocExtensions) == false
#guard extensionEnabled Ext_footnotes
  (disableExtensions (extensionsFromList [Ext_raw_html]) pandocExtensions) == true

-- ── show / read round-trip ────────────────────────────────────────────

#guard showExtension Ext_footnotes == "footnotes"
#guard showExtension (CustomExtension "myext") == "myext"
#guard readExtension "footnotes" == Ext_footnotes
#guard readExtension "lhs" == Ext_literate_haskell
#guard readExtension "not_a_real_extension" == CustomExtension "not_a_real_extension"

-- ── getDefaultExtensions ──────────────────────────────────────────────

#guard (getDefaultExtensions "markdown") == pandocExtensions
#guard (getDefaultExtensions "markdown_strict") == strictExtensions
#guard extensionEnabled Ext_auto_identifiers (getDefaultExtensions "some_unknown_format") == true
#guard extensionEnabled Ext_epub_html_exts (getDefaultExtensions "epub3") == true

end Tests.Linen.Text.Pandoc.Extensions
