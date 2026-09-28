/-
  Tests for `Linen.Text.Pandoc.Translations`.
-/
import Linen.Text.Pandoc.Translations

namespace Tests.Linen.Text.Pandoc.Translations

open _root_.Linen.Text.Pandoc

-- ── Term ──────────────────────────────────────────────────────────────

#guard Term.ofName? "Figure" == some Term.Figure
#guard Term.ofName? "Nonsense" == none
#guard toString Term.ListOfTables == "ListOfTables"

-- ── readTranslations / lookupTerm ─────────────────────────────────────

private def tr : Option Translations :=
  (readTranslations "# a comment\nFigure: Figura\nTable: \"Tabla\"\n").toOption

#guard (tr.bind (·.lookupTerm Term.Figure)) == some "Figura"
#guard (tr.bind (·.lookupTerm Term.Table)) == some "Tabla"
#guard (tr.bind (·.lookupTerm Term.Page)) == none

-- ── Lang ──────────────────────────────────────────────────────────────

#guard (parseLang "en-US").toOption.map renderLang == some "en-US"
#guard (parseLang "zh-Hant-TW").toOption.map renderLang == some "zh-Hant-TW"
#guard ((parseLang "fr").toOption.map (·.langLanguage)) == some "fr"

end Tests.Linen.Text.Pandoc.Translations
