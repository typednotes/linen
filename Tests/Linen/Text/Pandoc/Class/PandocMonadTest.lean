/-
  Tests for `Linen.Text.Pandoc.Class.PandocMonad` (exercised through the pure
  `PandocPure` instance).
-/
import Linen.Text.Pandoc.Class.PandocPure

namespace Tests.Linen.Text.Pandoc.Class.PandocMonad

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.PandocMonad

-- ── verbosity / logging ───────────────────────────────────────────────

#guard (runPure (do setVerbosity Verbosity.INFO; getVerbosity)).toOption == some Verbosity.INFO
#guard (runPure (do report (LogMessage.IgnoredElement "x"); (·.length) <$> getLog)).toOption == some 1

-- ── media bag ─────────────────────────────────────────────────────────

#guard (runPure (do
          insertMedia "a.png" none (String.toUTF8 "x")
          let mb ← getMediaBag
          pure mb.mediaDirectory.length)).toOption == some 1

-- fetchItem reads from the media bag first
#guard (runPure (do
          insertMedia "a.png" (some "image/png") (String.toUTF8 "x")
          let (bytes, mime) ← fetchItem "a.png"
          pure (bytes.size, mime))).toOption == some (1, some "image/png")

-- ── input/output files ────────────────────────────────────────────────

#guard (runPure (do setInputFiles ["in.md"]; getInputFiles)).toOption == some ["in.md"]
#guard (runPure (do setOutputFile (some "out.html"); getOutputFile)).toOption == some (some "out.html")

-- ── timestamp honours SOURCE_DATE_EPOCH via the pure env (absent ⇒ 0) ──

#guard (runPure getTimestamp).toOption == some (0 : Int)

-- ── translations ──────────────────────────────────────────────────────

#guard (runPure (do setTranslations { langLanguage := "en" }; translateTerm Term.Figure)).toOption == some ""

end Tests.Linen.Text.Pandoc.Class.PandocMonad
