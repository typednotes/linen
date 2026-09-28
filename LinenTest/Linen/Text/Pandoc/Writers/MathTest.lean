/-
  Tests for `Linen.Text.Pandoc.Writers.Math` (raw / MathML-passthrough scope).
-/
import Linen.Text.Pandoc.Writers.Math
import Linen.Text.Pandoc.Class.PandocPure

namespace Tests.Linen.Text.Pandoc.Writers.Math

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Writers
open _root_.Linen.Text.Pandoc.PandocMonad

-- ── The raw-math fallback wrapping ────────────────────────────────────

#guard mkFallback .InlineMath "a+b" == Inline.Str "$a+b$"
#guard mkFallback .DisplayMath "a+b" == Inline.Str "$$a+b$$"

-- ── Default CDN URLs ──────────────────────────────────────────────────

#guard defaultMathJaxURL == "https://cdn.jsdelivr.net/npm/mathjax@3/es5/"
#guard defaultKaTeXURL == "https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/"

-- ── texMathToInlines degrades to the raw fallback (texmath deferred) ──

#guard (runPure (texMathToInlines .InlineMath "x^2")).toOption
  == some [Inline.Str "$x^2$"]
#guard (runPure (texMathToInlines .DisplayMath "\\sum x")).toOption
  == some [Inline.Str "$$\\sum x$$"]

-- convertMath always takes the failure branch and returns the fallback
#guard (match (runPure (convertMath (α := List Inline) .InlineMath "y")).toOption with
        | some (.error (Inline.Str s)) => s == "$y$"
        | _ => false)

-- the failure is reported to the log
#guard ((runPure (do
  let _ ← texMathToInlines .InlineMath "z"
  getLog)).toOption.map (·.length)) == some 1

end Tests.Linen.Text.Pandoc.Writers.Math
