/-
  `Linen.Text.Pandoc.Writers.Math` — math rendering helpers.

  ## Haskell source

  Ported from `Text.Pandoc.Writers.Math` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Writers/Math.hs`).

  Upstream exports `texMathToInlines`, `convertMath`, and re-exports
  `defaultMathJaxURL`/`defaultKaTeXURL`.  `convertMath` is the generic engine:
  it maps a `MathType` to texmath's `DisplayType`, parses the TeX with
  `readTeX`, applies a supplied texmath writer (`writePandoc`, `writeMathML`,
  …), and on parse failure reports `CouldNotConvertTeXMath` and falls back to
  the raw math wrapped in `$`/`$$` delimiters.

  ### Deviations from upstream (raw / MathML-passthrough scope)

  Per `docs/imports/pandoc/dependencies.md`, the `texmath` TeX→MathML engine is
  **deferred**; the in-scope writers "degrade to raw/MathML passthrough".
  Since neither `readTeX` (TeX→`Exp`) nor the texmath writers are ported:

  * `convertMath` cannot parse TeX, so it always takes the failure branch:
    it reports `CouldNotConvertTeXMath` and returns the `mkFallback` inline
    (`Left`).  Its polymorphic writer argument and the successful `Right`
    branch — which need texmath's `Exp` AST — are therefore dropped; the
    signature keeps the free result type `α` so callers still type-check.
  * `texMathToInlines` consequently always yields the single fallback inline
    (`Str` wrapping the raw math in `$`/`$$`), matching upstream's behaviour
    when texmath conversion fails.
  * `defaultMathJaxURL`/`defaultKaTeXURL` are re-exported from `Options`
    upstream; `Options` does not carry them in this port, so they are defined
    here directly with pandoc's pinned CDN URLs.
-/

import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.Logging
import Linen.Text.Pandoc.Options
import Linen.Text.Pandoc.Class.PandocMonad

namespace Linen.Text.Pandoc.Writers

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.PandocMonad

/-- The default MathJax CDN URL (upstream re-exports this from `Options`). -/
def defaultMathJaxURL : String :=
  "https://cdn.jsdelivr.net/npm/mathjax@3/es5/"

/-- The default KaTeX CDN URL (upstream re-exports this from `Options`). -/
def defaultKaTeXURL : String :=
  "https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/"

/-- The raw-math fallback: wrap the original TeX in `$$` for display math or
    `$` for inline math (upstream's `mkFallback`). -/
def mkFallback : MathType → String → Inline
  | .DisplayMath, inp => .Str ("$$" ++ inp ++ "$$")
  | .InlineMath, inp => .Str ("$" ++ inp ++ "$")

variable {m : Type → Type} [Monad m] [PandocMonad m]

/-- Convert TeX math via a texmath writer.  Raw-passthrough scope: with the
    texmath engine deferred there is no `readTeX`/writer to run, so this always
    reports `CouldNotConvertTeXMath` and returns the raw-math fallback inline
    (`.error`); the free result type `α` mirrors upstream's polymorphic
    `Either Inline a`. -/
def convertMath {α : Type} (mt : MathType) (str : String) : m (Except Inline α) := do
  report (.CouldNotConvertTeXMath str "texmath engine not ported (raw passthrough)")
  pure (.error (mkFallback mt str))

/-- Convert TeX math to a list of inlines.  Raw-passthrough scope: always the
    single raw-math fallback inline (upstream returns the parsed inlines when
    texmath succeeds). -/
def texMathToInlines (mt : MathType) (inp : String) : m (List Inline) := do
  match ← convertMath (α := List Inline) mt inp with
  | .ok ils => pure ils
  | .error il => pure [il]

end Linen.Text.Pandoc.Writers
