/-
  Linen.Data.Colour — the human perception of colour

  ## Haskell equivalent
  `Data.Colour` from https://hackage.haskell.org/package/colour

  ## Design
  Upstream's `Data.Colour` is chiefly a re-export facade over
  `Data.Colour.Internal` (`Colour`, `AlphaColour`, `black`, `opaque`,
  `withOpacity`, `transparent`, `alphaChannel`, `blend`, `dissolve`, `atop`),
  plus custom `Show`/`Read` instances for `Colour`/`AlphaColour` that print
  and parse through `Data.Colour.SRGB.Linear`'s `rgb`/`toRGB` constructor.

  Those custom instances are dropped, per the same precedent as
  `Data.Colour.CIE.Chromaticity`: `Colour`/`AlphaColour` already derive
  `Repr`, which serves the same illustrative purpose without hand-rolled
  parsing. Everything else this module exposes already lives in
  `Data.Colour.Internal`, so this module simply gathers the transitive
  imports that give the rest of the `colour` package (`CIE`, `Names`,
  `SRGB`) a single entry point, matching upstream's dependency shape.
-/
import Linen.Data.Colour.CIE.Chromaticity
import Linen.Data.Colour.Internal
import Linen.Data.Colour.SRGB.Linear
