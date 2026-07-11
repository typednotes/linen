/-
  Linen.Data.Colour.Internal — the human perception of colour

  ## Haskell equivalent
  `Data.Colour.Internal` from https://hackage.haskell.org/package/colour

  ## Design
  As with `Chan`, upstream's `Colour a`/`AlphaColour a` are generic over any
  `Num`/`Fractional`/`Real` channel representation; both are specialized to
  `Float` here, and `colourConvert`/`alphaColourConvert` (upstream's
  representation converters) are dropped as no-ops.

  Upstream ties `blend`/`over`/`darken` together across `Colour` and
  `AlphaColour` with the `AffineSpace`/`ColourOps` type classes, but those
  classes only ever have these two instances — a closed world — so here
  they're plain functions namespaced under `Colour`/`AlphaColour`, which
  gives the same overloaded call sites (`Colour.blend`, `AlphaColour.blend`,
  …) without an abstraction that buys no genericity.

  `quantize` is deferred to `Data.Colour.SRGB`, the first module that fixes
  its bound type (`UInt8`) concretely.
-/
import Linen.Data.Colour.Chan

namespace Data.Colour

/-- Phantom tag for a `Chan`'s red channel. -/
inductive Red where
  | mk
  deriving BEq

/-- Phantom tag for a `Chan`'s green channel. -/
inductive Green where
  | mk
  deriving BEq

/-- Phantom tag for a `Chan`'s blue channel. -/
inductive Blue where
  | mk
  deriving BEq

/-- Phantom tag for an `AlphaColour`'s opacity channel. -/
inductive Alpha where
  | mk
  deriving BEq

/-- The human perception of colour, stored internally in linear
    ITU-R BT.709 RGB colour space. -/
structure Colour where
  r : Chan Red
  g : Chan Green
  b : Chan Blue
  deriving BEq

namespace Colour

/-- The colourless colour; the identity colour in additive colour spaces. -/
def black : Colour := ⟨Chan.empty _, Chan.empty _, Chan.empty _⟩

/-- Adds two colours (may take you out of gamut; prefer `blend`). -/
def add (c₀ c₁ : Colour) : Colour :=
  ⟨Chan.add c₀.r c₁.r, Chan.add c₀.g c₁.g, Chan.add c₀.b c₁.b⟩

/-- Sums a list of colours (may take you out of gamut; prefer `blend`). -/
def sum (l : List Colour) : Colour :=
  ⟨Chan.sum (l.map r), Chan.sum (l.map g), Chan.sum (l.map b)⟩

/-- Blends a colour with black without changing its opacity. -/
def darken (s : Float) (c : Colour) : Colour :=
  ⟨Chan.scale s c.r, Chan.scale s c.g, Chan.scale s c.b⟩

/-- Computes the affine combination (weighted average) of colours, giving
    the last parameter the remaining weight. Weights can be negative or
    greater than 1.0; non-convex combinations may go out of gamut. -/
def affineCombo (l : List (Float × Colour)) (z : Colour) : Colour :=
  let total := (l.map Prod.fst).foldl (· + ·) 0
  (((1 - total, z) :: l).map fun (w, c) => darken w c).foldl add black

/-- The weighted average of two colours: `blend 0.4 a b = 0.4*a + 0.6*b`.
    The weight can be negative or greater than 1.0; non-convex combinations
    may go out of gamut. -/
def blend (weight : Float) (c₀ c₁ : Colour) : Colour := affineCombo [(weight, c₀)] c₁

end Colour

/-- A `Colour` that may be semi-transparent, stored internally as
    premultiplied alpha. -/
structure AlphaColour where
  colour : Colour
  alpha : Chan Alpha
  deriving BEq

namespace AlphaColour

/-- The entirely transparent `AlphaColour`, with no associated colour
    channel. -/
def transparent : AlphaColour := ⟨Colour.black, Chan.empty _⟩

/-- Creates an opaque `AlphaColour` from a `Colour`. -/
def «opaque» (c : Colour) : AlphaColour := ⟨c, Chan.full _⟩

/-- Blends an `AlphaColour` with black without changing its opacity. -/
def darken (s : Float) (c : AlphaColour) : AlphaColour := ⟨Colour.darken s c.colour, c.alpha⟩

/-- Makes an `AlphaColour` more transparent by a factor of `o`. -/
def dissolve (o : Float) (c : AlphaColour) : AlphaColour :=
  ⟨Colour.darken o c.colour, Chan.scale o c.alpha⟩

/-- Creates an `AlphaColour` from a `Colour` with a given opacity;
    `withOpacity c o == dissolve o (opaque c)`. -/
def withOpacity (c : Colour) (o : Float) : AlphaColour := ⟨Colour.darken o c, ⟨o⟩⟩

/-- Adds two premultiplied `AlphaColour`s directly (internal helper for
    `affineCombo`; not exposed upstream either). -/
def add (c₀ c₁ : AlphaColour) : AlphaColour :=
  ⟨Colour.add c₀.colour c₁.colour, Chan.add c₀.alpha c₁.alpha⟩

/-- Computes the affine combination (weighted average) of `AlphaColour`s,
    giving the last parameter the remaining weight. -/
def affineCombo (l : List (Float × AlphaColour)) (z : AlphaColour) : AlphaColour :=
  let total := (l.map Prod.fst).foldl (· + ·) 0
  (((1 - total, z) :: l).map fun (w, c) => dissolve w c).foldl add transparent

/-- The weighted average of two `AlphaColour`s. -/
def blend (weight : Float) (c₀ c₁ : AlphaColour) : AlphaColour := affineCombo [(weight, c₀)] c₁

/-- Returns the opacity of an `AlphaColour`. -/
def alphaChannel (c : AlphaColour) : Float := c.alpha.val

/-- Returns the colour of an `AlphaColour`. `colourChannel transparent` is
    discouraged: it may return `nan`. -/
def colourChannel (c : AlphaColour) : Colour := Colour.darken (1 / c.alpha.val) c.colour

end AlphaColour

namespace Colour

/-- `c₀.over c₁` composites the `AlphaColour` `c₀` over the `Colour` `c₁`. -/
def over (c₀ : AlphaColour) (c₁ : Colour) : Colour :=
  ⟨Chan.over c₀.colour.r c₀.alpha.val c₁.r,
   Chan.over c₀.colour.g c₀.alpha.val c₁.g,
   Chan.over c₀.colour.b c₀.alpha.val c₁.b⟩

end Colour

namespace AlphaColour

/-- `c₀.over c₁` composites the `AlphaColour` `c₀` over the `AlphaColour`
    `c₁`. -/
def over (c₀ c₁ : AlphaColour) : AlphaColour :=
  ⟨Colour.over c₀ c₁.colour, Chan.over c₀.alpha c₀.alpha.val c₁.alpha⟩

/-- `c₀.atop c₁` covers the portion of `c₁` visible by `c₀`; the resulting
    alpha channel is always `c₁`'s. -/
def atop (c₀ c₁ : AlphaColour) : AlphaColour :=
  ⟨Colour.add (Colour.darken c₁.alpha.val c₀.colour) (Colour.darken (1 - c₀.alpha.val) c₁.colour),
   c₁.alpha⟩

end AlphaColour
end Data.Colour
