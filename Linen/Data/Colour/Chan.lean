/-
  Linen.Data.Colour.Chan — a single, phantom-tagged colour channel

  ## Haskell equivalent
  `Data.Colour.Chan` (internal to
  https://hackage.haskell.org/package/colour, not itself exported)

  ## Design
  Upstream is generic over any `Num`/`Fractional`/`Real` channel
  representation; Lean's stdlib has no equivalent numeric-class hierarchy
  (this repo isn't downstream of Mathlib), and every public `colour` module
  instantiates it at a floating-point type in practice, so `Chan` is
  specialized to `Float` directly rather than inventing one.

  The phantom type parameter `p` tags *which* colour space a channel
  belongs to (e.g. red vs. green vs. luminance) so unrelated channels can't
  be mixed by accident; it plays no runtime role, matching upstream.
-/

namespace Data.Colour

/-- A single channel value, tagged at the type level with the colour space
    `p` it belongs to. -/
structure Chan (p : Type) where
  val : Float
  deriving Repr, BEq

namespace Chan

/-- The zero channel value. -/
def empty (p : Type) : Chan p := ⟨0⟩

/-- The unit (fully saturated) channel value. -/
def full (p : Type) : Chan p := ⟨1⟩

/-- Scale a channel by a scalar. -/
def scale (s : Float) (c : Chan p) : Chan p := ⟨s * c.val⟩

/-- Add two channels. -/
def add (c₀ c₁ : Chan p) : Chan p := ⟨c₀.val + c₁.val⟩

/-- Invert a channel (`1 - x`). -/
def invert (c : Chan p) : Chan p := ⟨1 - c.val⟩

/-- Alpha-composite `c1` under `c0`, with `c0` occluding by opacity `a`.
    $$\text{over}(c_0, a, c_1) = c_0 + (1 - a) \cdot c_1$$ -/
def over (c₀ : Chan p) (a : Float) (c₁ : Chan p) : Chan p :=
  add c₀ (scale (1 - a) c₁)

/-- Sum a list of channels. -/
def sum (l : List (Chan p)) : Chan p := ⟨(l.map val).foldl (· + ·) 0⟩

end Chan
end Data.Colour
