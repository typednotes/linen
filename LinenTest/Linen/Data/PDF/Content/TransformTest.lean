import Linen.Data.PDF.Content.Transform

open Data.PDF.Content.Transform

-- ── `identity` ──

#guard (identity : Transform Int) == ⟨1, 0, 0, 1, 0, 0⟩

#guard transform (identity : Transform Int) ⟨3, 4⟩ == ⟨3, 4⟩

-- ── `translation` / `translate` ──

#guard translation (2 : Int) 5 == (⟨1, 0, 0, 1, 2, 5⟩ : Transform Int)

#guard transform (translation (2 : Int) 5) ⟨0, 0⟩ == (⟨2, 5⟩ : Vector Int)

#guard translate (2 : Int) 5 identity == translation 2 5

-- ── `scale` ──

#guard scale (2 : Int) 3 == (⟨2, 0, 0, 3, 0, 0⟩ : Transform Int)

#guard transform (scale (2 : Int) 3) ⟨5, 7⟩ == (⟨10, 21⟩ : Vector Int)

-- ── `multiply` ──

-- Combining two scales multiplies their factors component-wise.
#guard multiply (scale (2 : Int) 3) (scale 5 7) == scale (2 * 5) (3 * 7)

-- Multiplying by the identity on either side is a no-op.
#guard multiply (identity : Transform Int) (translation 2 5) == translation 2 5
#guard multiply (translation (2 : Int) 5) identity == translation 2 5

-- Applying `multiply s t` to a vector matches applying `s` then `t`.
#guard transform (multiply (scale (2 : Int) 3) (translation 10 20)) ⟨1, 1⟩
  == transform (translation 10 20) (transform (scale 2 3) ⟨1, 1⟩)
