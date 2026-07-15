/-
  `Linen.Text.DocLayout.Attributed` — font-attributed string runs.

  ## Haskell source

  Ported from `Text.DocLayout.Attributed` in the `doclayout` package
  (v0.5.0.3, `src/Text/DocLayout/Attributed.hs`).

  An `Attributed a` is an ordered run of `Attr a` chunks, each pairing a
  payload with a `Font` and an optional hyperlink `Link`.  Upstream backs the
  run with `Data.Sequence`; per the import plan we use a plain `List` (the
  payload is simply an ordered run of chunks).

  Provides the `Semigroup`/`Monoid` structure (as `Append`/`Inhabited` — Lean
  core has no `Monoid` class), `IsString`, `Functor`, and Haskell-style
  `Data.Foldable`/`Data.Traversable` instances.  The derived
  `Generic`/`Data`/`Typeable` instances are dropped (no Lean analogue).
-/

import Linen.Text.DocLayout.ANSIFont
import Linen.Data.String
import Linen.Data.Foldable
import Linen.Data.Traversable

namespace Text.DocLayout

open Data (Foldable Traversable)

/-- An optional hyperlink target. -/
abbrev Link := Option String

/- ── `Attr`: a single font-attributed chunk ───────────────────────── -/

/-- Font attributes over a payload `a`. -/
structure Attr (a : Type) where
  /-- Optional hyperlink target. -/
  link : Link := none
  /-- Font styling for this chunk. -/
  font : Font := baseFont
  /-- The payload. -/
  value : a
deriving Repr

instance [DecidableEq a] : DecidableEq (Attr a) := fun x y => by
  rcases x; rcases y with ⟨_, _, _⟩
  simp only [Attr.mk.injEq]
  exact inferInstance

/-- Convenience positional constructor matching upstream's `Attr l f x`. -/
@[inline] def mkAttr (l : Link) (f : Font) (x : a) : Attr a := ⟨l, f, x⟩

/-- Combining two `Attr`s keeps the left link/font and appends payloads.
This choice is arbitrary (see upstream comment). -/
instance [Append a] : Append (Attr a) where
  append x y := ⟨x.link, x.font, x.value ++ y.value⟩

instance [IsString a] : IsString (Attr a) where
  fromString x := ⟨none, baseFont, IsString.fromString x⟩

instance [IsString a] : Inhabited (Attr a) where
  default := ⟨none, baseFont, IsString.fromString ""⟩

instance : Functor Attr where
  map f x := ⟨x.link, x.font, f x.value⟩

instance : Foldable Attr where
  foldr f z x := f x.value z
  foldl f z x := f z x.value
  toList x := [x.value]

instance : Traversable Attr where
  traverse f x := (fun v => ⟨x.link, x.font, v⟩) <$> f x.value

/- ── `Attributed`: an ordered run of chunks ────────────────────────── -/

/-- A sequence of strings with font attributes. -/
structure Attributed (a : Type) where
  /-- The chunks, in order. -/
  chunks : List (Attr a)
deriving Repr

instance [DecidableEq a] : DecidableEq (Attributed a) := fun x y => by
  rcases x with ⟨xs⟩; rcases y with ⟨ys⟩
  simp only [Attributed.mk.injEq]
  exact inferInstance

/-- Build an `Attributed` from a list of chunks. -/
@[inline] def fromList (xs : List (Attr a)) : Attributed a := ⟨xs⟩

/-- Build a one-chunk `Attributed`. -/
@[inline] def singleton (x : Attr a) : Attributed a := ⟨[x]⟩

instance : Append (Attributed a) where
  append x y := ⟨x.chunks ++ y.chunks⟩

instance : Inhabited (Attributed a) where
  default := ⟨[]⟩

instance [IsString a] : IsString (Attributed a) where
  fromString x := ⟨[⟨none, baseFont, IsString.fromString x⟩]⟩

instance : Functor Attributed where
  map f x := ⟨x.chunks.map (fun c => f <$> c)⟩

instance : Foldable Attributed where
  foldr f z x := x.chunks.foldr (fun c acc => f c.value acc) z
  foldl f z x := x.chunks.foldl (fun acc c => f acc c.value) z
  toList x := x.chunks.map (·.value)

instance : Traversable Attributed where
  traverse f x :=
    (fun cs => (⟨cs⟩ : Attributed _)) <$>
      x.chunks.foldr
        (fun c acc =>
          (fun v cs => (⟨c.link, c.font, v⟩ : Attr _) :: cs) <$> f c.value <*> acc)
        (pure [])

end Text.DocLayout
