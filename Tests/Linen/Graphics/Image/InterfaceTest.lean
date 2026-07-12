/-
  Tests for `Linen.Graphics.Image.Interface` — the `Pixel`/`ColorSpace`/
  `AlphaSpace` class hierarchy (exercised via small local fixture instances,
  since no concrete colour space is defined until module #4 onward), the
  `Image`/`Manifest`-backed array operations, `fromIx`/`toIx`, and
  `handleBorderIndex`'s five border-handling strategies.

  Fixture names are prefixed `imgIface` to avoid clashing with any other
  test file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.Interface

open Graphics.Image.Interface
open Graphics.Image.Interface.Elevator (Elevator)

-- ── Fixture: a toy single-channel colour space ──

/-- The one channel of `imgIfaceV`, a toy single-channel colour space. -/
inductive ImgIfaceVChan where
  | v
deriving BEq, Repr, Inhabited

/-- A toy single-channel pixel, standing in for a real colour space like `Y`
(not yet ported — see module #4 in `docs/imports/hip/dependencies.md`). -/
structure ImgIfaceVPixel (e : Type) where
  v : e
deriving BEq, Repr, Inhabited

instance : Pixel ImgIfaceVChan e (ImgIfaceVPixel e) where

instance [Elevator e] : ColorSpace ImgIfaceVChan e e where
  channels := [ImgIfaceVChan.v]
  toComponents px := px.v
  fromComponents c := ⟨c⟩
  promote x := ⟨x⟩
  getPxC px _ := px.v
  setPxC _ _ x := ⟨x⟩
  mapPxC f px := ⟨f ImgIfaceVChan.v px.v⟩
  liftPx f px := ⟨f px.v⟩
  liftPx2 f px1 px2 := ⟨f px1.v px2.v⟩
  foldlPx2 f z px1 px2 := f z px1.v px2.v

-- ── Fixture: a toy two-channel colour space with an alpha channel ──

/-- The two channels of `imgIfaceVA`, a toy value+alpha colour space. -/
inductive ImgIfaceVAChan where
  | v
  | a
deriving BEq, Repr, Inhabited

/-- A toy value+alpha pixel, whose `Opaque` counterpart is `ImgIfaceVPixel`.
-/
structure ImgIfaceVAPixel (e : Type) where
  v : e
  a : e
deriving BEq, Repr, Inhabited

instance : Pixel ImgIfaceVAChan e (ImgIfaceVAPixel e) where

instance [Elevator e] : ColorSpace ImgIfaceVAChan e (e × e) where
  channels := [ImgIfaceVAChan.v, ImgIfaceVAChan.a]
  toComponents px := (px.v, px.a)
  fromComponents | (v, a) => ⟨v, a⟩
  promote x := ⟨x, x⟩
  getPxC px
    | .v => px.v
    | .a => px.a
  setPxC px
    | .v => fun x => ⟨x, px.a⟩
    | .a => fun x => ⟨px.v, x⟩
  mapPxC f px := ⟨f .v px.v, f .a px.a⟩
  liftPx f px := ⟨f px.v, f px.a⟩
  liftPx2 f px1 px2 := ⟨f px1.v px2.v, f px1.a px2.a⟩
  foldlPx2 f z px1 px2 := f (f z px1.v px2.v) px1.a px2.a

instance : AlphaSpace ImgIfaceVAChan e ImgIfaceVChan where
  getAlpha px := px.a
  addAlpha a px := ⟨px.v, a⟩
  dropAlpha px := ⟨px.v⟩

-- ── `ColorSpace` operations ──

#guard (channels (cs := ImgIfaceVChan) (e := Int)) == [ImgIfaceVChan.v]
#guard getPxC (cs := ImgIfaceVChan) (e := Int) (⟨(7 : Int)⟩ : ImgIfaceVPixel Int) ImgIfaceVChan.v == 7
#guard (setPxC (cs := ImgIfaceVChan) (e := Int) (⟨(7 : Int)⟩ : ImgIfaceVPixel Int) ImgIfaceVChan.v 9)
  == ⟨9⟩
#guard (promote (cs := ImgIfaceVChan) (7 : Int)) == (⟨7⟩ : ImgIfaceVPixel Int)
#guard (liftPx (cs := ImgIfaceVChan) (e := Int) (· + 1) (⟨(7 : Int)⟩ : ImgIfaceVPixel Int)) == ⟨8⟩
#guard (liftPx2 (cs := ImgIfaceVChan) (e := Int) (· + ·) (⟨(3 : Int)⟩ : ImgIfaceVPixel Int) ⟨4⟩)
  == ⟨7⟩

#guard toListPx (cs := ImgIfaceVAChan) (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int) == [3, 4]
#guard foldlPx (cs := ImgIfaceVAChan) (· + ·) 0 (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int) == 7
#guard foldrPx (cs := ImgIfaceVAChan) (· + ·) 0 (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int) == 7
#guard foldl1Px (cs := ImgIfaceVAChan) max (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int) == 4

-- ── `AlphaSpace` operations ──

#guard getAlpha (cs := ImgIfaceVAChan) (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int) == 4
#guard addAlpha (cs := ImgIfaceVAChan) (4 : Int) (⟨(3 : Int)⟩ : ImgIfaceVPixel Int)
  == (⟨3, 4⟩ : ImgIfaceVAPixel Int)
#guard dropAlpha (cs := ImgIfaceVAChan) (e := Int) (⟨(3 : Int), 4⟩ : ImgIfaceVAPixel Int)
  == (⟨3⟩ : ImgIfaceVPixel Int)

-- ── `fromIx`/`toIx` ──

#guard fromIx 4 (2, 1) == 9
#guard toIx 4 9 == (2, 1)
#guard toIx 4 0 == (0, 0)
#guard toIx 4 11 == (2, 3)

-- ── `checkDims` ──

#guard checkDims "test" (3, 4) == (3, 4)

-- ── `Image`-level array operations, over plain `Int` "pixels" ──

def imgIfaceImg : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Int :=
  makeImage (2, 3) (fun (i, j) => i * 10 + j)

#guard dims imgIfaceImg == (2, 3)
#guard unsafeIndex imgIfaceImg (0, 0) == 0
#guard unsafeIndex imgIfaceImg (1, 2) == 12
#guard index00 imgIfaceImg == 0
#guard index imgIfaceImg (1, 2) == 12
#guard maybeIndex imgIfaceImg (1, 2) == some 12
#guard maybeIndex imgIfaceImg (5, 5) == none

#guard (scalar (42 : Int)).elems == #[42]
#guard dims (scalar (42 : Int)) == (1, 1)

def imgIfaceFromLists : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Int :=
  fromLists [[1, 2], [3, 4], [5, 6]]

#guard dims imgIfaceFromLists == (3, 2)
#guard unsafeIndex imgIfaceFromLists (0, 0) == 1
#guard unsafeIndex imgIfaceFromLists (1, 1) == 4
#guard unsafeIndex imgIfaceFromLists (2, 1) == 6

#guard dims (map (· + 1) imgIfaceImg) == (2, 3)
#guard unsafeIndex (map (· + 1) imgIfaceImg) (1, 2) == 13

#guard unsafeIndex (imap (fun (i, j) v => v + i + j) imgIfaceImg) (1, 2) == 15

#guard unsafeIndex (zipWith (· + ·) imgIfaceImg imgIfaceImg) (1, 2) == 24
#guard unsafeIndex (izipWith (fun (i, j) v1 v2 => v1 + v2 + i + j) imgIfaceImg imgIfaceImg) (1, 2)
  == 27

#guard unsafeIndex (transpose imgIfaceImg) (2, 1) == unsafeIndex imgIfaceImg (1, 2)
#guard dims (transpose imgIfaceImg) == (3, 2)

#guard unsafeIndex (backpermute (3, 2) (fun (i, j) => (j, i)) imgIfaceImg) (2, 1)
  == unsafeIndex imgIfaceImg (1, 2)

#guard fold (· + ·) 0 imgIfaceImg == (0 + 1 + 2 + 10 + 11 + 12 : Int)
#guard foldIx (fun acc (i, j) v => acc + v + i + j) 0 imgIfaceImg
  == (0 + 1 + 2 + 10 + 11 + 12 + (0+0 + 0+1 + 0+2 + 1+0 + 1+1 + 1+2) : Int)

#guard eq imgIfaceImg imgIfaceImg == true
#guard eq imgIfaceImg (map (· + 1) imgIfaceImg) == false

-- ── `traverse`/`traverse2` ──

#guard unsafeIndex (traverse imgIfaceImg (fun (m, n) => (m, n)) (fun get ij => get ij + 1))
  (1, 2) == 13

#guard unsafeIndex
  (traverse2 imgIfaceImg imgIfaceImg (fun mn _ => mn) (fun get1 get2 ij => get1 ij + get2 ij))
  (1, 2) == 24

-- ── `makeImageWindowed` ──

def imgIfaceWindowed : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 Int :=
  makeImageWindowed (3, 3) (1, 1) (1, 1) (fun _ => 1) (fun _ => 0)

#guard unsafeIndex imgIfaceWindowed (1, 1) == 1
#guard unsafeIndex imgIfaceWindowed (0, 0) == 0
#guard unsafeIndex imgIfaceWindowed (2, 2) == 0

-- ── Border handling: `handleBorderIndex` over the five strategies ──

-- A 3×3 "image" via a plain getter function `get (i, j) = 10*i + j`.
def imgIfaceGet (ij : Int × Int) : Int := 10 * ij.1 + ij.2

-- In-bounds indices are unaffected by any border strategy.
#guard handleBorderIndex .wrap (3, 3) imgIfaceGet (1, 1) == 11

-- `Fill`: constant pixel outside the bounds.
#guard handleBorderIndex (.fill (99 : Int)) (3, 3) imgIfaceGet (-1, 0) == 99
#guard handleBorderIndex (.fill (99 : Int)) (3, 3) imgIfaceGet (0, 3) == 99

-- `Wrap`: wraps around to the opposite edge.
#guard handleBorderIndex .wrap (3, 3) imgIfaceGet (-1, 0) == imgIfaceGet (2, 0)
#guard handleBorderIndex .wrap (3, 3) imgIfaceGet (0, 3) == imgIfaceGet (0, 0)
#guard handleBorderIndex .wrap (3, 3) imgIfaceGet (3, 0) == imgIfaceGet (0, 0)

-- `Edge`: replicates the nearest edge pixel.
#guard handleBorderIndex .edge (3, 3) imgIfaceGet (-1, 0) == imgIfaceGet (0, 0)
#guard handleBorderIndex .edge (3, 3) imgIfaceGet (3, 1) == imgIfaceGet (2, 1)
#guard handleBorderIndex .edge (3, 3) imgIfaceGet (1, -1) == imgIfaceGet (1, 0)
#guard handleBorderIndex .edge (3, 3) imgIfaceGet (1, 3) == imgIfaceGet (1, 2)

-- `Reflect`: mirrors, repeating the edge pixel.
#guard handleBorderIndex .reflect (3, 3) imgIfaceGet (-1, 0) == imgIfaceGet (0, 0)
#guard handleBorderIndex .reflect (3, 3) imgIfaceGet (-2, 0) == imgIfaceGet (1, 0)
#guard handleBorderIndex .reflect (3, 3) imgIfaceGet (3, 0) == imgIfaceGet (2, 0)
#guard handleBorderIndex .reflect (3, 3) imgIfaceGet (4, 0) == imgIfaceGet (1, 0)

-- `Continue`: mirrors, without repeating the edge pixel.
#guard handleBorderIndex .continue (3, 3) imgIfaceGet (-1, 0) == imgIfaceGet (1, 0)
#guard handleBorderIndex .continue (3, 3) imgIfaceGet (-2, 0) == imgIfaceGet (2, 0)
#guard handleBorderIndex .continue (3, 3) imgIfaceGet (3, 0) == imgIfaceGet (1, 0)
#guard handleBorderIndex .continue (3, 3) imgIfaceGet (4, 0) == imgIfaceGet (0, 0)

-- ── `borderIndex`/`defaultIndex` on a real `Image` ──

#guard borderIndex .wrap imgIfaceImg (-1, 0) == unsafeIndex imgIfaceImg (1, 0)
#guard borderIndex .edge imgIfaceImg (5, 5) == unsafeIndex imgIfaceImg (1, 2)
#guard defaultIndex (-1 : Int) imgIfaceImg (5, 5) == -1
#guard defaultIndex (-1 : Int) imgIfaceImg (1, 2) == unsafeIndex imgIfaceImg (1, 2)
