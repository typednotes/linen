import Linen.Codec.Picture.Types
import Linen.Data.Set

/-!
  Port of `Codec.Picture.ColorQuant` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 7 of 29). Colour
  quantisation for building an image's palette: a modified median-cut
  algorithm (`medianMeanCutQuantization`, the default), a one-pass uniform
  quantiser, and ordered dithering.

  Upstream's `palettizeWithAlpha` (plus its two small helpers `alphaToBlack`/
  `alphaTo255`) needs `Codec.Picture.Gif`'s `GifFrame`/`GifDisposalMethod`/
  `GifDelay` types, creating a genuine mutual dependency with the (much
  later) GIF codec — `Codec.Picture.Gif` itself needs this module's
  `palettize` for palette generation on encode. That cycle is broken the
  same way upstream's own module boundary already implies: this module ports
  everything *except* `palettizeWithAlpha`, and `palettizeWithAlpha` is
  ported alongside `Linen.Codec.Picture.Gif` once that module exists (see
  `docs/imports/JuicyPixels/dependencies.md`).

  Upstream's `Data.Set Cluster`/`Set.deleteFindMax`-driven priority queue is
  ported onto `Linen.Data.Set`'s `Data.Set'` (`deleteFindMax` doesn't exist
  there, so it is assembled from `findMax` + `delete`). Lean's standard
  library has no `Ord` instance for `Float`/`Float32` (unlike Haskell's
  derived `Ord Double`, which is a real, if `NaN`-unsound, total order in
  practice) or for the pixel/`Cluster` types built from it, so this module
  defines the (file-local) linear-order instances `Cluster`/`PixelRGBF`/
  `Float32` needed to reproduce the same priority-queue behaviour.

  Upstream's small `Control.Foldl`-style `Fold`/`Pair` applicative (there to
  fuse the min/max/mean/volume computation of `mkCluster` into one traversal
  of the pixel array) is a Haskell performance trick with no bearing on
  observable behaviour; this module instead computes each aggregate with its
  own straightforward `Array.foldl`.

  `initCluster`'s upstream sampling walks `imageData`'s raw component array
  directly via `unsafePixelAt`; this module instead samples through
  `Image.getPixel`, matching this library's established typed-access
  convention for `Image` (see `Linen.Codec.Picture.Types`'s module
  doc-comment).

  `uniformQuantization`'s `paletteIndex` upstream does an `elemIndex` linear
  search over the (list-comprehension-built) palette list; ported here via
  `List.finIdxOf?` over the same list, for the same result.
-/

namespace Codec.Picture

-- ── Palette options ──

/-- Which palette-creation algorithm `palettize` should use. -/
inductive PaletteCreationMethod where
  /-- Median-mean-cut, the best-looking results at higher computational
      cost. -/
  | medianMeanCut
  /-- A very fast, single-pass algorithm; does not produce good-looking
      results. -/
  | uniform

/-- How the palette will be created. -/
structure PaletteOptions where
  /-- Algorithm used to find the palette. -/
  paletteCreationMethod : PaletteCreationMethod
  /-- Whether to apply ordered dithering to the image. Enabling it often
      reduces compression ratio but enhances the perceived quality of the
      final image. -/
  enableImageDithering : Bool
  /-- Maximum number of colours wanted in the palette. -/
  paletteColorCount : Nat

/-- Default palette options, aiming for best quality and the maximum
    possible colour count (256). -/
def defaultPaletteOptions : PaletteOptions :=
  { paletteCreationMethod := .medianMeanCut, enableImageDithering := true, paletteColorCount := 256 }

-- ── Shared building blocks ──

private def cmpU8 (a b : Pixel8) : Ordering :=
  if a < b then .lt else if b < a then .gt else .eq

local instance : Ord PixelRGB8 where
  compare a b := ((cmpU8 a.r b.r).then (cmpU8 a.g b.g)).then (cmpU8 a.b b.b)

/-- Determine the set of distinct colours in `img`, unless there are more
    than `maxColorCount` of them (in which case the returned set is
    `Data.Set'.empty` and the `Bool` is `false`). -/
def isColorCountBelow (maxColorCount : Nat) (img : Image PixelRGB8) : Data.Set' PixelRGB8 × Bool :=
  Id.run do
    let mut colors : Data.Set' PixelRGB8 := ∅
    for y in [0:img.height] do
      for x in [0:img.width] do
        colors := colors.insert' (img.getPixel x y)
        if colors.size' > maxColorCount then
          return (Data.Set'.empty, false)
    return (colors, true)

def vecToPalette (ps : Array PixelRGB8) : Palette :=
  generateImage (fun x _ => ps[x]!) ps.size 1

def listToPalette (ps : List PixelRGB8) : Palette :=
  generateImage (fun x _ => ps[x]!) ps.length 1

/-- Euclidean distance squared between two pixels. -/
def dist2Px (p1 p2 : PixelRGB8) : Int :=
  let dr : Int := p1.r.toNat - p2.r.toNat
  let dg : Int := p1.g.toNat - p2.g.toNat
  let db : Int := p1.b.toNat - p2.b.toNat
  dr * dr + dg * dg + db * db

/-- Index into `ps` of the colour nearest to `p`. -/
def nearestColorIdx (p : PixelRGB8) (ps : Array PixelRGB8) : Pixel8 :=
  Id.run do
    let mut bestIdx : Nat := 0
    let mut bestDist : Int := 0
    let mut first := true
    for i in [0:ps.size] do
      let d := dist2Px ps[i]! p
      if first || d < bestDist then
        bestIdx := i
        bestDist := d
        first := false
    return bestIdx.toUInt8

-- ── Dithering ──

/-- Add a dither mask to an image for ordered dithering. Uses a small,
    spatially stable dithering algorithm based on magic numbers and
    arithmetic inspired by Øyvind Kolås's *a dither* algorithm, 2013
    (<http://pippin.gimp.org/a_dither/>). -/
def dither (x y : Nat) (p : PixelRGB8) : PixelRGB8 :=
  -- Should view 16 as a parameter that can be optimized for best-looking
  -- results.
  let x' := 119 * x
  let y' := 28084 * y
  let r' := min 255 (p.r.toNat + ((x' + y') &&& 16))
  let g' := min 255 (p.g.toNat + ((x' + y' + 7973) &&& 16))
  let b' := min 255 (p.b.toNat + ((x' + y' + 15946) &&& 16))
  ⟨r'.toUInt8, g'.toUInt8, b'.toUInt8⟩

-- ── Modified median-cut algorithm ──
--
-- Based on the OCaml implementation at
-- <http://rosettacode.org/wiki/Color_quantization>, in turn based on
-- <https://www.leptonica.org/papers/mediancut.pdf>. Uses the product of
-- volume and population to determine the next cluster to split, and the
-- parent cluster's mean (not its median, despite the name) to place each
-- colour.

/-- An RGB8 pixel packed into a single machine word, for compact storage of
    a cluster's member pixels. -/
abbrev PackedRGB := UInt32

def rgbIntPack (p : PixelRGB8) : PackedRGB :=
  (p.r.toUInt32 <<< 16) ||| (p.g.toUInt32 <<< 8) ||| p.b.toUInt32

def rgbIntUnpack (v : PackedRGB) : PixelRGB8 :=
  ⟨(v >>> 16).toUInt8, (v >>> 8).toUInt8, v.toUInt8⟩

private def fromRGB8 (p : PixelRGB8) : PixelRGBF :=
  ⟨p.r.toNat.toFloat32, p.g.toNat.toFloat32, p.b.toNat.toFloat32⟩

private def toRGB8 (p : PixelRGBF) : PixelRGB8 :=
  ⟨p.r.round.toUInt8, p.g.round.toUInt8, p.b.round.toUInt8⟩

private def inf : PixelF := 1.0 / 0.0

/-- A cluster of similar colours, plus the bookkeeping (`value`, `meanColor`,
    `dims`) used to pick the next cluster to split and where to place each
    colour. -/
structure Cluster where
  /-- `volume * population`; the priority used to choose the next cluster to
      split. -/
  value : PixelF
  meanColor : PixelRGBF
  /-- Per-channel extent (`max - min`) of the cluster's colours. -/
  dims : PixelRGBF
  colors : Array PackedRGB

private def cmpF32 (a b : PixelF) : Ordering :=
  if a < b then .lt else if b < a then .gt else .eq

private def cmpRGBF (a b : PixelRGBF) : Ordering :=
  (cmpF32 a.r b.r).then (cmpF32 a.g b.g) |>.then (cmpF32 a.b b.b)

local instance : Ord Cluster where
  compare a b :=
    ((cmpF32 a.value b.value).then (cmpRGBF a.meanColor b.meanColor)).then (cmpRGBF a.dims b.dims)

/-- Build a cluster's summary statistics (`value`/`meanColor`/`dims`) from
    its member pixels. -/
def mkCluster (colors : Array PackedRGB) : Cluster :=
  let pixels := colors.map (fromRGB8 ∘ rgbIntUnpack)
  let n := pixels.size
  let mini := pixels.foldl (fun acc p => ⟨min acc.r p.r, min acc.g p.g, min acc.b p.b⟩) (⟨inf, inf, inf⟩ : PixelRGBF)
  let maxi := pixels.foldl (fun acc p => ⟨max acc.r p.r, max acc.g p.g, max acc.b p.b⟩)
    (⟨-inf, -inf, -inf⟩ : PixelRGBF)
  let total := pixels.foldl (fun acc p => ⟨acc.r + p.r, acc.g + p.g, acc.b + p.b⟩) (⟨0, 0, 0⟩ : PixelRGBF)
  let nf := n.toFloat32
  let mean : PixelRGBF := ⟨total.r / nf, total.g / nf, total.b / nf⟩
  let dims : PixelRGBF := ⟨maxi.r - mini.r, maxi.g - mini.g, maxi.b - mini.b⟩
  let vol := dims.r * dims.g * dims.b
  { value := vol * nf, meanColor := mean, dims := dims, colors := colors }

/-- The colour channel with the largest extent. -/
inductive Axis where
  | rAxis
  | gAxis
  | bAxis

def maxAxis (p : PixelRGBF) : Axis :=
  match cmpF32 p.r p.g, cmpF32 p.r p.b, cmpF32 p.g p.b with
  | .gt, .gt, _ => .rAxis
  | .lt, .gt, _ => .gAxis
  | .gt, .lt, _ => .bAxis
  | .lt, .lt, .gt => .gAxis
  | .eq, .gt, _ => .rAxis
  | _, _, _ => .bAxis

/-- Split a cluster about its largest axis, using the mean to divide the
    pixels. -/
def subdivide (cluster : Cluster) : Cluster × Cluster :=
  let m := cluster.meanColor
  let cond : PackedRGB → Bool := fun v =>
    let p := rgbIntUnpack v
    match maxAxis cluster.dims with
    | .rAxis => p.r.toNat.toFloat32 < m.r
    | .gAxis => p.g.toNat.toFloat32 < m.g
    | .bAxis => p.b.toNat.toFloat32 < m.b
  let (px1, px2) := cluster.colors.partition cond
  (mkCluster px1, mkCluster px2)

/-- Sample `img` down to a single seed cluster. -/
def initCluster (img : Image PixelRGB8) : Cluster :=
  let samplingFactor := 3
  let subSampling := samplingFactor * samplingFactor
  let w := img.width
  let n := (w * img.height) / subSampling
  let packer (ix : Nat) : PackedRGB :=
    let linIdx := ix * subSampling
    rgbIntPack (img.getPixel (linIdx % w) (linIdx / w))
  mkCluster ((Array.range n).map packer)

/-- Take the cluster with the largest `value` (volume × population) out of
    the priority queue, subdivide it about its largest axis, and put the two
    new clusters back on the queue. -/
def split (cs : Data.Set' Cluster) : Data.Set' Cluster :=
  match cs.findMax with
  -- Unreachable in practice: `split` is only ever called on a non-empty
  -- queue (`clusters` always seeds it with `initCluster`'s result first).
  | none => cs
  | some c =>
    let (c1, c2) := subdivide c
    ((cs.delete c).insert' c1).insert' c2

/-- Keep splitting the initial cluster until there are `maxCols` clusters. -/
def clusters (maxCols : Nat) (img : Image PixelRGB8) : Data.Set' Cluster :=
  let c := initCluster img
  let rec go : Nat → Data.Set' Cluster
    | 0 => Data.Set'.singleton c
    | n + 1 => split (go n)
  go (maxCols - 1)

def mkPaletteVec (cs : List Cluster) : Array PixelRGB8 :=
  (cs.map (toRGB8 ∘ Cluster.meanColor)).toArray

/-- Modified median-cut algorithm with optional ordered dithering. Returns an
    image of `Pixel8` acting as a matrix of indices into the `Palette`. -/
def medianMeanCutQuantization (opts : PaletteOptions) (img : Image PixelRGB8) : Image Pixel8 × Palette :=
  let maxColorCount := opts.paletteColorCount
  let (okColors, isBelow) := isColorCountBelow maxColorCount img
  if isBelow then
    let okPaletteVec := okColors.toAscList.toArray
    (pixelMap (nearestColorIdx · okPaletteVec) img, vecToPalette okPaletteVec)
  else
    let cs := (clusters maxColorCount img).toAscList
    let paletteVec := mkPaletteVec cs
    let palette := vecToPalette paletteVec
    if opts.enableImageDithering then
      (pixelMap (nearestColorIdx · paletteVec) (pixelMapXY dither img), palette)
    else
      (pixelMap (nearestColorIdx · paletteVec) img, palette)

-- ── Uniform (one-pass) quantization ──

/-- Divide `n`'s bit budget (`⌊log₂ n⌋`) among the three colour channels,
    with priority order green, red, blue. -/
def bitDiv3 (n : Nat) : Nat × Nat × Nat :=
  let m := Nat.log2 n
  let q := m / 3
  match m % 3 with
  | 0 => (q, q, q)
  | 1 => (q + 1, q, q)
  | _ => (q + 1, q + 1, q)

/-- `[0, step, 2*step, ...]` up to and including `limit`, matching Haskell's
    `[0, step .. limit]` arithmetic sequence (`step` is always a positive
    power of two here). -/
private def stepsUpTo (step limit : Nat) : List Nat :=
  (List.range (limit / step + 1)).map (· * step)

/-- A naive, one-pass colour-quantisation algorithm. Simply takes the most
    significant bits: `maxCols` is rounded down to the nearest power of two,
    and its bits are divided among the three colour channels with priority
    order green, red, blue. Returns an image of `Pixel8` acting as a matrix
    of indices into the `Palette`. -/
def uniformQuantization (opts : PaletteOptions) (img : Image PixelRGB8) : Image Pixel8 × Palette :=
  let maxCols := opts.paletteColorCount
  let (bg, br, bb) := bitDiv3 maxCols
  let (dr, dg, db) := (2 ^ (8 - br), 2 ^ (8 - bg), 2 ^ (8 - bb))
  let rs := stepsUpTo dr 255
  let gs := stepsUpTo dg 255
  let bs := stepsUpTo db 255
  let paletteList := rs.flatMap fun r => gs.flatMap fun g => bs.map fun b =>
    (⟨r.toUInt8, g.toUInt8, b.toUInt8⟩ : PixelRGB8)
  let palette := listToPalette paletteList
  let paletteIndex : PixelRGB8 → Pixel8 := fun p =>
    let masked : PixelRGB8 :=
      ⟨(p.r.toNat - p.r.toNat % dr).toUInt8, (p.g.toNat - p.g.toNat % dg).toUInt8,
        (p.b.toNat - p.b.toNat % db).toUInt8⟩
    match paletteList.finIdxOf? masked with
    | some i => i.val.toUInt8
    | none => 0
  if opts.enableImageDithering then
    (pixelMap paletteIndex (pixelMapXY dither img), palette)
  else
    (pixelMap paletteIndex img, palette)

-- ── Public entry point ──

/-- Reduce an image to a colour palette according to `PaletteOptions` and
    return the *indices image* along with its `Palette`. -/
def palettize (opts : PaletteOptions) (img : Image PixelRGB8) : Image Pixel8 × Palette :=
  match opts.paletteCreationMethod with
  | .medianMeanCut => medianMeanCutQuantization opts img
  | .uniform => uniformQuantization opts img

end Codec.Picture
