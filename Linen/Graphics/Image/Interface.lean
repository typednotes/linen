/-
  Linen.Graphics.Image.Interface — the central `Pixel`/`ColorSpace`/
  `AlphaSpace` classes, plus a `Manifest`-backed `Image` type and generic
  border-handling indexing

  ## Haskell equivalent
  `Graphics.Image.Interface` from https://hackage.haskell.org/package/hip
  (module #3 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).
  This is hip's central typeclass hub, tying together a pixel type, a colour
  space, a component type, and an array representation.

  ## Representation-collapse decision (the key architectural choice)

  Upstream is parameterised over an abstract array representation `arr`
  throughout: `BaseArray arr cs e`, `Array arr cs e`, `MArray arr cs e`, the
  associated `data family Image arr cs e`/`data family MImage s arr cs e`, and
  the `exchange`/`toVector`/`fromVector` functions that move an image between
  representations. Per `docs/imports/hip/dependencies.md`'s precedence-check
  note, hip's own representation-selection layer
  (`Interface.Vector.{Generic,Unboxing,Storable,Unboxed}`, `Interface.Vector`,
  `Interface.Repa.{Generic,Storable,Unboxed}`, `Interface.Repa` — 8 modules)
  exists only to let a user pick which of several backing stores an image
  uses, exactly the problem `repa` itself had and already solved by
  collapsing to one `Manifest` representation
  (`Linen.Data.Array.Shaped.Repr.Manifest`). So this port does the same thing
  one level up: there is no `arr` type parameter, no `BaseArray`/`Array`/
  `MArray` typeclass hierarchy, and no `Image arr cs e`/`MImage s arr cs e`
  data family. Instead, `Image cs e` (for a `Pixel cs e px` instance, see
  below) is a bare type synonym for `Data.Array.Shaped.Manifest DIM2 px`
  reusing the existing 2-D shaped-array representation directly, and every
  method that upstream's now-dropped classes declared is ported as a plain
  function operating on that one concrete type. Consequences of this
  collapse:

  * `exchange`/`toVector`/`fromVector` are dropped outright: they exist
    purely to convert an image between representations, and with only one
    representation left there is nothing to convert between.
  * The `MArray`/`MImage` mutable-image API (`new`/`read`/`write`/`swap`/
    `thaw`/`freeze`/`mdims`/`createImage`) is dropped for the same reason
    `Linen.Codec.Picture.Types` already drops `MutableImage`/freeze/thaw
    (see that module's doc-comment, bullet 1): Lean has no `ST`-region
    distinction to mirror — there is only ever one, pure, persistent `Image`
    — and `makeImage`/`map`/`zipWith`/… below already build a whole image at
    once with plain functions, covering the same ground `new`+`write`+
    `freeze` did upstream.
  * `deepSeqImage`/`rnf`/the `NFData`/`Typeable`/`Show` instances on `Image`
    are GHC strictness/reflection machinery with no Lean counterpart and are
    dropped, per the package-wide scope note in `dependencies.md`.
  * `compute`/`toManifest` become the identity function: an `Image` here
    *is* already a `Manifest`, so there is nothing left to force into a
    computed state or convert to a manifest representation.
  * `(|*|)` (image matrix multiplication) is dropped: upstream declares it
    abstractly in the `Array` class but never gives a concrete definition in
    this file — every concrete definition lived in the now-dropped
    `Interface.Vector`/`Interface.Repa` modules, so there is no faithful
    body to port here either; it is left for whichever later `Processing`
    module needs matrix multiplication to define directly.
  * The generic `Num`/`Fractional`/`Floating` instances upstream derives for
    `Pixel cs e` and `Image arr cs e` (via `liftPx`/`liftPx2`/`promote`/
    `map`/`zipWith`/`scalar`) are deferred to the concrete colour-space
    modules (#4 onward): Lean's stdlib splits `Num` into `Add`/`Sub`/`Mul`/
    `Neg`/`OfNat`, with no single `Floating` counterpart, so which of these
    make sense to instantiate is a per-colour-space decision better made once
    a concrete `Pixel cs e` exists to instantiate them for.

  ## `Pixel`/`ColorSpace`/`AlphaSpace`

  Upstream's `Pixel cs e` is a *data family*: each colour space picks its own
  concrete pixel representation (e.g. one channel for `Y`, three for `RGB`).
  Lean has no data-family mechanism, so — following the exact precedent
  already established for the structurally identical problem in
  `Linen.Codec.Picture.Types.Pixel` (see that module's doc-comment) — `Pixel`
  becomes a marker class relating a colour space `cs` and a component type
  `e` to their concrete pixel type `px`, with `px` an `outParam` so it is
  resolved as soon as a `Pixel cs e px` instance is found:

  ```
  class Pixel (cs e : Type) (px : outParam Type)
  ```

  `ColorSpace cs e` (an `outParam`-parameterised `px`, plus a further
  `outParam Components`, mirroring upstream's associated `type Components cs
  e`) then carries the actual per-colour-space operations: `toComponents`/
  `fromComponents`/`promote`/`getPxC`/`setPxC`/`mapPxC`/`liftPx`/`liftPx2`/
  `foldlPx2`, exactly as upstream. `AlphaSpace cs e Opaque` mirrors upstream's
  `AlphaSpace`, relating a colour space to its `Opaque` (alpha-dropped)
  counterpart via `getAlpha`/`addAlpha`/`dropAlpha`; its `ColorSpace`
  superclass constraints are dropped as unused machinery (none of its three
  methods mention a `ColorSpace` operation), the same simplification already
  used for `ColorConvertible`/`ColorSpaceConvertible`/`ColorPlane` in
  `Linen.Codec.Picture.Types`.

  This module defines **only** the class hierarchy (per the task scope): no
  concrete `Pixel`/`ColorSpace` instance is declared here — those belong to
  the colour-space modules `Y`/`RGB`/`HSI`/`CMYK`/`YCbCr`/`Complex`/`X`/
  `Binary` (#4–#11 in the plan), which will each supply their own pixel
  representation on top of the classes defined here.

  `Eq cs, Enum cs, Show cs, Bounded cs, Typeable cs` on upstream's
  `ColorSpace` are simplified: `Typeable`/`Show` are GHC reflection features
  with no Lean counterpart (dropped, per the package-wide scope note); `Enum
  cs, Bounded cs` existed solely so `toListPx`/`foldrPx`/`foldlPx`/
  `foldl1Px`'s *default* implementations could enumerate every channel via
  `enumFrom (toEnum 0)` — ported here as an explicit `channels : List cs`
  class field instead, since Lean's stdlib has no polymorphic `Enum`/
  `Bounded` abstraction to derive this generically (the same "list-based
  enumeration field" pattern `Linen.Data.Array.Shaped.Shape` already uses for
  `listOfShape`/`shapeOfList`). With `channels` explicit, `foldrPx`/
  `foldlPx`/`foldl1Px`/`toListPx` no longer need to be class fields with
  cross-referential default bodies (upstream defines each in terms of the
  other) — they are ordinary functions built directly from `channels` and
  `getPxC` below the class. `foldl1Px` additionally requires `[Inhabited e]`
  so its empty-channel-list case can `panic!` (Lean's counterpart of
  upstream's `error "foldl1Px: empty Pixel"`), matching the same
  `Inhabited`-for-`panic!` convention `Linen.Data.Array.Shaped.Base.Source`
  already uses.

  ## Border handling and indexing

  `Border`/`handleBorderIndex`/`index`/`defaultIndex`/`borderIndex`/
  `maybeIndex`/`fromIx`/`toIx`/`checkDims` are ported as literal
  transcriptions of upstream's pure arithmetic dispatch, specialised to
  `Image cs e := Manifest DIM2 px` in place of the abstract `Image arr cs e`.
  Lean's `Int` `%`/`/` are Euclidean (floored, same sign as a positive
  divisor) exactly like Haskell's `mod`/`divMod`, so `Wrap`/`Reflect`/
  `Continue`/`toIx` translate directly with no sign-correction needed (see
  `Linen.Data.Array.Shaped.Index`'s own use of `Int.tdiv`/`Int.tmod`, whose
  *truncating* semantics are explicitly opted into elsewhere precisely
  because they differ from the ambient floored `/`/`%` used here).
-/

import Linen.Data.Array.Shaped
import Linen.Graphics.Image.Interface.Elevator

open Graphics.Image.Interface.Elevator (Elevator)

namespace Graphics.Image.Interface

-- ── `Pixel` — associates a pixel representation to a colour space/component ──

/-- Marker class: `px` is the concrete pixel representation for colour space
`cs` over component type `e`. Ported from upstream's `data family Pixel cs e`
— see the module doc-comment for why this becomes a class with an `outParam`
rather than a data family. No concrete instance is declared in this module;
each colour-space module (`Y`, `RGB`, …) supplies its own. -/
class Pixel (cs e : Type) (px : outParam Type)

-- ── `ColorSpace` — the operations every pixel representation supports ──

/-- A colour space `cs` over component type `e`, with associated pixel type
`px` and component-tuple type `Components` (both `outParam`s). Ported from
upstream's `ColorSpace cs e` — see the module doc-comment for the `Enum
cs`/`Bounded cs` → `channels` simplification. -/
class ColorSpace (cs e : Type) {px : outParam Type} [Pixel cs e px]
    (Components : outParam Type) [Elevator e] where
  /-- Every channel of this colour space, in canonical order. Upstream's
  `Enum cs, Bounded cs` constraint (used via `enumFrom (toEnum 0)`) — see the
  module doc-comment. -/
  channels : List cs
  /-- Convert a pixel to its component-tuple representation. -/
  toComponents : px → Components
  /-- Convert a component tuple back into a pixel. -/
  fromComponents : Components → px
  /-- Construct a pixel by replicating the same value across every channel. -/
  promote : e → px
  /-- Retrieve a pixel's value at a given channel. -/
  getPxC : px → cs → e
  /-- Set a pixel's value at a given channel. -/
  setPxC : px → cs → e → px
  /-- Map a channel-aware function over every channel of a pixel. -/
  mapPxC : (cs → e → e) → px → px
  /-- Map a function over every channel of a pixel. -/
  liftPx : (e → e) → px → px
  /-- Combine two pixels channel-wise with a function. -/
  liftPx2 : (e → e → e) → px → px → px
  /-- Left fold over two pixels' channels at the same time. -/
  foldlPx2 : {β : Type} → (β → e → e → β) → β → px → px → β

export ColorSpace
  (channels toComponents fromComponents promote getPxC setPxC mapPxC liftPx liftPx2 foldlPx2)

/-- Right fold over a pixel's channels. Upstream's `foldrPx`, ported as a
plain function over `channels` rather than a class field with a
cross-referential default (see the module doc-comment).

`px`/`Components` must be repeated as their own explicit binders (rather than
left implicit inside `[ColorSpace cs e Components]` alone): they are
`outParam`s of `Pixel`/`ColorSpace`, so mentioning them again here — as with
`Component` in `Linen.Codec.Picture.Types.Pixel`'s own call sites — is what
makes Lean unify this function's `px`/`Components` with the ones the
`ColorSpace`/`Pixel` instances in scope actually resolve to. -/
def foldrPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] {β : Type} (f : e → β → β) (z0 : β) (xs : px) : β :=
  (channels (cs := cs) (e := e)).foldr (fun c acc => f (getPxC xs c) acc) z0

/-- Left fold over a pixel's channels. Upstream's `foldlPx`. -/
def foldlPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] {β : Type} (f : β → e → β) (z0 : β) (xs : px) : β :=
  (channels (cs := cs) (e := e)).foldl (fun acc c => f acc (getPxC xs c)) z0

/-- Left fold over a pixel's channels with no starting accumulator, using the
first channel's value as the seed. `panic!`s on a colour space with no
channels — Lean's counterpart of upstream's `error "foldl1Px: empty Pixel"`
(see the module doc-comment for why `[Inhabited e]` is required). Upstream's
`foldl1Px`. -/
def foldl1Px {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] [Inhabited e] (f : e → e → e) (xs : px) : e :=
  match channels (cs := cs) (e := e) with
  | [] => panic! "Graphics.Image.Interface.foldl1Px: empty Pixel"
  | c :: cs' => cs'.foldl (fun acc c' => f acc (getPxC xs c')) (getPxC xs c)

/-- Convert a pixel to the list of its channel values, in channel order.
Upstream's `toListPx`. -/
def toListPx {cs e : Type} {px : Type} [Pixel cs e px] {Components : Type} [Elevator e]
    [ColorSpace cs e Components] (xs : px) : List e :=
  (channels (cs := cs) (e := e)).map (getPxC xs)

-- ── `AlphaSpace` — a colour space that supports transparency ──

/-- A colour space `cs` that supports transparency, with `Opaque` its
alpha-dropped counterpart (an `outParam`, resolved once `cs`/`e` are known).
Ported from upstream's `AlphaSpace cs e`; the `ColorSpace`/superclass
constraints are dropped as unused machinery — see the module doc-comment. -/
class AlphaSpace (cs e : Type) {px : outParam Type} [Pixel cs e px]
    (Opaque : outParam Type) {opaquePx : outParam Type} [Pixel Opaque e opaquePx] where
  /-- Get a transparent pixel's alpha channel. -/
  getAlpha : px → e
  /-- Add an alpha channel to an opaque pixel. -/
  addAlpha : e → opaquePx → px
  /-- Convert a transparent pixel to an opaque one by dropping the alpha
  channel. -/
  dropAlpha : px → opaquePx

export AlphaSpace (getAlpha addAlpha dropAlpha)

-- ── `Image` — a `Manifest`-backed 2-D array of pixels ──

/-- hip's `Image arr cs e`, hard-wired to `Data.Array.Shaped.Manifest DIM2
px` — see the module doc-comment's representation-collapse decision. -/
abbrev Image (cs e : Type) {px : outParam Type} [Pixel cs e px] : Type :=
  Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px

/-- Get the `(m, n)` = (rows, columns) dimensions of an image. Upstream's
`dims`. -/
def dims {px} (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : Int × Int :=
  match img.extent with
  | _ :. m :. n => (m, n)

-- ── Tools ──

/-- 2-D to flat index conversion: `n` is the number of columns. Upstream's
`fromIx`. -/
def fromIx (n : Int) (ij : Int × Int) : Int :=
  n * ij.1 + ij.2

/-- Flat to 2-D index conversion: `n` is the number of columns. Upstream's
`toIx`. -/
def toIx (n k : Int) : Int × Int :=
  (k / n, k % n)

/-- Check that a pair of dimensions is positive in both components,
`panic!`ing (upstream's `error`) otherwise. Upstream's `checkDims`. -/
def checkDims (err : String) (mn : Int × Int) : Int × Int :=
  if mn.1 <= 0 || mn.2 <= 0 then
    panic! s!"{err}: dimensions are expected to be positive: {mn}"
  else
    mn

/-- Build a single-pixel (scalar) image. Upstream's `scalar`. -/
def scalar {px} (p : px) : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  ⟨Data.Array.Shaped.ix2 1 1, #[p]⟩

/-- Retrieve the pixel at `(0, 0)` without bounds checking. Upstream's
`index00`. -/
def index00 {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  img.elems.getD 0 default

/-- Get the pixel at `(i, j)` without any bounds checks. Upstream's
`unsafeIndex`. -/
def unsafeIndex {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) (ij : Int × Int) : px :=
  Data.Array.Shaped.unsafeIndex img (Data.Array.Shaped.ix2 ij.1 ij.2)

/-- Build an image from its dimensions and a pixel-generating function.
Upstream's `makeImage`. -/
def makeImage {px} [Inhabited px] (mn : Int × Int) (f : Int × Int → px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (m, n) := mn
  Id.run do
    let mut elems := Array.mkEmpty (m.toNat * n.toNat)
    for i in [0:m.toNat] do
      for j in [0:n.toNat] do
        elems := elems.push (f (Int.ofNat i, Int.ofNat j))
    pure ⟨Data.Array.Shaped.ix2 m n, elems⟩

/-- Build an image from its dimensions, an inner window (starting index and
size), and separate pixel-generating functions for the window's interior and
its border. Upstream's `makeImageWindowed`. -/
def makeImageWindowed {px} [Inhabited px] (mn ix0 winSz : Int × Int)
    (inner border : Int × Int → px) : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  let (m, n) := mn
  let (i0, j0) := ix0
  let (wm, wn) := winSz
  Id.run do
    let mut elems := Array.mkEmpty (m.toNat * n.toNat)
    for i in [0:m.toNat] do
      for j in [0:n.toNat] do
        let ii := Int.ofNat i
        let jj := Int.ofNat j
        let p :=
          if ii >= i0 && ii < i0 + wm && jj >= j0 && jj < j0 + wn then
            inner (ii, jj)
          else
            border (ii, jj)
        elems := elems.push p
    pure ⟨Data.Array.Shaped.ix2 m n, elems⟩

/-- Build an image from a rectangular, non-empty nested list of pixels: the
outer list's length is the number of rows, the first inner list's length the
number of columns. `panic!`s on an empty outer list. Upstream's `fromLists`
(abstract there; every concrete implementation lived in the now-dropped
`Interface.Vector`/`Interface.Repa` modules, so this is a fresh, direct
implementation against `Manifest`). -/
def fromLists {px} [Inhabited px] (pxss : List (List px)) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  match pxss with
  | [] => panic! "Graphics.Image.Interface.fromLists: empty list of pixels"
  | row :: _ =>
    ⟨Data.Array.Shaped.ix2 (Int.ofNat pxss.length) (Int.ofNat row.length), pxss.flatten.toArray⟩

/-- Map a function over every pixel of an image. Upstream's `map`. -/
def map {px1 px2} (f : px1 → px2)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2 :=
  ⟨img.extent, img.elems.map f⟩

/-- Map an index-aware function over every pixel of an image. Upstream's
`imap`. -/
def imap {px1 px2} [Inhabited px1] (f : Int × Int → px1 → px2)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2 :=
  match img.extent with
  | _ :. _ :. n =>
    ⟨img.extent,
      (List.range img.elems.size).foldl
        (fun acc k => acc.push (f (toIx n (Int.ofNat k)) (img.elems.getD k default)))
        (Array.mkEmpty img.elems.size)⟩

/-- Zip two images of identical dimensions with a function. `panic!`s if the
dimensions differ. Upstream's `zipWith`. -/
def zipWith {px1 px2 px3} [Inhabited px3] (f : px1 → px2 → px3)
    (img1 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1)
    (img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px3 :=
  if img1.extent == img2.extent then
    ⟨img1.extent, (img1.elems.zip img2.elems).map (fun (a, b) => f a b)⟩
  else
    panic! "Graphics.Image.Interface.zipWith: images have different dimensions"

/-- Zip two images of identical dimensions with an index-aware function.
`panic!`s if the dimensions differ. Upstream's `izipWith`. -/
def izipWith {px1 px2 px3} [Inhabited px1] [Inhabited px2] [Inhabited px3]
    (f : Int × Int → px1 → px2 → px3)
    (img1 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1)
    (img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px3 :=
  if img1.extent == img2.extent then
    match img1.extent with
    | _ :. _ :. n =>
      ⟨img1.extent,
        (List.range img1.elems.size).foldl
          (fun acc k =>
            acc.push (f (toIx n (Int.ofNat k)) (img1.elems.getD k default) (img2.elems.getD k default)))
          (Array.mkEmpty img1.elems.size)⟩
  else
    panic! "Graphics.Image.Interface.izipWith: images have different dimensions"

/-- Traverse an image: compute new dimensions from the source's, then build
the result image from a pixel getter into the source and a target index.
Upstream's `traverse`. -/
def traverse {px1 px2} [Inhabited px1] [Inhabited px2]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1)
    (mkDims : Int × Int → Int × Int)
    (mkPixel : (Int × Int → px1) → Int × Int → px2) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2 :=
  makeImage (mkDims (dims img)) (mkPixel (unsafeIndex img))

/-- Traverse two images: compute new dimensions from both sources', then
build the result image from two pixel getters and a target index. Upstream's
`traverse2`. -/
def traverse2 {px1 px2 px3} [Inhabited px1] [Inhabited px2] [Inhabited px3]
    (img1 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px1)
    (img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px2)
    (mkDims : Int × Int → Int × Int → Int × Int)
    (mkPixel : (Int × Int → px1) → (Int × Int → px2) → Int × Int → px3) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px3 :=
  makeImage (mkDims (dims img1) (dims img2)) (mkPixel (unsafeIndex img1) (unsafeIndex img2))

/-- Transpose an image (swap rows and columns). Upstream's `transpose`. -/
def transpose {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  match img.extent with
  | _ :. m :. n => makeImage (n, m) (fun (i, j) => unsafeIndex img (j, i))

/-- Backwards-permute an image: build a new image of the given dimensions by
mapping each of its indices back into the source image via `perm`. Upstream's
`backpermute`. -/
def backpermute {px} [Inhabited px] (mn' : Int × Int) (perm : Int × Int → Int × Int)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) :
    Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px :=
  makeImage mn' (fun ij => unsafeIndex img (perm ij))

/-- Undirected reduction of an image, in row-major order. Upstream's `fold`.
-/
def fold {px} (f : px → px → px) (z0 : px)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  img.elems.foldl f z0

/-- Undirected, index-aware reduction of an image, in row-major order.
Upstream's `foldIx`. -/
def foldIx {px} [Inhabited px] (f : px → Int × Int → px → px) (z0 : px)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : px :=
  match img.extent with
  | _ :. _ :. n =>
    (List.range img.elems.size).foldl
      (fun acc k => f acc (toIx n (Int.ofNat k)) (img.elems.getD k default)) z0

/-- Pixelwise equality of two images: distinct if either their dimensions or
any pair of corresponding pixels differ. Upstream's `eq` — a direct alias for
`Manifest`'s own derived `BEq`, since an `Image` here already *is* a
`Manifest` (dimensions + elements). -/
def eq {px} [BEq px] (img1 img2 : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) : Bool :=
  img1 == img2

-- ── Border handling ──

/-- Approach to be used near the borders during various transformations, when
a function needs information about a pixel's out-of-bounds neighbours.
Upstream's `Border px`. -/
inductive Border (px : Type) where
  /-- Fill in a constant pixel. -/
  | fill : px → Border px
  /-- Wrap around from the opposite border of the image. -/
  | wrap : Border px
  /-- Replicate the pixel at the edge. -/
  | edge : Border px
  /-- Mirror-like reflection. -/
  | reflect : Border px
  /-- Mirror-like reflection, without repeating the edge pixel. -/
  | continue : Border px

/-- Absolute value of an `Int`, staying in `Int` (as opposed to
`Int.natAbs`'s `Nat`). Used by `handleBorderIndex`'s `Reflect`/`Continue`
cases below, mirroring upstream's `abs`. -/
private def absI (x : Int) : Int :=
  if x < 0 then -x else x

/-- Border handling function. If the `(i, j)` location is within bounds, the
supplied lookup function is used; otherwise it is handled according to the
supplied border strategy. Upstream's `handleBorderIndex`. -/
def handleBorderIndex {px} (border : Border px) (mn : Int × Int) (getPx : Int × Int → px)
    (ij : Int × Int) : px :=
  let (m, n) := mn
  let (i, j) := ij
  let north := i < 0
  let south := i >= m
  let west := j < 0
  let east := j >= n
  if north || east || south || west then
    match border with
    | .fill p => p
    | .wrap => getPx (i % m, j % n)
    | .edge =>
      getPx (if north then 0 else if south then m - 1 else i,
             if west then 0 else if east then n - 1 else j)
    | .reflect =>
      getPx (if north then (absI i - 1) % m else if south then (-i - 1) % m else i,
             if west then (absI j - 1) % n else if east then (-j - 1) % n else j)
    | .continue =>
      getPx (if north then absI i % m else if south then (-i - 2) % m else i,
             if west then absI j % n else if east then (-j - 2) % n else j)
  else
    getPx (i, j)

/-- Image indexing function that uses a border-resolution strategy for
out-of-bounds pixels. Upstream's `borderIndex`. -/
def borderIndex {px} [Inhabited px] (atBorder : Border px)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) (ij : Int × Int) : px :=
  handleBorderIndex atBorder (dims img) (unsafeIndex img) ij

/-- Image indexing function that returns a default pixel if the index is out
of bounds. Upstream's `defaultIndex`. -/
def defaultIndex {px} [Inhabited px] (p : px)
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) (ij : Int × Int) : px :=
  handleBorderIndex (.fill p) (dims img) (unsafeIndex img) ij

/-- Image indexing function that returns `none` if the index is out of
bounds, `some px` otherwise. Upstream's `maybeIndex`. -/
def maybeIndex {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) (ij : Int × Int) : Option px :=
  let (m, n) := dims img
  if ij.1 >= 0 && ij.2 >= 0 && ij.1 < m && ij.2 < n then
    some (unsafeIndex img ij)
  else
    none

/-- Get the pixel at `(i, j)`, `panic!`ing (upstream's `error`) if out of
bounds. Upstream's `index`. -/
def index {px} [Inhabited px]
    (img : Data.Array.Shaped.Manifest Data.Array.Shaped.DIM2 px) (ij : Int × Int) : px :=
  match maybeIndex img ij with
  | some p => p
  | none => panic! "Graphics.Image.Interface.index: index out of bounds"

end Graphics.Image.Interface
