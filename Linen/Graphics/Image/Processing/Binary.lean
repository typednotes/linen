/-
  Linen.Graphics.Image.Processing.Binary — binary-image construction and
  morphology (erode/dilate/opening/closing) built on `ColorSpace.Binary`
  (module #11) and `Processing.Convolution` (module #17)

  ## Haskell equivalent
  `Graphics.Image.Processing.Binary` from
  https://hackage.haskell.org/package/hip (module #19 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`), on module #1
  (`Linen.Graphics.Image.Utils`), #12 (`Linen.Graphics.Image.ColorSpace`) and
  #17 (`Linen.Graphics.Image.Processing.Convolution`). Read directly against
  the tarball source
  (`hip-1.5.6.0/src/Graphics/Image/Processing/Binary.hs`).

  Upstream's export list is `toImageBinaryUsing, toImageBinaryUsing2,
  threshold, threshold2, thresholdWith, thresholdWith2, compareWith,
  Thresholding(..), or, and, (!&&!), (!||!), (.&&.), (.||.), invert,
  disjunction, conjunction, erode, dialate, open, close`. See below for which
  of these are ported and which are deferred.

  ## Deferred: `Thresholding`, `threshold`/`threshold2`, `thresholdWith`/
  ## `thresholdWith2`/`compareWith`

  Upstream's `Thresholding` class overloads `.==./.<./…/!==!/!<!/…` between
  every combination of a bare `Pixel` and an `Image`, dispatched purely via a
  multi-parameter-typeclass functional dependency (`a b -> arr`) — a GHC
  overloading mechanism with no Lean counterpart (Lean has no functional
  dependencies), and every instance body is itself a thin wrapper around
  `toImageBinaryUsing`/`toImageBinaryUsing2` (both ported below) or
  `threshold`/`threshold2` (not ported — see next paragraph). The class and
  its dozen operators are therefore dropped as GHC-dispatch sugar with no
  remaining functionality beyond what `toImageBinaryUsing`/
  `toImageBinaryUsing2` (or a direct predicate written by the caller) already
  provide as plain functions.

  `threshold`/`threshold2` need `Applicative (Pixel cs)`; `thresholdWith`/
  `thresholdWith2`/`compareWith` additionally need `Foldable (Pixel cs)` (to
  fold a per-channel `Bool` pixel down to one `Bool` with `F.and`) and
  `.<.`'s upstream siblings need `Ord (Pixel cs e)`. None of these three
  abstractions — a colour-space-generic `Applicative`/`Foldable` over `Pixel
  cs`, or a total order on an arbitrary pixel — exist anywhere in this port:
  `Linen.Graphics.Image.ColorSpace.X`'s own doc-comment already defers `Ord
  (Pixel X e)` for lack of a call site, `Linen.Graphics.Image.ColorSpace.
  Binary`'s doc-comment defers `Ord Bit` the same way, and
  `Linen.Graphics.Image.ColorSpace`'s own doc-comment already declares a
  colour-space-generic `Functor (Pixel cs)` (needed by upstream's
  `toWord8Px`/`toWord8I`) out of scope as "a whole new generic-`Functor`-
  equivalent abstraction the rest of this port's architecture does not
  provide anywhere" — `Applicative (Pixel cs)`/`Foldable (Pixel cs)` are the
  exact same architectural gap, one notch further up the same class
  hierarchy. Following that precedent, `threshold`/`threshold2`/
  `thresholdWith`/`thresholdWith2`/`compareWith` (upstream's own `DEPRECATED`
  alias for `thresholdWith2`) are left unported here; a caller needing
  pointwise per-channel thresholding on a *known, concrete* colour space can
  already build it directly from `toImageBinaryUsing`/`toImageBinaryUsing2`
  and that colour space's own `getPxC`/`foldlPx2`, exactly as `ColorSpace.
  lean`'s own doc-comment recommends for the analogous `toWord8I` gap.

  ## Ported: `toImageBinaryUsing`/`toImageBinaryUsing2`

  These need no `Applicative`/`Ord` at all — just a plain predicate supplied
  by the caller — so they are ported directly below, unchanged in shape from
  upstream (`I.map (fromBool . f)` / `I.zipWith (fromBool .:! f)`, the latter
  using this port's plain function application in place of `.:!`'s strictness
  annotation — see `Convolution.lean`'s own note on `BangPatterns` being
  dropped throughout this port).

  ## Ported: bitwise image operators

  `!&&!`/`!||!` (channel-preserving, pointwise `AndOp`/`OrOp` per channel) and
  `.&&.`/`.||.` (channel-*collapsing*, folding every channel of both source
  pixels together with `AndOp`/`OrOp` into a single-channel `X` result) are
  ported as `zipAnd`/`zipOr` and `squashAnd`/`squashOr` respectively — plain
  names in place of upstream's bespoke operators, matching this port's
  convention of spelling out ad hoc Haskell operators as ordinary
  identifiers when they are not backed by one of Lean's own operator classes
  (unlike `AndOp`/`OrOp`/`Complement`, already given genuine `&&&`/`|||`/`~~~`
  notation in `ColorSpace/Binary.lean`, which these two functions build on
  directly). `zipAnd`/`zipOr` use `Interface.liftPx2` per pixel (no channel
  collapsing); `squashAnd`/`squashOr` are built on `ColorSpace.X.squashWith2`
  (already ported in module #10, module #19's own dependency list already
  names #12 which re-exports it) with the fold function `fun a c1 c2 => a &&&
  c1 &&& c2` / `fun a c1 c2 => a ||| c1 ||| c2` — the literal transcription of
  upstream's `(.&.) .: (.&.)` / `(.|.) .: (.|.)` (`(f .: g) x y z = f (g x y)
  z = (x \`f\` y) \`f\` z`, per `Graphics.Image.Utils.hs`'s own definition of
  `.:`).

  `invert` (`I.map (liftPx complement)`), `disjunction`/`conjunction`
  (`squashWith (.|.) zero` / `squashWith (.&.) one`, using `ColorSpace.X.
  squashWith`, also already ported in module #10) and the whole-image boolean
  reducers `or`/`and` (`isOn . fold (.|.) off` / `isOn . fold (.&.) on`, using
  `Interface.fold`) are ported directly below, unchanged from upstream's
  shape.

  ## Structuring-element representation

  Upstream represents a structuring element as an ordinary `Image arr X Bit`
  — the exact same type as the binary image it operates on, with no separate
  "structuring element" type (checked directly against `erode`'s/`dialate`'s
  signatures: both take `Image arr X Bit -> Image arr X Bit -> Image arr X
  Bit`). This port carries that over unchanged: a structuring element here is
  simply a `Graphics.Image.Interface.Image
  Graphics.Image.ColorSpace.X.X Bit`, e.g. built with `Interface.fromLists`
  from nested lists of `on`/`off` pixels, exactly as upstream's own
  `$morphology` doc-comment example (`struct = fromLists [[0,1,0],[1,1,0],
  [0,1,0]]`). Centering, border handling near the image edges, and the
  underlying sliding-window sum are all inherited unchanged from
  `Processing.Convolution`'s `convolve`/`correlate` (module #17) — no new
  windowed-scan logic is written in this module; see that module's own
  doc-comment for the centering convention and the (already-bounded, no new
  termination proof needed) kernel loop.

  ## `erode`/`dilate`/`opening`/`closing`

  Upstream's `erode`/`dialate` are, verbatim:
  ```
  erode !struc !img = invert $ convolve (Fill on) struc (invert img)
  dialate !struc !img = convolve (Fill off) struc img
  open !struc = dialate struc . erode struc
  close !struc = erode struc . dialate struc
  ```
  ported directly below with three purely cosmetic name changes:

  * `dialate` → `dilate`, correcting upstream's own misspelling (checked
    against multiple dictionaries and every other English-language reference
    to this morphological operation — "dilate" is the correct spelling,
    "dialate" a typo present in upstream's exported identifier itself). The
    *formula* (`convolve (Fill off) struc img`, unchanged) is carried over
    exactly; only the identifier is corrected.
  * `open` → `opening`, `close` → `closing`: `open` is a reserved keyword in
    Lean (the `open Namespace` command), so it cannot name a `def` at all;
    `closing` is renamed to match for symmetry, and because "opening"/
    "closing" are themselves the standard English names for these two
    morphological operations (the same terms upstream's own `$morphology`
    Haddock section uses in prose, e.g. "Opening is defined as…"), so this
    is, if anything, a move *toward* upstream's own terminology rather than
    away from it.

  The `Fill on`/`Fill off` border strategy, the `BangPatterns` strictness
  annotations (dropped per the package-wide convention already noted in
  `Convolution.lean`'s own doc-comment), and the composition order of
  `opening`/`closing` are all unchanged.

  ## `BangPatterns` strictness annotations

  As throughout this port (see, e.g., `Convolution.lean`'s own doc-comment),
  every `!`-prefixed argument in the upstream source is a GHC strictness hint
  with no Lean surface-syntax counterpart and is simply absent from the port.
-/

import Linen.Graphics.Image.Interface
import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.ColorSpace.Binary
import Linen.Graphics.Image.Processing.Convolution

open Graphics.Image.Interface (Pixel ColorSpace Border Image map zipWith fold liftPx liftPx2)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.X (X PixelX squashWith squashWith2)
open Graphics.Image.ColorSpace.Binary (Bit on off zero one fromBool isOn)
open Graphics.Image.Processing.Convolution (convolve)

namespace Graphics.Image.Processing.Binary

-- ── Construction: predicate-based binary images ──

/-- Construct a binary image using a predicate from a source image. Upstream's
`toImageBinaryUsing`. -/
def toImageBinaryUsing {cs e px Components : Type} [Pixel cs e px] [Elevator e]
    [ColorSpace cs e Components] (f : px → Bool) (img : Image cs e) : Image X Bit :=
  map (fun p => fromBool (f p)) img

/-- Construct a binary image using a predicate from two source images.
Upstream's `toImageBinaryUsing2`. -/
def toImageBinaryUsing2 {cs e px Components : Type} [Pixel cs e px] [Elevator e]
    [ColorSpace cs e Components] (f : px → px → Bool)
    (img1 img2 : Image cs e) : Image X Bit :=
  zipWith (fun p1 p2 => fromBool (f p1 p2)) img1 img2

-- ── Bitwise operations on binary images ──

/-- Pixel-wise `AND` operator on binary images, preserving every channel of
the source colour space. Upstream's `!&&!`. -/
def zipAnd {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    [Inhabited px] (img1 img2 : Image cs Bit) : Image cs Bit :=
  zipWith (liftPx2 (cs := cs) (e := Bit) (· &&& ·)) img1 img2

/-- Pixel-wise `OR` operator on binary images, preserving every channel of
the source colour space. Upstream's `!||!`. -/
def zipOr {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    [Inhabited px] (img1 img2 : Image cs Bit) : Image cs Bit :=
  zipWith (liftPx2 (cs := cs) (e := Bit) (· ||| ·)) img1 img2

/-- Pixel-wise `AND` operator on binary images that also `AND`s every channel
of both source pixels together into a single-channel `X` result. Upstream's
`.&&.`. -/
def squashAnd {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    (img1 img2 : Image cs Bit) : Image X Bit :=
  squashWith2 (cs := cs) (e := Bit) (fun a c1 c2 => a &&& c1 &&& c2) one img1 img2

/-- Pixel-wise `OR` operator on binary images that also `OR`s every channel of
both source pixels together into a single-channel `X` result. Upstream's
`.||.`. -/
def squashOr {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    (img1 img2 : Image cs Bit) : Image X Bit :=
  squashWith2 (cs := cs) (e := Bit) (fun a c1 c2 => a ||| c1 ||| c2) zero img1 img2

/-- Complement each pixel in a binary image. Upstream's `invert`. -/
def invert {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    (img : Image cs Bit) : Image cs Bit :=
  map (liftPx (cs := cs) (e := Bit) (~~~ ·)) img

/-- Join each component of a pixel with a binary `OR`, collapsing to a
single-channel `X` image. Upstream's `disjunction`. -/
def disjunction {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    (img : Image cs Bit) : Image X Bit :=
  squashWith (cs := cs) (e := Bit) (· ||| ·) zero img

/-- Join each component of a pixel with a binary `AND`, collapsing to a
single-channel `X` image. Upstream's `conjunction`. -/
def conjunction {cs px Components : Type} [Pixel cs Bit px] [ColorSpace cs Bit Components]
    (img : Image cs Bit) : Image X Bit :=
  squashWith (cs := cs) (e := Bit) (· &&& ·) one img

/-- Disjunction of all pixels in a binary image. Upstream's `or`. -/
def or (img : Image X Bit) : Bool :=
  isOn (fold (· ||| ·) off img)

/-- Conjunction of all pixels in a binary image. Upstream's `and`. -/
def and (img : Image X Bit) : Bool :=
  isOn (fold (· &&& ·) on img)

-- ── Binary morphology ──
-- See the module doc-comment for the structuring-element representation and
-- the `dialate`/`open`/`close` → `dilate`/`opening`/`closing` renames.

/-- Erosion: __{E = B ⊖ S = {m,n|Sₘₙ⊆B}__ — a pixel is on in the result only
if the structuring element, placed at that pixel, lands entirely within the
source's foreground. See the test module for a worked example. Upstream's
`erode`. -/
def erode (struc img : Image X Bit) : Image X Bit :=
  invert (cs := X) (convolve (.fill on) struc (invert (cs := X) img))

/-- Dilation: __{D = B ⊕ S = {m,n|Sₘₙ∩B≠∅}__ — a pixel is on in the result if
the structuring element, placed at that pixel, overlaps the source's
foreground anywhere. Upstream's `dialate` (renamed — see the module
doc-comment). -/
def dilate (struc img : Image X Bit) : Image X Bit :=
  convolve (.fill off) struc img

/-- Opening: __{B ○ S = (B ⊖ S) ⊕ S}__ — erosion followed by dilation with the
same structuring element; removes small foreground protrusions and thin
connections while otherwise preserving shape. Upstream's `open` (renamed —
`open` is a reserved Lean keyword; see the module doc-comment). -/
def opening (struc img : Image X Bit) : Image X Bit :=
  dilate struc (erode struc img)

/-- Closing: __{B ● S = (B ⊕ S) ⊖ S}__ — dilation followed by erosion with the
same structuring element; fills small background gaps and holes while
otherwise preserving shape. Upstream's `close` (renamed — see the module
doc-comment). -/
def closing (struc img : Image X Bit) : Image X Bit :=
  erode struc (dilate struc img)

end Graphics.Image.Processing.Binary
