/-
  Linen.Data.Array.Shaped.Stencil.Dim2 — applying a stencil to a 2D array

  Ported from Haskell's `Data.Array.Repa.Stencil.Dim2` (package `repa`).

  Upstream statically unrolls the stencil accumulation into a fixed 7x7
  tile (`template7x7`) purely as a GHC-optimiser workaround — stencils
  larger than 7x7 are a hard `error` there. That unrolling has no distinct
  *observable* behavior: for any stencil size it computes the same
  accumulated value as folding `Stencil.acc` over every offset in the
  stencil's extent. This port does that fold directly, generalizing away
  the 7x7 cap (which was an implementation limitation, not a semantic one).

  Likewise, upstream preserves the `Cursored`/`Partitioned`/`HintSmall`
  representation of the result purely to fuse index computations between
  neighbouring stencil applications — a performance optimisation with no
  effect on the array's element values. This port returns a plain
  `Delayed` array with the same values, following the precedent already
  established for `Eval`'s Gang-based splitting (see
  `Repr/Manifest.lean`).

  The Template Haskell `stencil2` quasiquoter is dropped (see
  `docs/imports/repa/dependencies.md`).
-/

import Linen.Data.Array.Shaped.Repr.Delayed
import Linen.Data.Array.Shaped.Stencil.Base

namespace Data.Array.Shaped

/-- Apply a stencil to every element of a 2D array, handling out-of-bounds
    reads per `boundary`. -/
def mapStencil2 {arr a} [Inhabited a] [Add a] [Source arr]
    (boundary : Boundary a) (stencil : Stencil DIM2 a) (a' : arr DIM2 a) : Delayed DIM2 a :=
  match Source.extent a', stencil.extent with
  | _ :. aHeight :. aWidth, _ :. sHeight :. sWidth =>
    let sHeight2 := sHeight / 2
    let sWidth2 := sWidth / 2
    let getVal (y x : Int) : a :=
      if y < 0 || y >= aHeight || x < 0 || x >= aWidth then
        match boundary with
        | Boundary.fixed c => c
        | Boundary.const c => c
        | Boundary.clamp =>
          let cy := if y < 0 then 0 else if y >= aHeight then aHeight - 1 else y
          let cx := if x < 0 then 0 else if x >= aWidth then aWidth - 1 else x
          unsafeIndex a' (ix2 cy cx)
      else unsafeIndex a' (ix2 y x)
    let offsets :=
      (List.range sHeight.toNat).flatMap (fun oy =>
        (List.range sWidth.toNat).map (fun ox => (Int.ofNat oy - sHeight2, Int.ofNat ox - sWidth2)))
    fromFunction (Source.extent a') (fun ix => match ix with
      | Z.Z :. y :. x =>
        offsets.foldr (fun (oy, ox) acc => stencil.acc (ix2 oy ox) (getVal (y + oy) (x + ox)) acc)
          stencil.zero)

/-- Like `mapStencil2`, but with the parameters flipped. -/
def forStencil2 {arr a} [Inhabited a] [Add a] [Source arr]
    (boundary : Boundary a) (a' : arr DIM2 a) (stencil : Stencil DIM2 a) : Delayed DIM2 a :=
  mapStencil2 boundary stencil a'

end Data.Array.Shaped
