/-
  Linen.Data.Vector.Lens — `vector`, `toArrayOf`, `ordinals`

  Port of Hackage's `lens-5.3.6`'s `Data.Vector.Lens` (`Data.Vector.
  Generic.Lens` upstream; fetched and read via Hackage's rendered source).
  Upstream's real content:

  ```
  vector      :: Vector v a => Iso' [a] (v a)
  forced      :: Vector v a => Iso' (v a) (v a)   -- force strictness; skipped, see below
  toVectorOf  :: Vector v a => Getting (Endo [a]) s a -> s -> v a
  ordinals    :: Vector v a => [Int] -> IndexedTraversal' Int (v a) a
  ```

  translated against `Linen.Data.Vector`'s own design: that module is
  `namespace Array` extensions on Lean's *native* `Array` — `linen` has no
  separate `Vector` type distinct from `Array` (confirmed: `Linen/Data/
  Vector.lean` adds methods like `generate`/`ifilter`/`backpermute` directly
  to `Array`, with no `structure`/`abbrev Vector` of its own). Every
  combinator below is therefore stated directly over Lean's `Array`.

  **Scope note (`Ixed`, already covered by `Linen.Data.Array.Lens`).**
  Since `Vector` and `Array` are, in `linen`, the exact same type, an
  `Ixed (Array A) Nat A` instance already exists
  (`Linen.Data.Array.Lens.instIxedArray`) — a second one here would be a
  duplicate-instance error. This module focuses on `vector`/`toArrayOf`/
  `ordinals` instead, upstream's other genuinely distinct combinators.

  **Scope note (`forced`).** Upstream's `forced` exists purely to force a
  lazy `Vector`'s elements to WHNF as a side effect of viewing/setting
  through it (`force :: Vector v a => v a -> v a`); Lean's `Array` is
  already a strict, eagerly-evaluated structure with no comparable
  laziness to force. Skipped — there is nothing for it to do here. -/

import Linen.Control.Lens.At
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Internal.Indexed
import Linen.Control.Lens.Internal.List
import Linen.Control.Lens.Iso
import Linen.Data.Array.Lens
import Linen.Data.Vector

open Control.Lens.Internal

namespace Control.Lens

-- ── vector ───────────────────────────────────────

/-- `vector :: Vector v a => Iso' [a] (v a)` — `iso V.fromList V.toList`,
    over Lean's native `List ↔ Array` conversion. -/
@[inline] def vector {A : Type u} : Iso' (List A) (Array A) :=
  iso List.toArray Array.toList

-- ── toArrayOf ────────────────────────────────────

/-- `toVectorOf :: Vector v a => Getting (Endo [a]) s a -> s -> v a`:
    collect every element an `IndexedFold` visits into an `Array`, in
    visitation order — implemented via `itoListOf` (`Linen.Control.Lens.
    Indexed`), discarding the index. -/
@[inline] def toArrayOf {S A : Type} (l : IndexedFold Unit S A) (s : S) : Array A :=
  let pairs : List (Unit × A) := itoListOf l s
  (pairs.map Prod.snd).toArray

-- ── ordinals ─────────────────────────────────────

/-- `ordinals :: Vector v a => [Int] -> IndexedTraversal' Int (v a) a`:
    focus on the elements at every position in a given list, skipping
    out-of-range and duplicate positions (via `Control.Lens.Internal.
    ordinalNub`, matching upstream's own de-duplication) — narrowed to `Nat`
    positions, matching this batch's other `Ixed`/index-taking combinators. -/
@[inline] def ordinals {A : Type} (is : List Nat) : IndexedTraversal' Nat (Array A) A :=
  fun {F} [Applicative F] {P} [Indexable Nat P] (pab : P A (F A)) (arr : Array A) =>
    let positions := (Control.Lens.Internal.ordinalNub (Int.ofNat arr.size)
      (is.map Int.ofNat)).map Int.toNat
    positions.foldl
      (fun (accF : F (Array A)) p =>
        match arr[p]? with
        | some a => (fun acc' a' => Array.setIfInBounds acc' p a') <$> accF <*> Indexable.indexed pab p a
        | none => accF)
      (pure arr)

end Control.Lens
