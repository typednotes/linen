/-
  Linen.Control.Lens.Level — `levels`, breadth-first-numbered traversal order
  over a `Plated` structure

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Level` (fetched and read via
  Hackage's rendered Haddock and source). Upstream's real `levels`/`ilevels`
  are startlingly general: `levels :: Applicative f => Traversing (->) f s t
  a b -> IndexedLensLike Int f s t (Level () a) (Level () b)` takes *any*
  single `Traversal`/`Fold` (not necessarily one tied to a `Plated`
  recursive type at all) and slices the *applicative-combination tree* that
  traversal's own `traverse`-style implementation happens to build out of
  nested `<*>` calls into per-depth `Level () a` layers, via an internal
  "illegal `Applicative`" (`Deepening`) that upstream's own Haddock
  documents as deliberately violating the `Applicative` laws — it exists
  purely to extract this shape via iterative deepening, never to compute a
  real effect.

  **Deviation (a `Plated`-generation BFS, not a generic combining-tree
  slicer).** Faithfully reproducing that internal `Deepening`/`Flows`
  machinery — laziness-driven iterative deepening over an arbitrary
  reified `Bazaar`'s free-applicative shape, immediately consumed via a
  further "replay" pass that pattern-matches a very specific `Level`
  shape or raises a shape-mismatch error — has no other call site in this
  batch's scope, and (having actually worked out its semantics: see the
  reasoning below) is not even the tool that gives a *useful* breadth-first
  walk of a `Plated` recursive structure in the first place. Concretely: for
  a hand-written `Plated` instance's `plate` (e.g. `plate f (Add l r) = Add
  <$> f l <*> f r`), a *single* application of `plate` never recurses into
  grandchildren at all — every leaf `f` visits sits at the exact same
  `<*>`-nesting depth (there is only ever one depth, `0`), so
  `levels plate` degenerates to one trivial layer holding all of a value's
  *immediate* children, regardless of how deep the value's own recursive
  structure actually goes. Upstream's combining-tree-depth notion of
  "level" only lines up with genuine generation-by-generation BFS for
  traversals whose *own* `traverse` implementation is itself written
  recursively in a way that nests one extra `<*>` per generation (e.g.
  `Data.Tree`'s hand-written `Traversable` instance) — not for `plate`
  composed with nothing else, and not in general for any traversal that
  combines more than two elements via a flat chain of `<*>` (each element of
  a plain `List`'s default `traverse`, for instance, ends up at its own
  distinct nesting depth purely as an artifact of how the chain associates,
  not because of any real "generation").

  This port therefore gives what is actually useful for a `Plated`
  structure specifically — a genuine breadth-first walk *of the
  `plate`-recursion tree itself*, generation by generation (depth `0` is
  the value itself, depth `1` its immediate `plate`-children, depth `2`
  their children, and so on) — built directly from `Linen.Control.Lens.
  Internal.Level`'s `Level` type exactly as its own docstring anticipates
  ("a path-compressed copy of one level of a source data structure"):
  `Level.lappend` merges the same-depth layers found in two sibling
  subtrees, so one whole generation's `Level` slice is simply the merge of
  every child's own generation-`(k-1)` slice. Shares `Linen.Control.Lens.
  Plated`'s termination note on the real `hDec`/`[SizeOf A]` witness this
  recursion needs (through the same opaque, caller-supplied `plate`/
  `Traversal'`), built on that module's shared `foldChildrenOf` primitive.

  **Scope note (read-only: no writable `IndexedTraversal'`).** Upstream's
  real `levels` is a full `IndexedLensLike` — it can *rewrite* every visited
  layer, not just read it, using the `Deepening`/`Flows` pair to both slice
  a traversal's shape apart and *replay* modified slices back through it
  in the same shape. A coherent, generally-well-defined rewrite semantics
  for a breadth-first walk needs exactly that replay step (deciding, when an
  ancestor is itself replaced, what happens to its already-computed
  descendants' own replacements is precisely what `Flows` resolves, by
  always rebuilding from the *original* skeleton one leaf at a time); this
  port has not built that replay machinery (see the deviation note above for
  why the machinery it would replay has no other call site here), so `levels`
  below is given only as an `IndexedFold` — genuinely useful for exactly the
  case this batch's task calls for (listing a `Plated` value's transitive
  contents in breadth-first order), but not a writable optic. Matches this
  codebase's existing precedent for a combinator's non-writable core (e.g.
  `Control.Lens.Fold`'s own `folded`, ported without the writable `Setter`
  half nothing in that module's scope needed either).

  **Scope note (`ilevels`).** Upstream's `ilevels` is `levels` generalized to
  also preserve an *inner* index already carried by an `IndexedTraversal`
  argument. `Plated`'s `plate` carries no index of its own (it is a plain
  `Traversal'`, not an `IndexedTraversal'`), so there is no inner index for
  a `Plated`-specialized `ilevels` to preserve here; skipped for the same
  reason `Linen.Control.Lens.Fold`'s `folded` gives only the non-indexed
  core (see that module's own scope note) rather than manufacturing an index
  with nothing to carry.

  **Universe note.** `levelsOf`/`levels` fix their element type at `Type`
  (not the more general `Type u` most of `Linen.Control.Lens.Plated` uses),
  the same restriction `Linen.Control.Lens.Indexed.indexing` already places
  on itself: `IndexedFold`'s three type parameters share one universe
  (visible from `#check @Control.Lens.IndexedFold : Type u_1 → Type u_1 →
  Type u_1 → Type (u_1 + 1)`), and the index here is concretely `Nat :
  Type`, which pins that shared universe to `Type` for the element type
  too. `levelSlices`/`bfsOf`/`bfs` have no such restriction (no `Indexed`
  optic involved) and stay at `Type u`. -/

import Linen.Control.Lens.Plated
import Linen.Control.Lens.Indexed
import Linen.Control.Lens.Internal.Level

open Control.Lens.Internal Data.Functor

namespace Control.Lens

-- ── levelSlices ─────────────────────────────────

/-- Merge two same-depth `Level` layers found in two sibling subtrees,
    element-wise by depth — the zip-longest merge `levelSlices` builds each
    generation's layer out of, treating a missing (already-exhausted) side
    as `Level.zero` (`Level.lappend`'s identity). -/
private def mergeLevelLists {I A : Type u} :
    List (Level I A) → List (Level I A) → List (Level I A)
  | [], ys => ys
  | xs, [] => xs
  | x :: xs, y :: ys => Level.lappend x y :: mergeLevelLists xs ys

/-- Merge every child's own list of generation-slices into one combined list
    of generation-slices for the whole (sub)tree. -/
private def mergeAllLevelLists {I A : Type u} (ls : List (List (Level I A))) :
    List (Level I A) :=
  ls.foldl mergeLevelLists []

/-- `levelSlices l hDec a`: one `Level PUnit A` layer per generation of the
    `l`-recursion tree rooted at `a` — depth `0` is `a` itself, depth `1`
    the merge of every immediate child's own layer at depth `0`, and so on.
    `PUnit` (rather than `Unit`) is used only because `Level`'s own index
    parameter shares a single universe with its element parameter, so a
    `Level _ A` at an arbitrary `A : Type u` needs a `Type u`-level "no
    real index" placeholder, which `Unit : Type` cannot supply for `u > 0`.
    See the module's deviation note for why this walks `l`'s *own*
    recursion tree generation-by-generation, rather than reproducing
    upstream's combining-tree-depth `Level () a`/`Deepening` machinery
    directly, and `Linen.Control.Lens.Plated`'s termination note (this is
    built directly on that module's `foldChildrenOf`) for `hDec`, in place
    of the previous `fuel` bound. -/
def levelSlices {A : Type u} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (a : A) : List (Level PUnit A) :=
  foldChildrenOf (toListOf l) hDec
    (fun a rs => Level.one PUnit.unit a :: mergeAllLevelLists rs) a

-- ── bfsOf / bfs ─────────────────────────────────

/-- `bfsOf l hDec a`: every node reachable from `a` via `l`, flattened into a
    single breadth-first-ordered list (generation `0` first, then every
    generation-`1` node in order, and so on) — `levelSlices`, flattened. -/
def bfsOf {A : Type u} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (a : A) : List A :=
  (levelSlices l hDec a).flatMap (fun lvl => lvl.toListWithIndex.map Prod.snd)

/-- `bfs :: Plated a => a -> [a]`: `bfsOf plate` — every transitive
    descendant of a `Plated` value (including the value itself), in
    breadth-first order. Contrast with `Linen.Control.Lens.Plated.universe`,
    which lists the same set of nodes in pre-order (depth-first) instead. -/
@[inline] def bfs {A : Type u} [SizeOf A] [Plated A] (a : A) : List A :=
  bfsOf plate plate_decreasing a

-- ── ifoldingList ────────────────────────────────

/-- Build an `IndexedFold` directly out of an arbitrary
    `S -> List (I × A)` function, indexed-`Fold`'s analogue of `Control.
    Lens.Fold.folding` — the shared primitive `levelsOf`/`levels` below are
    built from, mirroring `Control.Lens.Indexed.ifolded`'s own
    implementation shape. -/
@[inline] def ifoldingList {I S A : Type u} (sia : S → List (I × A)) : IndexedFold I S A :=
  fun {F} [Contravariant F] [Applicative F] {P} [Indexable I P] pab s =>
    Contravariant.contramap (fun (_ : S) => PUnit.unit)
      (Data.Foldable.foldr
        (fun (p : I × A) acc => SeqRight.seqRight (Indexable.indexed pab p.1 p.2) (fun _ => acc))
        (Pure.pure PUnit.unit : F PUnit)
        (sia s))

-- ── levelsOf / levels ────────────────────────────

/-- `levelsOf l hDec : IndexedFold Nat a a`: every node reachable from a
    value via `l`, in breadth-first order, indexed by its position in that
    breadth-first order (`0`, `1`, `2`, …). See the module's scope note for
    why this is the read-only `IndexedFold` core rather than upstream's
    writable `IndexedLensLike`. -/
@[inline] def levelsOf {A : Type} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) : IndexedFold Nat A A :=
  ifoldingList (fun a => (List.range (bfsOf l hDec a).length).zip (bfsOf l hDec a))

/-- `levels :: Plated a => IndexedFold Int a a`: `levelsOf plate` — every
    transitive descendant of a `Plated` value (including the value itself),
    in breadth-first order, indexed by breadth-first position. See the
    module's deviation note on why this walks `plate`'s own recursion tree
    generation-by-generation rather than reproducing upstream's
    combining-tree-depth `Level`/`Deepening` machinery, and the scope note
    on why it is read-only. -/
@[inline] def levels {A : Type} [SizeOf A] [Plated A] : IndexedFold Nat A A :=
  levelsOf plate plate_decreasing

end Control.Lens
