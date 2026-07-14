/-
  Linen.Control.Lens.Internal.Magma — `Magma`/`Molten`, the free-applicative
  tree a `Bazaar` reifies a traversal's shape into

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Magma` (fetched and
  read via Hackage's rendered source: the exact GADT constructor signatures
  below were pulled from the real source, not recalled from memory):

  ```
  data Magma i t b a where
    MagmaAp   :: Magma i (x -> y) b a -> Magma i x b a -> Magma i y b a
    MagmaPure :: x -> Magma i x b a
    MagmaFmap :: (x -> y) -> Magma i x b a -> Magma i y b a
    Magma     :: i -> a -> Magma i b b a
  newtype Molten i a b t = Molten { runMolten :: Magma i t b a }
  runMagma :: Magma i t a a -> t
  ```

  `Magma i t b a` is the **initial (data, not function) encoding** of a free
  applicative over "one leaf per traversed element": `MagmaPure`/`MagmaFmap`/
  `MagmaAp` are exactly `pure`/`fmap`/`(<*>)`, reified as a tree instead of
  run immediately, and `Magma i a` (the constructor, shadowing the type's own
  name, matching upstream) is a single leaf holding one element `a` at index
  `i`. `t` is the GADT's only *varying* index (the "result so far" type);
  `i`/`b`/`a` are uniform across an entire tree. Lean's inductive families
  express this directly: below, `Magma I B A` is declared as a one-index
  family in `T`, mirroring the GADT exactly.

  **Scope note.** Deriving `Functor`/`Foldable`/`Traversable` in Haskell maps
  over a GADT's *last* type parameter, `a` here (the leaf payload) — not the
  variable index `t`. Lean's own `Functor`/`Data.Foldable`/`Data.Traversable`
  classes are keyed to the *one remaining* `Type u → Type u` slot of a type
  constructor, which for `Magma I B A : Type u → Type u` is `T`, not `A`.
  Since that is a structural mismatch (not a capability gap), the
  leaf-payload-mapping operations below are given as plain named functions
  (`Magma.mapLeaves`/`.foldrWithIndex`/`.traverseWithIndex`) rather than as
  instances of those generic classes.

  **Scope note (`assoc`/`these`/`semigroupoids` slivers).** Per the plan,
  this module folds in three tiny external slivers used only by a later
  batch's `Magma`-shape-merging helper (combining two magma trees of
  possibly different length, e.g. behind `partsOf`): `Data.These` (a
  three-way sum of "only left", "only right", or "both"), and the
  one-method `Swap`/`Assoc` classes. They are ported here per the plan
  (so the later module that needs them has them ready) but are not yet
  wired into any of this module's own recursive functions, since no
  batch-A module calls the merge helper itself.
-/

namespace Control.Lens.Internal

-- ── Magma ──────────────────────────────────────

/-- `Magma I B A T`: a free-applicative tree over leaves of type `A`, each
    tagged with an index `I`; `T` is the "value so far" type and is the
    family's only varying index. See the module docstring for the exact
    correspondence with upstream's GADT.

    **Universe note.** `MagmaAp`/`MagmaFmap` existentially quantify over an
    arbitrary intermediate type `X`/`Y : Type u` at every node — the tree
    itself, not just its index, must be able to record *which* `Type u` was
    chosen at each `ap`/`fmap`. That is genuinely large data (on par with
    storing a `Type u` value), so `Magma I B A T` is placed one universe
    above `T` itself (`Type u → Type (u + 1)`) rather than `Type u → Type u`;
    Lean's kernel rejects the naively-`Type u`-valued family with a universe
    error for exactly this reason. -/
inductive Magma (I B A : Type u) : Type u → Type (u + 1) where
  /-- Reified `(<*>)`: apply a function-shaped subtree to a value-shaped one. -/
  | ap {X Y : Type u} : Magma I B A (X → Y) → Magma I B A X → Magma I B A Y
  /-- Reified `pure`: a leafless value, carrying no traversed elements. -/
  | pure {X : Type u} : X → Magma I B A X
  /-- Reified `fmap`. -/
  | fmap {X Y : Type u} : (X → Y) → Magma I B A X → Magma I B A Y
  /-- A single leaf: one traversed element `a`, at index `i`, not yet
      converted to `b` (upstream's constructor, named `Magma` itself,
      shadowing the type). -/
  | leaf : I → A → Magma I B A B

namespace Magma

/-- Run a `Magma` whose leaves have already been converted to their target
    type (`b = a`), collapsing `MagmaAp`/`MagmaFmap`/`MagmaPure` into
    ordinary application and returning each leaf's stored value unchanged. -/
def run : Magma I A A T → T
  | .ap mf mx => (run mf) (run mx)
  | .pure x => x
  | .fmap f m => f (run m)
  | .leaf _ a => a

/-- Map over every leaf's payload type, leaving the tree shape and the
    "value so far" type `T` untouched (upstream's derived `Functor`, over the
    GADT's last parameter — see the module's scope note on why this is a
    plain function here rather than a `Functor` instance). -/
def mapLeaves {A' : Type u} (f : A → A') : Magma I B A T → Magma I B A' T
  | .ap mf mx => .ap (mapLeaves f mf) (mapLeaves f mx)
  | .pure x => .pure x
  | .fmap g m => .fmap g (mapLeaves f m)
  | .leaf i a => .leaf i (f a)

/-- Right-fold over every leaf, with access to its index (upstream's derived
    `Foldable`/`FoldableWithIndex`), visiting the function-subtree of every
    `MagmaAp` node before its argument-subtree (matching `(<*>)`'s
    left-to-right effect order). -/
def foldrWithIndex (g : I → A → S → S) : Magma I B A T → S → S
  | .ap mf mx, z => foldrWithIndex g mf (foldrWithIndex g mx z)
  | .pure _, z => z
  | .fmap _ m, z => foldrWithIndex g m z
  | .leaf i a, z => g i a z

/-- Collect every leaf's index and payload, left to right. -/
def toListWithIndex (m : Magma I B A T) : List (I × A) :=
  foldrWithIndex (fun i a acc => (i, a) :: acc) m []

/-- Traverse every leaf with an effectful, index-aware function, rebuilding
    the tree with the same shape but a (possibly different) leaf payload
    type (upstream's derived `Traversable`/`TraversableWithIndex`).

    **Universe note.** Since `Magma I B A' T` lives one universe above `T`
    itself (see `Magma`'s docstring), the effect `F` has to be able to hold
    a value at that raised level, so it is typed `Type (u+1) → Type (u+1)`
    here rather than `Type u → Type u`; `g`'s own result is correspondingly
    wrapped in `ULift` to lift it up to meet `F`, and unwrapped again at each
    leaf. -/
def traverseWithIndex {F : Type (u + 1) → Type (u + 1)} {A' : Type u} [Applicative F]
    (g : I → A → F (ULift.{u + 1} A')) : Magma I B A T → F (Magma I B A' T)
  | .ap mf mx => Magma.ap <$> traverseWithIndex g mf <*> traverseWithIndex g mx
  | .pure x => (Pure.pure (Magma.pure x) : F (Magma I B A' T))
  | .fmap f m => Magma.fmap f <$> traverseWithIndex g m
  | .leaf i a => (fun x => Magma.leaf i x.down) <$> g i a

end Magma

-- ── Molten ─────────────────────────────────────

/-- `Molten I A B T := Magma I B A T`: `Magma` with its two "leaf" type
    parameters reordered to match the `s a b t`-style naming convention the
    rest of this `lens` port uses (index, source-element, target-element,
    result). -/
def Molten (I A B T : Type u) := Magma I B A T

namespace Molten

/-- Wrap a `Magma` as a `Molten`. -/
@[inline] def mk (m : Magma I B A T) : Molten I A B T := m

/-- Unwrap a `Molten` back to its underlying `Magma`. -/
@[inline] def runMolten (m : Molten I A B T) : Magma I B A T := m

end Molten

-- ── `assoc`/`these` slivers ────────────────────

/-- `These α β`: holds a left value, a right value, or both (Haskell's
    `Data.These`, folded in per the plan — see the module's scope note). -/
inductive These (α β : Type u) where
  | this : α → These α β
  | that : β → These α β
  | both : α → β → These α β

/-- One-method class for swapping a symmetric binary structure (Haskell's
    `Data.Bifunctor.Swap`, folded in per the plan). -/
class Swap (P : Type u → Type u → Type u) where
  /-- Swap the two sides. -/
  swap : P α β → P β α

instance : Swap Prod where
  swap p := (p.2, p.1)

instance : Swap These where
  swap
    | .this a => .that a
    | .that b => .this b
    | .both a b => .both b a

/-- One-method class for reassociating a nested binary structure (Haskell's
    `Data.Bifunctor.Assoc`, folded in per the plan). -/
class Assoc (P : Type u → Type u → Type u) where
  /-- Reassociate to the right. -/
  assoc : P (P α β) γ → P α (P β γ)

instance : Assoc Prod where
  assoc p := (p.1.1, (p.1.2, p.2))

end Control.Lens.Internal
