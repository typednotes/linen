/-
  Linen.Control.Lens.Internal.Level — `Level`, one breadth-first layer of a
  traversed structure

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Level` (fetched and
  read via Hackage's rendered source, which for this module could only be
  retrieved as a detailed structural description rather than a verbatim
  quote — the summary below was cross-checked for the exact type signatures
  it does give). Upstream's real declaration is

  ```
  data Level i a
    = Two {-# UNPACK #-} !Word (Level i a) (Level i a)
    | One i a
    | Zero
  ```

  a "path-compressed copy of one level of a source data structure": `Zero`
  is empty (upstream notes it "can only occur at the root"), `One i a` is a
  single element at index `i`, and `Two n l r` is the concatenation of two
  non-empty sub-levels, with `n` caching the total element count of the
  merged node so later operations that need its size (`size` itself, and any
  future consumer that needs to know how far to descend to reach a given
  position) are O(1) rather than re-counting. It ships `Functor`/`Foldable`/
  `Traversable` and their
  `WithIndex` variants (`imap`/`ifoldMap`/`itraverse`), all via a recursive
  `go` over the three constructors — exactly the shape ported below as
  `Level.map`/`Level.foldrWithIndex`/`Level.traverseWithIndex`, plus the
  internal `lappend` helper that merges two `Level`s while preserving the
  `n`-caches (used to combine the levels found on either side of a branch
  during a breadth-first walk).

  **Scope note.** Upstream's `Deepening`/`Flows` are further internal
  helpers — explicitly documented in the source itself as "illegal"
  `Monoid`/`Applicative` instances (i.e. ones that only work because of how
  they are used, not lawful in general) — used solely to *build*/*consume* a
  `Level` inside the public `levels`/`ilevels` combinators. Those combinators
  live in the separate (non-`Internal`) module `Control.Lens.Level`, out of
  this batch's scope (a later batch, alongside `ibreadthFirst` itself, is
  the one real consumer), so `Deepening`/`Flows` are deferred there rather
  than ported with no call site — matching this codebase's existing
  precedent of deferring unconsumed machinery (e.g. `Indexed.lean`'s
  deferral of `Indexing`/`withIndex` to whichever later batch needs them).

  **Termination note.** `Level I A` is an ordinary (non-existential, non-GADT)
  recursive type — unlike `Magma`, no constructor quantifies over a fresh
  type at each node, so no universe bump is needed here. Every function below
  recurses structurally on `Level`'s own two children, so Lean's
  structural-recursion checker accepts all of them directly, with no `partial`
  and no explicit termination proof required.
-/

namespace Control.Lens.Internal

-- ── Level ──────────────────────────────────────

/-- One breadth-first layer of a source structure: empty (`zero`), a single
    indexed element (`one`), or the concatenation of two non-empty layers
    (`two`, caching the merged node's total element count). See the module
    docstring for the exact correspondence with upstream's `Zero`/`One`/
    `Two` GADT-free ADT. -/
inductive Level (I A : Type u) : Type u where
  /-- The empty level (upstream: only ever produced at the root). -/
  | zero : Level I A
  /-- A single element `a` at index `i`. -/
  | one : I → A → Level I A
  /-- The concatenation of two non-empty levels, caching the merged node's
      total element count `n`. -/
  | two : Nat → Level I A → Level I A → Level I A
  deriving BEq, DecidableEq, Repr

namespace Level

/-- The number of elements held by a level (upstream caches this at every
    `Two` node so it is O(1) rather than a fresh traversal). -/
def size : Level I A → Nat
  | .zero => 0
  | .one _ _ => 1
  | .two n _ _ => n

/-- Merge two levels into one (upstream's internal `lappend`, not exposed as
    a public `Semigroup` instance since `Level` is not generally
    associative-append-friendly outside this module's own use). `Zero` is
    the identity on either side; otherwise the result is `Two`, caching the
    combined size. -/
def lappend : Level I A → Level I A → Level I A
  | .zero, y => y
  | x, .zero => x
  | x, y => .two (x.size + y.size) x y

/-- Collect every `(index, element)` pair, in level order. -/
def toListWithIndex : Level I A → List (I × A)
  | .zero => []
  | .one i a => [(i, a)]
  | .two _ l r => l.toListWithIndex ++ r.toListWithIndex

/-- Map over every element, keeping indices and shape (upstream's derived
    `Functor`). -/
def map (f : A → A') : Level I A → Level I A'
  | .zero => .zero
  | .one i a => .one i (f a)
  | .two n l r => .two n (l.map f) (r.map f)

/-- Right-fold over every element with access to its index (upstream's
    derived `FoldableWithIndex`). -/
def foldrWithIndex (g : I → A → S → S) : Level I A → S → S
  | .zero, z => z
  | .one i a, z => g i a z
  | .two _ l r, z => l.foldrWithIndex g (r.foldrWithIndex g z)

/-- Traverse every element with an effectful, index-aware function,
    rebuilding the level with the same shape (upstream's derived
    `TraversableWithIndex`). Unlike `Magma`, `Level` carries no existentially
    quantified intermediate types, so no universe bump is needed: `F` stays
    at the ordinary `Type u → Type u`. -/
def traverseWithIndex {F : Type u → Type u} [Applicative F] (g : I → A → F A') :
    Level I A → F (Level I A')
  | .zero => Pure.pure .zero
  | .one i a => .one i <$> g i a
  | .two n l r => .two n <$> l.traverseWithIndex g <*> r.traverseWithIndex g

end Level

/-- `Level I` is a `Functor`: map over every element. -/
instance : Functor (Level I) where
  map := Level.map

end Control.Lens.Internal
