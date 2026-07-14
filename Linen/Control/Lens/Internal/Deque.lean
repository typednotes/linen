/-
  Linen.Control.Lens.Internal.Deque — `Deque`, a purely functional
  double-ended queue

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Deque` (fetched and
  read via Hackage's rendered source, which for this module could only be
  retrieved as a detailed structural description rather than a verbatim
  quote — the summary below was cross-checked for the exact type signatures
  it does give). It implements a **Banker's deque**, following Chris
  Okasaki's "Purely Functional Data Structures": upstream's real
  declaration is

  ```
  data Deque a = BD !Int [a] !Int [a]
  ```

  a front list, its length, a rear list (stored reversed, so the *last*
  element of the deque is at its head), and its length. `cons`/`snoc` are
  O(1); `uncons`/`unsnoc` are O(1) amortized, with an internal `check`
  helper that rebalances whenever one side grows to more than three times
  the other, splitting the longer side in half and reversing the surplus
  onto the shorter side — the same invariant Okasaki's book uses to bound
  the *worst-case* single-operation cost while keeping the *amortized* cost
  O(1).

  **Scope note.** Upstream also derives a long list of typeclass instances
  (`Applicative`/`Alt`/`Alternative`/`Monad`/`Foldable`/`Traversable`/
  `Semigroup`/`Monoid`/indexed variants/`Cons`/`Snoc` optics, …) — none of
  which has a call site anywhere in this batch's scope, whose only
  consumer-to-be is a later batch's breadth-first traversal machinery, which
  needs exactly `empty`/`cons`/`snoc`/`uncons`/`unsnoc` (plus `size`/`null`/
  `toList` for testing and interop). Only those, plus the plain `Functor`
  instance every one of upstream's other instances is itself built from, are
  ported here; the rest are deferred to whichever later batch first needs
  them, matching this codebase's existing precedent of deferring
  unconsumed machinery (e.g. `Indexed.lean`'s deferral of `Indexing`/
  `withIndex`).

  **Termination note.** Every operation below is *non*-self-referential
  structural recursion over the two plain `List`s a `Deque` carries (no
  operation recurses on `Deque` itself) — `check`, `cons`, `snoc`, `uncons`,
  and `unsnoc` all terminate immediately from Lean's structural-recursion
  checker with no fuel or `partial`.
-/

namespace Control.Lens.Internal

-- ── Deque ──────────────────────────────────────

/-- A Banker's deque: a front list (in order) and a rear list (stored
    reversed, i.e. its head is the deque's *last* element), each paired with
    its own length so `size`/`null` are O(1). Maintains the invariant that
    neither length exceeds `3 * other + 1`, giving O(1) amortized `cons`/
    `snoc`/`uncons`/`unsnoc`. See the module docstring for the correspondence
    with upstream's `BD !Int [a] !Int [a]`. -/
structure Deque (A : Type u) where
  /-- The length of `front`. -/
  frontLen : Nat
  /-- The front half, in deque order. -/
  front : List A
  /-- The length of `rear`. -/
  rearLen : Nat
  /-- The rear half, stored reversed (its head is the deque's last element). -/
  rear : List A
  deriving BEq, DecidableEq, Repr

namespace Deque

/-- The empty deque. -/
def empty : Deque A := ⟨0, [], 0, []⟩

/-- A deque holding a single element. -/
def singleton (a : A) : Deque A := ⟨1, [a], 0, []⟩

/-- `O(1)`: is the deque empty? -/
def null (d : Deque A) : Bool := d.frontLen == 0 && d.rearLen == 0

/-- `O(1)`: the number of elements in the deque. -/
def size (d : Deque A) : Nat := d.frontLen + d.rearLen

/-- Rebalance a front/rear pair whose lengths may have just grown by one,
    restoring the `3 * other + 1` invariant by splitting the longer side in
    half and reversing the surplus onto the shorter side (Okasaki's
    "check", the operation every one of upstream's O(1)-amortized bounds
    rests on). -/
def check (fl : Nat) (fs : List A) (rl : Nat) (rs : List A) : Deque A :=
  if fl > 3 * rl + 1 then
    let sp := (fl + rl) / 2
    let fs' := fs.take sp
    let rs' := rs ++ (fs.drop sp).reverse
    ⟨sp, fs', fl + rl - sp, rs'⟩
  else if rl > 3 * fl + 1 then
    let sp := (fl + rl) / 2
    let rs' := rs.take sp
    let fs' := fs ++ (rs.drop sp).reverse
    ⟨fl + rl - sp, fs', sp, rs'⟩
  else
    ⟨fl, fs, rl, rs⟩

/-- `O(1)` amortized: prepend an element. -/
def cons (a : A) (d : Deque A) : Deque A :=
  check (d.frontLen + 1) (a :: d.front) d.rearLen d.rear

/-- `O(1)` amortized: append an element. -/
def snoc (d : Deque A) (a : A) : Deque A :=
  check d.frontLen d.front (d.rearLen + 1) (a :: d.rear)

/-- `O(1)` amortized: split off the first element, if any. When `front` is
    empty the invariant forces `rear` to hold at most one element, so this
    falls back to `rear` exactly as upstream's `_Cons` prism does. -/
def uncons (d : Deque A) : Option (A × Deque A) :=
  match d.front with
  | a :: fs => some (a, check (d.frontLen - 1) fs d.rearLen d.rear)
  | [] =>
    match d.rear with
    | a :: rs => some (a, check d.frontLen d.front (d.rearLen - 1) rs)
    | [] => none

/-- `O(1)` amortized: split off the last element, if any. Symmetric to
    `uncons`, falling back to `front` when `rear` is empty. -/
def unsnoc (d : Deque A) : Option (Deque A × A) :=
  match d.rear with
  | a :: rs => some (check d.frontLen d.front (d.rearLen - 1) rs, a)
  | [] =>
    match d.front with
    | a :: fs => some (check (d.frontLen - 1) fs d.rearLen d.rear, a)
    | [] => none

/-- Collect every element in deque order. -/
def toList (d : Deque A) : List A := d.front ++ d.rear.reverse

/-- `O(n)` amortized: build a deque from a list, front-to-back (upstream's
    `fromList = foldr cons empty`). -/
def fromList (xs : List A) : Deque A := xs.foldr cons empty

end Deque

/-- `Deque` is a `Functor`: map over every element, on both halves. -/
instance : Functor Deque where
  map f d := ⟨d.frontLen, d.front.map f, d.rearLen, d.rear.map f⟩

end Control.Lens.Internal
