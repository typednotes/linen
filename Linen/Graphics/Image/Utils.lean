/-
  Linen.Graphics.Image.Utils — small numeric/list helpers

  ## Haskell equivalent
  `Graphics.Image.Utils` from https://hackage.haskell.org/package/hip
  (module #1 of the `hip` import plan, see `docs/imports/hip/dependencies.md`).

  ## Design

  Upstream exposes five bindings: the composition combinators `(.:)` and
  `(.:!)`, the general-purpose loop combinators `loop`/`loopM_`, and the pair
  helper `swapIx`.

  * `(.:)`/`(.:!)` are the "blackbird" combinator `(.) . (.)`, i.e.
    `f .: g = \x y -> f (g x y)`; `(.:!)` is only a strictness-annotated
    (`BangPatterns`) variant of the exact same function. Lean has no
    surface-level strict/lazy distinction for this kind of pure combinator
    (there is no `seq`/`BangPatterns` equivalent to thread through), so both
    collapse to one definition, `compose₂`, with `compose₂!` kept as a
    doc-only alias for call sites that ported from a `.:!` use, to preserve a
    1:1 mapping with upstream call sites without duplicating behaviour.
  * `swapIx` is exactly `Prod.swap` from the Lean standard library (swap the
    two components of a pair) — reused rather than re-defined, per
    `AGENTS.md`'s stdlib-precedence rule.
  * `loop`/`loopM_` are upstream's hand-rolled tight iteration primitives:
    `loop init cond incr initAcc f` repeatedly applies `f` to a mutable index
    `step` (starting at `init`, advanced by `incr`) and an accumulator, until
    `cond step` turns false; `loopM_` is the same shape specialised to a
    monadic side-effecting body with no accumulator. Both are given fully
    polymorphic `t`/`condition`/`increment` arguments in Haskell, so in
    general nothing guarantees `loop`/`loopM_` ever terminates — termination
    is entirely on the caller, who must supply a `condition`/`increment` pair
    that eventually disagree. That can't be ported as a total Lean function
    for arbitrary `condition`/`increment` without either a fuel parameter
    (which `AGENTS.md` rules out as a way to dodge a real termination proof)
    or requiring the caller to supply their own termination proof for an
    arbitrary step function (which upstream's own type does not require, so
    porting that requirement in would not be a faithful port either).

    Checking every real call site of `loop`/`loopM_` in the `hip` source tree
    (`Graphics.Image.Interface.Vector.Generic`, `Graphics.Image.Processing.
    Convolution`) shows every single one instantiates the general combinator
    to exactly the same concrete shape: `loop 0 (< n) (+ 1) initAcc f` /
    `loopM_ lo (< hi) (+ 1) f` — a plain bounded, increasing `Nat` index walk.
    So this port narrows `loop`/`loopM_` to that one concrete, always-
    terminating shape (bounded increasing integer iteration over
    `[start, start + len)`), which is a genuine restriction of upstream's
    fully general (and only conditionally terminating) type but is faithful
    to every actual use of it. Once narrowed this way, both combinators are
    just the stdlib's own `List.range`/`List.foldl` and `forM` under another
    name, so they are implemented directly in terms of those rather than via
    fresh recursion, per the stdlib-reuse precedence rule.
-/

namespace Graphics.Image.Utils

-- ── Composition combinators ──

/-- The "blackbird" combinator: compose a unary function after a binary one,
`f ∘₂ g = fun x y => f (g x y)`. Upstream's `(.:) :: (a -> b) -> (c -> d -> a)
-> (c -> d -> b)`. -/
def compose₂ (f : α → β) (g : γ → δ → α) : γ → δ → β :=
  fun x y => f (g x y)

/-- Upstream's `(.:!)`, the `BangPatterns`-strict variant of `compose₂`. Lean
has no separate strict-evaluation pragma for a pure combinator like this one,
so it is definitionally identical to `compose₂`; kept only so a ported call
site that used `.:!` upstream has a like-named target. -/
abbrev compose₂! (f : α → β) (g : γ → δ → α) : γ → δ → β :=
  compose₂ f g

-- ── Pair helper ──

/-- Upstream's `swapIx :: (a, b) -> (b, a)`, swapping the two components of a
pair. Exactly the Lean standard library's `Prod.swap`, reused directly. -/
abbrev swapIx : α × β → β × α :=
  Prod.swap

-- ── Bounded loops ──

/-- Upstream's `loop`, narrowed to the one shape every real call site uses:
a bounded, increasing walk over the `len` indices `start, start + 1, …,
start + len - 1`, threading an accumulator `acc` through `f`. See the module
doc-comment for why the fully general, arbitrary-`condition`/`increment`
version of `loop` cannot be ported as a total function. Implemented directly
via the standard library's `List.range`/`List.foldl` (a finite fold, so
termination is immediate) rather than fresh recursion. -/
def loop (start len : Nat) (acc : α) (f : Nat → α → α) : α :=
  (List.range len).foldl (fun acc' i => f (start + i) acc') acc

/-- Upstream's `loopM_`, narrowed the same way as `loop` (see its
doc-comment): a bounded, increasing monadic walk over `start, start + 1, …,
start + len - 1`, run purely for effect. -/
def loopM_ [Monad m] (start len : Nat) (f : Nat → m PUnit) : m PUnit :=
  forM (List.range len) (fun i => f (start + i))

end Graphics.Image.Utils
