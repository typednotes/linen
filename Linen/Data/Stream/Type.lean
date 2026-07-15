/-
  Linen.Data.Stream.Type — the fused, direct-style stream (`Stream`)

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Stream.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Stream/Type.hs),
  module #19 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`) — the library's centerpiece.

  A `Stream m a` is streamly's **fused, direct-style** stream (upstream's
  `StreamD`): a stepper `State → s → m (Step s a)` paired with a current state
  `s`, where the state type is existentially hidden. `Step` is the
  `Yield`/`Skip`/`Stop` state machine of `Data.Stream.Step` (#7). This is the
  encoding GHC's fusion plugin inlines into tight loops; it is a *different*
  representation from `StreamK` (#12, the CPS/recursive stream) — the two are
  bridged by `fromStreamK`/`toStreamK`.

  ## Representation: an ordinary existential-state structure (not `unsafe`)

  Unlike `StreamK` (a non-strictly-positive/corecursive inductive that needs
  `unsafe inductive`), the fused `Stream` is a plain `structure` with an
  existential state field — exactly like `Unfold`/`Fold`/`Producer` (#15/#14/#17).
  The **type itself needs no `unsafe`**.

  What *does* need `unsafe` is a subset of the *functions*:

  - **The consumption/driver loops** (`fold`, `foldl'`/`foldlM'`/`foldlx'`/
    `foldlMx'`, `foldr`/`foldrM`, `drain`, `head`, `toList`, `eqBy`). Driving a
    fused stepper is a `while`-style loop over
    `Skip`: a stream may `Skip` arbitrarily many times between yields (e.g.
    `Stream (fun _ s => pure (.Skip s)) ()` never terminates), so the loop has
    **no structural or well-founded termination measure**. Per AGENTS.md this
    may not be dodged with `partial def` or a fuel parameter; the sanctioned
    alternative is `unsafe def` (the same call the `Data.Conduit` /
    `Data.StreamK` ports make). `unsafe` is neither `partial` nor `sorry`.
  - **The `StreamK` bridge** (`fromStreamK`/`toStreamK`) and the `concatMap`
    family, which are built on the `unsafe` `StreamK`.

  All the *construction/transformation builders* (`nilM`/`consM`/`fromPure`/
  `fromEffect`/`fromList`/`unfold`/`map`/`mapM`/`take`/`takeWhile`/`append`/
  `zipWith`/`crossApply`/`unfoldEach`/…) merely assemble a new stepper without
  driving it, so they are ordinary total `def`s.

  ## Substitutions / deviations

  - **Monomorphic in `Type`** (`m : Type → Type`, `a : Type`), matching the
    `StreamK` port. The `State` argument is our pure scheduling record
    (`Data.Stream.State`, #6); `adaptState`/`unShare` are identities on it, so
    the `Stream`/`UnStream` pattern-synonym distinction and the `State`
    threading are no-ops here (threaded plainly).
  - **`Fuse` annotations dropped**; local fusion-state types are inlined as
    `Option`/`Bool`/`Sum`/`Prod` (the `AppendState`/`ConcatMapUState`/… records).
  - **`Applicative`/`Monad` live on `Nested`.** Upstream's `Applicative`/`Monad`
    instances for `Stream` itself are commented out; the real instances are on
    the `Nested` newtype (a cross-product monad). We port `Nested` with those
    instances. Only `Functor (Stream m)` is a direct instance, as upstream.
  - **`uncons`/`foldBreak`/`foldEither`/`concat` dropped — a universe wall.**
    Because a `Stream` packs an existential state, `Stream m a : Type 1`, while
    `m : Type → Type` can only wrap `Type 0` values. So any signature that puts
    a residual `Stream` *inside* `m` — `uncons : … → m (Option (a × Stream m a))`,
    `foldBreak : … → m (b × Stream m a)`, `foldEither` — is not expressible
    here, and `concat :: Stream m (Stream m a) → …` cannot even carry a
    `Type 1` stream as its `Type 0` element. `fold` is therefore given as a
    standalone driver (returning `m b`, no residual) rather than via
    `foldBreak`; `concatMap` is routed through the `StreamK` bridge
    (`StreamK : Type 0`, so it stores no nested stream in a fused state), while
    its monadic siblings `concatMapM`/`concatForM`/`concatEffect` (which wrap a
    `Stream` inside `m`) are likewise dropped. `unfoldEach`/`unfoldCross` cover
    `concatMap`'s expressive power fusibly and are kept. The breadth-first/
    iterate/`fairCross` families and the numeric `Foldable` helpers belong to
    later stream-operation tiers, matching the plan's scoping.
  - **`fromStreamK` needs `[Monad m]`** (upstream `Applicative`) because our
    `StreamK.foldStreamShared` sequences the `effect` case.
-/

import Linen.Data.Stream.Step
import Linen.Data.Stream.SVarType
import Linen.Data.StreamK.Type
import Linen.Data.Fold.Type
import Linen.Data.Unfold.Type

namespace Data.Stream

open Data.Stream (Step State defState adaptState)
open Data.StreamK (StreamK)
open Data.Fold (Fold)
open Data.Unfold (Unfold)

-- ── The fused stream type ────────────────────────────────────────────────────

/-- The fused, direct-style stream: a stepper `step : State → s → m (Step s a)`
    driven from a current `state : s`, with the state type `s` existentially
    hidden (an implicit field), exactly as for `Unfold`/`Fold`/`Producer`. -/
structure Stream (m : Type → Type) (a : Type) where
  /-- The hidden stepper-state type. -/
  {s : Type}
  /-- Advance the state, yielding a value, skipping, or stopping. -/
  step : State → s → m (Step s a)
  /-- The current state. -/
  state : s

namespace Stream

-- ── Primitives ───────────────────────────────────────────────────────────────

/-- A stream that stops immediately after running `act` for its effect. -/
@[inline] def nilM [Functor m] (act : m b) : Stream m a where
  step := fun _ _ => (fun _ => Step.Stop) <$> act
  state := ()

/-- The empty stream. -/
@[inline] def nil [Applicative m] : Stream m a where
  step := fun _ _ => pure Step.Stop
  state := ()

/-- Prepend an effectful head to a stream (`consM`, right-associative). -/
@[inline] def consM [Functor m] (act : m a) (r : Stream m a) : Stream m a where
  s := Option r.s
  step gst := fun
    | none => (fun x => Step.Yield x (some r.state)) <$> act
    | some st => (fun res => match res with
        | .Yield x s => Step.Yield x (some s)
        | .Skip s => Step.Skip (some s)
        | .Stop => Step.Stop) <$> r.step gst st
  state := none

@[inherit_doc] scoped infixr:60 " .:: " => consM

-- ── From `Unfold` ────────────────────────────────────────────────────────────

/-- Convert an `Unfold` into a stream by supplying its input seed. -/
@[inline] def unfold [Applicative m] (u : Unfold m a b) (seed : a) : Stream m b where
  s := Option u.s
  step := fun _ => fun
    | none => (fun st => Step.Skip (some st)) <$> u.inject seed
    | some st => (fun res => match res with
        | .Yield x s => Step.Yield x (some s)
        | .Skip s => Step.Skip (some s)
        | .Stop => Step.Stop) <$> u.step st
  state := none

-- ── From values ──────────────────────────────────────────────────────────────

/-- A singleton stream from a pure value (`pure`). -/
@[inline] def fromPure [Applicative m] (x : a) : Stream m a where
  s := Bool
  step := fun _ => fun
    | true => pure (Step.Yield x false)
    | false => pure Step.Stop
  state := true

/-- A singleton stream from a monadic action. -/
@[inline] def fromEffect [Applicative m] (act : m a) : Stream m a where
  s := Bool
  step := fun _ => fun
    | true => (fun x => Step.Yield x false) <$> act
    | false => pure Step.Stop
  state := true

-- ── From containers ──────────────────────────────────────────────────────────

/-- Construct a stream from a list of pure values. -/
@[inline] def fromList [Applicative m] (l : List a) : Stream m a where
  s := List a
  step := fun _ => fun
    | x :: xs => pure (Step.Yield x xs)
    | [] => pure Step.Stop
  state := l

-- ── Mapping ──────────────────────────────────────────────────────────────────

/-- Map a function over the yielded values. -/
@[inline] def map [Functor m] (f : a → b) (t : Stream m a) : Stream m b where
  s := t.s
  step gst st := Step.map f <$> t.step gst st
  state := t.state

/-- `Functor`: map over the yielded values. -/
instance [Functor m] : Functor (Stream m) where
  map := map

/-- Apply a monadic function to each yielded element. -/
@[inline] def mapM [Monad m] (f : a → m b) (t : Stream m a) : Stream m b where
  s := t.s
  step gst st := do
    match ← t.step gst st with
    | .Yield x s => (fun a => Step.Yield a s) <$> f x
    | .Skip s => pure (Step.Skip s)
    | .Stop => pure Step.Stop
  state := t.state

-- ── Stateful filters ─────────────────────────────────────────────────────────

/-- Take the first `n` elements and discard the rest. -/
@[inline] def take [Applicative m] (n : Nat) (t : Stream m a) : Stream m a where
  s := t.s × Nat
  step gst := fun (st, i) =>
    if i < n then
      (fun res => match res with
        | .Yield x s => Step.Yield x (s, i + 1)
        | .Skip s => Step.Skip (s, i)
        | .Stop => Step.Stop) <$> t.step gst st
    else pure Step.Stop
  state := (t.state, 0)

/-- End the stream as soon as the monadic predicate fails on an element. -/
@[inline] def takeWhileM [Monad m] (f : a → m Bool) (t : Stream m a) : Stream m a where
  s := t.s
  step gst st := do
    match ← t.step gst st with
    | .Yield x s => do if ← f x then pure (Step.Yield x s) else pure Step.Stop
    | .Skip s => pure (Step.Skip s)
    | .Stop => pure Step.Stop
  state := t.state

/-- End the stream as soon as the predicate fails on an element. -/
@[inline] def takeWhile [Monad m] (f : a → Bool) (t : Stream m a) : Stream m a :=
  takeWhileM (fun x => pure (f x)) t

-- ── Appending ────────────────────────────────────────────────────────────────

/-- Fuse two streams sequentially: all of the first, then all of the second.
    (`O(n²)` in the number of appends — use `StreamK.append` for `O(n)`.) -/
@[inline] def append [Functor m] (t1 t2 : Stream m a) : Stream m a where
  s := t1.s ⊕ t2.s
  step gst := fun
    | .inl st => (fun res => match res with
        | .Yield a s => Step.Yield a (Sum.inl s)
        | .Skip s => Step.Skip (Sum.inl s)
        | .Stop => Step.Skip (Sum.inr t2.state)) <$> t1.step gst st
    | .inr st => (fun res => match res with
        | .Yield a s => Step.Yield a (Sum.inr s)
        | .Skip s => Step.Skip (Sum.inr s)
        | .Stop => Step.Stop) <$> t2.step gst st
  state := Sum.inl t1.state

-- ── Zipping ──────────────────────────────────────────────────────────────────

/-- Zip corresponding elements of two streams with a monadic function. -/
@[inline] def zipWithM [Monad m] (f : a → b → m c) (ta : Stream m a) (tb : Stream m b) :
    Stream m c where
  s := ta.s × tb.s × Option a
  step gst := fun
    | (sa, sb, none) => do
        match ← ta.step gst sa with
        | .Yield x sa' => pure (Step.Skip (sa', sb, some x))
        | .Skip sa' => pure (Step.Skip (sa', sb, none))
        | .Stop => pure Step.Stop
    | (sa, sb, some x) => do
        match ← tb.step gst sb with
        | .Yield y sb' => (fun z => Step.Yield z (sa, sb', none)) <$> f x y
        | .Skip sb' => pure (Step.Skip (sa, sb', some x))
        | .Stop => pure Step.Stop
  state := (ta.state, tb.state, none)

/-- Zip corresponding elements of two streams with a pure function. -/
@[inline] def zipWith [Monad m] (f : a → b → c) (ta : Stream m a) (tb : Stream m b) :
    Stream m c :=
  zipWithM (fun x y => pure (f x y)) ta tb

-- ── Cross product ────────────────────────────────────────────────────────────

/-- Apply a stream of functions to a stream of values and flatten the results.
    (The second stream is re-evaluated for each function.) -/
@[inline] def crossApply [Functor m] (fs : Stream m (a → b)) (xs : Stream m a) :
    Stream m b where
  s := fs.s ⊕ ((a → b) × fs.s × xs.s)
  step gst := fun
    | .inl st => (fun res => match res with
        | .Yield f s => Step.Skip (Sum.inr (f, s, xs.state))
        | .Skip s => Step.Skip (Sum.inl s)
        | .Stop => Step.Stop) <$> fs.step gst st
    | .inr (f, os, st) => (fun res => match res with
        | .Yield a s => Step.Yield (f a) (Sum.inr (f, os, s))
        | .Skip s => Step.Skip (Sum.inr (f, os, s))
        | .Stop => Step.Skip (Sum.inl os)) <$> xs.step gst st
  state := Sum.inl fs.state

/-- Cross product keeping the second stream's values (`*>`). -/
@[inline] def crossApplySnd [Functor m] (t1 : Stream m a) (t2 : Stream m b) :
    Stream m b where
  s := t1.s ⊕ (t1.s × t2.s)
  step gst := fun
    | .inl st => (fun res => match res with
        | .Yield _ s => Step.Skip (Sum.inr (s, t2.state))
        | .Skip s => Step.Skip (Sum.inl s)
        | .Stop => Step.Stop) <$> t1.step gst st
    | .inr (os, st) => (fun res => match res with
        | .Yield b s => Step.Yield b (Sum.inr (os, s))
        | .Skip s => Step.Skip (Sum.inr (os, s))
        | .Stop => Step.Skip (Sum.inl os)) <$> t2.step gst st
  state := Sum.inl t1.state

/-- Cross product keeping the first stream's values (`<*`). -/
@[inline] def crossApplyFst [Functor m] (t1 : Stream m a) (t2 : Stream m b) :
    Stream m a where
  s := t1.s ⊕ (t1.s × t2.s × a)
  step gst := fun
    | .inl st => (fun res => match res with
        | .Yield b s => Step.Skip (Sum.inr (s, t2.state, b))
        | .Skip s => Step.Skip (Sum.inl s)
        | .Stop => Step.Stop) <$> t1.step gst st
    | .inr (os, st, b) => (fun res => match res with
        | .Yield _ s => Step.Yield b (Sum.inr (os, s, b))
        | .Skip s => Step.Skip (Sum.inr (os, s, b))
        | .Stop => Step.Skip (Sum.inl os)) <$> t2.step gst st
  state := Sum.inl t1.state

/-- Combine two streams with `f` over their cross product. -/
@[inline] def crossWith [Functor m] (f : a → b → c) (m1 : Stream m a) (m2 : Stream m b) :
    Stream m c :=
  crossApply (map f m1) m2

/-- The cross product of two streams as pairs. -/
@[inline] def cross [Functor m] (m1 : Stream m a) (m2 : Stream m b) : Stream m (a × b) :=
  crossWith Prod.mk m1 m2

-- ── Unfold-many (fused `concatMap`) ──────────────────────────────────────────

/-- `unfoldEach u t` maps each element of `t` through the `Unfold u` and
    flattens the generated streams — the fusible analogue of `concatMap`. -/
@[inline] def unfoldEach [Monad m] (u : Unfold m a b) (t : Stream m a) : Stream m b where
  s := t.s ⊕ (t.s × u.s)
  step gst := fun
    | .inl o => do
        match ← t.step gst o with
        | .Yield a o' => do let i ← u.inject a; pure (Step.Skip (Sum.inr (o', i)))
        | .Skip o' => pure (Step.Skip (Sum.inl o'))
        | .Stop => pure Step.Stop
    | .inr (o, i) => (fun res => match res with
        | .Yield x i' => Step.Yield x (Sum.inr (o, i'))
        | .Skip i' => Step.Skip (Sum.inr (o, i'))
        | .Stop => Step.Skip (Sum.inl o)) <$> u.step i
  state := Sum.inl t.state

/-- Generate a cross product of two streams and then unfold each tuple. -/
@[inline] def unfoldCross [Monad m] (u : Unfold m (a × b) c) (m1 : Stream m a)
    (m2 : Stream m b) : Stream m c :=
  unfoldEach u (crossWith Prod.mk m1 m2)

-- ── Conversions from/to `StreamK` ────────────────────────────────────────────

/-- Convert a CPS-encoded `StreamK` (#12) into a fused direct-style `Stream`.
    Its stepper carries the residual `StreamK` as state. `unsafe` because it
    drives the `unsafe` `StreamK.foldStreamShared`. -/
@[inline] unsafe def fromStreamK [Monad m] (k : StreamK m a) : Stream m a where
  s := StreamK m a
  step gst m1 :=
    StreamK.foldStreamShared gst
      (fun x r => pure (Step.Yield x r))
      (fun x => pure (Step.Yield x StreamK.nil))
      (pure Step.Stop)
      m1
  state := k

/-- Convert a fused `Stream` into a CPS-encoded `StreamK` (#12). `unsafe`
    because it builds the `unsafe` `StreamK` and drives the `Skip` loop. -/
@[specialize] unsafe def toStreamK [Monad m] (t : Stream m a) : StreamK m a :=
  go t.state
where
  go (st : t.s) : StreamK m a :=
    StreamK.concatEffect do
      match ← t.step defState st with
      | .Yield x s => pure (StreamK.yield x (go s))
      | .Skip s => pure (go s)
      | .Stop => pure StreamK.stop

-- ── Elimination: strict left folds ───────────────────────────────────────────

/-- Strict left fold with a monadic step and a monadic seed. `unsafe` (driver). -/
@[specialize] unsafe def foldlM' [Monad m] (fstep : b → a → m b) (mbegin : m b)
    (t : Stream m a) : m b := do
  go (← mbegin) t.state
where
  go (acc : b) (st : t.s) : m b := do
    match ← t.step defState st with
    | .Yield x s => do let acc' ← fstep acc x; go acc' s
    | .Skip s => go acc s
    | .Stop => pure acc

/-- Strict left fold with a pure step. `unsafe` (driver). -/
@[inline] unsafe def foldl' [Monad m] (fstep : b → a → b) (begin : b) (t : Stream m a) :
    m b :=
  foldlM' (fun acc x => pure (fstep acc x)) (pure begin) t

/-- Strict left fold with a monadic step, seed, and extraction. `unsafe`. -/
@[specialize] unsafe def foldlMx' [Monad m] (fstep : x → a → m x) (begin : m x)
    (done : x → m b) (t : Stream m a) : m b := do
  go (← begin) t.state
where
  go (acc : x) (st : t.s) : m b := do
    match ← t.step defState st with
    | .Yield y s => do let acc' ← fstep acc y; go acc' s
    | .Skip s => go acc s
    | .Stop => done acc

/-- Strict left fold with a pure step, seed, and extraction. `unsafe`. -/
@[inline] unsafe def foldlx' [Monad m] (fstep : x → a → x) (begin : x) (done : x → b)
    (t : Stream m a) : m b :=
  foldlMx' (fun acc y => pure (fstep acc y)) (pure begin) (fun s => pure (done s)) t

-- ── Elimination: lazy right folds ────────────────────────────────────────────

/-- Right fold with a monadic step (evaluated eagerly here). `unsafe`. -/
@[specialize] unsafe def foldrM [Monad m] (f : a → m b → m b) (z : m b) (t : Stream m a) :
    m b :=
  go t.state
where
  go (st : t.s) : m b := do
    match ← t.step defState st with
    | .Yield x s => f x (go s)
    | .Skip s => go s
    | .Stop => z

/-- Right fold with a pure step. `unsafe`. -/
@[inline] unsafe def foldr [Monad m] (f : a → b → b) (z : b) (t : Stream m a) : m b :=
  foldrM (fun x mb => f x <$> mb) (pure z) t

-- ── Elimination: specific folds ──────────────────────────────────────────────

/-- Run a stream, discarding the results. `unsafe` (driver). -/
@[specialize] unsafe def drain [Monad m] (t : Stream m a) : m PUnit :=
  go t.state
where
  go (st : t.s) : m PUnit := do
    match ← t.step defState st with
    | .Yield _ s => go s
    | .Skip s => go s
    | .Stop => pure ⟨⟩

/-- The head element, if any. `unsafe`. -/
@[inline] unsafe def head [Monad m] (t : Stream m a) : m (Option a) :=
  foldrM (fun x _ => pure (some x)) (pure none) t

/-- Collect the stream into a list. `unsafe`. -/
@[inline] unsafe def toList [Monad m] (t : Stream m a) : m (List a) :=
  foldr (· :: ·) [] t

-- ── Running a `Fold` ─────────────────────────────────────────────────────────

/-- Fold the stream with a terminating left `Fold`. A standalone driver
    returning `m b` (unlike upstream's `foldBreak`-based definition, whose
    residual-stream signature is not expressible here — see the module header).
    `unsafe` (driver loop). -/
@[specialize] unsafe def fold [Monad m] (fld : Fold m a b) (t : Stream m a) : m b := do
  match ← fld.initial with
  | .Done b => pure b
  | .Partial fs => go fs t.state
where
  go (fs : fld.s) (st : t.s) : m b := do
    match ← t.step defState st with
    | .Yield x s => do
        match ← fld.step fs x with
        | .Done b => pure b
        | .Partial fs1 => go fs1 s
    | .Skip s => go fs s
    | .Stop => fld.final fs

-- ── Multi-stream folds ───────────────────────────────────────────────────────

/-- Compare two streams for element-wise equality. `unsafe` (driver). -/
@[specialize] unsafe def eqBy [Monad m] (eq : a → b → Bool) (ta : Stream m a)
    (tb : Stream m b) : m Bool :=
  loop0 ta.state tb.state
where
  loop0 (s1 : ta.s) (s2 : tb.s) : m Bool := do
    match ← ta.step defState s1 with
    | .Yield x s1' => loop1 x s1' s2
    | .Skip s1' => loop0 s1' s2
    | .Stop => eqNull s2
  loop1 (x : a) (s1 : ta.s) (s2 : tb.s) : m Bool := do
    match ← tb.step defState s2 with
    | .Yield y s2' => if eq x y then loop0 s1 s2' else pure false
    | .Skip s2' => loop1 x s1 s2'
    | .Stop => pure false
  eqNull (s2 : tb.s) : m Bool := do
    match ← tb.step defState s2 with
    | .Yield _ _ => pure false
    | .Skip s2' => eqNull s2'
    | .Stop => pure true

-- ── ConcatMap (via the `StreamK` bridge) ─────────────────────────────────────

/-- Map a stream-producing function over each element and flatten. Routed
    through the `StreamK` bridge (see module header): the fused `Left/Right`
    encoding would store a `Stream` in the state, which the existential-state
    structure cannot. `unsafe`. -/
@[inline] unsafe def concatMap [Monad m] (f : a → Stream m b) (t : Stream m a) :
    Stream m b :=
  fromStreamK (StreamK.concatMap (fun x => toStreamK (f x)) (toStreamK t))

/-- Argument-flipped `concatMap` — a concatenating `for` loop. `unsafe`. -/
@[inline] unsafe def concatFor [Monad m] (t : Stream m a) (f : a → Stream m b) :
    Stream m b :=
  concatMap f t

end Stream

-- ── The `Nested` cross-product monad ─────────────────────────────────────────

/-- A `newtype`-style wrapper on `Stream` carrying the cross-product
    `Applicative`/`Monad` (upstream's `Nested`, formerly `CrossStream`). The
    plain `Stream` has only a `Functor`; the applicative/monad instances live
    here because upstream leaves the `Stream` ones commented out. -/
structure Nested (m : Type → Type) (a : Type) where
  /-- The wrapped stream. -/
  unNested : Stream m a

namespace Nested

/-- `Functor`: map over the wrapped stream. -/
instance [Functor m] : Functor (Nested m) where
  map f n := ⟨Stream.map f n.unNested⟩

/-- `pure x` is the singleton stream. -/
instance [Applicative m] : Pure (Nested m) where
  pure x := ⟨Stream.fromPure x⟩

/-- `Seq`/`SeqLeft`/`SeqRight` are the cross products. -/
instance [Functor m] : Seq (Nested m) where
  seq fs xs := ⟨Stream.crossApply fs.unNested (xs ()).unNested⟩

instance [Functor m] : SeqLeft (Nested m) where
  seqLeft s1 s2 := ⟨Stream.crossApplyFst s1.unNested (s2 ()).unNested⟩

instance [Functor m] : SeqRight (Nested m) where
  seqRight s1 s2 := ⟨Stream.crossApplySnd s1.unNested (s2 ()).unNested⟩

/-- `Applicative` built from `pure` and the cross product. -/
instance [Applicative m] : Applicative (Nested m) where

/-- `Bind`/`Monad`: `s >>= f = concatMap (unNested ∘ f) s`. `unsafe` (built on
    the `unsafe` `concatMap`). -/
unsafe instance [Monad m] : Bind (Nested m) where
  bind n f := ⟨Stream.concatMap (fun x => (f x).unNested) n.unNested⟩

unsafe instance [Monad m] : Monad (Nested m) where

end Nested
end Data.Stream
