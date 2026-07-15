/-
  Linen.Data.StreamK.Type — the `StreamK` stream (streamly's non-fused stream)

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.StreamK.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/StreamK/Type.hs),
  module #12 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  `StreamK` is streamly's recursively-composable stream: a possibly-effectful,
  possibly-empty sequence supporting `O(1)` `cons`/`append` and scalable
  recursive/monadic composition (unlike the fused direct `Stream`).

  ## Representation: direct-style `unsafe inductive` (not the CPS newtype)

  Upstream's `StreamK` is a **CPS newtype**
  `newtype StreamK m a = MkStream (∀ r. State → (a → StreamK m a → m r) → (a → m r) → m r → m r)`.
  Two facts make that exact type unusable in Lean:

  1. The recursive occurrence `StreamK m a` sits to the *left* of an arrow
     (in the yield continuation), so the type is **not strictly positive** and
     the kernel rejects it as a safe `inductive`.
  2. The `∀ r` rank-2 quantifier forces the type into `Type 1`, but core
     operations such as `uncons : StreamK m a → m (Option (a × StreamK m a))`
     apply `m : Type → Type` to a value mentioning `StreamK`, which is
     impossible once `StreamK : Type 1`.

  We therefore use the **direct-style representation streamly's own source
  documents as equivalent** (`data StreamK m a = Stop | Singleton a | Yield a
  (StreamK m a)`, plus a monadic-`effect` case for `m`), as an
  `unsafe inductive` living in `Type 0`. `unsafe` is required because the
  `effect` case `m (StreamK m a)` is non-strictly-positive over an arbitrary
  `m`, and because streamly's recursive stream combinators are genuinely
  productive/corecursive (no structural or well-founded measure). This is the
  **same sanctioned pattern the `Data.Conduit` port uses** for its CPS
  `ConduitT` (`unsafe`, explicitly "rather than reaching for a `partial def`");
  `unsafe` is neither `partial def` nor `sorry`, so it satisfies AGENTS.md.

  No stream *behavior* is dropped: `stop`/`single`/`yield`/`effect` are exactly
  the four continuations of the CPS encoding. `foldStream`/`foldStreamShared`
  are the eliminator with those four continuations.

  ## Substitutions / deviations

  - **CPS builder API dropped** (`mkStream`, `consK`, `fromYieldK`,
    `fromStopK`, `build`/`buildS`/`augmentS`, `foldrS*` and the `Fuse`/rewrite
    machinery) — these traffic in the CPS closures that the direct-style type
    replaces; the equivalent construction is via the direct constructors
    (`nil`/`cons`/`fromPure`/`fromEffect`/`consM`).
  - **`State` threading is a no-op.** `foldStream` vs `foldStreamShared` differ
    only in `adaptState`, which is identity on the pure `State` record we
    ported (#6, concurrency dropped); `unShare` is likewise identity here.
  - **Structural combinators take `[Functor m]`** (to recurse under `effect`);
    upstream needs none because the CPS stores the continuation. Eliminators
    take `[Monad m]` to sequence `effect`.
  - **`repeat`/`cycle`/other unbounded generators dropped.** They rely on
    Haskell laziness to build a cyclic value; Lean is eager, so
    `repeat x = cons x (repeat x)` diverges at construction. The finite
    generators (`fromList`, `unfoldr`/`unfoldrM` with a terminating step) are
    ported.
  - **Only the core type, constructors, eliminators, mapping, appending,
    interleaving, the cross/applicative product, and `concatMap`/`bind` (plus
    the `Functor`/`Applicative`/`Monad`/`Append`/`Alternative` instances) are
    ported.** The large secondary layer of the 3000-line upstream module (bfs/
    fair concat variants, `mergeMapWith`, `concatIterate*`, `mfix`, the
    `Nested`/`FairNested`/`CrossStreamK` wrappers, and deprecated combinators)
    belongs to later stream-operation tiers, matching the plan's own scoping.
-/

import Linen.Data.Stream.SVarType

namespace Data.StreamK

open Data.Stream (State adaptState defState)

-- ── The StreamK type ────────────────────────────────────────────────────────

/-- The direct-style rendering of streamly's CPS `StreamK` (see module header):
    `stop` ends the stream, `single a` is a one-element stream, `yield a r`
    prepends `a` to the tail `r`, and `effect e` runs an effect producing the
    rest of the stream. -/
unsafe inductive StreamK (m : Type → Type) (a : Type) where
  /-- The empty stream. -/
  | stop
  /-- A singleton stream (streamly keeps this separate from `yield … stop` to
      optimize single-element composition). -/
  | single : a → StreamK m a
  /-- Prepend a pure head to a tail stream. -/
  | yield : a → StreamK m a → StreamK m a
  /-- Run an effect that yields the rest of the stream. -/
  | effect : m (StreamK m a) → StreamK m a

namespace StreamK

-- ── Construction ────────────────────────────────────────────────────────────

/-- The empty stream. -/
@[inline] unsafe def nil : StreamK m a := .stop

/-- Prepend a pure value at the head (`O(1)`, right-associative). -/
@[inline] unsafe def cons (a : α) (r : StreamK m α) : StreamK m α := .yield a r

@[inherit_doc] scoped infixr:60 " .: " => cons

/-- A singleton stream from a pure value (`pure`/`return`). -/
@[inline] unsafe def fromPure (a : α) : StreamK m α := .single a

/-- A singleton stream from a monadic action. -/
@[inline] unsafe def fromEffect [Functor m] (eff : m α) : StreamK m α :=
  .effect (Functor.map (fun x => StreamK.single x) eff)

/-- Prepend an effectful value at the head. -/
@[inline] unsafe def consM [Functor m] (eff : m α) (r : StreamK m α) : StreamK m α :=
  .effect (Functor.map (fun x => StreamK.yield x r) eff)

/-- Turn an effect producing a stream into a stream (`concatEffect`). -/
@[inline] unsafe def concatEffect (eff : m (StreamK m a)) : StreamK m a := .effect eff

-- ── Folding a stream (the eliminator) ────────────────────────────────────────

/-- Fold a stream with a yield / singleton / stop continuation. This is the
    direct-style eliminator standing in for upstream's CPS `foldStreamShared`;
    it needs `[Monad m]` to sequence the `effect` case. -/
@[specialize] unsafe def foldStreamShared [Monad m]
    (st : State) (yield : a → StreamK m a → m r) (single : a → m r) (stop : m r) :
    StreamK m a → m r
  | .stop => stop
  | .single a => single a
  | .yield a r => yield a r
  | .effect eff => eff >>= foldStreamShared st yield single stop

/-- Like `foldStreamShared`; `adaptState` is identity on the pure `State`. -/
@[inline] unsafe def foldStream [Monad m]
    (st : State) (yield : a → StreamK m a → m r) (single : a → m r) (stop : m r)
    (s : StreamK m a) : m r :=
  foldStreamShared (adaptState st) yield single stop s

-- ── Elimination ───────────────────────────────────────────────────────────────

/-- Decompose the stream into its head and tail, running effects as needed. -/
@[specialize] unsafe def uncons [Monad m] : StreamK m a → m (Option (a × StreamK m a))
  | .stop => pure none
  | .single a => pure (some (a, .stop))
  | .yield a r => pure (some (a, r))
  | .effect eff => eff >>= uncons

/-- Strict left fold. -/
@[specialize] unsafe def foldl' [Monad m] (step : b → a → b) (begin : b)
    (s : StreamK m a) : m b :=
  go begin s
where
  go (acc : b) : StreamK m a → m b
    | .stop => pure acc
    | .single a => pure (step acc a)
    | .yield a r => go (step acc a) r
    | .effect eff => eff >>= go acc

/-- Strict left fold with a monadic step. -/
@[specialize] unsafe def foldlM' [Monad m] (step : b → a → m b) (begin : m b)
    (s : StreamK m a) : m b := do
  go (← begin) s
where
  go (acc : b) : StreamK m a → m b
    | .stop => pure acc
    | .single a => step acc a
    | .yield a r => step acc a >>= (go · r)
    | .effect eff => eff >>= go acc

/-- Lazy-style right fold with a monadic step (evaluated eagerly here). -/
@[specialize] unsafe def foldrM [Monad m] (step : a → m b → m b) (acc : m b) :
    StreamK m a → m b
  | .stop => acc
  | .single a => step a acc
  | .yield a r => step a (foldrM step acc r)
  | .effect eff => eff >>= foldrM step acc

/-- Right fold with a pure step. -/
@[inline] unsafe def foldr [Monad m] (step : a → b → b) (acc : b) : StreamK m a → m b :=
  foldrM (fun x xs => xs >>= fun b => pure (step x b)) (pure acc)

/-- Run a stream purely for its effects. -/
@[specialize] unsafe def drain [Monad m] : StreamK m a → m PUnit
  | .stop => pure ⟨⟩
  | .single _ => pure ⟨⟩
  | .yield _ r => drain r
  | .effect eff => eff >>= drain

/-- Is the stream empty? -/
@[specialize] unsafe def null [Monad m] : StreamK m a → m Bool
  | .stop => pure true
  | .single _ => pure false
  | .yield _ _ => pure false
  | .effect eff => eff >>= null

/-- Collect the stream into a list. -/
@[specialize] unsafe def toList [Monad m] : StreamK m a → m (List a)
  | .stop => pure []
  | .single a => pure [a]
  | .yield a r => (a :: ·) <$> toList r
  | .effect eff => eff >>= toList

/-- The head, if any (`uncons` mapped to the first component). -/
@[inline] unsafe def head [Monad m] (s : StreamK m a) : m (Option a) :=
  (·.map (·.1)) <$> uncons s

/-- The tail, if the stream is non-empty. -/
@[inline] unsafe def tail [Monad m] (s : StreamK m a) : m (Option (StreamK m a)) :=
  (·.map (·.2)) <$> uncons s

-- ── From containers ────────────────────────────────────────────────────────

/-- Build a stream from a list (`foldr cons nil`). -/
@[inline] unsafe def fromList (l : List a) : StreamK m a :=
  l.foldr (fun a s => .yield a s) .stop

-- ── Unfolding ──────────────────────────────────────────────────────────────

/-- Build a stream by unfolding a pure step from a seed; ends on `none`. -/
@[specialize] unsafe def unfoldr (next : b → Option (a × b)) (s : b) : StreamK m a :=
  match next s with
  | some (a, b) => .yield a (unfoldr next b)
  | none => .stop

/-- Build a stream by unfolding a monadic step from a seed; ends on `none`. -/
@[specialize] unsafe def unfoldrM [Monad m] (next : b → m (Option (a × b))) (s : b) :
    StreamK m a :=
  .effect do
    match ← next s with
    | some (a, b) => pure (.yield a (unfoldrM next b))
    | none => pure .stop

-- ── Mapping ────────────────────────────────────────────────────────────────

/-- Map a function over the yielded values. -/
@[specialize] unsafe def map [Functor m] (f : a → b) : StreamK m a → StreamK m b
  | .stop => .stop
  | .single a => .single (f a)
  | .yield a r => .yield (f a) (map f r)
  | .effect eff => .effect (Functor.map (map f) eff)

/-- Map an effectful function over the stream (`mapMWith consM`). -/
@[specialize] unsafe def mapM [Monad m] (f : a → m b) : StreamK m a → StreamK m b
  | .stop => .stop
  | .single a => .effect ((fun x => StreamK.single x) <$> f a)
  | .yield a r => .effect ((fun x => StreamK.yield x (mapM f r)) <$> f a)
  | .effect eff => .effect (Functor.map (mapM f) eff)

-- ── Appending ────────────────────────────────────────────────────────────────

/-- Append two streams (depth-first: all of the first, then the second). -/
@[specialize] unsafe def append [Functor m] (s1 s2 : StreamK m a) : StreamK m a :=
  match s1 with
  | .stop => s2
  | .single a => .yield a s2
  | .yield a r => .yield a (append r s2)
  | .effect eff => .effect (Functor.map (append · s2) eff)

-- ── Interleaving ──────────────────────────────────────────────────────────────

/-- Interleave two streams, alternating elements. -/
@[specialize] unsafe def interleave [Functor m] (s1 s2 : StreamK m a) : StreamK m a :=
  match s1 with
  | .stop => s2
  | .single a => .yield a s2
  | .yield a r => .yield a (interleave s2 r)
  | .effect eff => .effect (Functor.map (interleave · s2) eff)

-- ── Reversing ──────────────────────────────────────────────────────────────

/-- Reverse a (finite) stream. -/
@[specialize] unsafe def reverse [Functor m] (s : StreamK m a) : StreamK m a :=
  go .stop s
where
  go (acc : StreamK m a) : StreamK m a → StreamK m a
    | .stop => acc
    | .single a => .yield a acc
    | .yield a r => go (.yield a acc) r
    | .effect eff => .effect (Functor.map (go acc) eff)

-- ── ConcatMap / bind ──────────────────────────────────────────────────────────

/-- `concatMap` with a chosen combine strategy (upstream `bindWith`/
    `concatForWith`): map `f` over each element and combine the results with
    `combine` (`unShare` is identity here). -/
@[specialize] unsafe def bindWith [Functor m]
    (combine : StreamK m b → StreamK m b → StreamK m b) (s : StreamK m a)
    (f : a → StreamK m b) : StreamK m b :=
  match s with
  | .stop => .stop
  | .single a => f a
  | .yield a r => combine (f a) (bindWith combine r f)
  | .effect eff => .effect (Functor.map (bindWith combine · f) eff)

/-- `concatMap` with a chosen combine strategy. -/
@[inline] unsafe def concatMapWith [Functor m]
    (combine : StreamK m b → StreamK m b → StreamK m b) (f : a → StreamK m b)
    (s : StreamK m a) : StreamK m b := bindWith combine s f

/-- Flatten a stream of streams depth-first (combine via `append`). -/
@[inline] unsafe def concatMap [Functor m] (f : a → StreamK m b) (s : StreamK m a) :
    StreamK m b := concatMapWith append f s

-- ── Cross product (applicative) ───────────────────────────────────────────────

/-- Apply each function in a stream to every value of another stream. -/
@[inline] unsafe def crossApply [Functor m] (fs : StreamK m (a → b)) (xs : StreamK m a) :
    StreamK m b := concatMap (fun f => map f xs) fs

/-- Combine two streams with `f` over their cross product. -/
@[inline] unsafe def crossWith [Functor m] (f : a → b → c)
    (s1 : StreamK m a) (s2 : StreamK m b) : StreamK m c :=
  concatMap (fun x => map (f x) s2) s1

/-- The cross product of two streams as pairs. -/
@[inline] unsafe def cross [Functor m] (s1 : StreamK m a) (s2 : StreamK m b) :
    StreamK m (a × b) := crossWith Prod.mk s1 s2

/-- Cross product keeping the second stream's values (`*>`). -/
@[inline] unsafe def crossApplySnd [Functor m] (s1 : StreamK m a) (s2 : StreamK m b) :
    StreamK m b := concatMap (fun _ => s2) s1

/-- Cross product keeping the first stream's values (`<*`). -/
@[inline] unsafe def crossApplyFst [Functor m] (s1 : StreamK m a) (s2 : StreamK m b) :
    StreamK m a := concatMap (fun x => map (fun _ => x) s2) s1

-- ── Instances ────────────────────────────────────────────────────────────────

/-- `Functor`: map over the yielded values. -/
unsafe instance [Functor m] : Functor (StreamK m) where
  map := map

/-- `pure a` is the singleton stream. -/
unsafe instance : Pure (StreamK m) where
  pure := fromPure

/-- `Seq`/`SeqLeft`/`SeqRight` are the cross products. -/
unsafe instance [Functor m] : Seq (StreamK m) where
  seq fs xs := crossApply fs (xs ())

unsafe instance [Functor m] : SeqLeft (StreamK m) where
  seqLeft s1 s2 := crossApplyFst s1 (s2 ())

unsafe instance [Functor m] : SeqRight (StreamK m) where
  seqRight s1 s2 := crossApplySnd s1 (s2 ())

/-- `Applicative` built from `pure` and the cross product. -/
unsafe instance [Functor m] : Applicative (StreamK m) where

/-- `Bind`/`Monad`: `s >>= f = concatMap f s`. -/
unsafe instance [Functor m] : Bind (StreamK m) where
  bind s f := concatMap f s

unsafe instance [Functor m] : Monad (StreamK m) where

/-- `Append`: `s1 ++ s2 = append s1 s2` (streamly's `Semigroup`). -/
unsafe instance [Functor m] : Append (StreamK m a) where
  append := append

/-- `Alternative`: `failure = nil`, `<|> = append` (streamly's `Alternative`). -/
unsafe instance [Functor m] : Alternative (StreamK m) where
  failure := nil
  orElse s1 s2 := append s1 (s2 ())

end StreamK
end Data.StreamK
