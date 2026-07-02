/-
  `Data.Conduit.Combinators` — stream combinators

  Port of Haskell's `Data.Conduit.Combinators` and `Data.Conduit.List`.
  Provides sources, sinks, and transformers for conduit pipelines.

  `unsafe`, for the same reason as `Data.Conduit` (see its module docstring):
  every combinator here loops on `await` until a runtime source is
  exhausted, which is not a structurally-decreasing recursion.

  ## Usage
  ```lean
  open Data.Conduit
  let result := runConduitPure (sourceList [1,2,3,4,5] .| mapC (· * 2) .| sinkList)
  -- result = [2, 4, 6, 8, 10]
  ```
-/

import Linen.Data.Conduit.Internal.Conduit

namespace Data.Conduit.Combinators

open Data.Conduit
open Data.Conduit.Internal

-- ══════════════════════════════════════════════════════════════
-- Sources (producers)
-- ══════════════════════════════════════════════════════════════

/-- Yield each element of a list.
    $$\text{sourceList} : \text{List}\ \alpha \to \text{ConduitT}\ i\ \alpha\ m\ ()$$ -/
unsafe def sourceList [Monad m] (xs : List α) : ConduitT i α m Unit :=
  match xs with
  | [] => pure ()
  | x :: rest => do yield x; sourceList rest

/-- Yield each element of an array. -/
unsafe def sourceArray [Monad m] (xs : Array α) : ConduitT i α m Unit := do
  for x in xs do
    yield x

/-- Generate values from a seed function until it returns `none`.
    $$\text{unfoldC} : (s \to \text{Option}(\alpha \times s)) \to s \to \text{ConduitT}\ i\ \alpha\ m\ ()$$ -/
unsafe def unfoldC [Monad m] (f : s → Option (α × s)) (seed : s) : ConduitT i α m Unit :=
  match f seed with
  | none => pure ()
  | some (a, s') => do yield a; unfoldC f s'

/-- Repeat a value forever.
    $$\text{repeatC} : \alpha \to \text{ConduitT}\ i\ \alpha\ m\ ()$$ -/
unsafe def repeatC [Monad m] (a : α) : ConduitT i α m Unit := do
  yield a; repeatC a

/-- Repeat a value `n` times.
    $$\text{replicateC} : \mathbb{N} \to \alpha \to \text{ConduitT}\ i\ \alpha\ m\ ()$$ -/
unsafe def replicateC [Monad m] (n : Nat) (a : α) : ConduitT i α m Unit := do
  for _ in [:n] do
    yield a

/-- Yield integers from `from` to `to` (inclusive).
    $$\text{enumFromToC} : \mathbb{N} \to \mathbb{N} \to \text{ConduitT}\ i\ \mathbb{N}\ m\ ()$$ -/
unsafe def enumFromToC [Monad m] (lo hi : Nat) : ConduitT i Nat m Unit := do
  for idx in [lo:hi+1] do
    yield idx

/-- Repeatedly run a monadic action and yield results forever. -/
unsafe def repeatMC [Monad m] (action : m α) : ConduitT i α m Unit := do
  let a ← liftConduit action
  yield a
  repeatMC action

-- ══════════════════════════════════════════════════════════════
-- Sinks (consumers)
-- ══════════════════════════════════════════════════════════════

/-- Collect all values into a list.
    $$\text{sinkList} : \text{ConduitT}\ \alpha\ o\ m\ (\text{List}\ \alpha)$$ -/
unsafe def sinkList [Monad m] : ConduitT α o m (List α) := do
  match ← await with
  | none => pure []
  | some a => do
    let rest ← sinkList
    pure (a :: rest)

/-- Collect all values into an array. -/
unsafe def sinkArray [Monad m] : ConduitT α o m (Array α) := do
  let rec go (result : Array α) : ConduitT α o m (Array α) := do
    match ← await with
    | none => pure result
    | some a => go (result.push a)
  go #[]

/-- Discard all input. -/
unsafe def sinkNull [Monad m] : ConduitT α o m Unit := do
  match ← await with
  | none => pure ()
  | some _ => sinkNull

/-- Strict left fold over the stream.
    $$\text{foldlC} : (\text{acc} \to \alpha \to \text{acc}) \to \text{acc} \to \text{ConduitT}\ \alpha\ o\ m\ \text{acc}$$ -/
unsafe def foldlC [Monad m] (f : acc → α → acc) (init : acc) : ConduitT α o m acc := do
  match ← await with
  | none => pure init
  | some a => foldlC f (f init a)

/-- Monadic left fold.
    $$\text{foldMC} : (\text{acc} \to \alpha \to m\ \text{acc}) \to \text{acc} \to \text{ConduitT}\ \alpha\ o\ m\ \text{acc}$$ -/
unsafe def foldMC [Monad m] (f : acc → α → m acc) (init : acc) : ConduitT α o m acc := do
  match ← await with
  | none => pure init
  | some a => do
    let acc' ← liftConduit (f init a)
    foldMC f acc'

/-- Map to a monoid and fold.
    $$\text{foldMapC} : (\alpha \to w) \to \text{ConduitT}\ \alpha\ o\ m\ w$$ -/
unsafe def foldMapC [Monad m] [Append w] [EmptyCollection w] (f : α → w) : ConduitT α o m w :=
  foldlC (fun acc a => acc ++ f a) {}

/-- Get the first element. -/
unsafe def headC [Monad m] : ConduitT α o m (Option α) := await

/-- Get the last element. -/
unsafe def lastC [Monad m] : ConduitT α o m (Option α) := do
  match ← await with
  | none => pure none
  | some a => go a
where
  go (prev : α) : ConduitT α o m (Option α) := do
    match ← await with
    | none => pure (some prev)
    | some a => go a

/-- Count the number of elements.
    $$\text{lengthC} : \text{ConduitT}\ \alpha\ o\ m\ \mathbb{N}$$ -/
unsafe def lengthC [Monad m] : ConduitT α o m Nat :=
  foldlC (fun n _ => n + 1) 0

/-- Sum of elements.
    $$\text{sumC} : \text{ConduitT}\ \alpha\ o\ m\ \alpha$$ -/
unsafe def sumC [Monad m] [Add α] [OfNat α 0] : ConduitT α o m α :=
  foldlC (· + ·) 0

/-- Product of elements.
    $$\text{productC} : \text{ConduitT}\ \alpha\ o\ m\ \alpha$$ -/
unsafe def productC [Monad m] [Mul α] [OfNat α 1] : ConduitT α o m α :=
  foldlC (· * ·) 1

/-- Check if the stream is empty (no elements). -/
unsafe def nullC [Monad m] : ConduitT α o m Bool := do
  match ← await with
  | none => pure true
  | some a => do leftoverC a; pure false

/-- Check if all elements satisfy a predicate. -/
unsafe def allC [Monad m] (p : α → Bool) : ConduitT α o m Bool := do
  match ← await with
  | none => pure true
  | some a => if p a then allC p else pure false

/-- Check if any element satisfies a predicate. -/
unsafe def anyC [Monad m] (p : α → Bool) : ConduitT α o m Bool := do
  match ← await with
  | none => pure false
  | some a => if p a then pure true else anyC p

/-- Check if an element is in the stream. -/
unsafe def elemC [Monad m] [BEq α] (x : α) : ConduitT α o m Bool :=
  anyC (· == x)

/-- Find the first element satisfying a predicate. -/
unsafe def findC [Monad m] (p : α → Bool) : ConduitT α o m (Option α) := do
  match ← await with
  | none => pure none
  | some a => if p a then pure (some a) else findC p

/-- Get the maximum element. -/
unsafe def maximumC [Monad m] [Ord α] : ConduitT α o m (Option α) := do
  match ← await with
  | none => pure none
  | some first => do
    let result ← foldlC (fun best a =>
      match Ord.compare a best with
      | .gt => a
      | _ => best) first
    pure (some result)

/-- Get the minimum element. -/
unsafe def minimumC [Monad m] [Ord α] : ConduitT α o m (Option α) := do
  match ← await with
  | none => pure none
  | some first => do
    let result ← foldlC (fun best a =>
      match Ord.compare a best with
      | .lt => a
      | _ => best) first
    pure (some result)

-- ══════════════════════════════════════════════════════════════
-- Transformers (conduits)
-- ══════════════════════════════════════════════════════════════

/-- Apply a function to each element.
    $$\text{mapC} : (\alpha \to \beta) \to \text{ConduitT}\ \alpha\ \beta\ m\ ()$$ -/
unsafe def mapC [Monad m] (f : α → β) : ConduitT α β m Unit :=
  awaitForever fun a => yield (f a)

/-- Apply a monadic function to each element.
    $$\text{mapMC} : (\alpha \to m\ \beta) \to \text{ConduitT}\ \alpha\ \beta\ m\ ()$$ -/
unsafe def mapMC [Monad m] (f : α → m β) : ConduitT α β m Unit :=
  awaitForever fun a => do
    let b ← liftConduit (f a)
    yield b

/-- Keep only elements satisfying a predicate.
    $$\text{filterC} : (\alpha \to \text{Bool}) \to \text{ConduitT}\ \alpha\ \alpha\ m\ ()$$ -/
unsafe def filterC [Monad m] (p : α → Bool) : ConduitT α α m Unit :=
  awaitForever fun a => if p a then yield a else pure ()

/-- Keep only elements for which the monadic predicate returns true. -/
unsafe def filterMC [Monad m] (p : α → m Bool) : ConduitT α α m Unit :=
  awaitForever fun a => do
    let keep ← liftConduit (p a)
    if keep then yield a

/-- Take at most `n` elements.
    $$\text{takeC} : \mathbb{N} \to \text{ConduitT}\ \alpha\ \alpha\ m\ ()$$ -/
unsafe def takeC [Monad m] (n : Nat) : ConduitT α α m Unit := do
  for _ in [:n] do
    match ← await with
    | none => return
    | some a => yield a

/-- Drop the first `n` elements, pass the rest through.
    $$\text{dropC} : \mathbb{N} \to \text{ConduitT}\ \alpha\ \alpha\ m\ ()$$ -/
unsafe def dropC [Monad m] (n : Nat) : ConduitT α α m Unit := do
  for _ in [:n] do
    match ← await with
    | none => return
    | some _ => pure ()
  -- Pass remaining through
  awaitForever fun a => yield a

/-- Take elements while predicate holds. -/
unsafe def takeWhileC [Monad m] (p : α → Bool) : ConduitT α α m Unit := do
  match ← await with
  | none => pure ()
  | some a =>
    if p a then do yield a; takeWhileC p
    else leftoverC a

/-- Drop elements while predicate holds, then pass the rest. -/
unsafe def dropWhileC [Monad m] (p : α → Bool) : ConduitT α α m Unit := do
  match ← await with
  | none => pure ()
  | some a =>
    if p a then dropWhileC p
    else do leftoverC a; awaitForever fun a => yield a

/-- Map each element to a list and yield each result.
    $$\text{concatMapC} : (\alpha \to \text{List}\ \beta) \to \text{ConduitT}\ \alpha\ \beta\ m\ ()$$ -/
unsafe def concatMapC [Monad m] (f : α → List β) : ConduitT α β m Unit :=
  awaitForever fun a => do
    for b in f a do yield b

/-- Map each element, keeping only `some` results. -/
unsafe def mapMaybeC [Monad m] (f : α → Option β) : ConduitT α β m Unit :=
  awaitForever fun a =>
    match f a with
    | some b => yield b
    | none => pure ()

/-- Insert a separator between each pair of elements. -/
unsafe def intersperseC [Monad m] (sep : α) : ConduitT α α m Unit := do
  match ← await with
  | none => pure ()
  | some first => do
    yield first
    awaitForever fun a => do yield sep; yield a

/-- Emit running accumulation.
    $$\text{scanlC} : (s \to \alpha \to s) \to s \to \text{ConduitT}\ \alpha\ s\ m\ ()$$ -/
unsafe def scanlC [Monad m] (f : s → α → s) (init : s) : ConduitT α s m Unit := do
  yield init
  go init
where
  go (acc : s) : ConduitT α s m Unit := do
    match ← await with
    | none => pure ()
    | some a =>
      let acc' := f acc a
      yield acc'
      go acc'

/-- Run a monadic side-effect on each element, passing it through unchanged. -/
unsafe def iterMC [Monad m] (f : α → m Unit) : ConduitT α α m Unit :=
  awaitForever fun a => do
    liftConduit (f a)
    yield a

/-- Consume all input with a monadic side-effect, producing no output. -/
unsafe def mapM_C [Monad m] (f : α → m Unit) : ConduitT α o m Unit :=
  awaitForever fun a => liftConduit (f a)

/-- Flatten a stream of lists. -/
unsafe def concatC [Monad m] : ConduitT (List α) α m Unit :=
  awaitForever fun xs => do
    for x in xs do yield x

/-- Chunk a stream into lists of at most `n` elements. -/
unsafe def chunksOfC [Monad m] (n : Nat) : ConduitT α (List α) m Unit := do
  let rec go (chunk : List α) (count : Nat) : ConduitT α (List α) m Unit := do
    match ← await with
    | none =>
      if !chunk.isEmpty then yield chunk.reverse
    | some a =>
      let chunk' := a :: chunk
      let count' := count + 1
      if count' >= n then do
        yield chunk'.reverse
        go [] 0
      else
        go chunk' count'
  go [] 0

end Data.Conduit.Combinators
