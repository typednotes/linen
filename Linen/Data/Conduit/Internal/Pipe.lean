/-
  Linen.Data.Conduit.Internal.Pipe — the core streaming `Pipe` type

  The fundamental streaming type of Haskell's `conduit`
  (`Data.Conduit.Internal.Pipe`), ported **without `unsafe`**.

  ## Two sound, semantics-preserving divergences from the Haskell type

  Haskell's `Pipe` has a constructor `PipeM (m (Pipe …))`, which is **not
  strictly positive** (the recursive occurrence sits under an arbitrary functor
  `m`). A faithful transcription therefore needs `unsafe inductive`. To stay
  total and kernel-checked we instead use:

  1. **A Freer/operational `pipeM`:** `pipeM : {β} → m β → (β → Pipe …) → Pipe …`.
     The effect `m β` no longer mentions `Pipe`, and the recursive occurrence is
     under the continuation `β → Pipe …` — a strictly-positive position. This is
     isomorphic to the free monad over `m` when `m` is a functor, and (as a
     bonus) means `mapResult`/`bind` need no `Functor m`/`Monad m` constraint.
     Quantifying over `β : Type` lifts `Pipe` to `Type 1`.

  2. **A strict spine:** `haveOutput`/`leftover` hold the continuation `Pipe …`
     directly rather than `Thunk (Pipe …)`. Haskell uses `Thunk` only to model
     its lazy spine; Lean is strict, so this changes nothing semantically — and
     it lets `bind` be plain *structural* recursion (recursion through `Thunk`
     defeats the termination checker here).

  ## Type parameters

  - `l` — leftover values (pushed back upstream)
  - `i` — input stream values
  - `o` — output stream values
  - `u` — upstream result
  - `m` — effect "functor" (any `Type → Type`)
  - `r` — final result
-/

namespace Data.Conduit.Internal

/-- The core streaming pipe: a free-monad-like structure with five states.

    $$\text{Pipe}\ l\ i\ o\ u\ m\ r ::=$$
    $$\quad \mid\ \text{haveOutput}(\text{Pipe}, o)
            \mid \text{needInput}(i \to \text{Pipe}, u \to \text{Pipe})
            \mid \text{done}(r)$$
    $$\quad \mid\ \text{pipeM}(m\ \beta, \beta \to \text{Pipe})
            \mid \text{leftover}(\text{Pipe}, l)$$

    Strictly positive (so no `unsafe`), at the cost of living in `Type 1`. -/
inductive Pipe (l i o u : Type) (m : Type → Type) (r : Type) : Type 1 where
  /-- Yield an output value `o` downstream, with the continuation pipe. -/
  | haveOutput : Pipe l i o u m r → o → Pipe l i o u m r
  /-- Await input: `onInput : i → Pipe` on a value, `onUpstreamDone : u → Pipe`
      when upstream is exhausted. -/
  | needInput : (i → Pipe l i o u m r) → (u → Pipe l i o u m r) → Pipe l i o u m r
  /-- Terminal state carrying the final result. -/
  | done : r → Pipe l i o u m r
  /-- Run an effect `m β`, then continue with its result (operational/Freer form). -/
  | pipeM : {β : Type} → m β → (β → Pipe l i o u m r) → Pipe l i o u m r
  /-- Push an unconsumed input value back for re-processing. -/
  | leftover : Pipe l i o u m r → l → Pipe l i o u m r

namespace Pipe

/-- Map a function over the result of a pipe (the `Functor` action): apply `f`
    at every `done` leaf. Structural recursion on the pipe. -/
def mapResult (f : α → β) : Pipe l i o u m α → Pipe l i o u m β
  | .done r => .done (f r)
  | .haveOutput next out => .haveOutput (next.mapResult f) out
  | .needInput onIn onUp => .needInput (fun i => (onIn i).mapResult f) (fun u => (onUp u).mapResult f)
  | .pipeM act k => .pipeM act (fun b => (k b).mapResult f)
  | .leftover next l => .leftover (next.mapResult f) l

/-- Monadic bind: replace every `done r` leaf with `f r` (the free-monad bind).
    Structural recursion on the pipe. -/
def bind (f : α → Pipe l i o u m β) : Pipe l i o u m α → Pipe l i o u m β
  | .done r => f r
  | .haveOutput next out => .haveOutput (next.bind f) out
  | .needInput onIn onUp => .needInput (fun i => (onIn i).bind f) (fun u => (onUp u).bind f)
  | .pipeM act k => .pipeM act (fun b => (k b).bind f)
  | .leftover next l => .leftover (next.bind f) l

/-- `Functor`: `map = mapResult`. -/
instance : Functor (Pipe l i o u m) where
  map := mapResult

/-- `pure = done`. -/
instance : Pure (Pipe l i o u m) where
  pure := .done

/-- `bind` is the free-monad substitution. -/
instance : Bind (Pipe l i o u m) where
  bind p f := p.bind f

/-- `Pipe l i o u m` is a `Monad` for **any** `m` (the free monad over `m`). -/
instance : Monad (Pipe l i o u m) where

/-! ── Computation rules ── -/

/-- Left identity (`pure a >>= f = f a`), definitionally. -/
theorem pure_bind (a : α) (f : α → Pipe l i o u m β) :
    (pure a : Pipe l i o u m α) >>= f = f a := rfl

/-- `mapResult` at a `done` leaf. -/
theorem mapResult_done (f : α → β) (a : α) :
    mapResult (l := l) (i := i) (o := o) (u := u) (m := m) f (.done a) = .done (f a) := rfl

/-- `bind` at a `done` leaf. -/
theorem bind_done (f : α → Pipe l i o u m β) (a : α) :
    bind f (.done a) = f a := rfl

end Pipe
end Data.Conduit.Internal
