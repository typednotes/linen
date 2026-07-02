/-
  `Data.Conduit` (internal) — the `ConduitT` wrapper and its operations

  The `ConduitT` type wraps `Pipe` with a codensity/CPS transform, giving
  O(1) monadic bind (left-associated binds do not build up thunks). This is
  the same technique used in Haskell's `conduit` library.

  ## Contents

  1. `ConduitT i o m r` — the CPS wrapper over `Pipe`
  2. Type aliases: `Source`, `Sink`, `ConduitM`
  3. Primitives: `await`, `yield`, `leftoverC`, `awaitForever`, `liftConduit`
  4. Fusion: `fusePipes`, `pipe`, and the `.|` notation
  5. Execution: `runConduit`, `runConduitPure`, `runConduitRes`
  6. `bracketP` — resource-safe bracket

  ## Haskell equivalent

  `Data.Conduit.Internal.Conduit` from the `conduit` package.

  ## Design

  $$\text{ConduitT}\ i\ o\ m\ r = \forall b.\; (r \to \text{Pipe}\ i\ i\ o\ ()\ m\ b) \to \text{Pipe}\ i\ i\ o\ ()\ m\ b$$

  The CPS encoding means `>>=` is function composition — O(1) regardless of
  nesting depth. Converting to a concrete `Pipe` (for fusion) costs O(n) in
  the number of binds, but this cost is paid once at fusion time.

  ## Why `unsafe`

  `awaitForever` and most of `Data.Conduit.Combinators` build pipes that
  recurse until a runtime source is exhausted — e.g. `await` returning
  `none` — which is a property of data flowing through the pipe at
  execution time, not a structural property of the term being elaborated.
  Unlike `Pipe.bind`/`Pipe.mapResult` (plain structural recursion on an
  already-built value, see `Data.Conduit.Internal.Pipe`), these are
  genuinely unbounded producer/consumer loops with no decreasing measure —
  the same shape as Haskell's laziness-driven corecursion. There is no
  proof of termination to give, so — matching the reference Hale port of
  this package — the whole `ConduitT` layer is `unsafe`, rather than
  reaching for a `partial def` (disallowed by project convention) or a
  fuel parameter that would falsify the streams' semantics.
-/

import Linen.Data.Conduit.Internal.Pipe
import Linen.Control.Monad.Trans.Resource

namespace Data.Conduit

open Data.Conduit.Internal

-- ══════════════════════════════════════════════════════════════
-- ConduitT — CPS wrapper
-- ══════════════════════════════════════════════════════════════

/-- The conduit type: a CPS/codensity wrapper over `Pipe` for efficient composition.

    $$\text{ConduitT}\ i\ o\ m\ r = \forall b.\; (r \to \text{Pipe}\ i\ i\ o\ ()\ m\ b) \to \text{Pipe}\ i\ i\ o\ ()\ m\ b$$

    The rank-2 quantification ensures the continuation's result type is abstract,
    which is key to the codensity transform's efficiency guarantee. -/
unsafe structure ConduitT (i o : Type) (m : Type → Type) (r : Type) where
  /-- Unwrap the CPS representation. Pass a continuation `r → Pipe i i o () m b`
      to obtain a concrete `Pipe i i o () m b`. -/
  unConduitT : {b : Type} → (r → Pipe i i o Unit m b) → Pipe i i o Unit m b

-- ── Type aliases ────────────────────────────────────────────

/-- A source produces output values without consuming input.
    Input type is `PEmpty` (uninhabited), so the pipe can never receive input.

    $$\text{Source}\ m\ o = \text{ConduitT}\ \text{PEmpty}\ o\ m\ ()$$ -/
unsafe abbrev Source (m : Type → Type) (o : Type) := ConduitT PEmpty o m Unit

/-- A sink consumes input and produces a final result, with no output.
    Output type is `PEmpty` (uninhabited), so the pipe can never yield downstream.

    $$\text{Sink}\ i\ m\ r = \text{ConduitT}\ i\ \text{PEmpty}\ m\ r$$ -/
unsafe abbrev Sink (i : Type) (m : Type → Type) (r : Type) := ConduitT i PEmpty m r

/-- A conduit transforms a stream: consumes input `i`, produces output `o`.

    $$\text{ConduitM}\ i\ o\ m = \text{ConduitT}\ i\ o\ m\ ()$$ -/
unsafe abbrev ConduitM (i o : Type) (m : Type → Type) := ConduitT i o m Unit

-- ══════════════════════════════════════════════════════════════
-- Monad instances for ConduitT
-- ══════════════════════════════════════════════════════════════

/-- `Functor` instance for `ConduitT i o m`.
    $$\text{map}\ f\ c = \lambda k.\; c.\text{unConduitT}\ (k \circ f)$$ -/
unsafe instance [Functor m] : Functor (ConduitT i o m) where
  map f ca := ⟨fun k => ca.unConduitT (fun a => k (f a))⟩

/-- `Pure` instance for `ConduitT i o m`.
    $$\text{pure}\ a = \lambda k.\; k\ a$$ -/
unsafe instance [Functor m] : Pure (ConduitT i o m) where
  pure a := ⟨fun k => k a⟩

/-- `Bind` instance for `ConduitT i o m`.
    CPS bind is function composition — O(1).
    $$\text{bind}\ c\ f = \lambda k.\; c.\text{unConduitT}\ (\lambda a.\; (f\ a).\text{unConduitT}\ k)$$ -/
unsafe instance [Functor m] : Bind (ConduitT i o m) where
  bind ca f := ⟨fun k => ca.unConduitT (fun a => (f a).unConduitT k)⟩

/-- `Monad` instance for `ConduitT i o m`.

    Monad laws follow from the CPS encoding:
    - **Left identity:** `pure a >>= f = ⟨fun k => (f a).unConduitT k⟩ = f a`
    - **Right identity:** `c >>= pure = ⟨fun k => c.unConduitT (fun a => k a)⟩ = c` (η-reduction)
    - **Associativity:** both sides reduce to
      `⟨fun k => c.unConduitT (fun a => (f a).unConduitT (fun b => (g b).unConduitT k))⟩` -/
unsafe instance [Monad m] : Monad (ConduitT i o m) where

-- ══════════════════════════════════════════════════════════════
-- Primitives
-- ══════════════════════════════════════════════════════════════

/-- Receive the next input value from upstream.
    Returns `none` when upstream is exhausted.

    $$\text{await} : \text{ConduitT}\ i\ o\ m\ (\text{Option}\ i)$$ -/
unsafe def await [Functor m] : ConduitT i o m (Option i) :=
  ⟨fun k => .needInput (fun val => k (some val)) (fun () => k none)⟩

/-- Send a value downstream.

    $$\text{yield} : o \to \text{ConduitT}\ i\ o\ m\ ()$$ -/
unsafe def yield [Functor m] (val : o) : ConduitT i o m Unit :=
  ⟨fun k => .haveOutput (k ()) val⟩

/-- Push an unconsumed input value back for re-processing by the next `await`.

    $$\text{leftoverC} : i \to \text{ConduitT}\ i\ o\ m\ ()$$ -/
unsafe def leftoverC [Functor m] (val : i) : ConduitT i o m Unit :=
  ⟨fun k => .leftover (k ()) val⟩

/-- Lift a monadic action into a conduit.

    $$\text{liftConduit} : m\ \alpha \to \text{ConduitT}\ i\ o\ m\ \alpha$$ -/
unsafe def liftConduit [Functor m] (action : m α) : ConduitT i o m α :=
  ⟨fun k => .pipeM action k⟩

/-- Repeatedly `await` and apply a function until upstream is exhausted.

    $$\text{awaitForever}\ f = \text{loop where loop} = \text{await} \gg\!= \begin{cases}
      \text{None} &\to \text{pure}\ () \\
      \text{Some}\ i &\to f\ i \gg\!= \lambda \_.\ \text{loop}
    \end{cases}$$

    Uses a recursive `needInput` node directly on the `Pipe` level for efficiency,
    avoiding the overhead of repeated CPS wrapping/unwrapping. -/
unsafe def awaitForever [Monad m] (f : i → ConduitT i o m r) : ConduitT i o m Unit :=
  ⟨fun k =>
    let rec go : Pipe i i o Unit m Unit :=
      .needInput
        (fun inp => ((f inp).unConduitT (fun _ => go)))
        (fun () => .done ())
    go.bind k⟩

-- ══════════════════════════════════════════════════════════════
-- MonadLift
-- ══════════════════════════════════════════════════════════════

/-- `MonadLift` instance allows writing `MonadLift.monadLift action` or using
    `liftM` to lift monadic actions into `ConduitT`. -/
unsafe instance [Functor m] : MonadLift m (ConduitT i o m) where
  monadLift := liftConduit

-- ══════════════════════════════════════════════════════════════
-- Fusion
-- ══════════════════════════════════════════════════════════════

/-- Fuse two concrete pipes: connect upstream's output to downstream's input.

    The fusion algorithm pattern-matches on the downstream pipe:
    - `done r` — propagate terminal
    - `haveOutput next o` — yield `o` to the outer pipe, continue fusing
    - `pipeM action` — run the effect, continue fusing
    - `leftover next l` — feed the leftover back to upstream as a `haveOutput`
    - `needInput onInput onUp` — pull from upstream:
      - `done ()` — upstream exhausted, call downstream's `onUp`
      - `haveOutput next o` — feed `o` to downstream's `onInput`
      - `pipeM action` — run the effect, continue pulling
      - `needInput onIn onUp` — pass through as outer pipe's `needInput`
      - `leftover next l` — emit leftover to outer pipe

    $$\text{fusePipes} : \text{Pipe}\ l_1\ a\ b\ ()\ m\ ()
      \to \text{Pipe}\ b\ b\ c\ ()\ m\ r
      \to \text{Pipe}\ l_1\ a\ c\ ()\ m\ r$$ -/
unsafe def fusePipes [Monad m]
    (upstream : Pipe l₁ a b Unit m Unit) (downstream : Pipe b b c Unit m r)
    : Pipe l₁ a c Unit m r :=
  match downstream with
  | .done r => .done r
  | .haveOutput next o =>
    .haveOutput (fusePipes upstream next) o
  | .pipeM action k => .pipeM action (fun b => fusePipes upstream (k b))
  | .leftover next l =>
    -- Feed the leftover back to upstream as output
    fusePipes (.haveOutput upstream l) next
  | .needInput onInput onUpDone =>
    goUp upstream onInput onUpDone
where
  /-- Pull from upstream to satisfy downstream's `needInput`. -/
  goUp (up : Pipe l₁ a b Unit m Unit)
      (onInput : b → Pipe b b c Unit m r)
      (onUpDone : Unit → Pipe b b c Unit m r)
      : Pipe l₁ a c Unit m r :=
    match up with
    | .done () => fusePipes (.done ()) (onUpDone ())
    | .haveOutput next o => fusePipes next (onInput o)
    | .pipeM action k =>
      .pipeM action (fun b => goUp (k b) onInput onUpDone)
    | .needInput onIn onUp =>
      .needInput (fun a => goUp (onIn a) onInput onUpDone)
                 (fun u => goUp (onUp u) onInput onUpDone)
    | .leftover next l =>
      .leftover (goUp next onInput onUpDone) l

/-- Fuse two conduits: connect the output of `up` to the input of `down`.

    Converts both `ConduitT` values to concrete `Pipe`s (by passing
    `fun r => .done r` as the CPS continuation), fuses the pipes, then
    re-wraps in `ConduitT`.

    $$(\text{.|}) : \text{ConduitT}\ a\ b\ m\ () \to \text{ConduitT}\ b\ c\ m\ r \to \text{ConduitT}\ a\ c\ m\ r$$ -/
unsafe def pipe [Monad m] (up : ConduitT a b m Unit) (down : ConduitT b c m r)
    : ConduitT a c m r :=
  ⟨fun k =>
    let upPipe : Pipe a a b Unit m Unit := up.unConduitT (fun () => .done ())
    let downPipe : Pipe b b c Unit m r := down.unConduitT (fun r => .done r)
    (fusePipes upPipe downPipe).bind k⟩

/-- Infix fusion operator `.|` — connects upstream output to downstream input.

    ```lean
    sourceList [1,2,3] .| mapC (· * 2) .| sinkList
    ```

    Declared via `HAppend` since we cannot directly use `scoped infixr` with
    `unsafe`. -/
unsafe instance [Monad m] : HAppend (ConduitT a b m Unit) (ConduitT b c m r) (ConduitT a c m r) where
  hAppend := pipe

-- Scoped infix notation for fusion
scoped notation:55 a " .| " b => pipe a b

-- ══════════════════════════════════════════════════════════════
-- Execution
-- ══════════════════════════════════════════════════════════════

/-- Run a closed pipe to extract the result.

    A "closed" pipe has `PEmpty` for both input and output types, meaning
    `needInput` and `haveOutput` branches are unreachable by construction
    (there are no values of type `PEmpty` to provide or receive).

    $$\text{runPipe} : \text{Pipe}\ \text{PEmpty}\ \text{PEmpty}\ \text{PEmpty}\ ()\ m\ r \to m\ r$$ -/
unsafe def runPipe [Monad m] : Pipe PEmpty PEmpty PEmpty Unit m r → m r
  | .done r => pure r
  | .pipeM action k => action >>= (runPipe ∘ k)
  -- The following branches are unreachable when PEmpty is truly empty,
  -- but Lean requires exhaustive matching:
  | .needInput _ onUp => runPipe (onUp ())
  | .haveOutput next _ => runPipe next
  | .leftover next _ => runPipe next

/-- Run a closed conduit to extract the result.

    Converts the `ConduitT` to a concrete `Pipe` and runs it.

    $$\text{runConduit} : \text{ConduitT}\ \text{PEmpty}\ \text{PEmpty}\ m\ r \to m\ r$$ -/
unsafe def runConduit [Monad m] (c : ConduitT PEmpty PEmpty m r) : m r :=
  runPipe (c.unConduitT (fun r => .done r))

/-- Run a pure conduit (no effects).

    $$\text{runConduitPure} : \text{ConduitT}\ \text{PEmpty}\ \text{PEmpty}\ \text{Id}\ r \to r$$ -/
unsafe def runConduitPure (c : ConduitT PEmpty PEmpty Id r) : r :=
  Id.run (runConduit c)

/-- Run a conduit with `ResourceT` for automatic resource cleanup.

    All resources acquired via `bracketP` or `allocate` are released
    when `runConduitRes` completes, even on exceptions.

    $$\text{runConduitRes} : \text{ConduitT}\ \text{PEmpty}\ \text{PEmpty}\ (\text{ResourceT}\ \text{IO})\ r \to \text{IO}\ r$$ -/
unsafe def runConduitRes
    (c : ConduitT PEmpty PEmpty (Control.Monad.Trans.Resource.ResourceT IO) r) : IO r :=
  Control.Monad.Trans.Resource.runResourceT (runConduit c)

-- ══════════════════════════════════════════════════════════════
-- Resource management
-- ══════════════════════════════════════════════════════════════

/-- Acquire a resource with guaranteed cleanup, running inside `ResourceT`.

    The resource is acquired immediately, and the cleanup action is registered
    with `ResourceT`. The cleanup runs when `runConduitRes` completes (or
    earlier via the `ReleaseKey`).

    $$\text{bracketP} : \text{IO}\ \alpha \to (\alpha \to \text{IO}\ ()) \to (\alpha \to \text{ConduitT}\ i\ o\ (\text{ResourceT}\ \text{IO})\ r) \to \text{ConduitT}\ i\ o\ (\text{ResourceT}\ \text{IO})\ r$$ -/
unsafe def bracketP
    (acquire : IO α)
    (release : α → IO Unit)
    (inner : α → ConduitT i o (Control.Monad.Trans.Resource.ResourceT IO) r)
    : ConduitT i o (Control.Monad.Trans.Resource.ResourceT IO) r := do
  let (_, a) ← liftConduit (Control.Monad.Trans.Resource.allocate acquire release)
  inner a

end Data.Conduit
