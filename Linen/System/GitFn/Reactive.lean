/-
  `System.GitFn.Reactive` — GitFn functions as nodes of reactive graphs

  A built `GitFn` worker is a function on JSON values. This module makes it a
  function of a `Control.Reactive` graph:

  ```lean
  let add ← built.spawn                                  -- a stdio worker (IO)
  let g := (do
      node x ← subject Nat
      node y ← subject Nat
      node addFn ← Reactive.remote [Nat, Nat] Nat add.remote   -- a registered function
      node sum ← addFn x y                                -- combineLatest over it
      sum.map (· * 10)                                    -- mixed with local functions
    : Reactive IO Lean.Json _).graph!
  ```

  - **`Remote`** is anything that calls a function on JSON arguments: a stdio
    worker, an HTTP worker, or a JSON-RPC endpoint (`Remote.endpoint`).
  - **`Reactive.remote args β r`** registers it under the signature
    `args → β`, as an ordinary `FnRef`: it applies like any registered
    function (`f x y`, `mapFn`, `zip`, `withLatestFrom`), and can be
    `rebind`-ed.
  - The graph's value type must travel as JSON: Lean core's `ToJson` and
    `FromJson`. The natural choice is `Lean.Json` itself — the worker's own
    format — whose `Codec`s (`Nat`, `Int`, `String`, `Bool`, and
    `Codec.ofJson`) are in `Control.Reactive.Graph`; linen's `Data.Json.Value`
    works too, through `Data.Json.Bridge`'s instances.
  - The graph runs in a monad that can do `IO` (the call is a process or a
    request); a failing call, or a result that does not decode, is an `error`
    notification of the node, and the rest of the graph carries on.
  - The signature `args → β` is the caller's claim about the descriptor's
    type — the worker has checked the remote function against the descriptor;
    what is checked here is only that values decode.
-/
import Linen.Control.Reactive
import Linen.System.GitFn.Worker
import Linen.Data.Json.Bridge

namespace System.GitFn

open Lean (Json ToJson FromJson toJson fromJson?)
open Control.Reactive (Codec Impl)

-- ── Remote functions ────────────────────────────────────────────────────────

/-- A function called on JSON arguments, somewhere else. -/
structure Remote where
  /-- Call it. -/
  call : List Json → IO (Except String Json)

/-- A stdio worker, as a remote function. -/
def StdioWorker.remote (w : StdioWorker) : Remote := ⟨w.call⟩

/-- An HTTP worker, as a remote function. -/
def HttpWorker.remote (w : HttpWorker) : Remote := ⟨w.call⟩

/-- A worker's JSON-RPC endpoint, as a remote function. -/
def Remote.endpoint (url : String) : Remote := ⟨callEndpoint url⟩

-- ── Values as JSON ──────────────────────────────────────────────────────────

/-- A remote function, erased for a graph over `V` in `m`: values travel as
    JSON through Lean core's `ToJson V`/`FromJson V`. -/
def Remote.impl {m : Type → Type} {V : Type} [Monad m] [MonadLiftT IO m] [ToJson V] [FromJson V]
    (r : Remote) : Impl m V := fun vs => do
  let res ← (monadLift (r.call (vs.map toJson)) : m (Except String Json))
  pure (res >>= fun j => (fromJson? j).map some)

end System.GitFn

namespace Control.Reactive.Reactive

/-- Register a remote function (a `System.GitFn` worker) under the signature
    `args → β`: an `FnRef` like any other. -/
def remote {m : Type → Type} {V : Type} [Monad m] [MonadLiftT IO m] [Lean.ToJson V] [Lean.FromJson V]
    (args : List Type) (β : Type) (r : System.GitFn.Remote) : Reactive m V (FnRef args β) :=
  fnImpl args β r.impl

end Control.Reactive.Reactive
