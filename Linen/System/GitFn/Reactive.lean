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
    worker, an HTTP worker, or a REST endpoint (`Remote.endpoint`).
  - **`Reactive.remote args β r`** registers it under the signature
    `args → β`, as an ordinary `FnRef`: it applies like any registered
    function (`f x y`, `mapFn`, `zip`, `withLatestFrom`), and can be
    `rebind`-ed.
  - The graph's value type must travel as JSON (`JsonValue`). The natural
    choice is `Lean.Json` itself — the worker's own format — for which this
    module gives `Codec`s of `Nat`, `Int`, `String` and `Bool`, with their
    round-trip laws proven, and `Codec.ofJson` for other types given a proof;
    linen's `Data.Json.Value` works too, through `Data.Json.Bridge`.
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

open Lean (Json ToJson FromJson)
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

/-- A worker's REST endpoint, as a remote function. -/
def Remote.endpoint (url : String) : Remote := ⟨callEndpoint url⟩

-- ── Values as JSON ──────────────────────────────────────────────────────────

/-- A value type that travels as JSON. -/
class JsonValue (V : Type) where
  /-- A value, as JSON. -/
  toJson : V → Json
  /-- JSON, as a value. -/
  fromJson : Json → Except String V

instance : JsonValue Json := ⟨id, .ok⟩

instance : JsonValue Data.Json.Value :=
  ⟨fun v => v.toLeanJson.toOption.getD .null, Data.Json.Value.ofLeanJson⟩

/-- A remote function, erased for a graph over `V` in `m`. -/
def Remote.impl {m : Type → Type} {V : Type} [Monad m] [MonadLiftT IO m] [JsonValue V]
    (r : Remote) : Impl m V := fun vs => do
  let res ← (monadLift (r.call (vs.map JsonValue.toJson)) : m (Except String Json))
  pure (res >>= fun j => (JsonValue.fromJson j).map some)

-- ── Codecs into `Lean.Json` ─────────────────────────────────────────────────

/-- A `Codec` into `Lean.Json` from `ToJson`/`FromJson`, given their round-trip
    law. -/
@[reducible] def Codec.ofJson {α : Type} [ToJson α] [FromJson α]
    (h : ∀ a : α, (FromJson.fromJson? (ToJson.toJson a) : Except String α) = .ok a) :
    Codec Json α :=
  ⟨ToJson.toJson, FromJson.fromJson?, h⟩

instance : Codec Json String := Codec.ofJson fun _ => rfl
instance : Codec Json Bool := Codec.ofJson fun _ => rfl
instance : Codec Json Nat := Codec.ofJson fun n => by
  simp [ToJson.toJson, FromJson.fromJson?, Json.getNat?, Lean.JsonNumber.fromNat]; rfl
instance : Codec Json Int := Codec.ofJson fun n => by
  simp [ToJson.toJson, FromJson.fromJson?, Json.getInt?, Lean.JsonNumber.fromInt]; rfl

end System.GitFn

namespace Control.Reactive.Reactive

/-- Register a remote function (a `System.GitFn` worker) under the signature
    `args → β`: an `FnRef` like any other. -/
def remote {m : Type → Type} {V : Type} [Monad m] [MonadLiftT IO m] [System.GitFn.JsonValue V]
    (args : List Type) (β : Type) (r : System.GitFn.Remote) : Reactive m V (FnRef args β) :=
  fnImpl args β r.impl

end Control.Reactive.Reactive
