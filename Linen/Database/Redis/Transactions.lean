/-
  Linen.Database.Redis.Transactions — `MULTI`/`EXEC`/`WATCH` transactions

  ## Haskell source
  `Database.Redis.Transactions` from https://hackage.haskell.org/package/hedis
  (module 14 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Transactions.hs`. Exposes `watch`, `unwatch`,
  `multiExec`, and the `Queued`/`TxResult`/`RedisTx` types.

  ## `RedisTx` as a `StateT`, not a `newtype`
  Upstream's `RedisTx a = RedisTx (StateT Int Redis a)` is a `newtype` that
  derives `Monad`/`MonadIO`/`Functor`/`Applicative` straight through from the
  underlying `StateT Int Redis`. Exactly as `Database.Redis.Core.Internal`'s
  `Redis` is a plain `abbrev` for `ReaderT RedisEnv IO` (see that module's
  doc-comment), `RedisTx` is here an `abbrev` for `StateT Nat Redis`: Lean's
  `StateT` already carries every one of those instances generically, so the
  `newtype`-plus-`deriving` boilerplate is unnecessary — this is not a
  behavioural simplification, just definitional transparency. The `Int`
  counter (a future index into the `EXEC` result list) is a `Nat` here since
  it only ever counts up from `0`.

  ## The free-applicative `Queued`
  A `Queued a` is upstream's proxy for a command's result inside a
  transaction: a function `Vector Reply -> Either Reply a` that, once the
  `EXEC` reply's element vector is available, plucks this command's element
  and decodes it. It is composable through its `Functor`/`Applicative`/`Monad`
  instances (all three ported faithfully, matching upstream's own three
  instance definitions), so a caller can write
  `multiExec $ (,) <$> get "k1" <*> get "k2"` and get back a well-typed tuple.
  The `Vector` is a Lean `Array`; upstream's `fromList` becomes `List.toArray`.

  ## Deviations
  - Upstream's `returnDecode` builds `Queued (decode . (! i))`, where `(! i)`
    is `Data.Vector`'s *partial* index — it crashes if the `EXEC` result has
    fewer elements than commands queued. AGENTS.md forbids introducing
    crashes; the ported `Queued` uses the safe `Array.get?` and, on an
    out-of-range index, returns `Except.error` carrying a synthetic error
    `Reply` instead (for a well-formed transaction the index is always in
    range, so this path is never taken).
  - Upstream's `multiExec` calls `error` (a crashing partial function) when
    the `EXEC` reply is not a multi-bulk at all. Since `multiExec` runs in the
    `IO`-based `Redis` monad, this ports to `throw (IO.userError …)` — a safe
    failure rather than a crash, the same "upstream crash → safe failure"
    treatment `Database.Redis.Core`/`.Types` already use.
  - `TxError` upstream carries `show reply` (GHC-derived `Show`). `Reply` has
    no `Repr`/`ToString` here (its `ByteArray` payloads have no `Repr`
    instance in Lean's stdlib), so a small local renderer `showReply`
    substitutes for the derived `Show`.
-/
import Linen.Database.Redis.Commands
import Linen.Database.Redis.Core
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.Types

namespace Database.Redis.Transactions

open Database.Redis.Core (Redis sendRequest RedisCtx MonadRedis)
open Database.Redis.Protocol (Reply)
open Database.Redis.Types (RedisResult Status decode)

-- ── The `Queued` proxy value ──

/-- A `Queued` value represents the result of a command inside a transaction.
    It is a proxy object for the *actual* result, which will only be available
    after returning from a `multiExec` transaction, and is composable via the
    `Functor`/`Applicative`/`Monad` instances below.

    Concretely it wraps a function that, given the vector of replies returned
    by `EXEC`, plucks and decodes this command's own result. Mirrors
    upstream's `data Queued a = Queued (Vector Reply -> Either Reply a)`. -/
structure Queued (α : Type) where
  /-- Apply the collected `EXEC` replies to obtain this command's decoded
      result (or the offending `Reply` on failure). -/
  runQueued : Array Reply → Except Reply α

/-- `fmap` over a `Queued`, matching upstream's
    `fmap f (Queued g) = Queued (fmap f . g)`. -/
instance : Functor Queued where
  map f q := ⟨fun rs => f <$> q.runQueued rs⟩

/-- `pure`/`<*>` for `Queued`, matching upstream's `Applicative` instance
    (`pure x = Queued (const $ Right x)` and the `<*>` that runs both decoders
    against the same reply vector and applies one result to the other). -/
instance : Applicative Queued where
  pure x := ⟨fun _ => Except.ok x⟩
  seq qf qx := ⟨fun rs => do
    let f' ← qf.runQueued rs
    let x' ← (qx ()).runQueued rs
    pure (f' x')⟩

/-- `>>=` for `Queued`, matching upstream's `Monad` instance
    (`Queued x >>= f = Queued $ \rs -> do x' <- x rs; let Queued f' = f x'; f' rs`). -/
instance : Monad Queued where
  bind qx f := ⟨fun rs => do
    let x' ← qx.runQueued rs
    (f x').runQueued rs⟩

-- ── The transaction context `RedisTx` ──

/-- Command-context inside of `MULTI`/`EXEC` transactions. Use `multiExec` to
    run actions of this type. In the `RedisTx` context, all commands return a
    `Queued` value — a proxy for the actual result, available only after the
    transaction finishes. Mirrors upstream's
    `newtype RedisTx a = RedisTx (StateT Int Redis a)` (see the module
    doc-comment for why this is a plain `abbrev`). -/
abbrev RedisTx (α : Type) : Type := StateT Nat Redis α

/-- Run a `RedisTx` action, starting the future-index counter at `0`. Mirrors
    upstream's `runRedisTx = evalStateT r 0`. -/
def runRedisTx (rtx : RedisTx α) : Redis α :=
  StateT.run' rtx 0

instance : MonadRedis RedisTx where
  liftRedis r := StateT.lift r

/-- A command run in a `RedisTx` context returns a `Queued`: `returnDecode`
    records the command's position `i` in the eventual `EXEC` result list and
    hands back a `Queued` that will decode element `i`. Mirrors upstream's
    `instance RedisCtx RedisTx Queued`. -/
instance : RedisCtx RedisTx Queued where
  returnDecode {α} [RedisResult α] _reply := do
    let i ← get
    set (i + 1)
    return ⟨fun rs =>
      match rs[i]? with
      | some r => decode r
      | none => Except.error (Reply.error "Redis.Transactions: EXEC result too short".toUTF8)⟩

-- ── Result of a transaction ──

/-- Result of a `multiExec` transaction. Mirrors upstream's `TxResult`. -/
inductive TxResult (α : Type) where
  /-- Transaction completed successfully; the wrapped value corresponds to the
      `Queued` value returned from the `multiExec` argument action. -/
  | success (a : α)
  /-- Transaction aborted due to an earlier `watch` command (the `EXEC` reply
      was a null multi-bulk). -/
  | aborted
  /-- At least one of the commands returned an error reply. -/
  | error (msg : String)
  deriving Repr, BEq, Inhabited

-- ── Rendering a `Reply` for `TxError` (the `Show` substitution) ──

/-- Best-effort rendering of a `ByteArray` payload as text, falling back to a
    byte list for non-UTF-8 content. -/
private def showBytes (b : ByteArray) : String :=
  (String.fromUTF8? b).getD (toString b.toList)

mutual
/-- Render a `Reply` to a diagnostic string (the substitute for upstream's
    derived `Show Reply`, used by `TxError`). -/
private def showReply : Reply → String
  | .singleLine s => s!"SingleLine {showBytes s}"
  | .error s => s!"Error {showBytes s}"
  | .integer i => s!"Integer {i}"
  | .bulk none => "Bulk none"
  | .bulk (some s) => s!"Bulk (some {showBytes s})"
  | .multiBulk none => "MultiBulk none"
  | .multiBulk (some rs) => s!"MultiBulk (some [{showReplyList rs}])"

/-- Render a list of `Reply` values, comma-separated. -/
private def showReplyList : List Reply → String
  | [] => ""
  | [r] => showReply r
  | r :: rs => showReply r ++ ", " ++ showReplyList rs
end

-- ── The transaction commands ──

/-- Watch the given keys to determine execution of the `MULTI`/`EXEC` block
    (<http://redis.io/commands/watch>). Mirrors upstream's `watch`. -/
def watch (key : List ByteArray) : Redis (Except Reply Status) :=
  sendRequest ("WATCH".toUTF8 :: key)

/-- Forget about all watched keys (<http://redis.io/commands/unwatch>).
    Mirrors upstream's `unwatch`. -/
def unwatch : Redis (Except Reply Status) :=
  sendRequest ["UNWATCH".toUTF8]

/-- Begin a transaction. Mirrors upstream's private `multi`. -/
private def multi : Redis (Except Reply Status) :=
  sendRequest ["MULTI".toUTF8]

/-- Execute the queued transaction, collapsing the `Either Reply Reply` a raw
    `sendRequest` yields into the single underlying `Reply`. Mirrors
    upstream's private `exec = either id id <$> sendRequest ["EXEC"]`. -/
private def exec : Redis Reply := do
  let r ← sendRequest (α := Reply) ["EXEC".toUTF8]
  pure (match r with | .ok x => x | .error x => x)

/-- Run commands inside a transaction. For the semantics of Redis
    transactions see <http://redis.io/topics/transactions>.

    Inside the transaction block, command functions return their result
    wrapped in a `Queued` — a proxy for the actual result, which only becomes
    available after `EXEC`ing the transaction. The `Queued` values are
    combined via their `Applicative` instance, e.g.

    ```
    runRedis conn do
      let _ ← set "hello" "hello"
      let _ ← set "world" "world"
      let helloworld ← multiExec do
        let hello ← get "hello"
        let world ← get "world"
        pure (Prod.mk <$> hello <*> world)
      IO.println (repr helloworld)
    ```

    Mirrors upstream's `multiExec`. See the module doc-comment for the
    crash-avoidance deviation on a non-multi-bulk `EXEC` reply. -/
def multiExec (rtx : RedisTx (Queued α)) : Redis (TxResult α) := do
  -- No need to catch exceptions and call DISCARD: the pool closes the
  -- connection anyway (upstream's note).
  let _ ← multi
  let q ← runRedisTx rtx
  let r ← exec
  match r with
  | .multiBulk none => pure .aborted
  | .multiBulk (some rs) =>
    match q.runQueued rs.toArray with
    | .error e => pure (.error (showReply e))
    | .ok a => pure (.success a)
  | other => throw (IO.userError s!"hedis: EXEC returned {showReply other}")

end Database.Redis.Transactions
