/-
  Linen.Control.Exception — structured exception handling for IO

  Haskell-style resource/cleanup combinators over Lean's IO error mechanism.
  Most of Haskell's `Control.Exception` already has a function-level Lean
  spelling, so only the named resource patterns core lacks are provided here:

  | Haskell     | Lean stdlib                                                   |
  |-------------|---------------------------------------------------------------|
  | `try`       | `IO.toBaseIO` (`BaseIO (Except IO.Error α)`)                   |
  | `catch`     | `tryCatch` (`MonadExcept`; the handler gets the real `IO.Error`) |
  | `finally`   | `tryFinally` (or `try … finally …`)                           |
  | `evaluate`  | `pure` (Lean is strict)                                       |

  `bracket` and `onException` have no single stdlib function, so they live here,
  built on `tryFinally` / `tryCatch`.

  ## Typing guarantees

  * **Resource safety (bracket):** `release` always runs after `use`, whether
    `use` succeeds or throws — structurally, via `tryFinally`. If `acquire`
    throws, `release` is not called (the resource was never acquired).
  * **Selective cleanup (onException):** `cleanup` runs only on failure, then
    the original error is re-thrown.

  Asynchronous exceptions (e.g. thread cancellation) are not handled — Lean does
  not expose an async-exception mechanism.
-/

namespace Control.Exception

/-- Bracket: acquire a resource, use it, then release it. `release` always runs
whether `use` succeeds or throws (on throw, the error is re-thrown after release).

$$\text{bracket} : \text{IO}\ \alpha \to (\alpha \to \text{IO}\ \text{Unit}) \to (\alpha \to \text{IO}\ \beta) \to \text{IO}\ \beta$$ -/
def bracket (acquire : IO α) (release : α → IO Unit) (use : α → IO β) : IO β := do
  let a ← acquire
  tryFinally (use a) (release a)

/-- Run `cleanup` only if `action` throws, then re-throw; on success `cleanup`
is skipped.

$$\text{onException} : \text{IO}\ \alpha \to \text{IO}\ \text{Unit} \to \text{IO}\ \alpha$$ -/
def onException (action : IO α) (cleanup : IO Unit) : IO α :=
  tryCatch action (fun e => do cleanup; throw e)

end Control.Exception
