/-
  Tests for `Linen.Network.HTTP.Client.Retry`.

  Delay computation and the retry predicate are pure, so they are checked with
  `#guard`. The retry loop itself is `IO`, so it runs under `#eval` against a
  fake action backed by an `IO.Ref` — no network, but it still exercises the
  real loop, including that it stops at `maxAttempts` and that a non-retryable
  status returns immediately.
-/
import Linen.Network.HTTP.Client.Retry

open Network.HTTP.Client Network.HTTP.Types

namespace Tests.Network.HTTP.Client.Retry

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

/-- A response with a given status and optional `Retry-After`. -/
private def resp (code : Nat) (retryAfter : Option String := none)
    (h : 100 ≤ code ∧ code ≤ 999 := by omega) : Response :=
  { statusCode := { statusCode := code, statusMessage := "", statusValid := h }
    headers := match retryAfter with
               | some v => [(hRetryAfter, v)]
               | none   => []
    body := ByteArray.empty }

/-! ### Backoff

  Doubling from the base, then capped. Computed over `Nat`, so a large attempt
  saturates at the cap rather than overflowing. -/

private def p : RetryPolicy := { baseDelayMillis := 100, maxDelayMillis := 20000 }

#guard backoffMillis p 1 == 100
#guard backoffMillis p 2 == 200
#guard backoffMillis p 3 == 400
#guard backoffMillis p 4 == 800
#guard backoffMillis p 8 == 12800
#guard backoffMillis p 9 == 20000      -- capped
#guard backoffMillis p 40 == 20000     -- saturates, does not wrap

/-! ### Which statuses are retried

  Only the transient ones. A `4xx` other than 408/429 means the request itself
  is wrong, so repeating it unchanged cannot help. -/

#guard shouldRetryResponse {} (resp 500)
#guard shouldRetryResponse {} (resp 502)
#guard shouldRetryResponse {} (resp 503)
#guard shouldRetryResponse {} (resp 429)      -- rate limited
#guard shouldRetryResponse {} (resp 408)      -- request timeout
#guard !(shouldRetryResponse {} (resp 200))
#guard !(shouldRetryResponse {} (resp 301))
#guard !(shouldRetryResponse {} (resp 400))
#guard !(shouldRetryResponse {} (resp 401))
#guard !(shouldRetryResponse {} (resp 404))
#guard !(shouldRetryResponse {} (resp 422))

-- The predicate is a field, so a caller can widen or narrow it.
#guard shouldRetryResponse { retryStatus := fun c => c == 404 } (resp 404)
#guard !(shouldRetryResponse { retryStatus := fun _ => false } (resp 500))

/-! ### Retry-After

  Seconds only. The HTTP-date form needs a trusted clock and correct skew
  handling, so it is ignored rather than guessed at. -/

#guard retryAfterMillis (resp 429 (some "5")) == some 5000
#guard retryAfterMillis (resp 429 (some " 5 ")) == some 5000       -- trimmed
#guard retryAfterMillis (resp 429 (some "0")) == some 0
#guard retryAfterMillis (resp 429) == none                          -- absent
#guard retryAfterMillis (resp 429 (some "Wed, 21 Oct 2015 07:28:00 GMT")) == none
#guard retryAfterMillis (resp 429 (some "soon")) == none            -- unparseable

/-! ### Policy presets -/

#guard noRetry.maxAttempts == 1
#guard !noRetry.retryOnError

/-! ### The loop

  A counting action lets the loop be observed without a network. -/

/-- An action that returns `script` in order, falling back to `200` once the
    script runs out, and records how many times it was called.

    Takes ready-made responses rather than codes: `Status` carries a validity
    proof, which cannot be discharged for a code only known at run time. -/
private def scripted (calls : IO.Ref Nat) (script : List Response) : IO Response := do
  let n ← calls.get
  calls.set (n + 1)
  pure (script[n]?.getD (resp 200))

-- Instant policy: same control flow, no waiting, so the test is fast.
private def fast : RetryPolicy :=
  { maxAttempts := 4, baseDelayMillis := 0, maxDelayMillis := 0, jitter := false }

-- Succeeds first time: exactly one call, no delay.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let r ← withRetry fast (scripted calls [resp 200])
  check (r.statusCode.statusCode == 200) "expected 200"
  check ((← calls.get) == 1) s!"expected 1 call, got {← calls.get}"

-- Fails twice then succeeds: three calls, and the success is returned.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let r ← withRetry fast (scripted calls [resp 503, resp 503, resp 200])
  check (r.statusCode.statusCode == 200) s!"expected 200, got {r.statusCode.statusCode}"
  check ((← calls.get) == 3) s!"expected 3 calls, got {← calls.get}"

-- Always fails: stops at maxAttempts and *returns* the last response rather
-- than raising, since whether a 503 is fatal is the caller's decision.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let r ← withRetry fast (scripted calls (List.replicate 6 (resp 500)))
  check (r.statusCode.statusCode == 500) "expected the failing status back"
  check ((← calls.get) == 4) s!"expected 4 calls, got {← calls.get}"

-- A non-retryable status short-circuits: one call, even though attempts remain.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let r ← withRetry fast (scripted calls [resp 404, resp 200])
  check (r.statusCode.statusCode == 404) "expected 404"
  check ((← calls.get) == 1) s!"expected 1 call, got {← calls.get}"

-- `noRetry` really does disable retrying.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let r ← withRetry noRetry (scripted calls [resp 500, resp 200])
  check (r.statusCode.statusCode == 500) "expected 500"
  check ((← calls.get) == 1) s!"expected 1 call, got {← calls.get}"

-- Connection errors are retried too, and the last one is re-raised, because
-- there is no response to hand back.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let failing : IO Response := do
    let n ← calls.get; calls.set (n + 1); throw (IO.userError "connection refused")
  match ← (withRetry fast failing).toBaseIO with
  | .ok _    => throw (IO.userError "expected the error to propagate")
  | .error _ => check ((← calls.get) == 4) s!"expected 4 calls, got {← calls.get}"

-- An error followed by success recovers.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let flaky : IO Response := do
    let n ← calls.get
    calls.set (n + 1)
    if n == 0 then throw (IO.userError "connection refused") else pure (resp 200)
  let r ← withRetry fast flaky
  check (r.statusCode.statusCode == 200) "expected recovery"
  check ((← calls.get) == 2) s!"expected 2 calls, got {← calls.get}"

-- With `retryOnError := false`, the first error propagates immediately.
#eval show IO Unit from do
  let calls ← IO.mkRef 0
  let failing : IO Response := do
    let n ← calls.get; calls.set (n + 1); throw (IO.userError "nope")
  match ← (withRetry { fast with retryOnError := false } failing).toBaseIO with
  | .ok _    => throw (IO.userError "expected the error to propagate")
  | .error _ => check ((← calls.get) == 1) s!"expected 1 call, got {← calls.get}"

/-! ### Jitter

  Equal jitter: never below half the intended delay, never above it. That keeps
  most of the backoff while still spreading clients out. -/

#eval show IO Unit from do
  let pol : RetryPolicy := { baseDelayMillis := 1000, maxDelayMillis := 100000 }
  for _ in [0:50] do
    let d ← jitterMillis pol 1000
    check (500 ≤ d && d ≤ 1000) s!"jittered delay out of range: {d}"

-- Jitter off is exact.
#eval show IO Unit from do
  let d ← jitterMillis { jitter := false } 1000
  check (d == 1000) s!"expected exactly 1000, got {d}"

-- A server's `Retry-After` wins over computed backoff.
#eval show IO Unit from do
  let d ← delayBefore { baseDelayMillis := 100 } 1 (some (resp 429 (some "7")))
  check (d == 7000) s!"expected the server's 7s, got {d}"

-- Unless it exceeds the cap, in which case backoff is used instead — a server
-- asking for an hour should not silently wedge the client.
#eval show IO Unit from do
  let pol : RetryPolicy := { baseDelayMillis := 100, maxDelayMillis := 5000, jitter := false }
  let d ← delayBefore pol 1 (some (resp 429 (some "3600")))
  check (d == 100) s!"expected backoff, got {d}"

/-! ### Signatures -/

example : RetryPolicy → Nat → Nat := backoffMillis
example : RetryPolicy → Response → Bool := shouldRetryResponse
example : Response → Option Nat := retryAfterMillis
example : RetryPolicy → IO Response → IO Response := fun p a => withRetry p a

end Tests.Network.HTTP.Client.Retry
