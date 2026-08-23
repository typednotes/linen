/-
  Linen.Network.HTTP.Client.Retry — retrying failed requests with backoff

  Retries transient HTTP failures: connection errors, `429 Too Many Requests`,
  and `5xx` responses. Waits between attempts with exponential backoff, honours
  a `Retry-After` header when the server sends one, and adds jitter so a fleet
  of clients recovering from an outage does not resynchronise into a thundering
  herd.

  ## Provenance
  Modelled on Hackage's `retry` (`Control.Retry`) for the policy shape —
  a base delay, exponential growth, a cap, and jitter — specialised here to
  HTTP, where the response carries its own retry advice.

  ## Relationship to timeouts
  Timeouts (`Client.Connection.defaultTimeoutMillis`) bound *one* attempt;
  retries bound *how many* attempts. They compose, and the worst-case wall
  clock is roughly `maxAttempts × timeout + Σ delays` — worth keeping in mind,
  since the defaults here allow a fair few minutes.

  ## Not retried
  Anything `4xx` other than `408` and `429`: those mean the request itself is
  wrong, and repeating it unchanged cannot help. Nor is a request with a body
  treated specially — retrying assumes the request is idempotent, which is the
  caller's judgement to make, not this module's.
-/
import Linen.Network.HTTP.Client.Redirect

namespace Network.HTTP.Client

-- ── Policy ──

/-- When and how hard to retry.

    Defaults allow four attempts with backoff from 100 ms, which recovers from
    a brief blip without hammering a service that is genuinely down. -/
structure RetryPolicy where
  /-- Total attempts including the first, so `1` disables retrying. -/
  maxAttempts     : Nat := 4
  /-- Delay before the second attempt; doubles thereafter. -/
  baseDelayMillis : Nat := 100
  /-- Cap on any single delay, before jitter. -/
  maxDelayMillis  : Nat := 20000
  /-- Randomise each delay downward, spreading a recovering fleet out. -/
  jitter          : Bool := true
  /-- Whether a status code is worth another attempt. -/
  retryStatus     : Nat → Bool := fun c => c == 408 || c == 429 || (500 ≤ c && c ≤ 599)
  /-- Whether to retry when the connection itself failed. -/
  retryOnError    : Bool := true
  /-- Trust a `Retry-After` header, when the server sends a usable one. -/
  honourRetryAfter : Bool := true

/-- Retry nothing: one attempt, and whatever it returns is the answer. -/
def noRetry : RetryPolicy := { maxAttempts := 1, retryOnError := false }

-- ── Delays ──

/-- Exponential backoff, capped: `base × 2^(attempt-1)`, clamped to
    `maxDelayMillis`. Attempt `1` is the first *retry*, i.e. the wait after the
    initial attempt failed.

    Doubling is computed by repeated multiplication over `Nat`, which cannot
    overflow, and the cap is applied after — so a large `attempt` saturates
    rather than wrapping. -/
def backoffMillis (p : RetryPolicy) (attempt : Nat) : Nat :=
  let raw := p.baseDelayMillis * 2 ^ (attempt - 1)
  min raw p.maxDelayMillis

/-- Apply jitter: a uniform pick from `[delay/2, delay]`.

    "Equal jitter" rather than full randomisation — it still guarantees most of
    the intended backoff, so a retry storm cannot collapse back onto the
    server, while breaking up synchronised clients. -/
def jitterMillis (p : RetryPolicy) (delay : Nat) : BaseIO Nat :=
  if p.jitter && delay > 1 then IO.rand (delay / 2) delay else pure delay

/-- The number of seconds a `Retry-After` header asks for, when it gives a
    plain delay.

    The HTTP-date form is deliberately not handled: acting on it needs a
    trusted clock and correct skew handling, and getting that wrong is worse
    than falling back to backoff. An unparseable value is simply ignored. -/
def retryAfterMillis (resp : Response) : Option Nat :=
  match resp.findHeader Network.HTTP.Types.hRetryAfter with
  | some v => (String.toNat? v.trimAscii.toString).map (· * 1000)
  | none   => none

/-- How long to wait before `attempt`, taking the server's advice when it gave
    any and it is not absurd. -/
def delayBefore (p : RetryPolicy) (attempt : Nat) (resp : Option Response) : BaseIO Nat := do
  let advised :=
    if p.honourRetryAfter then
      match resp with
      | some r => (retryAfterMillis r).filter (· ≤ p.maxDelayMillis)
      | none   => none
    else none
  match advised with
  | some ms => pure ms                            -- the server said; believe it
  | none    => jitterMillis p (backoffMillis p attempt)

-- ── Running ──

/-- Whether a response is worth another attempt. -/
def shouldRetryResponse (p : RetryPolicy) (resp : Response) : Bool :=
  p.retryStatus resp.statusCode.statusCode

/-- Run `action`, retrying per `policy`.

    Structurally recursive on `remaining`, so it terminates by construction:
    each attempt consumes exactly one. The last attempt's result is returned
    as-is — a failing status is *returned*, not raised, because whether a `503`
    is an error is the caller's call; a connection error on the last attempt is
    re-raised, because there is no response to return.

    `attempt` counts from `1` and is only used to size the backoff. -/
private def go (p : RetryPolicy) (action : IO Response) :
    Nat → Nat → IO Response
  | 0,             _       => action              -- unreachable; `run` passes ≥ 1
  | 1,             _       => action              -- last attempt: whatever it gives
  | (remaining+1), attempt => do
    match ← (action.toBaseIO) with
    | .ok resp =>
      if shouldRetryResponse p resp then
        IO.sleep (← delayBefore p attempt (some resp)).toUInt32
        go p action remaining (attempt + 1)
      else
        pure resp
    | .error e =>
      if p.retryOnError then
        IO.sleep (← delayBefore p attempt none).toUInt32
        go p action remaining (attempt + 1)
      else
        throw e

/-- Run `action` under a retry policy. -/
def withRetry (p : RetryPolicy := {}) (action : IO Response) : IO Response :=
  go p action (max p.maxAttempts 1) 1

/-- Perform a request with redirects and retries.

    The whole request is retried, redirects included, since a redirect chain
    that failed partway is not resumable. -/
def executeWithRetry (p : RetryPolicy := {}) (req : Request) (maxRedirects : Nat := 10) :
    IO Response :=
  withRetry p (executeWithRedirects maxRedirects req)

end Network.HTTP.Client
