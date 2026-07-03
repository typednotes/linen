/-
  Linen.Network.WebApp.Server.Date — HTTP date header caching

  Uses AutoUpdate to cache the formatted HTTP date header value,
  updated once per second. Avoids formatting the date on every response.

  ## Guarantees
  - Updated at most once per second (amortized O(1) per request)

  ## Note on date precision

  Lean 4 core has no portable wall-clock/epoch-time accessor, only
  `IO.monoNanosNow` (monotonic, not calendar time). This module preserves
  Hale's own honest placeholder — an epoch-seconds string derived from the
  monotonic clock — rather than dressing it up with fake-precision RFC 7231
  calendar formatting that the underlying clock cannot actually back, matching
  this project's established convention (e.g. `Data.Time.Clock.getCurrentTime`,
  `PostgREST.Logger`).
-/
import Linen.Control.AutoUpdate

namespace Network.WebApp.Server

/-- The type of the Date header value (formatted HTTP date string). -/
abbrev GMTDate := String

/-- Get the current date placeholder for HTTP headers.
    $$\text{getCurrentGMTDate} : \text{IO GMTDate}$$ -/
private def getCurrentGMTDate : IO GMTDate := do
  let now ← IO.monoNanosNow
  let secs := now / 1000000000
  return s!"Date: epoch {secs}"

/-- Create a cached date getter using AutoUpdate.
    The returned IO action retrieves the current cached date string.
    The cache is updated once per second.
    $$\text{withDateCache} : (\text{IO}(\text{GMTDate}) \to \text{IO}\ \alpha) \to \text{IO}\ \alpha$$ -/
def withDateCache (action : IO GMTDate → IO α) : IO α := do
  let au ← Control.mkAutoUpdate {
    updateFreq := 1000000  -- 1 second in microseconds
    updateAction := getCurrentGMTDate
  }
  let result ← action au.get
  au.stop
  return result

end Network.WebApp.Server
