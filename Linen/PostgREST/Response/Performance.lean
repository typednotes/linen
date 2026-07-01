/-
  PostgREST.Response.Performance — Performance timing headers

  ## Haskell source
  - `PostgREST.Response.Performance` (postgrest package)
-/

namespace PostgREST.Response.Performance

/-- The Server-Timing header name. -/
def serverTimingHeader : String := "Server-Timing"

/-- Format a Server-Timing header value.
    Format: `"total;dur=123.4"` -/
def serverTimingValue (totalMs : Nat) : String :=
  s!"total;dur={totalMs}"

/-- Format timing headers for a response. -/
def timingHeaders (totalMs : Nat) (planMs : Option Nat := none)
    (execMs : Option Nat := none) : List (String × String) :=
  let parts := [s!"total;dur={totalMs}"] ++
    (match planMs with | some ms => [s!"plan;dur={ms}"] | none => []) ++
    (match execMs with | some ms => [s!"exec;dur={ms}"] | none => [])
  [(serverTimingHeader, ", ".intercalate parts)]

end PostgREST.Response.Performance
