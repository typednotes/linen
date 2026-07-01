/-
  PostgREST.Response — HTTP response construction

  Transforms SQL query results into HTTP responses with appropriate
  status codes, headers (Content-Range, Location, etc.), and bodies.

  ## Haskell source
  - `PostgREST.Response` (postgrest package)
-/

import Linen.PostgREST.MediaType

namespace PostgREST.Response

open PostgREST.MediaType

-- ────────────────────────────────────────────────────────────────────
-- Response construction helpers
-- ────────────────────────────────────────────────────────────────────

/-- Build a Content-Range header value.
    Format: `"offset-end/total"` or `"offset-end/*"` -/
def contentRangeHeader (offset count : Nat) (total : Option Nat) : String :=
  if count == 0 then
    match total with
    | some t => s!"*/{t}"
    | none => "*/*"
  else
    let endIdx := offset + count - 1
    match total with
    | some t => s!"{offset}-{endIdx}/{t}"
    | none => s!"{offset}-{endIdx}/*"

/-- Build headers for a successful read response. -/
def readHeaders (mediaType : MediaType) (offset count : Nat)
    (total : Option Nat) : List (String × String) :=
  [ ("Content-Type", mediaType.toContentType)
  , ("Content-Range", contentRangeHeader offset count total)
  , ("Range-Unit", "items") ]

/-- Build headers for a successful mutate response. -/
def mutateHeaders (mediaType : MediaType) (location : Option String)
    : List (String × String) :=
  let base := [("Content-Type", mediaType.toContentType)]
  match location with
  | some loc => base ++ [("Location", loc)]
  | none => base

/-- Determine the HTTP status code for a read response. -/
def readStatus (offset count : Nat) (total : Option Nat) : Nat :=
  match total with
  | some t =>
    if count == 0 then
      if t == 0 then 200 else 416  -- Range Not Satisfiable
    else if offset == 0 && count >= t then 200  -- OK (full result)
    else 206  -- Partial Content
  | none =>
    if count == 0 then 200 else 206

-- ────────────────────────────────────────────────────────────────────
-- Status validity theorem
-- ────────────────────────────────────────────────────────────────────

/-- `readStatus` always returns a valid HTTP status code (100-599).
    $$\forall o\ c\ t,\; 100 \leq \text{readStatus}(o, c, t) \leq 599$$ -/
theorem readStatus_valid (offset count : Nat) (total : Option Nat) :
    100 ≤ readStatus offset count total ∧ readStatus offset count total ≤ 599 := by
  simp [readStatus]
  split <;> rename_i t
  · split
    · split <;> omega
    · split <;> omega
  · split <;> omega

end PostgREST.Response
