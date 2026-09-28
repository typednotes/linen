/-
  Tests for `Linen.PostgREST.Response`.
-/
import Linen.PostgREST.Response

open PostgREST.Response
open PostgREST.MediaType

namespace Tests.PostgREST.Response

/-! ### `contentRangeHeader` -/

#guard contentRangeHeader 0 25 (some 100) == "0-24/100"
#guard contentRangeHeader 0 25 none == "0-24/*"
#guard contentRangeHeader 0 0 (some 100) == "*/100"
#guard contentRangeHeader 0 0 none == "*/*"
#guard contentRangeHeader 10 5 (some 20) == "10-14/20"

/-! ### `readHeaders` -/

#guard readHeaders .applicationJSON 0 25 (some 100) ==
  [ ("Content-Type", "application/json; charset=utf-8"), ("Content-Range", "0-24/100"), ("Range-Unit", "items") ]
#guard readHeaders .textCSV 0 25 none ==
  [ ("Content-Type", "text/csv; charset=utf-8"), ("Content-Range", "0-24/*"), ("Range-Unit", "items") ]

/-! ### `mutateHeaders` -/

#guard mutateHeaders .applicationJSON none == [ ("Content-Type", "application/json; charset=utf-8") ]
#guard mutateHeaders .applicationJSON (some "/items?id=eq.1") ==
  [ ("Content-Type", "application/json; charset=utf-8"), ("Location", "/items?id=eq.1") ]

/-! ### `readStatus` -/

#guard readStatus 0 0 (some 0) == 200
#guard readStatus 0 0 (some 100) == 416
#guard readStatus 0 100 (some 100) == 200
#guard readStatus 0 50 (some 100) == 206
#guard readStatus 50 50 (some 100) == 206
#guard readStatus 0 0 none == 200
#guard readStatus 0 25 none == 206

/-! ### `readStatus_valid` -/

example (offset count : Nat) (total : Option Nat) :
    100 ≤ readStatus offset count total ∧ readStatus offset count total ≤ 599 :=
  readStatus_valid offset count total

end Tests.PostgREST.Response
