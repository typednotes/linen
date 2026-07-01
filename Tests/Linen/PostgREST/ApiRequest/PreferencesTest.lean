/-
  Tests for `Linen.PostgREST.ApiRequest.Preferences`.
-/
import Linen.PostgREST.ApiRequest.Preferences

open PostgREST.ApiRequest.Preferences

namespace Tests.PostgREST.ApiRequest.Preferences

/-! ### `ToString` instances -/

#guard toString PreferCount.exact == "count=exact"
#guard toString PreferCount.none_ == ""
#guard toString PreferReturn.representation == "return=representation"
#guard toString PreferReturn.headersOnly == "return=headers-only"
#guard toString PreferResolution.mergeDuplicates == "resolution=merge-duplicates"
#guard toString PreferTransaction.rollback == "tx=rollback"
#guard toString PreferMissing.default_ == "missing=default"
#guard toString PreferHandling.lenient == "handling=lenient"

/-! ### Defaults -/

#guard (default : Preferences) == {}
#guard (default : Preferences).preferCount == .none_
#guard (default : Preferences).preferMaxAffected == none

/-! ### `parsePreferences` — single tokens -/

#guard (parsePreferences ["count=exact"]).preferCount == .exact
#guard (parsePreferences ["return=minimal"]).preferReturn == .minimal
#guard (parsePreferences ["resolution=ignore-duplicates"]).preferResolution == .ignoreDuplicates
#guard (parsePreferences ["tx=commit"]).preferTransaction == .commit
#guard (parsePreferences ["missing=default"]).preferMissing == .default_
#guard (parsePreferences ["handling=strict"]).preferHandling == .strict
#guard (parsePreferences ["max-affected=5"]).preferMaxAffected == some 5

/-! ### Unknown / malformed tokens are ignored -/

#guard (parsePreferences ["bogus=1"]) == default
#guard (parsePreferences ["max-affected=abc"]).preferMaxAffected == none
#guard (parsePreferences []) == default

/-! ### Comma- and semicolon-separated tokens within one header line -/

#guard (parsePreferences ["count=exact,return=minimal"]) ==
  { preferCount := .exact, preferReturn := .minimal : Preferences }
#guard (parsePreferences ["count=exact;return=minimal"]) ==
  { preferCount := .exact, preferReturn := .minimal : Preferences }

/-! ### Multiple header lines accumulate -/

#guard (parsePreferences ["count=exact", "return=minimal", "tx=rollback"]) ==
  { preferCount := .exact, preferReturn := .minimal,
    preferTransaction := .rollback : Preferences }

/-! ### Whitespace around tokens is trimmed -/

#guard (parsePreferences [" count=exact , return=minimal "]) ==
  { preferCount := .exact, preferReturn := .minimal : Preferences }

end Tests.PostgREST.ApiRequest.Preferences
