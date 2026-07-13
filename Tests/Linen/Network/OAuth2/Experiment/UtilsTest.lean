/-
  Tests for `Linen.Network.OAuth2.Experiment.Utils`.
-/
import Linen.Network.OAuth2.Experiment.Utils

open Network.OAuth2.Experiment.Utils

namespace Tests.Network.OAuth2.Experiment.Utils

-- Earlier maps win on key clashes, matching Haskell's left-biased `Map.unions`.
#guard
  unionMapsToQueryParams
    [ Data.Map.fromList [("a", "1"), ("b", "2")]
    , Data.Map.fromList [("b", "conflict"), ("c", "3")] ]
    == [("a", "1"), ("b", "2"), ("c", "3")]

#guard unionMapsToQueryParams [] == []

-- `uriToText` renders a parsed URI back to text.
#guard
  (match Network.URI.parseURI "https://example.com/a?b=1" with
   | some uri => uriToText uri
   | none => "parse failed")
    == "https://example.com/a?b=1"

/-! ### Signatures -/

example : List (Data.Map String String) → QueryParams := unionMapsToQueryParams
example : Network.URI.URI → String := uriToText

end Tests.Network.OAuth2.Experiment.Utils
