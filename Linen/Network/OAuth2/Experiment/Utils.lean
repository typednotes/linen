/-
  Linen.Network.OAuth2.Experiment.Utils — small text/URI/param helpers

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Utils` (see
  `docs/imports/hoauth2/dependencies.md`).

  ## Substitutions
  Upstream's `tlToBS`/`bs8ToLazyText` are pure `Text`/lazy-`Text`/
  `ByteString` conversion boilerplate specific to Haskell's multiple string
  representations; `linen` uses a single `String` type throughout, so those
  two have no analogue and are dropped. `Data.Map.Strict` is `linen`'s own
  `Linen.Data.Map`, and `URI.ByteString`'s `URI`/`serializeURIRef'` are
  `Linen.Network.URI`'s `URI`/`uriToString`.
-/

import Linen.Network.URI
import Linen.Data.Map

namespace Network.OAuth2.Experiment.Utils

/-- A flat query-parameter list: key/value pairs. -/
abbrev QueryParams := List (String × String)

/-- Merge a list of param maps into a flat association list, earlier maps
    winning on key clashes (Haskell's `Map.unions`, which is left-biased).

    $$\text{unionMapsToQueryParams} : [\text{Map String String}] \to \text{QueryParams}$$ -/
def unionMapsToQueryParams (maps : List (Data.Map String String)) : QueryParams :=
  (maps.foldl Data.Map.union Data.Map.empty).toList'

/-- Render a `URI` as its text form.

    $$\text{uriToText} : \text{URI} \to \text{String}$$ -/
def uriToText (uri : Network.URI.URI) : String :=
  Network.URI.uriToString Network.URI.defaultUserInfoMap uri

end Network.OAuth2.Experiment.Utils
