/-
  Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest — Device
  Authorization Grant request/response types

  Port of `hoauth2`'s
  `Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest` (see
  `docs/imports/hoauth2/dependencies.md`), module #12: RFC 8628 §3.1's
  device-authorization request parameters and §3.2's response.

  ## Substitutions
  - `Data.Set`/`Data.Map.Strict` are `Linen.Data.Set`/`Linen.Data.Map`;
    `Data.Text.Lazy` is `linen`'s single `String` type (see
    `Network.OAuth2.Experiment.Types`'s doc-comment for the same
    substitutions).
  - `URI.ByteString.URI` is `Linen.Network.URI.URI`; the `uri-bytestring-aeson`
    `FromJSON URI` instance used here is `Linen.Network.OAuth2.Internal`'s
    `FromJSON Network.URI.URI` (already ported).
  - `newtype DeviceCode = DeviceCode Text deriving newtype (FromJSON)`
    delegates its `FromJSON` straight to the wrapped `String`'s; ported as
    an explicit `FromJSON DeviceCode` instance doing the same.
-/

import Linen.Network.OAuth2.Experiment.Types
import Linen.Network.OAuth2.Internal
import Linen.Data.Json.Types
import Linen.Data.Map
import Linen.Data.Set

namespace Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest

open Network.OAuth2.Experiment.Types
open Data.Json (Value ToJSON FromJSON)

-- ────────────────────────────────────────────────────────────────────
-- Device Code
-- ────────────────────────────────────────────────────────────────────

/-- The `device_code` value returned by the device-authorization endpoint
    and later sent back to poll the token endpoint, RFC 8628 §3.2. -/
structure DeviceCode where
  unDeviceCode : String
deriving Repr, BEq

/-- Delegates straight to `String`'s parsing, matching upstream's
    `deriving newtype (FromJSON)`. -/
instance : FromJSON DeviceCode where
  parseJSON
    | .string s => .ok ⟨s⟩
    | v => .error s!"expected a string for DeviceCode, got {repr v}"

instance : ToQueryParam DeviceCode where
  toQueryParam c := Data.Map.singleton "device_code" c.unDeviceCode

-- ────────────────────────────────────────────────────────────────────
-- Device Authorization Response
-- ────────────────────────────────────────────────────────────────────

/-- The device-authorization endpoint's successful response, RFC 8628 §3.2. -/
structure DeviceAuthorizationResponse where
  deviceCode : DeviceCode
  userCode : String
  verificationUri : Network.URI.URI
  verificationUriComplete : Option Network.URI.URI
  expiresIn : Int
  interval : Option Int
deriving Repr

instance : FromJSON DeviceAuthorizationResponse where
  parseJSON v := do
    -- Some providers send `verification_url` instead of the RFC's
    -- `verification_uri` (upstream's `t .: "verification_uri" <|> t .:
    -- "verification_url"`).
    let verificationUri ←
      match ← Value.getFieldOpt v "verification_uri" with
      | some vu => pure vu
      | none => Value.getField v "verification_url"
    pure
      { deviceCode := ← Value.getField v "device_code" >>= FromJSON.parseJSON
        userCode := ← Value.getField v "user_code" >>= FromJSON.parseJSON
        verificationUri := ← FromJSON.parseJSON verificationUri
        verificationUriComplete :=
          ← (← Value.getFieldOpt v "verification_uri_complete").mapM FromJSON.parseJSON
        expiresIn := ← Value.getField v "expires_in" >>= FromJSON.parseJSON
        interval := ← (← Value.getFieldOpt v "interval").mapM FromJSON.parseJSON }

-- ────────────────────────────────────────────────────────────────────
-- Device Authorization Request
-- ────────────────────────────────────────────────────────────────────

/-- The parameters of a device-authorization request, RFC 8628 §3.1. -/
structure DeviceAuthorizationRequestParam where
  darScope : Data.Set' Scope
  darClientId : Option ClientId
  darExtraParams : Data.Map String String

instance : ToQueryParam DeviceAuthorizationRequestParam where
  toQueryParam p :=
    Data.Map.union (toQueryParam p.darScope) <|
    Data.Map.union (toQueryParam p.darClientId) p.darExtraParams

end Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
