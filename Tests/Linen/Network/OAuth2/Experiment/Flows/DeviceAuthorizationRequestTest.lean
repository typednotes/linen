/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest`.
-/
import Linen.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest

open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
open Data.Json (Value FromJSON)

namespace Tests.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest

private instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .error a, .error b => a == b
    | .ok a, .ok b => a == b
    | _, _ => false

-- `DeviceCode`'s `FromJSON` delegates straight to `String`.
#guard (FromJSON.parseJSON (Value.string "dc123") : Except String DeviceCode) == .ok ⟨"dc123"⟩
#guard Data.Map.toList' (toQueryParam (DeviceCode.mk "dc123")) == [("device_code", "dc123")]

-- RFC 8628 §3.2 response, using the RFC's own `verification_uri` field.
#guard
  (match FromJSON.parseJSON
      (Value.object
        [ ("device_code", .string "dc1")
        , ("user_code", .string "WDJB-MJHT")
        , ("verification_uri", .string "https://example.com/device")
        , ("expires_in", .number 1800)
        , ("interval", .number 5) ])
      with
   | Except.ok (r : DeviceAuthorizationResponse) =>
     (r.deviceCode, r.userCode, r.expiresIn, r.interval)
   | Except.error _ => (⟨""⟩, "", 0, none))
    == (DeviceCode.mk "dc1", "WDJB-MJHT", 1800, some 5)

-- Providers sending `verification_url` instead of `verification_uri` still parse.
#guard
  (match FromJSON.parseJSON
      (Value.object
        [ ("device_code", .string "dc2")
        , ("user_code", .string "ABCD-EFGH")
        , ("verification_url", .string "https://example.com/device")
        , ("expires_in", .number 900) ])
      with
   | Except.ok (r : DeviceAuthorizationResponse) => Network.URI.uriToString id r.verificationUri
   | Except.error e => e)
    == "https://example.com/device"

-- `DeviceAuthorizationRequestParam`: scope + optional client id + extra params.
private def sampleParam : DeviceAuthorizationRequestParam :=
  { darScope := Data.Set'.fromList [Scope.mk "profile"]
    darClientId := some ⟨"client1"⟩
    darExtraParams := Data.Map.empty }

#guard Data.Map.toList' (toQueryParam sampleParam) ==
  [("client_id", "client1"), ("scope", "profile")]

end Tests.Network.OAuth2.Experiment.Flows.DeviceAuthorizationRequest
