/-
  Tests for `Linen.CDP.Domains.Fetch`.
-/
import Linen.CDP.Domains.Fetch

open CDP.Domains.Fetch
open CDP.Domains
open CDP.Internal.Utils (Command Event)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.Fetch

-- ── Types ──

#guard decodeAs "\"Request\"" (α := RequestStage) = .ok .request
#guard decodeAs "\"Response\"" (α := RequestStage) = .ok .response
#guard encode (ToJSON.toJSON RequestStage.request) = "\"Request\""

#guard decodeAs "{}" (α := RequestPattern)
  = .ok { urlPattern := none, resourceType := none, requestStage := none }
#guard decodeAs "{\"urlPattern\": \"*.png\", \"resourceType\": \"Image\", \"requestStage\": \"Request\"}"
    (α := RequestPattern)
  = .ok { urlPattern := some "*.png"
        , resourceType := some .image
        , requestStage := some .request }
#guard encode (ToJSON.toJSON ({ urlPattern := some "*.png" } : RequestPattern))
  = "{\"urlPattern\":\"*.png\"}"

#guard decodeAs "{\"name\": \"Content-Type\", \"value\": \"text/html\"}" (α := HeaderEntry)
  = .ok { name := "Content-Type", value := "text/html" }
#guard encode (ToJSON.toJSON ({ name := "n", value := "v" } : HeaderEntry))
  = "{\"name\":\"n\",\"value\":\"v\"}"

#guard decodeAs "\"Server\"" (α := AuthChallengeSource) = .ok .server
#guard encode (ToJSON.toJSON AuthChallengeSource.proxy) = "\"Proxy\""

#guard decodeAs "{\"origin\": \"https://x\", \"scheme\": \"basic\", \"realm\": \"\"}" (α := AuthChallenge)
  = .ok { source := none, origin := "https://x", scheme := "basic", realm := "" }
#guard decodeAs
    "{\"source\": \"Server\", \"origin\": \"https://x\", \"scheme\": \"basic\", \"realm\": \"r\"}"
    (α := AuthChallenge)
  = .ok { source := some .server, origin := "https://x", scheme := "basic", realm := "r" }

#guard decodeAs "\"ProvideCredentials\"" (α := AuthChallengeResponseResponse)
  = .ok .provideCredentials

#guard decodeAs
    "{\"response\": \"ProvideCredentials\", \"username\": \"u\", \"password\": \"p\"}"
    (α := AuthChallengeResponse)
  = .ok { response := .provideCredentials, username := some "u", password := some "p" }
#guard encode (ToJSON.toJSON ({ response := .default } : AuthChallengeResponse))
  = "{\"response\":\"Default\"}"

-- ── Events ──

/-- A minimal `Network.Request` value shared by the event tests below. -/
def sampleRequest : DOMPageNetworkEmulationSecurity.Network.Request :=
  { url := "https://example.com"
    method := "GET"
    headers := []
    initialPriority := .medium
    referrerPolicy := .noReferrer }

def sampleRequestJson : String :=
  "{\"url\": \"https://example.com\", \"method\": \"GET\", \"headers\": [], \
   \"initialPriority\": \"Medium\", \"referrerPolicy\": \"no-referrer\"}"

#guard match decodeAs sampleRequestJson (α := DOMPageNetworkEmulationSecurity.Network.Request) with
  | .ok v => v == sampleRequest
  | .error _ => false

#guard Event.eventName (α := RequestPaused) = "Fetch.requestPaused"
#guard match decodeAs
    ("{\"requestId\": \"1\", \"request\": " ++ sampleRequestJson
      ++ ", \"frameId\": \"f1\", \"resourceType\": \"Document\"}")
    (α := RequestPaused) with
  | .ok v => v ==
      { requestId := "1", request := sampleRequest, frameId := "f1"
        resourceType := .document }
  | .error _ => false

#guard Event.eventName (α := AuthRequired) = "Fetch.authRequired"
#guard match decodeAs
    ("{\"requestId\": \"1\", \"request\": " ++ sampleRequestJson
      ++ ", \"frameId\": \"f1\", \"resourceType\": \"Document\", \"authChallenge\": "
      ++ "{\"origin\": \"https://x\", \"scheme\": \"basic\", \"realm\": \"\"}}")
    (α := AuthRequired) with
  | .ok v => v ==
      { requestId := "1", request := sampleRequest, frameId := "f1"
        resourceType := .document
        authChallenge := { source := none, origin := "https://x", scheme := "basic", realm := "" } }
  | .error _ => false

-- ── Commands ──

#guard encode (ToJSON.toJSON (PDisable.mk)) = "null"
#guard Command.commandName (PDisable.mk) = "Fetch.disable"

#guard encode (ToJSON.toJSON ({ patterns := none, handleAuthRequests := none } : PEnable)) = "{}"
#guard encode (ToJSON.toJSON
    ({ patterns := some [{ urlPattern := some "*" }], handleAuthRequests := some true } : PEnable))
  = "{\"patterns\":[{\"urlPattern\":\"*\"}],\"handleAuthRequests\":true}"
#guard Command.commandName ({ patterns := none, handleAuthRequests := none } : PEnable) = "Fetch.enable"

#guard encode (ToJSON.toJSON ({ requestId := "1", errorReason := .failed } : PFailRequest))
  = "{\"requestId\":\"1\",\"errorReason\":\"Failed\"}"
#guard Command.commandName ({ requestId := "1", errorReason := .failed } : PFailRequest)
  = "Fetch.failRequest"

#guard encode (ToJSON.toJSON ({ requestId := "1", responseCode := 200 } : PFulfillRequest))
  = "{\"requestId\":\"1\",\"responseCode\":200}"
#guard encode (ToJSON.toJSON
    ({ requestId := "1", responseCode := 200, body := some "aGk=" } : PFulfillRequest))
  = "{\"requestId\":\"1\",\"responseCode\":200,\"body\":\"aGk=\"}"
#guard Command.commandName ({ requestId := "1", responseCode := 200 } : PFulfillRequest)
  = "Fetch.fulfillRequest"

#guard encode (ToJSON.toJSON ({ requestId := "1" } : PContinueRequest)) = "{\"requestId\":\"1\"}"
#guard encode (ToJSON.toJSON ({ requestId := "1", url := some "https://y" } : PContinueRequest))
  = "{\"requestId\":\"1\",\"url\":\"https:\\/\\/y\"}"
#guard Command.commandName ({ requestId := "1" } : PContinueRequest) = "Fetch.continueRequest"

#guard encode (ToJSON.toJSON
    ({ requestId := "1", authChallengeResponse := { response := .cancelAuth } }
      : PContinueWithAuth))
  = "{\"requestId\":\"1\",\"authChallengeResponse\":{\"response\":\"CancelAuth\"}}"
#guard Command.commandName
    ({ requestId := "1", authChallengeResponse := { response := .cancelAuth } }
      : PContinueWithAuth)
  = "Fetch.continueWithAuth"

#guard encode (ToJSON.toJSON ({ requestId := "1" } : PContinueResponse)) = "{\"requestId\":\"1\"}"
#guard encode (ToJSON.toJSON
    ({ requestId := "1", responseCode := some 200 } : PContinueResponse))
  = "{\"requestId\":\"1\",\"responseCode\":200}"
#guard Command.commandName ({ requestId := "1" } : PContinueResponse) = "Fetch.continueResponse"

#guard encode (ToJSON.toJSON ({ requestId := "1" } : PGetResponseBody)) = "{\"requestId\":\"1\"}"
#guard Command.commandName ({ requestId := "1" } : PGetResponseBody) = "Fetch.getResponseBody"
#guard decodeAs "{\"body\": \"aGk=\", \"base64Encoded\": true}" (α := GetResponseBody)
  = .ok { body := "aGk=", base64Encoded := true }

#guard encode (ToJSON.toJSON ({ requestId := "1" } : PTakeResponseBodyAsStream))
  = "{\"requestId\":\"1\"}"
#guard Command.commandName ({ requestId := "1" } : PTakeResponseBodyAsStream)
  = "Fetch.takeResponseBodyAsStream"
#guard decodeAs "{\"stream\": \"s1\"}" (α := TakeResponseBodyAsStream) = .ok { stream := "s1" }

end Tests.CDP.Domains.Fetch
