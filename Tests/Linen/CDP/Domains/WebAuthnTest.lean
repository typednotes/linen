/-
  Tests for `Linen.CDP.Domains.WebAuthn`.
-/
import Linen.CDP.Domains.WebAuthn

open CDP.Domains.WebAuthn
open CDP.Internal.Utils (Command)
open Data.Json (ToJSON FromJSON)
open Data.Json.Decode (decodeAs)
open Data.Json.Encode (encode)

namespace Tests.CDP.Domains.WebAuthn

#guard decodeAs "\"u2f\"" (α := AuthenticatorProtocol) = .ok .u2f
#guard encode (ToJSON.toJSON AuthenticatorProtocol.ctap2) = "\"ctap2\""

#guard decodeAs "\"ctap2_1\"" (α := Ctap2Version) = .ok .ctap2_1
#guard encode (ToJSON.toJSON Ctap2Version.ctap2_0) = "\"ctap2_0\""

#guard decodeAs "\"cable\"" (α := AuthenticatorTransport) = .ok .cable
#guard encode (ToJSON.toJSON AuthenticatorTransport.internal) = "\"internal\""

#guard decodeAs "{\"protocol\": \"ctap2\", \"transport\": \"usb\"}" (α := VirtualAuthenticatorOptions)
  = .ok { protocol := .ctap2, transport := .usb }
#guard encode (ToJSON.toJSON ({ protocol := .ctap2, transport := .usb } : VirtualAuthenticatorOptions))
  = "{\"protocol\":\"ctap2\",\"transport\":\"usb\"}"
#guard encode (ToJSON.toJSON
    ({ protocol := .u2f, transport := .internal, hasResidentKey := some true } : VirtualAuthenticatorOptions))
  = "{\"protocol\":\"u2f\",\"transport\":\"internal\",\"hasResidentKey\":true}"

#guard decodeAs
    "{\"credentialId\": \"c1\", \"isResidentCredential\": true, \"privateKey\": \"k\", \"signCount\": 3}"
    (α := Credential)
  = .ok { credentialId := "c1", isResidentCredential := true, privateKey := "k", signCount := 3 }
#guard encode (ToJSON.toJSON
    ({ credentialId := "c1", isResidentCredential := true, privateKey := "k", signCount := 3 } : Credential))
  = "{\"credentialId\":\"c1\",\"isResidentCredential\":true,\"privateKey\":\"k\",\"signCount\":3}"
#guard encode (ToJSON.toJSON
    ({ credentialId := "c1", isResidentCredential := false, rpId := some "example.com", privateKey := "k",
       userHandle := some "u", signCount := 0, largeBlob := some "b" } : Credential))
  = "{\"credentialId\":\"c1\",\"isResidentCredential\":false,\"rpId\":\"example.com\",\"privateKey\":\"k\"," ++
    "\"userHandle\":\"u\",\"signCount\":0,\"largeBlob\":\"b\"}"

#guard encode (ToJSON.toJSON ({} : PEnable)) = "{}"
#guard encode (ToJSON.toJSON ({ enableUI := some true } : PEnable)) = "{\"enableUI\":true}"
#guard Command.commandName ({} : PEnable) = "WebAuthn.enable"

#guard encode (ToJSON.toJSON ({} : PDisable)) = "null"
#guard Command.commandName ({} : PDisable) = "WebAuthn.disable"

#guard encode (ToJSON.toJSON
    ({ options := { protocol := .ctap2, transport := .usb } } : PAddVirtualAuthenticator))
  = "{\"options\":{\"protocol\":\"ctap2\",\"transport\":\"usb\"}}"
#guard Command.commandName ({ options := { protocol := .ctap2, transport := .usb } } : PAddVirtualAuthenticator)
  = "WebAuthn.addVirtualAuthenticator"
#guard decodeAs "{\"authenticatorId\": \"auth1\"}" (α := AddVirtualAuthenticator)
  = .ok { authenticatorId := "auth1" }

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1" } : PRemoveVirtualAuthenticator))
  = "{\"authenticatorId\":\"auth1\"}"
#guard Command.commandName ({ authenticatorId := "auth1" } : PRemoveVirtualAuthenticator)
  = "WebAuthn.removeVirtualAuthenticator"

#guard encode (ToJSON.toJSON
    ({ authenticatorId := "auth1"
       credential :=
         { credentialId := "c1", isResidentCredential := true, privateKey := "k", signCount := 0 } } :
      PAddCredential))
  = "{\"authenticatorId\":\"auth1\",\"credential\":{\"credentialId\":\"c1\"," ++
    "\"isResidentCredential\":true,\"privateKey\":\"k\",\"signCount\":0}}"
#guard Command.commandName
    ({ authenticatorId := "auth1"
       credential :=
         { credentialId := "c1", isResidentCredential := true, privateKey := "k", signCount := 0 } } :
      PAddCredential)
  = "WebAuthn.addCredential"

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1", credentialId := "c1" } : PGetCredential))
  = "{\"authenticatorId\":\"auth1\",\"credentialId\":\"c1\"}"
#guard Command.commandName ({ authenticatorId := "auth1", credentialId := "c1" } : PGetCredential)
  = "WebAuthn.getCredential"
#guard decodeAs
    ("{\"credential\": {\"credentialId\": \"c1\", \"isResidentCredential\": true, \"privateKey\": \"k\"," ++
     " \"signCount\": 0}}")
    (α := GetCredential)
  = .ok { credential := { credentialId := "c1", isResidentCredential := true, privateKey := "k", signCount := 0 } }

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1" } : PGetCredentials))
  = "{\"authenticatorId\":\"auth1\"}"
#guard Command.commandName ({ authenticatorId := "auth1" } : PGetCredentials) = "WebAuthn.getCredentials"
#guard decodeAs "{\"credentials\": []}" (α := GetCredentials) = .ok { credentials := [] }

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1", credentialId := "c1" } : PRemoveCredential))
  = "{\"authenticatorId\":\"auth1\",\"credentialId\":\"c1\"}"
#guard Command.commandName ({ authenticatorId := "auth1", credentialId := "c1" } : PRemoveCredential)
  = "WebAuthn.removeCredential"

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1" } : PClearCredentials))
  = "{\"authenticatorId\":\"auth1\"}"
#guard Command.commandName ({ authenticatorId := "auth1" } : PClearCredentials) = "WebAuthn.clearCredentials"

#guard encode (ToJSON.toJSON ({ authenticatorId := "auth1", isUserVerified := true } : PSetUserVerified))
  = "{\"authenticatorId\":\"auth1\",\"isUserVerified\":true}"
#guard Command.commandName ({ authenticatorId := "auth1", isUserVerified := true } : PSetUserVerified)
  = "WebAuthn.setUserVerified"

#guard encode (ToJSON.toJSON
    ({ authenticatorId := "auth1", enabled := false } : PSetAutomaticPresenceSimulation))
  = "{\"authenticatorId\":\"auth1\",\"enabled\":false}"
#guard Command.commandName ({ authenticatorId := "auth1", enabled := false } : PSetAutomaticPresenceSimulation)
  = "WebAuthn.setAutomaticPresenceSimulation"

end Tests.CDP.Domains.WebAuthn
