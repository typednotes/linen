/-
  Tests for `Linen.Network.OAuth2.Experiment.Grants` — a pure re-export
  facade over the five grant-type modules (see the module's own
  doc-comment). This smoke-tests that each re-exported name actually
  resolves through the facade namespace and behaves exactly like its
  submodule counterpart; the submodules' own test files already exercise
  the underlying behaviour in depth.
-/
import Linen.Network.OAuth2.Experiment.Grants
import Linen.Network.URI

open Network.URI
open Network.OAuth2.Experiment.Types
open Network.OAuth2.Experiment.Flows.TokenRequest
open Network.OAuth2.Experiment.Grants

namespace Tests.Network.OAuth2.Experiment.Grants

-- `AuthorizationCodeApplication`/`mkAuthorizationRequestParam` resolve
-- through the facade.
private def redirectUri : Network.URI.URI := (parseURI "https://client.example.com/cb").get!

private def acApp : AuthorizationCodeApplication :=
  { acName := "sample"
    acClientId := ⟨"cid"⟩
    acClientSecret := ⟨"csecret"⟩
    acScope := Data.Set'.fromList [Scope.mk "openid"]
    acRedirectUri := redirectUri
    acAuthorizeState := ⟨"st8"⟩
    acAuthorizeRequestExtraParams := Data.Map.empty
    acClientAuthenticationMethod := .ClientSecretBasic }

#guard (mkAuthorizationRequestParam acApp).arResponseType == .Code

-- `ClientCredentialsApplication`/`ClientCredentialsTokenRequest` resolve.
private def ccApp : ClientCredentialsApplication :=
  { ccClientId := ⟨"cid"⟩
    ccClientSecret := ⟨"csecret"⟩
    ccName := "sample"
    ccScope := Data.Set'.empty
    ccTokenRequestExtraParams := Data.Map.empty
    ccClientAuthenticationMethod := .ClientSecretBasic }

#guard (mkTokenRequestParam ccApp {} : ClientCredentialsTokenRequest).trGrantType == .GTClientCredentials

-- `DeviceAuthorizationApplication`/`mkDeviceAuthorizationRequestParam` resolve.
private def daApp : DeviceAuthorizationApplication :=
  { daName := "sample"
    daClientId := ⟨"cid"⟩
    daClientSecret := ⟨"csecret"⟩
    daScope := Data.Set'.empty
    daAuthorizationRequestExtraParam := Data.Map.empty
    daAuthorizationRequestAuthenticationMethod := .ClientSecretBasic }

#guard (mkDeviceAuthorizationRequestParam daApp).darClientId == none

-- `JwtBearerApplication`/`JwtBearerTokenRequest` resolve.
private def jbApp : JwtBearerApplication := { jbName := "sample", jbJwtAssertion := "jwt".toUTF8 }

#guard (mkTokenRequestParam jbApp {} : JwtBearerTokenRequest).trGrantType == .GTJwtBearer

-- `ResourceOwnerPasswordApplication`/`PasswordTokenRequest` resolve.
private def ropApp : ResourceOwnerPasswordApplication :=
  { ropClientId := ⟨"cid"⟩
    ropClientSecret := ⟨"csecret"⟩
    ropName := "sample"
    ropScope := Data.Set'.empty
    ropUserName := ⟨"alice"⟩
    ropPassword := ⟨"pw"⟩
    ropTokenRequestExtraParams := Data.Map.empty
    ropClientAuthenticationMethod := .ClientSecretBasic }

#guard (mkTokenRequestParam ropApp {} : PasswordTokenRequest).trGrantType == .GTPassword

end Tests.Network.OAuth2.Experiment.Grants
