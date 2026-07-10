/-
  Linen.CDP.Domains.WebAuthn — the `WebAuthn` CDP domain

  Ports `CDP.Domains.WebAuthn` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.CacheStorage`'s docstring: every upstream
  `WebAuthn`/`pWebAuthn...` prefix is dropped since Lean's namespace already
  disambiguates, and upstream's separate smart-constructor functions
  (`pWebAuthnEnable`, ...) are dropped in favour of structure field defaults
  and `{ field := val }` literals. This domain has no events upstream — every
  operation is a command.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.WebAuthn

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

-- ── Types ──

/-- Unique identifier of a virtual authenticator. -/
abbrev AuthenticatorId := String

/-- The transport protocol a virtual authenticator speaks. -/
inductive AuthenticatorProtocol where
  | u2f | ctap2
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthenticatorProtocol where
  parseJSON
    | .string "u2f" => .ok .u2f
    | .string "ctap2" => .ok .ctap2
    | v => .error s!"failed to parse AuthenticatorProtocol: {repr v}"

instance : ToJSON AuthenticatorProtocol where
  toJSON | .u2f => .string "u2f" | .ctap2 => .string "ctap2"

/-- The CTAP2 protocol version a virtual authenticator implements. -/
inductive Ctap2Version where
  | ctap2_0 | ctap2_1
  deriving Repr, BEq, DecidableEq

instance : FromJSON Ctap2Version where
  parseJSON
    | .string "ctap2_0" => .ok .ctap2_0
    | .string "ctap2_1" => .ok .ctap2_1
    | v => .error s!"failed to parse Ctap2Version: {repr v}"

instance : ToJSON Ctap2Version where
  toJSON | .ctap2_0 => .string "ctap2_0" | .ctap2_1 => .string "ctap2_1"

/-- The transport a virtual authenticator communicates over. -/
inductive AuthenticatorTransport where
  | usb | nfc | ble | cable | internal
  deriving Repr, BEq, DecidableEq

instance : FromJSON AuthenticatorTransport where
  parseJSON
    | .string "usb" => .ok .usb
    | .string "nfc" => .ok .nfc
    | .string "ble" => .ok .ble
    | .string "cable" => .ok .cable
    | .string "internal" => .ok .internal
    | v => .error s!"failed to parse AuthenticatorTransport: {repr v}"

instance : ToJSON AuthenticatorTransport where
  toJSON
    | .usb => .string "usb"
    | .nfc => .string "nfc"
    | .ble => .string "ble"
    | .cable => .string "cable"
    | .internal => .string "internal"

/-- Configuration used to create a virtual authenticator. -/
structure VirtualAuthenticatorOptions where
  /-- The protocol the authenticator speaks. -/
  protocol : AuthenticatorProtocol
  /-- Defaults to `ctap2_0`. Ignored if `protocol == u2f`. -/
  ctap2Version : Option Ctap2Version := none
  /-- The transport the authenticator communicates over. -/
  transport : AuthenticatorTransport
  /-- Defaults to `false`. -/
  hasResidentKey : Option Bool := none
  /-- Defaults to `false`. -/
  hasUserVerification : Option Bool := none
  /-- If set to `true`, the authenticator will support the largeBlob
      extension. <https://w3c.github.io/webauthn#largeBlob>. Defaults to
      `false`. -/
  hasLargeBlob : Option Bool := none
  /-- If set to `true`, the authenticator will support the credBlob
      extension.
      <https://fidoalliance.org/specs/fido-v2.1-rd-20201208/fido-client-to-authenticator-protocol-v2.1-rd-20201208.html#sctn-credBlob-extension>.
      Defaults to `false`. -/
  hasCredBlob : Option Bool := none
  /-- If set to `true`, the authenticator will support the minPinLength
      extension.
      <https://fidoalliance.org/specs/fido-v2.1-ps-20210615/fido-client-to-authenticator-protocol-v2.1-ps-20210615.html#sctn-minpinlength-extension>.
      Defaults to `false`. -/
  hasMinPinLength : Option Bool := none
  /-- If set to `true`, tests of user presence will succeed immediately.
      Otherwise, they will not be resolved. Defaults to `true`. -/
  automaticPresenceSimulation : Option Bool := none
  /-- Sets whether User Verification succeeds or fails for an authenticator.
      Defaults to `false`. -/
  isUserVerified : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON VirtualAuthenticatorOptions where
  parseJSON v := do
    .ok
      { protocol := ← Value.getField v "protocol" >>= FromJSON.parseJSON
        ctap2Version := ← (← Value.getFieldOpt v "ctap2Version").mapM FromJSON.parseJSON
        transport := ← Value.getField v "transport" >>= FromJSON.parseJSON
        hasResidentKey := ← (← Value.getFieldOpt v "hasResidentKey").mapM FromJSON.parseJSON
        hasUserVerification := ← (← Value.getFieldOpt v "hasUserVerification").mapM FromJSON.parseJSON
        hasLargeBlob := ← (← Value.getFieldOpt v "hasLargeBlob").mapM FromJSON.parseJSON
        hasCredBlob := ← (← Value.getFieldOpt v "hasCredBlob").mapM FromJSON.parseJSON
        hasMinPinLength := ← (← Value.getFieldOpt v "hasMinPinLength").mapM FromJSON.parseJSON
        automaticPresenceSimulation :=
          ← (← Value.getFieldOpt v "automaticPresenceSimulation").mapM FromJSON.parseJSON
        isUserVerified := ← (← Value.getFieldOpt v "isUserVerified").mapM FromJSON.parseJSON }

instance : ToJSON VirtualAuthenticatorOptions where
  toJSON p := Data.Json.object <|
    [ ("protocol", ToJSON.toJSON p.protocol) ]
    ++ (p.ctap2Version.map fun v => ("ctap2Version", ToJSON.toJSON v)).toList
    ++ [ ("transport", ToJSON.toJSON p.transport) ]
    ++ (p.hasResidentKey.map fun v => ("hasResidentKey", ToJSON.toJSON v)).toList
    ++ (p.hasUserVerification.map fun v => ("hasUserVerification", ToJSON.toJSON v)).toList
    ++ (p.hasLargeBlob.map fun v => ("hasLargeBlob", ToJSON.toJSON v)).toList
    ++ (p.hasCredBlob.map fun v => ("hasCredBlob", ToJSON.toJSON v)).toList
    ++ (p.hasMinPinLength.map fun v => ("hasMinPinLength", ToJSON.toJSON v)).toList
    ++ (p.automaticPresenceSimulation.map fun v => ("automaticPresenceSimulation", ToJSON.toJSON v)).toList
    ++ (p.isUserVerified.map fun v => ("isUserVerified", ToJSON.toJSON v)).toList

/-- A WebAuthn credential stored in a virtual authenticator. -/
structure Credential where
  credentialId : String
  isResidentCredential : Bool
  /-- Relying Party ID the credential is scoped to. Must be set when adding a
      credential. -/
  rpId : Option String := none
  /-- The ECDSA P-256 private key in PKCS#8 format. (Encoded as a base64
      string when passed over JSON.) -/
  privateKey : String
  /-- An opaque byte sequence with a maximum size of 64 bytes mapping the
      credential to a specific user. (Encoded as a base64 string when passed
      over JSON.) -/
  userHandle : Option String := none
  /-- Signature counter. This is incremented by one for each successful
      assertion. See <https://w3c.github.io/webauthn/#signature-counter>. -/
  signCount : Int
  /-- The large blob associated with the credential. See
      <https://w3c.github.io/webauthn/#sctn-large-blob-extension>. (Encoded as
      a base64 string when passed over JSON.) -/
  largeBlob : Option String := none
  deriving Repr, BEq, DecidableEq

instance : FromJSON Credential where
  parseJSON v := do
    .ok
      { credentialId := ← Value.getField v "credentialId" >>= FromJSON.parseJSON
        isResidentCredential := ← Value.getField v "isResidentCredential" >>= FromJSON.parseJSON
        rpId := ← (← Value.getFieldOpt v "rpId").mapM FromJSON.parseJSON
        privateKey := ← Value.getField v "privateKey" >>= FromJSON.parseJSON
        userHandle := ← (← Value.getFieldOpt v "userHandle").mapM FromJSON.parseJSON
        signCount := ← Value.getField v "signCount" >>= FromJSON.parseJSON
        largeBlob := ← (← Value.getFieldOpt v "largeBlob").mapM FromJSON.parseJSON }

instance : ToJSON Credential where
  toJSON p := Data.Json.object <|
    [ ("credentialId", ToJSON.toJSON p.credentialId)
    , ("isResidentCredential", ToJSON.toJSON p.isResidentCredential) ]
    ++ (p.rpId.map fun v => ("rpId", ToJSON.toJSON v)).toList
    ++ [ ("privateKey", ToJSON.toJSON p.privateKey) ]
    ++ (p.userHandle.map fun v => ("userHandle", ToJSON.toJSON v)).toList
    ++ [ ("signCount", ToJSON.toJSON p.signCount) ]
    ++ (p.largeBlob.map fun v => ("largeBlob", ToJSON.toJSON v)).toList

-- ── Commands ──

/-- Parameters of the `WebAuthn.enable` command: enable the WebAuthn domain
    and start intercepting credential storage and retrieval with a virtual
    authenticator. -/
structure PEnable where
  /-- Whether to enable the WebAuthn user interface. Enabling the UI is
      recommended for debugging and demo purposes, as it is closer to the
      real experience. Disabling the UI is recommended for automated
      testing. Supported at the embedder's discretion if UI is available.
      Defaults to `false`. -/
  enableUI : Option Bool := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PEnable where
  toJSON p := Data.Json.object <| (p.enableUI.map fun v => ("enableUI", ToJSON.toJSON v)).toList

instance : Command PEnable where
  Response := Unit
  commandName _ := "WebAuthn.enable"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.disable` command: disable the WebAuthn
    domain. -/
structure PDisable where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PDisable where toJSON _ := .null

instance : Command PDisable where
  Response := Unit
  commandName _ := "WebAuthn.disable"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.addVirtualAuthenticator` command: creates and
    adds a virtual authenticator. -/
structure PAddVirtualAuthenticator where
  options : VirtualAuthenticatorOptions
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAddVirtualAuthenticator where
  toJSON p := Data.Json.object [("options", ToJSON.toJSON p.options)]

/-- Response of the `WebAuthn.addVirtualAuthenticator` command. -/
structure AddVirtualAuthenticator where
  authenticatorId : AuthenticatorId
  deriving Repr, BEq, DecidableEq

instance : FromJSON AddVirtualAuthenticator where
  parseJSON v := do .ok { authenticatorId := ← Value.getField v "authenticatorId" >>= FromJSON.parseJSON }

instance : Command PAddVirtualAuthenticator where
  Response := AddVirtualAuthenticator
  commandName _ := "WebAuthn.addVirtualAuthenticator"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `WebAuthn.removeVirtualAuthenticator` command: removes
    the given authenticator. -/
structure PRemoveVirtualAuthenticator where
  authenticatorId : AuthenticatorId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveVirtualAuthenticator where
  toJSON p := Data.Json.object [("authenticatorId", ToJSON.toJSON p.authenticatorId)]

instance : Command PRemoveVirtualAuthenticator where
  Response := Unit
  commandName _ := "WebAuthn.removeVirtualAuthenticator"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.addCredential` command: adds the credential to
    the specified authenticator. -/
structure PAddCredential where
  authenticatorId : AuthenticatorId
  credential : Credential
  deriving Repr, BEq, DecidableEq

instance : ToJSON PAddCredential where
  toJSON p := Data.Json.object
    [("authenticatorId", ToJSON.toJSON p.authenticatorId), ("credential", ToJSON.toJSON p.credential)]

instance : Command PAddCredential where
  Response := Unit
  commandName _ := "WebAuthn.addCredential"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.getCredential` command: returns a single
    credential stored in the given virtual authenticator that matches the
    credential ID. -/
structure PGetCredential where
  authenticatorId : AuthenticatorId
  credentialId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetCredential where
  toJSON p := Data.Json.object
    [("authenticatorId", ToJSON.toJSON p.authenticatorId), ("credentialId", ToJSON.toJSON p.credentialId)]

/-- Response of the `WebAuthn.getCredential` command. -/
structure GetCredential where
  credential : Credential
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetCredential where
  parseJSON v := do .ok { credential := ← Value.getField v "credential" >>= FromJSON.parseJSON }

instance : Command PGetCredential where
  Response := GetCredential
  commandName _ := "WebAuthn.getCredential"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `WebAuthn.getCredentials` command: returns all the
    credentials stored in the given virtual authenticator. -/
structure PGetCredentials where
  authenticatorId : AuthenticatorId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PGetCredentials where
  toJSON p := Data.Json.object [("authenticatorId", ToJSON.toJSON p.authenticatorId)]

/-- Response of the `WebAuthn.getCredentials` command. -/
structure GetCredentials where
  credentials : List Credential
  deriving Repr, BEq, DecidableEq

instance : FromJSON GetCredentials where
  parseJSON v := do .ok { credentials := ← Value.getField v "credentials" >>= FromJSON.parseJSON }

instance : Command PGetCredentials where
  Response := GetCredentials
  commandName _ := "WebAuthn.getCredentials"
  decodeResponse := FromJSON.parseJSON

/-- Parameters of the `WebAuthn.removeCredential` command: removes a
    credential from the authenticator. -/
structure PRemoveCredential where
  authenticatorId : AuthenticatorId
  credentialId : String
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRemoveCredential where
  toJSON p := Data.Json.object
    [("authenticatorId", ToJSON.toJSON p.authenticatorId), ("credentialId", ToJSON.toJSON p.credentialId)]

instance : Command PRemoveCredential where
  Response := Unit
  commandName _ := "WebAuthn.removeCredential"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.clearCredentials` command: clears all the
    credentials from the specified device. -/
structure PClearCredentials where
  authenticatorId : AuthenticatorId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearCredentials where
  toJSON p := Data.Json.object [("authenticatorId", ToJSON.toJSON p.authenticatorId)]

instance : Command PClearCredentials where
  Response := Unit
  commandName _ := "WebAuthn.clearCredentials"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.setUserVerified` command: sets whether User
    Verification succeeds or fails for an authenticator. The default is
    `true`. -/
structure PSetUserVerified where
  authenticatorId : AuthenticatorId
  isUserVerified : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetUserVerified where
  toJSON p := Data.Json.object
    [("authenticatorId", ToJSON.toJSON p.authenticatorId), ("isUserVerified", ToJSON.toJSON p.isUserVerified)]

instance : Command PSetUserVerified where
  Response := Unit
  commandName _ := "WebAuthn.setUserVerified"
  decodeResponse _ := .ok ()

/-- Parameters of the `WebAuthn.setAutomaticPresenceSimulation` command: sets
    whether tests of user presence will succeed immediately (if `true`) or
    fail to resolve (if `false`) for an authenticator. The default is
    `true`. -/
structure PSetAutomaticPresenceSimulation where
  authenticatorId : AuthenticatorId
  enabled : Bool
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetAutomaticPresenceSimulation where
  toJSON p := Data.Json.object
    [("authenticatorId", ToJSON.toJSON p.authenticatorId), ("enabled", ToJSON.toJSON p.enabled)]

instance : Command PSetAutomaticPresenceSimulation where
  Response := Unit
  commandName _ := "WebAuthn.setAutomaticPresenceSimulation"
  decodeResponse _ := .ok ()

end CDP.Domains.WebAuthn
