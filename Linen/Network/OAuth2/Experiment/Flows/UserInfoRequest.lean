/-
  Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest — user info request marker

  Port of `hoauth2`'s `Network.OAuth2.Experiment.Flows.UserInfoRequest` (see
  `docs/imports/hoauth2/dependencies.md`). Upstream is a bare marker
  typeclass with no methods of its own — a later batch's grants/flows
  (`Network.OAuth2.Experiment.Grants.AuthorizationCode`,
  `.DeviceAuthorization`, `.ResourceOwnerPassword`) each provide an
  instance together with associated-type/data-family declarations that
  describe how to fetch a user's profile once an access token is in hand.
  This batch only ports the marker itself; the flow-specific instances land
  with those later modules.
-/

namespace Network.OAuth2.Experiment.Flows.UserInfoRequest

/-- Marker class for flows that support fetching user info with an access
    token. Instances are supplied by the individual grant/flow modules that
    know how to build the request (a later `hoauth2` import batch). -/
class HasUserInfoRequest (a : Type) where

end Network.OAuth2.Experiment.Flows.UserInfoRequest
