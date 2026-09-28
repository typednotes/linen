/-
  Tests for `Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest`.

  Upstream is a bare marker typeclass with no methods, so there is nothing
  to exercise with `#guard` — the test just confirms an instance can be
  declared and used as a constraint, pinning down the class signature.
-/
import Linen.Network.OAuth2.Experiment.Flows.UserInfoRequest

open Network.OAuth2.Experiment.Flows.UserInfoRequest

namespace Tests.Network.OAuth2.Experiment.Flows.UserInfoRequest

private structure Dummy where

private instance : HasUserInfoRequest Dummy where

private def requiresUserInfoSupport [HasUserInfoRequest a] (_ : a) : Bool := true

#guard requiresUserInfoSupport (Dummy.mk)

end Tests.Network.OAuth2.Experiment.Flows.UserInfoRequest
