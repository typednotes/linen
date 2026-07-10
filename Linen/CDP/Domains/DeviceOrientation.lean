/-
  Linen.CDP.Domains.DeviceOrientation — the `DeviceOrientation` CDP domain

  Ports `CDP.Domains.DeviceOrientation` (see
  `docs/imports/cdp/dependencies.md`); naming conventions as in
  `CDP.Domains.CacheStorage`'s docstring.
-/
import Linen.CDP.Internal.Utils

namespace CDP.Domains.DeviceOrientation

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

/-- Parameters of the `DeviceOrientation.clearDeviceOrientationOverride`
    command: clears the overridden Device Orientation. -/
structure PClearDeviceOrientationOverride where
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClearDeviceOrientationOverride where toJSON _ := .null

instance : Command PClearDeviceOrientationOverride where
  Response := Unit
  commandName _ := "DeviceOrientation.clearDeviceOrientationOverride"
  decodeResponse _ := .ok ()

/-- Parameters of the `DeviceOrientation.setDeviceOrientationOverride` command:
    overrides the Device Orientation. -/
structure PSetDeviceOrientationOverride where
  /-- Mock alpha. -/
  alpha : Float
  /-- Mock beta. -/
  beta : Float
  /-- Mock gamma. -/
  gamma : Float
  deriving Repr, BEq, DecidableEq

instance : ToJSON PSetDeviceOrientationOverride where
  toJSON p := Data.Json.object
    [("alpha", ToJSON.toJSON p.alpha), ("beta", ToJSON.toJSON p.beta), ("gamma", ToJSON.toJSON p.gamma)]

instance : Command PSetDeviceOrientationOverride where
  Response := Unit
  commandName _ := "DeviceOrientation.setDeviceOrientationOverride"
  decodeResponse _ := .ok ()

end CDP.Domains.DeviceOrientation
