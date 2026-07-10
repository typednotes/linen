/-
  Linen.CDP — package aggregator for the Chrome DevTools Protocol client

  Ports the `CDP` package root (see `docs/imports/cdp/dependencies.md`):
  re-exports `CDP.Domains` (every command/event/type across the protocol) and
  `CDP.Runtime` (connecting, sending commands, subscribing to events — which
  itself re-exports `CDP.Endpoints` and `CDP.Internal.Utils`).
-/
import Linen.CDP.Domains
import Linen.CDP.Runtime
