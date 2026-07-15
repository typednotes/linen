/-
  Linen.Data.MutByteArray — the public facade for the mutable byte array and
  its `Unbox` (de)serialization class.

  ## Haskell source
  `Streamly.Data.MutByteArray` from
  [`streamly-core`](https://hackage.haskell.org/package/streamly-core)
  (module #37 of the `streamly` import, see
  `docs/imports/streamly/dependencies.md`), the public `Streamly.Data.*`
  facade over `Streamly.Internal.Data.MutByteArray.Type` (#10) and
  `Streamly.Internal.Data.Unbox` (#9). Upstream this module contains no logic
  of its own: it curates the two internal modules into a single public surface.

  This port does the same. Both the `MutByteArray` type (with `new`,
  `newPinned`, `pin`/`unpin`, `length`, …) and the `Unbox` class (with its
  concrete instances) already live in the `Data` namespace, so importing this
  facade makes the whole mutable-byte-array surface reachable under `Data.*`
  without needing to know the internal `.Type` split — mirroring how a user
  would `import Streamly.Data.MutByteArray` upstream.

  ## Note
  Module #10 (the byte-array type itself) lives at `Linen.Data.MutByteArray.Type`
  so this clean top-level path is free for the facade, mirroring the
  `Linen.Data.StreamK.Type` + `Linen.Data.StreamK` (facade) split.
-/

-- ── Re-exported modules ─────────────────────────────────────────────────────
import Linen.Data.MutByteArray.Type
import Linen.Data.Unbox
