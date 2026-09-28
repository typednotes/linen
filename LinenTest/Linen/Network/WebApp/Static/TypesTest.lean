import Linen.Network.WebApp.Static.Types

/-! ### Tests for `Linen.Network.WebApp.Static.Types`

    Coverage: `Piece`/`toPiece`/`unsafeToPiece` path-traversal validation,
    `toPieces`, `MaxAge`, and `StaticSettings`' defaults — all `#guard`. -/

open Network.WebApp.Static

namespace Tests.Network.WebApp.Static.Types

-- `toPiece`: accepts plain segments, rejects dotfiles and slashes.
#guard (toPiece "index.html").isSome
#guard (toPiece "").isSome
#guard (toPiece ".hidden").isNone
#guard (toPiece "a/b").isNone
#guard (toPiece ".").isNone

-- `unsafeToPiece` matches `toPiece` on a known-safe literal.
#guard some (unsafeToPiece "style.css") == toPiece "style.css"

-- `toPieces`: all-or-nothing across a list of segments.
#guard (toPieces ["a", "b", "c"]).isSome
#guard (toPieces ["a", ".b", "c"]).isNone
#guard toPieces [] == some []

-- `Piece`'s `BEq`/`ToString` instances.
#guard toString (unsafeToPiece "foo") == "foo"
#guard unsafeToPiece "foo" == unsafeToPiece "foo"
#guard unsafeToPiece "foo" != unsafeToPiece "bar"

-- `MaxAge` values are distinguishable.
#guard MaxAge.maxAgeSeconds 60 != MaxAge.maxAgeForever
#guard MaxAge.noStore != MaxAge.noCache

-- `StaticSettings`' defaults: 1-hour cache, redirect-to-index on, `index.html`.
#guard show Bool from Id.run do
  let settings : StaticSettings := { ssLookupFile := fun _ => pure .lrNotFound }
  pure (settings.ssMaxAge == .maxAgeSeconds 3600 &&
        settings.ssRedirectToIndex &&
        settings.ssIndices == [unsafeToPiece "index.html"] &&
        settings.ssListing.isNone)

end Tests.Network.WebApp.Static.Types
