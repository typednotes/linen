/-
  Tests for `Linen.Text.Pandoc.MediaBag`.
-/
import Linen.Text.Pandoc.MediaBag

namespace Tests.Linen.Text.Pandoc.MediaBag

open _root_.Linen.Text.Pandoc

private def bag : MediaBag :=
  MediaBag.empty.insertMedia "img.png" none (String.toUTF8 "hello")

-- ── insertMedia / lookupMedia ─────────────────────────────────────────

#guard (bag.lookupMedia "img.png").isSome
#guard ((bag.lookupMedia "img.png").map (·.mediaMimeType)) == some "image/png"
#guard ((bag.lookupMedia "img.png").map (·.mediaContents.size)) == some 5
-- explicit mime type overrides the extension guess
#guard (((MediaBag.empty.insertMedia "x.png" (some "image/gif") ByteArray.empty).lookupMedia "x.png").map (·.mediaMimeType))
        == some "image/gif"

-- ── deleteMedia ───────────────────────────────────────────────────────

#guard ((bag.deleteMedia "img.png").lookupMedia "img.png").isNone

-- ── mediaDirectory / mediaItems ───────────────────────────────────────

#guard bag.mediaDirectory.length == 1
#guard (bag.mediaDirectory.head?.map (·.1)) == some "img.png"
#guard bag.mediaItems.length == 1

end Tests.Linen.Text.Pandoc.MediaBag
