/-
  Tests for `Linen.Text.Pandoc.Emoji`.
-/
import Linen.Text.Pandoc.Emoji

namespace Tests.Linen.Text.Pandoc.Emoji

open _root_.Linen.Text.Pandoc

-- ── emojiFromAlias ────────────────────────────────────────────────────

#guard Emoji.emojiFromAlias "smile" == some "😄"
#guard Emoji.emojiFromAlias "heart" == some "❤"
#guard Emoji.emojiFromAlias "no_such_emoji" == none

-- ── emojiToInline ─────────────────────────────────────────────────────

#guard Emoji.emojiToInline "smile" ==
  some (Inline.Span ("", ["emoji"], [("data-emoji", "smile")]) [Inline.Str "😄"])
#guard (Emoji.emojiToInline "no_such_emoji").isNone == true

end Tests.Linen.Text.Pandoc.Emoji
