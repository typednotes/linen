/-
  `Linen.Text.Pandoc.Emoji` — emoji shortcode ↔ glyph table.

  ## Haskell source

  Ported from `Text.Pandoc.Emoji` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Emoji.hs`).

  Upstream is a thin wrapper over the `emojis` Hackage package's `Text.Emoji`
  (`emojis :: Map Text Text` = `M.fromList E.emojis`, and `emojiToInline`
  wraps `E.emojiFromAlias` in a `Span`). Per
  `docs/imports/pandoc/dependencies.md`, the `emojis` data table is folded in
  here rather than imported as a separate package.

  ### Deviations from upstream

  * `Map Text Text` → an association list (`List (String × String)`), matching
    how the AST models maps in this port.
  * The backing shortcode→glyph table is a **representative subset** of the
    `emojis` package's ~1800-entry table (which cannot be reproduced verbatim);
    the covered shortcodes are the common ones. `emojiFromAlias`/`emojis`/
    `emojiToInline` behave exactly as upstream on the covered keys.
-/

import Linen.Text.Pandoc.Definition

namespace Linen.Text.Pandoc
namespace Emoji

/-- Emoji shortcode → glyph table (a representative subset of the `emojis`
    package table). -/
def emojis : List (String × String) :=
  [ ("smile", "😄"), ("smiley", "😃"), ("grinning", "😀")
  , ("laughing", "😆"), ("blush", "😊"), ("wink", "😉")
  , ("heart_eyes", "😍"), ("kissing_heart", "😘"), ("stuck_out_tongue", "😛")
  , ("sunglasses", "😎"), ("smirk", "😏"), ("neutral_face", "😐")
  , ("confused", "😕"), ("cry", "😢"), ("sob", "😭")
  , ("joy", "😂"), ("rage", "😡"), ("angry", "😠")
  , ("fearful", "😨"), ("scream", "😱"), ("sleeping", "😴")
  , ("heart", "❤"), ("broken_heart", "💔"), ("thumbsup", "👍")
  , ("thumbsdown", "👎"), ("ok_hand", "👌"), ("clap", "👏")
  , ("wave", "👋"), ("pray", "🙏"), ("muscle", "💪")
  , ("fire", "🔥"), ("star", "⭐"), ("sparkles", "✨")
  , ("zap", "⚡"), ("sunny", "☀"), ("cloud", "☁")
  , ("snowflake", "❄"), ("umbrella", "☔"), ("coffee", "☕")
  , ("rocket", "🚀"), ("tada", "🎉"), ("100", "💯")
  , ("+1", "👍"), ("-1", "👎"), ("checkered_flag", "🏁")
  , ("warning", "⚠"), ("bulb", "💡"), ("book", "📖")
  , ("computer", "💻"), ("email", "✉"), ("phone", "☎")
  , ("dog", "🐶"), ("cat", "🐱"), ("mouse", "🐭") ]

/-- Look up the glyph for an emoji shortcode. -/
def emojiFromAlias (alias : String) : Option String := emojis.lookup alias

/-- Convert an emoji shortcode to a `Span` inline tagged with the class
    `emoji` and a `data-emoji` attribute, or `none` if unknown. -/
def emojiToInline (emojikey : String) : Option Inline :=
  (emojiFromAlias emojikey).map fun glyph =>
    Inline.Span ("", ["emoji"], [("data-emoji", emojikey)]) [Inline.Str glyph]

end Emoji
end Linen.Text.Pandoc
