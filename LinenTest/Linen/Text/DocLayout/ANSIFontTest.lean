/-
  Tests for `Linen.Text.DocLayout.ANSIFont`.
-/
import Linen.Text.DocLayout.ANSIFont

namespace Tests.Linen.Text.DocLayout.ANSIFont

open _root_.Text.DocLayout

-- ── Colour indices ───────────────────────────────

#guard Color8.Black.toNat = 0
#guard Color8.White.toNat = 7
#guard Color8.Blue.toNat = 4

-- ── Style application via `~>` ───────────────────

#guard (baseFont ~> .RWeight .Bold).ftWeight = .Bold
#guard (baseFont ~> .RShape .Italic).ftShape = .Italic
#guard (baseFont ~> .RForeground (.FG .Red)).ftForeground = .FG .Red
-- later requests override earlier ones on the same attribute
#guard (baseFont ~> .RWeight .Bold ~> .RWeight .Normal).ftWeight = .Normal
-- distinct attributes accumulate
#guard (baseFont ~> .RWeight .Bold ~> .RShape .Italic) =
  { baseFont with ftWeight := .Bold, ftShape := .Italic }

-- ── SGR / OSC-8 escape rendering ─────────────────

-- the base font renders as the reset sequence
#guard renderFont baseFont = "\x1b[0m"
-- a non-base font renders every attribute in turn
#guard renderFont (baseFont ~> .RWeight .Bold) =
  "\x1b[1m\x1b[23m\x1b[39m\x1b[49m\x1b[24m\x1b[29m"
#guard Foreground.renderSGR (.FG .Red) = "\x1b[31m"
#guard Background.renderSGR (.BG .Green) = "\x1b[42m"
#guard Underline.renderSGR .ULCurly = "\x1b[4:3m"

-- OSC-8 hyperlink open / close
#guard renderOSC8 (some "https://example.com") = "\x1b]8;;https://example.com\x1b\\"
#guard renderOSC8 none = "\x1b]8;;\x1b\\"

end Tests.Linen.Text.DocLayout.ANSIFont
