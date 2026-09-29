/-
  Tests for `Linen.System.Console.Ansi`.

  Covers the `Color`/`Intensity` enums and the escape-code builders.
-/
import Linen.System.Console.Ansi

open System.Console.Ansi

namespace Tests.Ansi

-- ── Enum instances (BEq) ──────────────────────────────────────────────

#guard (Color.red == Color.red)
#guard !(Color.red == Color.blue)
#guard (Intensity.bold == Intensity.bold)
#guard !(Intensity.bold == Intensity.normal)

-- ── reset ─────────────────────────────────────────────────────────────

#guard reset = "\x1b[0m"

-- ── setFg ─────────────────────────────────────────────────────────────

#guard setFg .black = "\x1b[30m"
#guard setFg .red = "\x1b[31m"
#guard setFg .green = "\x1b[32m"
#guard setFg .yellow = "\x1b[33m"
#guard setFg .blue = "\x1b[34m"
#guard setFg .magenta = "\x1b[35m"
#guard setFg .cyan = "\x1b[36m"
#guard setFg .white = "\x1b[37m"

-- ── setBg ─────────────────────────────────────────────────────────────

#guard setBg .black = "\x1b[40m"
#guard setBg .red = "\x1b[41m"
#guard setBg .white = "\x1b[47m"

-- ── setIntensity ──────────────────────────────────────────────────────

#guard setIntensity .bold = "\x1b[1m"
#guard setIntensity .faint = "\x1b[2m"
#guard setIntensity .normal = "\x1b[22m"

-- ── colored / bold (composition) ──────────────────────────────────────

#guard colored .red "hi" = "\x1b[31mhi\x1b[0m"
#guard bold "hi" = "\x1b[1mhi\x1b[22m"
#guard dim "hi" = "\x1b[2mhi\x1b[22m"

-- ── style: a switch, not an environment read ──────────────────────────

#guard style true (Color.fgCode .red) "DELETE" = "\x1b[31mDELETE\x1b[0m"
#guard style false (Color.fgCode .red) "DELETE" = "DELETE"
#guard style true faintCode "x" = "\x1b[2mx\x1b[0m"
#guard style true boldCode "x" = "\x1b[1mx\x1b[0m"
-- `fgCode` agrees with `setFg` for every colour.
#guard [Color.black, .red, .green, .yellow, .blue, .magenta, .cyan, .white].all
  fun c => setFg c = s!"\x1b[{c.fgCode}m"

-- ── shouldColor: NO_COLOR, then FORCE_COLOR, then the terminal ─────────────

#guard shouldColor none none true = true
#guard shouldColor none none false = false
#guard shouldColor (some "1") none true = false        -- NO_COLOR wins
#guard shouldColor (some "1") (some "1") true = false  -- even over FORCE_COLOR
#guard shouldColor none (some "1") false = true        -- FORCE_COLOR through a pipe
#guard shouldColor (some "") none true = true          -- empty is unset
#guard shouldColor (some "  ") (some "") false = false -- whitespace is unset

end Tests.Ansi
