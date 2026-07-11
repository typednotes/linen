/-
  Tests for `Linen.Data.Colour.Chan` — a single, phantom-tagged colour
  channel.
-/
import Linen.Data.Colour.Chan

open Data.Colour

namespace Tests.Data.Colour.Chan

#guard (Chan.empty Unit).val == 0
#guard (Chan.full Unit).val == 1
#guard (Chan.scale 2 (Chan.full Unit)).val == 2
#guard (Chan.add (Chan.full Unit) (Chan.full Unit)).val == 2
#guard (Chan.invert (Chan.full Unit)).val == 0
#guard (Chan.over (Chan.full Unit) 0.5 (Chan.empty Unit)).val == 1
#guard (Chan.sum ([Chan.full Unit, Chan.full Unit, Chan.empty Unit] : List (Chan Unit))).val == 2

end Tests.Data.Colour.Chan
