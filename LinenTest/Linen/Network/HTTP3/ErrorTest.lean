/-
  Tests for `Linen.Network.HTTP3.Error`.

  `H3Error` and its `toCode`/`fromCode` mapping (RFC 9114 §8.1) are pure, so
  behaviour is checked with `#guard` and the round-trip laws with `rfl` examples.
-/
import Linen.Network.HTTP3.Error

open Network.HTTP3

namespace Tests.Network.HTTP3.Error

/-! ### toCode (RFC 9114 §8.1: 0x100..0x110) -/

#guard H3Error.noError.toCode == 0x100
#guard H3Error.generalProtocolError.toCode == 0x101
#guard H3Error.missingSettings.toCode == 0x10A
#guard H3Error.versionFallback.toCode == 0x110
#guard (H3Error.unknown 0x999).toCode == 0x999

/-! ### fromCode -/

#guard H3Error.fromCode 0x100 == H3Error.noError
#guard H3Error.fromCode 0x102 == H3Error.internalError
#guard H3Error.fromCode 0x10A == H3Error.missingSettings
#guard H3Error.fromCode 0x110 == H3Error.versionFallback
#guard H3Error.fromCode 0x200 == H3Error.unknown 0x200   -- out of range ⇒ unknown
#guard H3Error.fromCode 0 == H3Error.unknown 0

/-! ### Round-trips -/

#guard H3Error.fromCode (H3Error.toCode .frameError) == H3Error.frameError
#guard H3Error.fromCode (H3Error.toCode .requestCancelled) == H3Error.requestCancelled
-- A non-colliding unknown code round-trips…
#guard H3Error.fromCode (H3Error.toCode (.unknown 0x999)) == H3Error.unknown 0x999
-- …but an unknown code that collides with a known one decodes to the known variant.
#guard H3Error.fromCode (H3Error.toCode (.unknown 0x100)) == H3Error.noError

/-! ### BEq / ToString -/

#guard H3Error.internalError == H3Error.internalError
#guard (H3Error.internalError == H3Error.frameError) == false
#guard toString H3Error.noError == "H3Error(256)"      -- 0x100 = 256
#guard toString (H3Error.unknown 4096) == "H3Error(4096)"

/-! ### Round-trip laws (compile-time) -/

example : H3Error.fromCode (H3Error.toCode .settingsError) = .settingsError :=
  H3Error.roundtrip_settingsError
example : H3Error.fromCode (H3Error.toCode .connectError) = .connectError :=
  H3Error.roundtrip_connectError

end Tests.Network.HTTP3.Error
