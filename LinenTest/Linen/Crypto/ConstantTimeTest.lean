/-
  Tests for `Linen.Crypto.ConstantTime`.

  The comparison is pure, so it is checked with `#guard`: it must agree with
  `==` on every input — equal, differing at the first, a middle or the last
  byte, empty, and of different sizes (a prefix included, which is where an
  implementation that only walks the shorter array goes wrong). Timing is not
  observable from a `#guard` and is not claimed.
-/
import Linen.Crypto.ConstantTime

open Crypto.ConstantTime

namespace Tests.Crypto.ConstantTime

private def bytes (l : List UInt8) : ByteArray := ⟨l.toArray⟩

/-! ### `eq` agrees with `==` -/

#guard eq (bytes [1, 2, 3]) (bytes [1, 2, 3])
#guard eq ByteArray.empty ByteArray.empty
#guard !eq (bytes [0, 2, 3]) (bytes [1, 2, 3])     -- first byte
#guard !eq (bytes [1, 0, 3]) (bytes [1, 2, 3])     -- middle byte
#guard !eq (bytes [1, 2, 3]) (bytes [1, 2, 4])     -- last byte
#guard !eq (bytes [0xff]) (bytes [0x7f])           -- a single bit
#guard !eq (bytes [1, 2]) (bytes [1, 2, 3])        -- a prefix
#guard !eq (bytes [1, 2, 3]) (bytes [1, 2])
#guard !eq ByteArray.empty (bytes [0])

-- Exhaustively over all pairs of two-byte arrays drawn from a small alphabet.
#guard
  let xs : List ByteArray :=
    (([0, 1, 0x80, 0xff] : List UInt8).flatMap fun a => [0, 1, 0x80, 0xff].map fun b => bytes [a, b])
  xs.all fun x => xs.all fun y => eq x y == (x == y)

/-! ### `eqString`: tokens, compared on their UTF-8 bytes -/

#guard eqString "Bearer abc" "Bearer abc"
#guard !eqString "Bearer abc" "Bearer abd"
#guard !eqString "Bearer abc" "Bearer ab"
#guard !eqString "" "x"
#guard eqString "" ""
#guard eqString "clé" "clé"
#guard !eqString "é" "e"                           -- same length in characters, not in bytes

end Tests.Crypto.ConstantTime
