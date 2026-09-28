/-
  Tests for `Linen.Data.Bits`.

  The `Bits` / `FiniteBits` typeclass operations on the fixed-width unsigned
  integer types, including `testBit`, `popCount`, leading/trailing-zero counts,
  and the derived `setBit` / `clearBit` / `complementBit`.
-/
import Linen.Data.Bits

open Data

namespace Tests.Data.Bits

/-! ### Core bitwise ops (UInt8) -/

#guard Bits.and (0b1100 : UInt8) 0b1010 == 0b1000
#guard Bits.or  (0b1100 : UInt8) 0b1010 == 0b1110
#guard Bits.xor (0b1100 : UInt8) 0b1010 == 0b0110
#guard Bits.complement (0 : UInt8) == 255
#guard Bits.shiftL (1 : UInt8) 3 == 0b1000
#guard Bits.shiftR (0b1000 : UInt8) 2 == 0b0010

/-! ### testBit / bit / popCount / zeroBits / bitSizeMaybe -/

#guard Bits.testBit (0b0100 : UInt8) 2 == true
#guard Bits.testBit (0b0100 : UInt8) 0 == false
#guard (Bits.bit 3 : UInt8) == 0b1000
#guard Bits.popCount (0b10110 : UInt8) == 3
#guard (Bits.zeroBits : UInt8) == 0
#guard Bits.bitSizeMaybe (α := UInt8) == some 8
#guard Bits.bitSizeMaybe (α := UInt64) == some 64

/-! ### FiniteBits: bounded counts -/

#guard FiniteBits.finiteBitSize (α := UInt8) == 8
#guard (FiniteBits.popCountBounded (0b10110 : UInt8)).val == 3
#guard (FiniteBits.countLeadingZeros  (0b00010000 : UInt8)).val == 3   -- top three bits clear
#guard (FiniteBits.countTrailingZeros (0b00010000 : UInt8)).val == 4   -- lowest set bit is #4
#guard (FiniteBits.countLeadingZeros  (0 : UInt8)).val == 8            -- all-zero ⇒ full width
#guard (FiniteBits.countTrailingZeros (0 : UInt8)).val == 8

/-! ### Derived: setBit / clearBit / complementBit -/

#guard Bits.setBit (0 : UInt8) 3 == 0b1000
#guard Bits.clearBit (0b1111 : UInt8) 1 == 0b1101
#guard Bits.complementBit (0b1010 : UInt8) 0 == 0b1011

/-! ### popCount across widths -/

#guard Bits.popCount (0xFFFF : UInt16) == 16
#guard Bits.popCount (0xFFFFFFFF : UInt32) == 32
#guard Bits.popCount (0xFFFFFFFFFFFFFFFF : UInt64) == 64

end Tests.Data.Bits
