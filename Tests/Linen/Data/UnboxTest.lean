import Linen.Data.Unbox

open Data Data.Unbox

-- Instance byte sizes match the fixed widths.
#guard (Unbox.size (a := UInt8)) == 1
#guard (Unbox.size (a := UInt16)) == 2
#guard (Unbox.size (a := UInt32)) == 4
#guard (Unbox.size (a := UInt64)) == 8
#guard (Unbox.size (a := Bool)) == 1

-- Round-trip `poke` then `peek` at a byte offset for each width.
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 1) (200 : UInt8)) : UInt8) == 200
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 2) (0xBEEF : UInt16)) : UInt16) == 0xBEEF
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 4) (0xDEADBEEF : UInt32)) : UInt32) == 0xDEADBEEF
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 8) (0x0123456789ABCDEF : UInt64)) : UInt64) == 0x0123456789ABCDEF
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 1) true) : Bool) == true
#guard (Unbox.peekAt 0 (Unbox.pokeAt 0 (MutByteArray.new 1) false) : Bool) == false

-- Little-endian layout: low byte first.
#guard (Unbox.pokeAt 0 (MutByteArray.new 2) (0xBEEF : UInt16)).bytes.get! 0 == 0xEF
#guard (Unbox.pokeAt 0 (MutByteArray.new 2) (0xBEEF : UInt16)).bytes.get! 1 == 0xBE

-- Writes at distinct offsets do not clobber each other.
#guard (Unbox.peekAt 1 (Unbox.pokeAt 1 (Unbox.pokeAt 0 (MutByteArray.new 3) (9 : UInt8)) (7 : UInt16)) : UInt16) == 7
