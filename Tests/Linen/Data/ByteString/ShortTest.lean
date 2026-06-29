/-
  Tests for `Linen.Data.ByteString.Short` — the `ByteArray`-newtype short byte
  string and its conversions with the strict slice `ByteString`.
-/
import Linen.Data.ByteString.Short

open Data
open Data.ByteString

namespace Tests.Data.ByteString.Short

/-! ### construction / basics -/

#guard (ShortByteString.pack [1, 2, 3]).unpack == [1, 2, 3]
#guard ShortByteString.empty.unpack == ([] : List UInt8)
#guard ShortByteString.empty.null == true
#guard (ShortByteString.pack [1]).null == false
#guard (ShortByteString.pack [1, 2, 3]).length == 3
#guard (ShortByteString.pack [10, 20, 30]).index 1 (by decide) == 20

/-! ### conversions with the strict ByteString (content round-trips) -/

#guard (ShortByteString.toShort (ByteString.pack [5, 6, 7])).unpack == [5, 6, 7]
#guard (ShortByteString.fromShort (ShortByteString.pack [9, 8])).unpack == [9, 8]
#guard (ShortByteString.fromShort (ShortByteString.toShort (ByteString.pack [1, 2, 3]))).unpack == [1, 2, 3]
-- toShort copies only the slice, not the whole backing array:
#guard (ShortByteString.toShort ((ByteString.pack [1, 2, 3, 4, 5]).drop 2)).unpack == [3, 4, 5]

/-! ### instances -/

#guard (ShortByteString.pack [1, 2]) == (ShortByteString.pack [1, 2])
#guard ((ShortByteString.pack [1, 2]) == (ShortByteString.pack [1, 2, 3])) == false
#guard compare (ShortByteString.pack [1, 2]) (ShortByteString.pack [1, 3]) == Ordering.lt
#guard toString (ShortByteString.pack [1, 2, 3]) == "[1, 2, 3]"

/-! ### length is preserved by toShort (compile-time) -/

example (bs : ByteString) : (ShortByteString.toShort bs).length = bs.len :=
  ShortByteString.length_toShort bs

end Tests.Data.ByteString.Short
