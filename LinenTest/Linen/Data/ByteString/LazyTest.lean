/-
  Tests for `Linen.Data.ByteString.Lazy` — chunked lazy byte strings.

  Results are checked via `unpack` (to `List UInt8`) or the content-based
  instances (which compare the forced strict form, so chunking is irrelevant).
-/
import Linen.Data.ByteString.Lazy

open Data
open Data.ByteString.Lazy

namespace Tests.Data.ByteString.Lazy

/-! ### construction / basics -/

#guard (LazyByteString.pack [1, 2, 3]).unpack == [1, 2, 3]
#guard LazyByteString.empty.unpack == ([] : List UInt8)
#guard LazyByteString.empty.null == true
#guard (LazyByteString.pack [1]).null == false
#guard (LazyByteString.pack [1, 2, 3]).length == 3

/-! ### chunks: fromChunks drops empties; round-trip via toStrict -/

#guard (LazyByteString.fromChunks [ByteString.pack [1, 2], ByteString.pack [3]]).unpack == [1, 2, 3]
#guard (LazyByteString.fromChunks [ByteString.pack [1], ByteString.empty, ByteString.pack [2]]).toChunks.length == 2
#guard (LazyByteString.fromChunks [ByteString.pack [1], ByteString.empty, ByteString.pack [2]]).unpack == [1, 2]
#guard (LazyByteString.fromStrict (ByteString.pack [1, 2, 3])).toStrict.unpack == [1, 2, 3]

/-! ### append / cons / snoc -/

#guard ((LazyByteString.pack [1, 2]) ++ (LazyByteString.pack [3, 4])).unpack == [1, 2, 3, 4]
#guard (LazyByteString.cons 0 (LazyByteString.pack [1, 2])).unpack == [0, 1, 2]
#guard ((LazyByteString.pack [1, 2]).snoc 3).unpack == [1, 2, 3]

/-! ### head? / uncons -/

#guard (LazyByteString.pack [10, 20]).head? == some 10
#guard LazyByteString.empty.head? == (none : Option UInt8)
#guard ((LazyByteString.pack [10, 20]).uncons).map (fun (w, r) => (w, r.unpack)) == some (10, [20])

/-! ### folds -/

#guard (LazyByteString.pack [1, 2, 3, 4]).foldl (· + ·) 0 == 10
#guard (LazyByteString.pack [1, 2, 3]).foldr (· + ·) 0 == 6
#guard (LazyByteString.fromChunks [ByteString.pack [1], ByteString.pack [2, 3]]).foldlChunks (fun n _ => n + 1) 0 == 2

/-! ### map / filter -/

#guard ((LazyByteString.pack [1, 2, 3]).map (· + 1)).unpack == [2, 3, 4]
#guard ((LazyByteString.pack [1, 2, 3, 4]).filter (· % 2 == 0)).unpack == [2, 4]

/-! ### take / drop / splitAt (including across chunk boundaries) -/

#guard ((LazyByteString.pack [1, 2, 3, 4, 5]).take 2).unpack == [1, 2]
#guard ((LazyByteString.pack [1, 2, 3, 4, 5]).drop 2).unpack == [3, 4, 5]
#guard ((LazyByteString.fromChunks [ByteString.pack [1, 2], ByteString.pack [3, 4, 5]]).take 3).unpack == [1, 2, 3]
#guard ((LazyByteString.fromChunks [ByteString.pack [1, 2], ByteString.pack [3, 4, 5]]).drop 3).unpack == [4, 5]
#guard ((LazyByteString.pack [1, 2, 3, 4]).splitAt 2).1.unpack == [1, 2]
#guard ((LazyByteString.pack [1, 2, 3, 4]).splitAt 2).2.unpack == [3, 4]

/-! ### reverse / any / all / elem / concat -/

#guard ((LazyByteString.pack [1, 2, 3]).reverse).unpack == [3, 2, 1]
#guard (LazyByteString.pack [1, 2, 3]).any (· == 2) == true
#guard (LazyByteString.pack [1, 2, 3]).all (· > 0) == true
#guard (LazyByteString.pack [1, 2, 3]).elem 3 == true
#guard (LazyByteString.concat [LazyByteString.pack [1], LazyByteString.pack [2, 3]]).unpack == [1, 2, 3]

/-! ### instances compare by content, independent of chunking -/

#guard (LazyByteString.pack [1, 2, 3]) == (LazyByteString.fromChunks [ByteString.pack [1], ByteString.pack [2, 3]])
#guard ((LazyByteString.pack [1, 2]) == (LazyByteString.pack [1, 2, 3])) == false
#guard compare (LazyByteString.pack [1, 2]) (LazyByteString.pack [1, 3]) == Ordering.lt
#guard toString (LazyByteString.pack [1, 2, 3]) == "[1, 2, 3]"

end Tests.Data.ByteString.Lazy
