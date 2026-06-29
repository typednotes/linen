/-
  Tests for `Linen.Data.ByteString` — the slice-based byte string.

  Results are checked via `unpack` (to `List UInt8`) or the content-based
  `BEq`/`Ord` instances.
-/
import Linen.Data.ByteString

open Data

namespace Tests.Data.ByteString

/-! ### construction / basic interface -/

#guard (ByteString.pack [1, 2, 3]).unpack == [1, 2, 3]
#guard ByteString.empty.unpack == ([] : List UInt8)
#guard (ByteString.singleton 65).unpack == [65]
#guard (ByteString.replicate 3 7).unpack == [7, 7, 7]
#guard ByteString.empty.null == true
#guard (ByteString.pack [1]).null == false
#guard (ByteString.pack [1, 2, 3]).length == 3
#guard ((ByteString.pack [1, 2]).cons 0).unpack == [0, 1, 2]
#guard ((ByteString.pack [1, 2]).snoc 3).unpack == [1, 2, 3]

/-! ### O(1) slicing -/

#guard ((ByteString.pack [1, 2, 3, 4, 5]).take 2).unpack == [1, 2]
#guard ((ByteString.pack [1, 2, 3, 4, 5]).drop 2).unpack == [3, 4, 5]
#guard ((ByteString.pack [1, 2]).take 10).unpack == [1, 2]      -- take beyond length
#guard ((ByteString.pack [1, 2]).drop 10).unpack == ([] : List UInt8)
#guard ((ByteString.pack [1, 2, 3]).splitAt 1).1.unpack == [1]
#guard ((ByteString.pack [1, 2, 3]).splitAt 1).2.unpack == [2, 3]

/-! ### head / tail / last / init (via the total decompositions) -/

#guard (ByteString.pack [10, 20, 30]).head? == some 10
#guard ByteString.empty.head? == (none : Option UInt8)
#guard ((ByteString.pack [10, 20]).uncons).map (fun (w, r) => (w, r.unpack)) == some (10, [20])
#guard ((ByteString.pack [10, 20, 30]).unsnoc).map (fun (i, w) => (i.unpack, w)) == some ([10, 20], 30)

/-! ### append / concat / intercalate -/

#guard ((ByteString.pack [1, 2]) ++ (ByteString.pack [3, 4])).unpack == [1, 2, 3, 4]
#guard (ByteString.concat [ByteString.pack [1], ByteString.pack [2, 3], ByteString.pack [4]]).unpack == [1, 2, 3, 4]
#guard (ByteString.intercalate (ByteString.pack [0]) [ByteString.pack [1], ByteString.pack [2], ByteString.pack [3]]).unpack == [1, 0, 2, 0, 3]

/-! ### transform -/

#guard ((ByteString.pack [1, 2, 3]).map (· + 1)).unpack == [2, 3, 4]
#guard ((ByteString.pack [1, 2, 3]).reverse).unpack == [3, 2, 1]
#guard ((ByteString.pack [1, 2, 3]).intersperse 0).unpack == [1, 0, 2, 0, 3]
#guard ((ByteString.pack [1, 2]).concatMap (fun w => ByteString.pack [w, w])).unpack == [1, 1, 2, 2]
#guard ((ByteString.transpose [ByteString.pack [1, 2], ByteString.pack [3, 4], ByteString.pack [5, 6]]).map (·.unpack)) == [[1, 3, 5], [2, 4, 6]]

/-! ### folds -/

#guard (ByteString.pack [1, 2, 3, 4]).foldl (· + ·) 0 == 10
#guard (ByteString.pack [1, 2, 3]).foldr (· + ·) 0 == 6
#guard (ByteString.pack [3, 1, 2]).any (· == 1) == true
#guard (ByteString.pack [3, 1, 2]).all (· > 0) == true
#guard (ByteString.pack [1, 2, 1, 3, 1]).count 1 == 3
#guard (ByteString.pack [3, 1, 2]).maximum (by decide) == 3
#guard (ByteString.pack [3, 1, 2]).minimum (by decide) == 1
#guard (ByteString.pack [1, 2, 3, 4]).foldl1 (· + ·) (by decide) == 10

/-! ### scans -/

#guard ((ByteString.pack [1, 2, 3]).scanl (· + ·) 0).unpack == [0, 1, 3, 6]
#guard ((ByteString.pack [1, 2, 3]).scanr (· + ·) 0).unpack == [6, 5, 3, 0]

/-! ### takeWhile / dropWhile / span / break -/

#guard ((ByteString.pack [2, 4, 5, 6]).takeWhile (fun w => w % 2 == 0)).unpack == [2, 4]
#guard ((ByteString.pack [2, 4, 5, 6]).dropWhile (fun w => w % 2 == 0)).unpack == [5, 6]
#guard ((ByteString.pack [2, 4, 5, 6]).span (fun w => w % 2 == 0)).1.unpack == [2, 4]
#guard ((ByteString.pack [1, 2, 3]).break (· == 2)).1.unpack == [1]

/-! ### group / prefix / suffix / infix -/

#guard ((ByteString.pack [1, 1, 2, 3, 3]).group).map (·.unpack) == [[1, 1], [2], [3, 3]]
#guard (ByteString.pack [1, 2]).isPrefixOf (ByteString.pack [1, 2, 3]) == true
#guard (ByteString.pack [2, 3]).isSuffixOf (ByteString.pack [1, 2, 3]) == true
#guard (ByteString.pack [2, 3]).isInfixOf (ByteString.pack [1, 2, 3, 4]) == true
#guard (ByteString.pack [9]).isInfixOf (ByteString.pack [1, 2, 3]) == false
#guard ((ByteString.pack [1, 2]).stripPrefix (ByteString.pack [1, 2, 3])).map (·.unpack) == some [3]

/-! ### search / filter / partition / index -/

#guard (ByteString.pack [10, 20, 30]).elem 20 == true
#guard (ByteString.pack [10, 20, 30]).find (· > 15) == some 20
#guard (ByteString.pack [10, 20, 30]).findIndex (· == 30) == some 2
#guard (ByteString.pack [10, 20, 10]).elemIndices 10 == [0, 2]
#guard ((ByteString.pack [1, 2, 3, 4]).filter (fun w => w % 2 == 0)).unpack == [2, 4]
#guard ((ByteString.pack [1, 2, 3, 4]).partition (fun w => w % 2 == 0)).1.unpack == [2, 4]
#guard (ByteString.pack [10, 20, 30]).index 1 (by decide) == 20

/-! ### instances -/

#guard (ByteString.pack [1, 2, 3]) == (ByteString.pack [1, 2, 3])
#guard ((ByteString.pack [1, 2, 3]).take 2) == (ByteString.pack [1, 2])   -- content eq across offset
#guard ((ByteString.pack [1, 2]) == (ByteString.pack [1, 2, 3])) == false
#guard compare (ByteString.pack [1, 2]) (ByteString.pack [1, 3]) == Ordering.lt
#guard compare (ByteString.pack [1, 2]) (ByteString.pack [1, 2, 3]) == Ordering.lt
#guard toString (ByteString.pack [1, 2, 3]) == "[1, 2, 3]"

/-! ### proofs -/

example (n : Nat) (bs : ByteString) : (bs.take n).len = min n bs.len := ByteString.take_length n bs
example (n : Nat) (bs : ByteString) : (bs.drop n).len = bs.len - min n bs.len := ByteString.drop_length n bs
example (bs : ByteString) : bs.null = true ↔ bs.length = 0 := ByteString.null_iff_length_zero bs
example (n : Nat) (bs : ByteString) :
    (bs.take n).off + (bs.take n).len ≤ (bs.take n).data.size := ByteString.take_valid n bs

end Tests.Data.ByteString
