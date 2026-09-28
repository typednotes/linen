/-
  Tests for `Linen.Numeric.Natural.Lens`.
-/
import Linen.Numeric.Natural.Lens

open Control.Lens Numeric.Natural.Lens

namespace Tests.Linen.Numeric.Natural.Lens

-- `bitsList`/`ofBits`: inverse to one another.
#guard ofBits (bitsList 0) = 0
#guard ofBits (bitsList 1) = 1
#guard ofBits (bitsList 13) = 13
#guard ofBits (bitsList 1000) = 1000

-- `interleaveBits`/`deinterleaveBits`: inverse to one another (padded with
-- trailing `false`s to a common length, since `interleaveBits` pads the
-- shorter input rather than simply appending the longer input's tail).
#guard deinterleaveBits (interleaveBits [true, false, true] [false, true]) = ([true, false, true], [false, true, false])
#guard deinterleaveBits (interleaveBits [] [true, true, false]) = ([false, false, false], [true, true, false])

-- `_Pair :: Iso' Natural (Natural, Natural)`.
#guard withIso _Pair (fun sa _ => sa 0) = (0, 0)
#guard withIso _Pair (fun _ bt => bt (0, 0)) = 0
#guard withIso _Pair (fun sa bt => bt (sa 42)) = 42
#guard withIso _Pair (fun sa bt => bt (sa 1000)) = 1000
#guard withIso _Pair (fun sa bt => bt (sa 123456)) = 123456

-- `_Sum :: Iso' Natural (Natural ⊕ Natural)`: evens land on the left, odds
-- on the right.
#guard withIso _Sum (fun sa _ => sa 8) = Sum.inl 4
#guard withIso _Sum (fun sa _ => sa 9) = Sum.inr 4
#guard withIso _Sum (fun _ bt => bt (Sum.inl 4)) = 8
#guard withIso _Sum (fun _ bt => bt (Sum.inr 4)) = 9
#guard withIso _Sum (fun sa bt => bt (sa 777)) = 777

-- `_Naturals :: Iso' Natural [Natural]`.
#guard withIso _Naturals (fun sa _ => sa 0) = []
#guard withIso _Naturals (fun _ bt => bt []) = 0
#guard withIso _Naturals (fun sa bt => bt (sa 0)) = 0
#guard withIso _Naturals (fun sa bt => bt (sa 1)) = 1
#guard withIso _Naturals (fun sa bt => bt (sa 5)) = 5
#guard withIso _Naturals (fun sa bt => bt (sa 100)) = 100
#guard withIso _Naturals (fun sa bt => bt (sa 999)) = 999

end Tests.Linen.Numeric.Natural.Lens
