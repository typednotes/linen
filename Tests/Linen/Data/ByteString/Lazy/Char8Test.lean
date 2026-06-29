/-
  Tests for `Linen.Data.ByteString.Lazy.Char8` — the Latin-1 `Char` view of
  `LazyByteString`.
-/
import Linen.Data.ByteString.Lazy.Char8

open Data.ByteString.Lazy

namespace Tests.Data.ByteString.Lazy.Char8

/-! ### pack / unpack round-trips -/

#guard Char8.unpack (Char8.pack "Hello, World!") == "Hello, World!"
#guard Char8.unpack (Char8.pack "") == ""

/-! ### cons / head? -/

#guard Char8.unpack (Char8.cons 'A' (Char8.pack "BC")) == "ABC"
#guard Char8.head? (Char8.pack "Hi") == some 'H'
#guard Char8.head? LazyByteString.empty == (none : Option Char)

/-! ### map / filter -/

#guard Char8.unpack (Char8.map Char.toUpper (Char8.pack "abc")) == "ABC"
#guard Char8.unpack (Char8.filter (· != ' ') (Char8.pack "a b c")) == "abc"

/-! ### folds -/

#guard Char8.foldl (fun (acc : String) c => acc.push c) "" (Char8.pack "Hi") == "Hi"
#guard Char8.foldr (fun c (acc : String) => acc.push c) "" (Char8.pack "Hi") == "iH"

/-! ### elem -/

#guard Char8.elem 'b' (Char8.pack "abc") == true
#guard Char8.elem 'z' (Char8.pack "abc") == false

end Tests.Data.ByteString.Lazy.Char8
