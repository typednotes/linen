/-
  Tests for `Linen.Data.PDF.Core.Name`.
-/
import Linen.Data.PDF.Core.Name

open Data.PDF.Core.Name

namespace Tests.Data.PDF.Core.Name

private def bs (s : String) : Data.ByteString :=
  Data.ByteString.pack s.toUTF8.toList

private def name! (s : String) : Name :=
  match Name.make (bs s) with
  | .ok n => n
  | .error _ => Name.empty

-- `make` accepts a name with no embedded NUL byte.
#guard (Name.make (bs "Type")).isOk

-- `make` rejects a NUL byte, mirroring upstream's `Name.make` guard.
#guard match Name.make (Data.ByteString.pack [0]) with
  | .error _ => true
  | .ok _ => false

-- `toByteString` round-trips the underlying bytes.
#guard (name! "Page").toByteString == bs "Page"

-- `empty` is the empty name.
#guard Name.empty.toByteString == Data.ByteString.empty

-- `append` concatenates the underlying byte strings.
#guard (Name.append (name! "Foo") (name! "Bar")).toByteString == bs "FooBar"

-- `++` uses the `Append` instance.
#guard ((name! "Foo") ++ (name! "Bar")) == name! "FooBar"

-- Distinct names compare unequal.
#guard name! "A" != name! "B"

end Tests.Data.PDF.Core.Name
