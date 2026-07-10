/-
  Tests for `Linen.Data.PDF.Core.Object`.
-/
import Linen.Data.PDF.Core.Object

open Data.PDF.Core.Object

namespace Tests.Data.PDF.Core.Object

private def name! (s : String) : Name :=
  match Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList) with
  | .ok n => n
  | .error _ => Data.PDF.Core.Name.Name.empty

-- `Ref`s hash as the pair of their fields, mirroring upstream's
-- `hashWithSalt salt (R a b) = hashWithSalt salt (a, b)`.
#guard (hash (⟨1, 0⟩ : Ref)) == hash ((1 : Int), (0 : Int))

-- `Ref` equality is field-wise.
#guard (⟨1, 0⟩ : Ref) == (⟨1, 0⟩ : Ref)
#guard (⟨1, 0⟩ : Ref) != (⟨2, 0⟩ : Ref)

-- `Object.dict`/`Stream.dict` round-trip a `Dict` through the internal
-- association-array representation (see the module doc-comment).
#guard
  let d : Dict := Std.HashMap.ofList [(name! "Type", Object.name (name! "Page"))]
  match Object.dict d with
  | .dictRaw entries => entries.toList == [(name! "Type", Object.name (name! "Page"))]
  | _ => false

#guard
  let d : Dict := Std.HashMap.ofList [(name! "Length", Object.number (Data.Scientific.mk 5 0))]
  (Stream.mk' d 17).dict.toList == d.toList &&
  (Stream.mk' d 17).offset == 17

-- Distinct `Object` cases compare unequal.
#guard (Object.null) != (Object.bool true)
#guard (Object.array #[]) != (Object.array #[Object.null])

end Tests.Data.PDF.Core.Object
