/-
  Tests for `Linen.Data.PDF.Core.Object.Util`.
-/
import Linen.Data.PDF.Core.Object.Util

open Data.PDF.Core.Object
open Data.PDF.Core.Object.Util

namespace Tests.Data.PDF.Core.Object.Util

private def name! (s : String) : Name :=
  match Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList) with
  | .ok n => n
  | .error _ => Data.PDF.Core.Name.Name.empty

-- `intValue`/`int64Value` succeed on an integral number (coinciding, see the
-- module doc-comment for why).
#guard intValue (.number (Data.Scientific.mk 42 0)) == some 42
#guard int64Value (.number (Data.Scientific.mk 42 0)) == some 42

-- `intValue`/`int64Value` fail on a fractional number.
#guard intValue (.number (Data.Scientific.mk 314 (-2))) == none
#guard int64Value (.number (Data.Scientific.mk 314 (-2))) == none

-- `intValue` fails on a non-`number` object.
#guard intValue .null == none

-- `boolValue` extracts the boolean, and fails on other cases.
#guard boolValue (.bool true) == some true
#guard boolValue (.number (Data.Scientific.mk 1 0)) == none

-- `realValue` converts an integral number too (unlike `intValue`).
#guard realValue (.number (Data.Scientific.mk 42 0)) == some 42.0
#guard match realValue (.number (Data.Scientific.mk 314 (-2))) with
  | some f => (f - 3.14).abs < 1e-9
  | none => false

-- `nameValue`/`stringValue` extract their respective payloads.
#guard nameValue (.name (name! "Foo")) == some (name! "Foo")
#guard stringValue (.string (Data.ByteString.pack "hi".toUTF8.toList)) ==
  some (Data.ByteString.pack "hi".toUTF8.toList)

-- `arrayValue` extracts the array.
#guard arrayValue (.array #[.null, .bool true]) == some #[.null, .bool true]

-- `refValue` extracts the reference.
#guard refValue (.ref ⟨3, 0⟩) == some (⟨3, 0⟩ : Ref)

-- `streamValue` extracts the stream.
#guard
  let s := Stream.mk' (Std.HashMap.ofList []) 5
  streamValue (.stream s) == some s

-- `dictValue` reconstructs the public `Dict` from `Object`'s internal
-- association-array representation.
#guard
  let d : Dict := Std.HashMap.ofList [(name! "Type", Object.name (name! "Page"))]
  match dictValue (Object.dict d) with
  | some d' => d'.toList == d.toList
  | none => false

-- `dictValue` fails on a non-`dictRaw` object.
#guard dictValue .null == none

end Tests.Data.PDF.Core.Object.Util
