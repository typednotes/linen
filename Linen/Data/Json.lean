/-
  Linen.Data.Json — Re-export of core JSON types, encoding, and decoding
-/

import Linen.Data.Json.Types
import Linen.Data.Json.Encode
import Linen.Data.Json.Decode

deriving instance DecidableEq for Except

namespace Data.Json

-- ── Encode/Decode roundtrip theorems ────────────────────────────────

theorem roundtrip_null :
    Decode.decode (Encode.encode .null) = .ok .null := by native_decide

theorem roundtrip_true :
    Decode.decode (Encode.encode (.bool true)) = .ok (.bool true) := by native_decide

theorem roundtrip_false :
    Decode.decode (Encode.encode (.bool false)) = .ok (.bool false) := by native_decide

theorem roundtrip_string_hello :
    Decode.decode (Encode.encode (.string "hello")) = .ok (.string "hello") := by native_decide

theorem roundtrip_number_42 :
    Decode.decode (Encode.encode (.number 42)) = .ok (.number 42) := by native_decide

theorem roundtrip_empty_array :
    Decode.decode (Encode.encode (.array #[])) = .ok (.array #[]) := by native_decide

theorem roundtrip_empty_object :
    Decode.decode (Encode.encode (.object [])) = .ok (.object []) := by native_decide

theorem roundtrip_nested_array :
    Decode.decode (Encode.encode (.array #[.null, .bool true, .string "test"]))
      = .ok (.array #[.null, .bool true, .string "test"]) := by native_decide

theorem roundtrip_object :
    Decode.decode (Encode.encode (.object [("key", .string "value"), ("num", .number 1)]))
      = .ok (.object [("key", .string "value"), ("num", .number 1)]) := by native_decide

end Data.Json
