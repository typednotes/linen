/-
  Tests for `Linen.Codec.Picture.InternalHelper` — checks `runGet`/
  `runGetStrict` and `getRemainingBytes` against `Std.Internal.Parsec`.
-/
import Linen.Codec.Picture.InternalHelper

open Codec.Picture
open Std.Internal.Parsec.ByteArray

#guard match runGet (pbyte 1) (ByteArray.mk #[1, 2, 3]) with | .ok b => b == 1 | .error _ => false
#guard match runGetStrict (pbyte 1) (ByteArray.mk #[1, 2, 3]) with
  | .ok b => b == 1 | .error _ => false

#guard match runGet (pbyte 9) (ByteArray.mk #[1, 2, 3]) with | .ok _ => false | .error _ => true

-- leftover input is not an error, matching upstream's `runGetOrFail`
#guard match runGet (pbyte 1) (ByteArray.mk #[1, 2, 3]) with | .ok b => b == 1 | .error _ => false

#guard match runGet getRemainingBytes (ByteArray.mk #[1, 2, 3]) with
  | .ok b => b == ByteArray.mk #[1, 2, 3] | .error _ => false
#guard match runGet (pbyte 1 *> getRemainingBytes) (ByteArray.mk #[1, 2, 3]) with
  | .ok b => b == ByteArray.mk #[2, 3] | .error _ => false
#guard match runGet getRemainingBytes (ByteArray.mk #[]) with
  | .ok b => b == ByteArray.empty | .error _ => false
