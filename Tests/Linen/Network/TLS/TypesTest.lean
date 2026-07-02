/-
  Tests for `Linen.Network.TLS.Types`.
-/
import Linen.Network.TLS.Types

open Network.TLS

namespace Tests.Network.TLS.Types

/-! ### `TLSVersion` -/

#guard toString TLSVersion.tls10 == "TLSv1.0"
#guard toString TLSVersion.tls11 == "TLSv1.1"
#guard toString TLSVersion.tls12 == "TLSv1.2"
#guard toString TLSVersion.tls13 == "TLSv1.3"
#guard TLSVersion.tls12 == TLSVersion.tls12
#guard TLSVersion.tls12 != TLSVersion.tls13

/-! ### `CipherID` -/

#guard (0x1301 : CipherID) == 0x1301

/-! ### `TLSOutcome` -/

#guard (match (TLSOutcome.ok 42 : TLSOutcome Nat) with | .ok n => n == 42 | _ => false)
#guard (match (TLSOutcome.wantRead : TLSOutcome Nat) with | .wantRead => true | _ => false)
#guard (match (TLSOutcome.wantWrite : TLSOutcome Nat) with | .wantWrite => true | _ => false)
#guard (match (TLSOutcome.error (IO.userError "boom") : TLSOutcome Nat) with
  | .error _ => true
  | _ => false)

end Tests.Network.TLS.Types
