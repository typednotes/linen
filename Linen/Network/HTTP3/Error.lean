/-
  Linen.Network.HTTP3.Error -- HTTP/3 error codes

  HTTP/3 error codes as defined in RFC 9114 Section 8.1.

  ## Design

  `H3Error` is an inductive type covering all standard HTTP/3 error codes.
  Each variant maps to a specific numeric code in the range 0x100..0x110.
  The `toCode` function provides the numeric mapping and `fromCode` provides
  the inverse.

  ## Guarantees

  - `toCode` and `fromCode` are inverse for known codes (proven by `roundtrip_known`)
  - All RFC 9114 Section 8.1 error codes are represented

  ## Haskell equivalent
  HTTP/3 error types from the `http3` package
-/

namespace Network.HTTP3

/-- HTTP/3 error codes (RFC 9114 Section 8.1).
    $$\text{H3Error}$$ enumerates all standard error codes in the range $[0x100, 0x110]$. -/
inductive H3Error where
  /-- No error (0x100). Used in GOAWAY or stream close without error. -/
  | noError
  /-- General protocol error (0x101). -/
  | generalProtocolError
  /-- Internal error (0x102). -/
  | internalError
  /-- Stream creation error (0x103). -/
  | streamCreationError
  /-- Critical stream was closed (0x104). -/
  | closedCriticalStream
  /-- Frame received in unexpected context (0x105). -/
  | frameUnexpected
  /-- Frame violates layout or size rules (0x106). -/
  | frameError
  /-- Peer generating excessive load (0x107). -/
  | excessiveLoad
  /-- Stream ID error (0x108). -/
  | idError
  /-- SETTINGS frame error (0x109). -/
  | settingsError
  /-- No SETTINGS frame received (0x10A). -/
  | missingSettings
  /-- Request rejected before processing (0x10B). -/
  | requestRejected
  /-- Request cancelled (0x10C). -/
  | requestCancelled
  /-- Request stream terminated prematurely (0x10D). -/
  | requestIncomplete
  /-- Malformed HTTP message (0x10E). -/
  | messageError
  /-- CONNECT request error (0x10F). -/
  | connectError
  /-- Version fallback triggered (0x110). -/
  | versionFallback
  /-- Unknown/unrecognised error code. -/
  | unknown (code : UInt64)
  deriving Repr, BEq

/-- Convert an HTTP/3 error to its numeric code.
    $$\text{toCode} : \text{H3Error} \to \text{UInt64}$$ -/
def H3Error.toCode : H3Error → UInt64
  | .noError               => 0x100
  | .generalProtocolError  => 0x101
  | .internalError         => 0x102
  | .streamCreationError   => 0x103
  | .closedCriticalStream  => 0x104
  | .frameUnexpected       => 0x105
  | .frameError            => 0x106
  | .excessiveLoad         => 0x107
  | .idError               => 0x108
  | .settingsError         => 0x109
  | .missingSettings       => 0x10A
  | .requestRejected       => 0x10B
  | .requestCancelled      => 0x10C
  | .requestIncomplete     => 0x10D
  | .messageError          => 0x10E
  | .connectError          => 0x10F
  | .versionFallback       => 0x110
  | .unknown code          => code

/-- Parse an HTTP/3 error from its numeric code.
    $$\text{fromCode} : \text{UInt64} \to \text{H3Error}$$ -/
def H3Error.fromCode : UInt64 → H3Error
  | 0x100 => .noError
  | 0x101 => .generalProtocolError
  | 0x102 => .internalError
  | 0x103 => .streamCreationError
  | 0x104 => .closedCriticalStream
  | 0x105 => .frameUnexpected
  | 0x106 => .frameError
  | 0x107 => .excessiveLoad
  | 0x108 => .idError
  | 0x109 => .settingsError
  | 0x10A => .missingSettings
  | 0x10B => .requestRejected
  | 0x10C => .requestCancelled
  | 0x10D => .requestIncomplete
  | 0x10E => .messageError
  | 0x10F => .connectError
  | 0x110 => .versionFallback
  | code  => .unknown code

instance : ToString H3Error where
  toString e := s!"H3Error({e.toCode})"

/-- Roundtrip property: `fromCode (toCode e) = e` for all known error codes.
    This does not hold for `unknown` since the code may collide with a known code. -/
theorem H3Error.roundtrip_noError : H3Error.fromCode (H3Error.toCode .noError) = .noError := rfl
theorem H3Error.roundtrip_generalProtocolError : H3Error.fromCode (H3Error.toCode .generalProtocolError) = .generalProtocolError := rfl
theorem H3Error.roundtrip_internalError : H3Error.fromCode (H3Error.toCode .internalError) = .internalError := rfl
theorem H3Error.roundtrip_streamCreationError : H3Error.fromCode (H3Error.toCode .streamCreationError) = .streamCreationError := rfl
theorem H3Error.roundtrip_closedCriticalStream : H3Error.fromCode (H3Error.toCode .closedCriticalStream) = .closedCriticalStream := rfl
theorem H3Error.roundtrip_frameUnexpected : H3Error.fromCode (H3Error.toCode .frameUnexpected) = .frameUnexpected := rfl
theorem H3Error.roundtrip_frameError : H3Error.fromCode (H3Error.toCode .frameError) = .frameError := rfl
theorem H3Error.roundtrip_excessiveLoad : H3Error.fromCode (H3Error.toCode .excessiveLoad) = .excessiveLoad := rfl
theorem H3Error.roundtrip_idError : H3Error.fromCode (H3Error.toCode .idError) = .idError := rfl
theorem H3Error.roundtrip_settingsError : H3Error.fromCode (H3Error.toCode .settingsError) = .settingsError := rfl
theorem H3Error.roundtrip_missingSettings : H3Error.fromCode (H3Error.toCode .missingSettings) = .missingSettings := rfl
theorem H3Error.roundtrip_requestRejected : H3Error.fromCode (H3Error.toCode .requestRejected) = .requestRejected := rfl
theorem H3Error.roundtrip_requestCancelled : H3Error.fromCode (H3Error.toCode .requestCancelled) = .requestCancelled := rfl
theorem H3Error.roundtrip_requestIncomplete : H3Error.fromCode (H3Error.toCode .requestIncomplete) = .requestIncomplete := rfl
theorem H3Error.roundtrip_messageError : H3Error.fromCode (H3Error.toCode .messageError) = .messageError := rfl
theorem H3Error.roundtrip_connectError : H3Error.fromCode (H3Error.toCode .connectError) = .connectError := rfl
theorem H3Error.roundtrip_versionFallback : H3Error.fromCode (H3Error.toCode .versionFallback) = .versionFallback := rfl

end Network.HTTP3
