/-
  Tests for `Linen.Network.QUIC.Types`.

  All definitions are pure, so behaviour is checked with `#guard`:
  `ConnectionId` construction and its length invariant, `Version`/`StreamId`
  equality and classification, `TransportParams` defaults, `TransportError`
  code mapping, and `TLSConfig` defaults.
-/
import Linen.Network.QUIC.Types

open Network.QUIC

namespace Tests.Network.QUIC.Types

/-! ### ConnectionId -/

#guard ConnectionId.empty.bytes.size == 0
#guard ConnectionId.empty.bytes.size ≤ 20
#guard ConnectionId.empty == ConnectionId.empty
#guard toString ConnectionId.empty == ""

#guard
  let cid : ConnectionId := { bytes := ByteArray.mk #[0x01, 0x02, 0x03], hLen := by native_decide }
  cid.bytes.size == 3 && toString cid == "010203"

/-! ### Version -/

#guard Version.v1 == Version.http3
#guard Version.v1.val == 1
#guard Version.v2.val == 0x6b3343cf
#guard (Version.v2 == Version.v1) == false
#guard toString Version.v1 == "0x1"

/-! ### TransportParams -/

#guard TransportParams.default.maxIdleTimeout == 30000
#guard TransportParams.default.maxUDPPayloadSize == 2048
#guard TransportParams.default.initialMaxData == 16777216
#guard TransportParams.default.initialMaxStreamDataBidiLocal == 262144
#guard TransportParams.default.initialMaxStreamDataBidiRemote == 262144
#guard TransportParams.default.initialMaxStreamDataUni == 262144
#guard TransportParams.default.initialMaxStreamsBidi == 64
#guard TransportParams.default.initialMaxStreamsUni == 3
#guard TransportParams.default.ackDelayExponent == 3
#guard TransportParams.default.maxAckDelay == 25
#guard TransportParams.default == TransportParams.default

/-! ### StreamId -/

#guard StreamId.streamType ⟨0⟩ == .clientBidi
#guard StreamId.streamType ⟨1⟩ == .serverBidi
#guard StreamId.streamType ⟨2⟩ == .clientUni
#guard StreamId.streamType ⟨3⟩ == .serverUni
#guard StreamId.streamType ⟨4⟩ == .clientBidi  -- wraps every 4 ids
#guard StreamId.isBidi ⟨0⟩
#guard StreamId.isUni ⟨2⟩
#guard StreamId.isClientInitiated ⟨0⟩
#guard StreamId.isServerInitiated ⟨1⟩
#guard StreamId.mk 42 == StreamId.mk 42
#guard toString (StreamId.mk 42) == "42"

/-! ### TransportError -/

#guard TransportError.noError.toCode == 0
#guard TransportError.internalError.toCode == 1
#guard TransportError.protocolViolation.toCode == 0xA
#guard (TransportError.cryptoError 48).toCode == 0x100 + 48
#guard (TransportError.unknown 999).toCode == 999
#guard toString TransportError.noError == "TransportError(0)"

/-! ### TLSConfig -/

#guard (({} : TLSConfig)).alpn == ["h3"]
#guard (({} : TLSConfig)).certFile == none
#guard (({} : TLSConfig)).keyFile == none

end Tests.Network.QUIC.Types
