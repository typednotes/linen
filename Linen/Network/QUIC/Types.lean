/-
  Linen.Network.QUIC.Types -- QUIC transport protocol types

  Core types for QUIC (RFC 9000). Ports Haskell's `Network.QUIC` (types) from
  the `quic` package, built on the standard library (`ByteArray`, `UInt32`,
  `UInt64`, `Option`). Defines connection identifiers, versions, transport
  parameters, stream identifiers, error codes, and TLS configuration.

  ## Design

  Types encode RFC 9000 invariants directly:
  - `ConnectionId` carries a proof that `bytes.size <= 20` (Section 17.2)
  - `StreamId` uses `UInt64` with helper functions for the 2-bit type encoding
  - `TransportParams` fields have sensible defaults per Section 18

  ## Guarantees

  - Connection IDs are bounded to 20 bytes by construction
  - Stream type is extractable from the low 2 bits of `StreamId.val`
  - Transport parameter defaults match RFC 9000 Section 18 recommendations
-/

namespace Network.QUIC

/-- A QUIC Connection ID, variable-length up to 20 bytes (RFC 9000 Section 17.2).
    $$\text{ConnectionId} = \{ b : \text{ByteArray} \mid |b| \leq 20 \}$$ -/
structure ConnectionId where
  /-- Raw bytes of the connection ID. -/
  bytes : ByteArray
  /-- Proof that the byte length does not exceed 20. -/
  hLen : bytes.size ≤ 20 := by omega

instance : BEq ConnectionId where
  beq a b := a.bytes == b.bytes

instance : Hashable ConnectionId where
  hash cid := hash cid.bytes.toList

instance : ToString ConnectionId where
  toString cid :=
    let hexDigit (n : UInt8) : Char :=
      if n < 10 then Char.ofNat (48 + n.toNat)  -- '0'..'9'
      else Char.ofNat (87 + n.toNat)             -- 'a'..'f'
    let bytes := cid.bytes
    let chars := bytes.foldl (init := #[]) fun acc b =>
      acc.push (hexDigit (b >>> 4)) |>.push (hexDigit (b &&& 0x0f))
    String.ofList chars.toList

/-- The empty connection ID (zero length).
    $$\text{empty} = \text{ConnectionId}\{\text{bytes} = []\}$$ -/
def ConnectionId.empty : ConnectionId :=
  { bytes := ByteArray.empty, hLen := by native_decide }

/-- QUIC protocol version (32-bit).
    $$\text{Version} = \text{UInt32}$$ -/
structure Version where
  val : UInt32
  deriving BEq, Repr

instance : ToString Version where
  toString v := s!"0x{v.val.toNat}"

/-- QUIC version 1 (RFC 9000). -/
def Version.v1 : Version := ⟨1⟩

/-- QUIC version 2 (RFC 9369). -/
def Version.v2 : Version := ⟨0x6b3343cf⟩

/-- The HTTP/3 ALPN uses QUIC v1. -/
def Version.http3 : Version := Version.v1

/-- Transport parameters as defined in RFC 9000 Section 18.
    Default values match the Haskell `quic` package's `defaultParameters`
    (RFC 9000 itself mandates no defaults for the flow-control/stream-count
    fields; they default to 0 if the parameter is absent).
    $$\text{TransportParams} = \{ \text{maxIdleTimeout} : \mathbb{N},\; \ldots \}$$ -/
structure TransportParams where
  /-- Maximum idle timeout in milliseconds. 0 means disabled. Default: 30000ms. -/
  maxIdleTimeout : Nat := 30000
  /-- Maximum UDP payload size in bytes. Default: 2048 (`quic`'s `defaultParameters`;
      RFC 9000's wire-format ceiling is 65527, exposed separately as a protocol limit). -/
  maxUDPPayloadSize : Nat := 2048
  /-- Initial flow control limit for the connection. Default: 16 MiB. -/
  initialMaxData : Nat := 16777216
  /-- Initial flow control limit for locally-initiated bidirectional streams. Default: 256 KiB. -/
  initialMaxStreamDataBidiLocal : Nat := 262144
  /-- Initial flow control limit for remotely-initiated bidirectional streams. Default: 256 KiB. -/
  initialMaxStreamDataBidiRemote : Nat := 262144
  /-- Initial flow control limit for unidirectional streams. Default: 256 KiB. -/
  initialMaxStreamDataUni : Nat := 262144
  /-- Maximum number of bidirectional streams the peer may initiate. Default: 64. -/
  initialMaxStreamsBidi : Nat := 64
  /-- Maximum number of unidirectional streams the peer may initiate. Default: 3. -/
  initialMaxStreamsUni : Nat := 3
  /-- ACK delay exponent (log2 microseconds). Default: 3 (= 8us). -/
  ackDelayExponent : Nat := 3
  /-- Maximum ACK delay in milliseconds. Default: 25ms. -/
  maxAckDelay : Nat := 25
  deriving Repr, BEq

/-- Default transport parameters, matching the Haskell `quic` package's
    `defaultParameters`.
    $$\text{defaultTransportParams} = \text{TransportParams}\{\}$$ -/
def TransportParams.default : TransportParams := {}

/-- QUIC Stream ID (62-bit, RFC 9000 Section 2.1).
    The two least-significant bits encode the stream type:
    - Bit 0: initiator (0 = client, 1 = server)
    - Bit 1: directionality (0 = bidirectional, 1 = unidirectional)
    $$\text{StreamId} = \text{UInt64}$$ -/
structure StreamId where
  val : UInt64
  deriving BEq, Repr, Hashable

instance : ToString StreamId where
  toString sid := s!"{sid.val}"

/-- Stream type classification based on the low 2 bits of the Stream ID. -/
inductive StreamType where
  /-- Client-initiated bidirectional stream (0x0). -/
  | clientBidi
  /-- Server-initiated bidirectional stream (0x1). -/
  | serverBidi
  /-- Client-initiated unidirectional stream (0x2). -/
  | clientUni
  /-- Server-initiated unidirectional stream (0x3). -/
  | serverUni
  deriving BEq, Repr

/-- Extract the stream type from a stream ID.
    $$\text{streamType}(id) = id.\text{val} \mathbin{\&} 0x03$$ -/
def StreamId.streamType (sid : StreamId) : StreamType :=
  match (sid.val &&& 0x03).toNat with
  | 0 => .clientBidi
  | 1 => .serverBidi
  | 2 => .clientUni
  | _ => .serverUni

/-- Check whether a stream is bidirectional.
    $$\text{isBidi}(id) \iff (id.\text{val} \mathbin{\&} 0x02) = 0$$ -/
def StreamId.isBidi (sid : StreamId) : Bool :=
  (sid.val &&& 0x02) == 0

/-- Check whether a stream is unidirectional.
    $$\text{isUni}(id) = \neg\,\text{isBidi}(id)$$ -/
def StreamId.isUni (sid : StreamId) : Bool :=
  !sid.isBidi

/-- Check whether a stream was initiated by the client.
    $$\text{isClientInitiated}(id) \iff (id.\text{val} \mathbin{\&} 0x01) = 0$$ -/
def StreamId.isClientInitiated (sid : StreamId) : Bool :=
  (sid.val &&& 0x01) == 0

/-- Check whether a stream was initiated by the server.
    $$\text{isServerInitiated}(id) = \neg\,\text{isClientInitiated}(id)$$ -/
def StreamId.isServerInitiated (sid : StreamId) : Bool :=
  !sid.isClientInitiated

/-- QUIC transport error codes (RFC 9000 Section 20).
    $$\text{TransportError}$$ enumerates all standard error codes. -/
inductive TransportError where
  /-- No error. This is used when the connection or stream needs
      to be closed but there is no error to signal. (0x0) -/
  | noError
  /-- Implementation error not covered by a more specific code. (0x1) -/
  | internalError
  /-- Server refuses to accept new connections. (0x2) -/
  | connectionRefused
  /-- Flow control limit violated. (0x3) -/
  | flowControlError
  /-- Too many streams opened. (0x4) -/
  | streamLimitError
  /-- Frame received in invalid stream state. (0x5) -/
  | streamStateError
  /-- Change to final size violates protocol. (0x6) -/
  | finalSizeError
  /-- Frame encoding is invalid. (0x7) -/
  | frameEncodingError
  /-- Transport parameter is invalid. (0x8) -/
  | transportParameterError
  /-- Connection ID limit exceeded. (0x9) -/
  | connectionIdLimitError
  /-- Protocol violation not covered by more specific codes. (0xA) -/
  | protocolViolation
  /-- Received token is invalid. (0xB) -/
  | invalidToken
  /-- Application-specific error. (0xC) -/
  | applicationError
  /-- CRYPTO frame data buffer overflowed. (0xD) -/
  | cryptoBufferExceeded
  /-- Key update error. (0xE) -/
  | keyUpdateError
  /-- AEAD limit reached. (0xF) -/
  | aeadLimitReached
  /-- No viable network path exists. (0x10) -/
  | noViablePath
  /-- TLS alert received, with the TLS alert code. (0x100 + alert) -/
  | cryptoError (alertCode : UInt8)
  /-- Unknown/unrecognised error code. -/
  | unknown (code : UInt64)
  deriving Repr, BEq

/-- Convert a transport error to its numeric code.
    $$\text{toCode} : \text{TransportError} \to \text{UInt64}$$ -/
def TransportError.toCode : TransportError → UInt64
  | .noError                  => 0x0
  | .internalError            => 0x1
  | .connectionRefused        => 0x2
  | .flowControlError         => 0x3
  | .streamLimitError         => 0x4
  | .streamStateError         => 0x5
  | .finalSizeError           => 0x6
  | .frameEncodingError       => 0x7
  | .transportParameterError  => 0x8
  | .connectionIdLimitError   => 0x9
  | .protocolViolation        => 0xA
  | .invalidToken             => 0xB
  | .applicationError         => 0xC
  | .cryptoBufferExceeded     => 0xD
  | .keyUpdateError           => 0xE
  | .aeadLimitReached         => 0xF
  | .noViablePath             => 0x10
  | .cryptoError alertCode    => 0x100 + alertCode.toUInt64
  | .unknown code             => code

instance : ToString TransportError where
  toString e := s!"TransportError({e.toCode})"

/-- TLS configuration for QUIC.
    $$\text{TLSConfig} = \{ \text{certFile} : \text{Option String},\; \text{keyFile} : \text{Option String},\; \text{alpn} : \text{List String} \}$$ -/
structure TLSConfig where
  /-- Path to the TLS certificate file. Required for servers. -/
  certFile : Option String := none
  /-- Path to the TLS private key file. Required for servers. -/
  keyFile : Option String := none
  /-- ALPN protocol identifiers. Default: ["h3"] for HTTP/3. -/
  alpn : List String := ["h3"]
  deriving Repr, BEq

end Network.QUIC
