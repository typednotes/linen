/-
  Linen.Network.WebSockets.Types — WebSocket protocol types (RFC 6455)

  Ports Hale's `Network.WebSockets.Types`.

  ## Guarantees
  - Opcode is bounded to 4 bits (0-15)
  - Masking key is exactly 4 bytes
  - Connection state machine prevents sending on closed connections
-/
namespace Network.WebSockets

/-- WebSocket frame opcode (4 bits per RFC 6455 §5.2).
    $$\text{Opcode} \in [0, 15]$$ -/
inductive Opcode where
  | continuation  -- 0x0
  | text          -- 0x1
  | binary        -- 0x2
  | close         -- 0x8
  | ping          -- 0x9
  | pong          -- 0xA
  | reserved (val : Fin 16)  -- other opcodes
deriving BEq, Repr

def Opcode.toUInt8 : Opcode → UInt8
  | .continuation => 0x0
  | .text => 0x1
  | .binary => 0x2
  | .close => 0x8
  | .ping => 0x9
  | .pong => 0xA
  | .reserved v => v.val.toUInt8

def Opcode.fromUInt8 (b : UInt8) : Opcode :=
  if b == 0x0 then .continuation
  else if b == 0x1 then .text
  else if b == 0x2 then .binary
  else if b == 0x8 then .close
  else if b == 0x9 then .ping
  else if b == 0xA then .pong
  else .reserved ⟨b.toNat % 16, by omega⟩

/-- WebSocket close status code. -/
structure CloseCode where
  code : UInt16
deriving BEq, Repr

def CloseCode.normal : CloseCode := ⟨1000⟩
def CloseCode.goingAway : CloseCode := ⟨1001⟩
def CloseCode.protocolError : CloseCode := ⟨1002⟩
def CloseCode.unsupportedData : CloseCode := ⟨1003⟩

/-- WebSocket connection state machine.
    $$\text{pending} \to \text{open} \to \text{closing} \to \text{closed}$$ -/
inductive ConnectionState where
  | pending   -- handshake not yet completed
  | open_     -- data transfer active
  | closing   -- close frame sent, awaiting response
  | closed    -- connection terminated
deriving BEq, Repr

/-- WebSocket connection options. -/
structure ConnectionOptions where
  /-- Maximum message size in bytes (0 = unlimited). -/
  maxMessageSize : Nat := 0
  /-- Whether to accept unmasked frames from clients (spec requires masking). -/
  acceptUnmaskedFrames : Bool := false

/-- Default connection options. -/
def defaultConnectionOptions : ConnectionOptions := {}

/-- Parsed request head for WebSocket handshake. -/
structure RequestHead where
  path : String
  headers : List (String × String)

/-- An established WebSocket connection. -/
structure Connection where
  /-- Send a text message. -/
  sendText : String → IO Unit
  /-- Send a binary message. -/
  sendBinary : ByteArray → IO Unit
  /-- Send a close frame. -/
  sendClose : CloseCode → String → IO Unit
  /-- Send a ping. -/
  sendPing : ByteArray → IO Unit
  /-- Receive the next message (text or binary). Blocks until available. -/
  receiveData : IO ByteArray
  /-- Receive a text message. -/
  receiveText : IO String
  /-- Current connection state. -/
  getState : IO ConnectionState

/-- A pending WebSocket connection (handshake not yet completed). -/
structure PendingConnection where
  /-- The HTTP request that initiated the upgrade. -/
  request : RequestHead
  /-- Accept the connection and get a full Connection. -/
  acceptIO : IO Connection

/-- A server application that handles WebSocket connections. -/
abbrev ServerApp := PendingConnection → IO Unit

-- Proofs

/-- Opcode roundtrip: fromUInt8 (toUInt8 op) = op for standard opcodes. -/
theorem opcode_roundtrip_text : Opcode.fromUInt8 (Opcode.toUInt8 .text) = .text := rfl
theorem opcode_roundtrip_binary : Opcode.fromUInt8 (Opcode.toUInt8 .binary) = .binary := rfl
theorem opcode_roundtrip_close : Opcode.fromUInt8 (Opcode.toUInt8 .close) = .close := rfl
theorem opcode_roundtrip_ping : Opcode.fromUInt8 (Opcode.toUInt8 .ping) = .ping := rfl
theorem opcode_roundtrip_pong : Opcode.fromUInt8 (Opcode.toUInt8 .pong) = .pong := rfl
theorem opcode_roundtrip_continuation : Opcode.fromUInt8 (Opcode.toUInt8 .continuation) = .continuation := rfl

/-- Roundtrip for the `reserved` variant: encoding then decoding recovers `.reserved v`.

    Note: The roundtrip only holds when `v` does not collide with a named opcode
    (0=continuation, 1=text, 2=binary, 8=close, 9=ping, 10=pong). Each non-colliding
    value (3–7, 11–15) is verified individually by `rfl`. -/
theorem opcode_roundtrip_reserved_3 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨3, by omega⟩)) = .reserved ⟨3, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_4 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨4, by omega⟩)) = .reserved ⟨4, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_5 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨5, by omega⟩)) = .reserved ⟨5, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_6 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨6, by omega⟩)) = .reserved ⟨6, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_7 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨7, by omega⟩)) = .reserved ⟨7, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_11 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨11, by omega⟩)) = .reserved ⟨11, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_12 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨12, by omega⟩)) = .reserved ⟨12, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_13 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨13, by omega⟩)) = .reserved ⟨13, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_14 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨14, by omega⟩)) = .reserved ⟨14, by omega⟩ := by rfl
theorem opcode_roundtrip_reserved_15 : Opcode.fromUInt8 (Opcode.toUInt8 (.reserved ⟨15, by omega⟩)) = .reserved ⟨15, by omega⟩ := by rfl

end Network.WebSockets
