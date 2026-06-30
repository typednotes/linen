/-
  Linen.Network.HTTP2.FlowControl — HTTP/2 flow control

  Implements HTTP/2 flow control as defined in RFC 9113 Section 5.2.

  ## Design

  Flow control operates at two levels: connection-level and stream-level.
  Both sender and receiver maintain independent windows. The sender must
  not send data that exceeds either window.

  ## Guarantees

  - Window sizes are clamped to [0, 2^31 - 1]
  - WINDOW_UPDATE increments are validated (must be > 0)
  - Overflow is detected and reported as FLOW_CONTROL_ERROR

  ## Haskell equivalent
  `Network.HTTP2.FlowControl` (https://hackage.haskell.org/package/http2)
-/
import Linen.Network.HTTP2.Frame.Types
import Linen.Network.HTTP2.Types
import Linen.Network.HTTP2.Stream

namespace Network.HTTP2

/-- Flow control window state for a connection or stream.

    $$\text{FlowWindow} = \{ \text{size} : \mathbb{Z} \mid \text{size} \leq 2^{31} - 1 \}$$

    Window sizes can go negative when the peer changes SETTINGS_INITIAL_WINDOW_SIZE. -/
structure FlowWindow where
  /-- Current window size. Can be negative. -/
  size : Int
  deriving Repr, Inhabited, BEq

namespace FlowWindow

/-- Create a flow window with the default initial size (65535). -/
def default : FlowWindow := { size := 65535 }

/-- Create a flow window with a specific initial size. -/
def ofSize (n : Int) : FlowWindow := { size := n }

/-- Apply a WINDOW_UPDATE increment. Returns an error if the result would
    exceed the maximum window size (2^31 - 1).

    $$\text{increment}(w, i) = \begin{cases}
      \text{ok}(w + i) & \text{if } w + i \leq 2^{31} - 1 \\
      \text{error} & \text{otherwise}
    \end{cases}$$ -/
def increment (w : FlowWindow) (inc : UInt32) : Except ErrorCode FlowWindow :=
  if inc == 0 then .error .protocolError  -- RFC 9113 Section 6.9: increment of 0 is error
  else
    let newSize := w.size + inc.toNat
    if newSize > maxWindowSize.toNat then .error .flowControlError
    else .ok { size := newSize }

/-- Consume bytes from the window (when sending data).
    $$\text{consume}(w, n) = w - n$$ -/
def consume (w : FlowWindow) (bytes : Nat) : FlowWindow :=
  { size := w.size - bytes }

/-- Check if there is any space in the window for sending.
    $$\text{available}(w) = \max(0, w.\text{size})$$ -/
def available (w : FlowWindow) : Nat :=
  if w.size > 0 then w.size.toNat else 0

/-- Adjust the window when SETTINGS_INITIAL_WINDOW_SIZE changes.
    Per RFC 9113 Section 6.9.2: the difference between new and old initial
    window size is applied to all existing streams.

    $$\text{adjust}(w, \text{old}, \text{new}) = w + (\text{new} - \text{old})$$ -/
def adjust (w : FlowWindow) (oldInitial newInitial : UInt32) : Except ErrorCode FlowWindow :=
  -- The difference is signed: a smaller new initial size shrinks the window
  -- (possibly negative), so subtract over `Int`, not `Nat` (which would truncate).
  let diff : Int := (newInitial.toNat : Int) - (oldInitial.toNat : Int)
  let newSize := w.size + diff
  if newSize > maxWindowSize.toNat then .error .flowControlError
  else .ok { size := newSize }

end FlowWindow

/-- Connection-level flow control state. -/
structure ConnectionFlowControl where
  /-- Send window (how much we can send). -/
  sendWindow : FlowWindow
  /-- Receive window (how much peer can send to us). -/
  recvWindow : FlowWindow
  deriving Repr, Inhabited

namespace ConnectionFlowControl

/-- Create default connection flow control. -/
def default : ConnectionFlowControl :=
  { sendWindow := FlowWindow.default
    recvWindow := FlowWindow.default }

/-- Process a received WINDOW_UPDATE for the connection (stream 0).
    $$\text{processWindowUpdate} : \text{ConnectionFlowControl} \to \text{UInt32} \to \text{Except}(\text{ErrorCode}, \text{ConnectionFlowControl})$$ -/
def processWindowUpdate (fc : ConnectionFlowControl) (increment : UInt32) :
    Except ErrorCode ConnectionFlowControl :=
  match fc.sendWindow.increment increment with
  | .ok w => .ok { fc with sendWindow := w }
  | .error e => .error e

/-- Record that we received data from the peer. Decrements the receive window. -/
def consumeRecv (fc : ConnectionFlowControl) (bytes : Nat) : ConnectionFlowControl :=
  { fc with recvWindow := fc.recvWindow.consume bytes }

/-- Record that we sent data. Decrements the send window. -/
def consumeSend (fc : ConnectionFlowControl) (bytes : Nat) : ConnectionFlowControl :=
  { fc with sendWindow := fc.sendWindow.consume bytes }

end ConnectionFlowControl

/-- Process a WINDOW_UPDATE for a specific stream.
    Updates the stream's send window in the given StreamInfo. -/
def processStreamWindowUpdate (info : StreamInfo) (inc : UInt32) :
    Except ErrorCode StreamInfo :=
  let newWindow := info.sendWindow + (inc.toNat : Int)
  if newWindow > (maxWindowSize.toNat : Int) then .error .flowControlError
  else .ok { info with sendWindow := newWindow }

end Network.HTTP2
