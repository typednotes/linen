/-
  Linen.Network.HTTP2.Types — HTTP/2 connection and error types

  Provides error types and state tracking structures for HTTP/2 connections.

  ## Design

  Connection-level and stream-level errors are distinguished as separate types,
  since they have different handling requirements per RFC 9113. Connection errors
  cause GOAWAY; stream errors cause RST_STREAM.

  `HeaderBlockState` tracks the assembly of header blocks that span multiple
  HEADERS + CONTINUATION frames.

  ## Haskell equivalent
  `Network.HTTP2.Types` (https://hackage.haskell.org/package/http2)
-/
import Linen.Network.HTTP2.Frame.Types

namespace Network.HTTP2

/-- A connection-level error that results in sending GOAWAY and closing the connection.

    $$\text{ConnectionError} = \text{ErrorCode} \times \text{String}$$ -/
structure ConnectionError where
  /-- The error code to send in the GOAWAY frame. -/
  errorCode : ErrorCode
  /-- Human-readable description for debugging. -/
  message : String
  deriving Repr, Inhabited

instance : BEq ConnectionError where
  beq a b := a.errorCode == b.errorCode && a.message == b.message

instance : ToString ConnectionError where
  toString e := s!"ConnectionError({e.errorCode}: {e.message})"

/-- A stream-level error that results in sending RST_STREAM for that stream only.

    $$\text{StreamError} = \text{StreamId} \times \text{ErrorCode} \times \text{String}$$ -/
structure StreamError where
  /-- The stream that experienced the error. -/
  streamId : StreamId
  /-- The error code to send in the RST_STREAM frame. -/
  errorCode : ErrorCode
  /-- Human-readable description for debugging. -/
  message : String
  deriving Repr, Inhabited

instance : BEq StreamError where
  beq a b := a.streamId == b.streamId && a.errorCode == b.errorCode && a.message == b.message

instance : ToString StreamError where
  toString e := s!"StreamError(stream={e.streamId}, {e.errorCode}: {e.message})"

/-- State for tracking header block assembly across HEADERS + CONTINUATION frames.

    RFC 9113 Section 4.3: A header block that does not fit within a single HEADERS
    or PUSH_PROMISE frame is continued in one or more CONTINUATION frames. The header
    block ends when END_HEADERS is set.

    $$\text{HeaderBlockState} = \text{Idle} \mid \text{Assembling}(\text{StreamId}, \text{ByteArray})$$ -/
inductive HeaderBlockState where
  /-- No header block is being assembled. -/
  | idle
  /-- A header block is being assembled for the given stream.
      The ByteArray accumulates the header block fragment bytes. -/
  | assembling (streamId : StreamId) (fragments : ByteArray)
  deriving Inhabited

instance : BEq HeaderBlockState where
  beq
    | .idle, .idle => true
    | .assembling s1 f1, .assembling s2 f2 => s1 == s2 && f1 == f2
    | _, _ => false

namespace HeaderBlockState

/-- Check if we are currently assembling a header block. -/
@[inline] def isAssembling : HeaderBlockState → Bool
  | .idle => false
  | .assembling _ _ => true

/-- Get the stream ID being assembled, if any. -/
@[inline] def streamId? : HeaderBlockState → Option StreamId
  | .idle => none
  | .assembling sid _ => some sid

/-- Append a fragment to the header block being assembled.
    Returns `none` if not currently assembling. -/
def appendFragment (state : HeaderBlockState) (fragment : ByteArray) : Option HeaderBlockState :=
  match state with
  | .idle => none
  | .assembling sid fragments => some (.assembling sid (fragments ++ fragment))

/-- Get the complete assembled header block and reset to idle.
    Returns `none` if not currently assembling. -/
def complete : HeaderBlockState → Option (StreamId × ByteArray)
  | .idle => none
  | .assembling sid fragments => some (sid, fragments)

end HeaderBlockState

/-- HTTP/2 connection-level protocol result. -/
inductive HTTP2Result (α : Type) where
  /-- Success with a value. -/
  | ok (value : α)
  /-- Connection error requiring GOAWAY. -/
  | connectionError (error : ConnectionError)
  /-- Stream error requiring RST_STREAM. -/
  | streamError (error : StreamError)
  deriving Repr

namespace HTTP2Result

/-- Map a function over the success value. -/
def map (f : α → β) : HTTP2Result α → HTTP2Result β
  | .ok v => .ok (f v)
  | .connectionError e => .connectionError e
  | .streamError e => .streamError e

/-- Bind operation for chaining HTTP2Results. -/
def bind (r : HTTP2Result α) (f : α → HTTP2Result β) : HTTP2Result β :=
  match r with
  | .ok v => f v
  | .connectionError e => .connectionError e
  | .streamError e => .streamError e

end HTTP2Result

end Network.HTTP2
