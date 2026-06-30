/-
  Linen.Network.HTTP2.Stream — HTTP/2 stream state management

  Implements HTTP/2 stream lifecycle and state machine as defined in
  RFC 9113 Section 5.1. Uses `Std.HashMap` for O(1) stream lookup.
-/
import Linen.Network.HTTP2.Frame.Types
import Linen.Network.HTTP2.Types
import Std.Data.HashMap

namespace Network.HTTP2

inductive StreamState where
  | idle | open | halfClosedLocal | halfClosedRemote
  | resetLocal | resetRemote | closed | reservedLocal | reservedRemote
  deriving Repr, Inhabited, BEq

instance : ToString StreamState where
  toString
    | .idle => "idle" | .open => "open"
    | .halfClosedLocal => "half-closed (local)"
    | .halfClosedRemote => "half-closed (remote)"
    | .resetLocal => "reset (local)" | .resetRemote => "reset (remote)"
    | .closed => "closed"
    | .reservedLocal => "reserved (local)" | .reservedRemote => "reserved (remote)"

structure StreamInfo where
  streamId : StreamId
  state : StreamState
  sendWindow : Int
  recvWindow : Int
  priorityExclusive : Bool := false
  priorityDependency : StreamId := StreamId.zero
  priorityWeight : UInt8 := 15
  deriving Repr, Inhabited

@[inline] def isClientStream (streamId : StreamId) : Bool := streamId.isClientInitiated
@[inline] def isServerStream (streamId : StreamId) : Bool := streamId.isServerInitiated
@[inline] def isConnectionStream (streamId : StreamId) : Bool := streamId.val == 0
@[inline] def validateStreamId (newId lastId : StreamId) : Bool := newId.val > lastId.val

def client_server_complementary_check (id : StreamId) (_h : id.val ≠ 0) : Bool :=
  isClientStream id || isServerStream id

structure StreamTable where
  streams : Std.HashMap StreamId StreamInfo
  lastClientStreamId : StreamId
  lastServerStreamId : StreamId
  nextServerStreamId : StreamId
  deriving Inhabited

instance : Repr StreamTable where
  reprPrec t _ :=
    "StreamTable(size=" ++ repr t.streams.size ++
    ", lastClient=" ++ repr t.lastClientStreamId ++
    ", lastServer=" ++ repr t.lastServerStreamId ++ ")"

namespace StreamTable

def empty : StreamTable :=
  { streams := {}
    lastClientStreamId := StreamId.zero
    lastServerStreamId := StreamId.zero
    nextServerStreamId := { val := 2, hBit := by native_decide } }

def lookup (table : StreamTable) (streamId : StreamId) : Option StreamInfo :=
  table.streams[streamId]?

def upsert (table : StreamTable) (info : StreamInfo) : StreamTable :=
  { table with streams := table.streams.insert info.streamId info }

def openClientStream (table : StreamTable) (streamId : StreamId)
    (initialWindow : Int) : Option StreamTable :=
  if !isClientStream streamId then none
  else if !validateStreamId streamId table.lastClientStreamId then none
  else
    let info : StreamInfo :=
      { streamId := streamId
        state := .open
        sendWindow := initialWindow
        recvWindow := initialWindow }
    some { (table.upsert info) with lastClientStreamId := streamId }

def updateState (table : StreamTable) (streamId : StreamId) (state : StreamState) : StreamTable :=
  match table.lookup streamId with
  | some info => table.upsert { info with state := state }
  | none => table

def updatePriority (table : StreamTable) (streamId : StreamId)
    (exclusive : Bool) (dependency : StreamId) (weight : UInt8) : StreamTable :=
  match table.lookup streamId with
  | some info =>
    table.upsert { info with
      priorityExclusive := exclusive
      priorityDependency := dependency
      priorityWeight := weight }
  | none =>
    table.upsert
      { streamId := streamId
        state := .idle
        sendWindow := 0
        recvWindow := 0
        priorityExclusive := exclusive
        priorityDependency := dependency
        priorityWeight := weight }

def activeStreamCount (table : StreamTable) : Nat :=
  table.streams.fold (fun acc _ s =>
    match s.state with
    | .idle | .closed | .resetLocal | .resetRemote => acc
    | _ => acc + 1) 0

end StreamTable

end Network.HTTP2
