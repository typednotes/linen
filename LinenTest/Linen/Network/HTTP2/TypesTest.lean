/-
  Tests for `Linen.Network.HTTP2.Types`.

  Connection/stream error types, the header-block assembly state machine, and
  the `HTTP2Result` map/bind are all pure, so they are checked with `#guard`.
-/
import Linen.Network.HTTP2.Types

open Network.HTTP2

namespace Tests.Network.HTTP2.Types

/-! ### ConnectionError / StreamError -/

#guard (ConnectionError.mk .protocolError "boom") == ConnectionError.mk .protocolError "boom"
#guard ((ConnectionError.mk .protocolError "a") == ConnectionError.mk .internalError "a") == false
#guard toString (ConnectionError.mk .protocolError "boom") == "ConnectionError(PROTOCOL_ERROR: boom)"

#guard (StreamError.mk (StreamId.fromWire 3) .cancel "x") == StreamError.mk (StreamId.fromWire 3) .cancel "x"
#guard ((StreamError.mk (StreamId.fromWire 3) .cancel "x")
          == StreamError.mk (StreamId.fromWire 5) .cancel "x") == false
#guard toString (StreamError.mk (StreamId.fromWire 3) .cancel "oops")
        == "StreamError(stream=3, CANCEL: oops)"

/-! ### HeaderBlockState — CONTINUATION assembly -/

#guard HeaderBlockState.idle.isAssembling == false
#guard (HeaderBlockState.assembling (StreamId.fromWire 1) "ab".toUTF8).isAssembling == true
#guard HeaderBlockState.idle.streamId? == none
#guard (HeaderBlockState.assembling (StreamId.fromWire 7) ByteArray.empty).streamId?
        == some (StreamId.fromWire 7)

-- appendFragment only works while assembling, and concatenates fragments.
#guard (HeaderBlockState.idle.appendFragment "x".toUTF8).isNone
#guard ((HeaderBlockState.assembling (StreamId.fromWire 1) "ab".toUTF8).appendFragment "cd".toUTF8)
        == some (HeaderBlockState.assembling (StreamId.fromWire 1) "abcd".toUTF8)

-- complete returns the stream + accumulated bytes (or none when idle).
#guard (HeaderBlockState.idle.complete).isNone
#guard ((HeaderBlockState.assembling (StreamId.fromWire 1) "hdr".toUTF8).complete).map (·.1)
        == some (StreamId.fromWire 1)
#guard ((HeaderBlockState.assembling (StreamId.fromWire 1) "hdr".toUTF8).complete).map (fun x => x.2)
        == some "hdr".toUTF8

/-! ### HTTP2Result — map / bind -/

-- map only touches the success branch.
#guard (match HTTP2Result.map (· + 1) (HTTP2Result.ok 41) with
        | .ok v => v == 42 | _ => false)
-- map over an error is the identity on that error.
#guard (match HTTP2Result.map (· + 1) (.connectionError (.mk .internalError "e") : HTTP2Result Nat) with
        | .connectionError e => e.message == "e" | _ => false)
-- bind threads success and short-circuits errors.
#guard (match (HTTP2Result.ok 10).bind (fun n => HTTP2Result.ok (n * 2)) with
        | .ok v => v == 20 | _ => false)
#guard (match (HTTP2Result.streamError (.mk (StreamId.fromWire 1) .cancel "x")
              : HTTP2Result Nat).bind (fun n => .ok (n + 1)) with
        | .streamError e => e.errorCode == ErrorCode.cancel | _ => false)

end Tests.Network.HTTP2.Types
