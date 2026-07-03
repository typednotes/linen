import Linen.Network.WebApp.Extra.EventSource.EventStream

/-! ### Tests for `Linen.Network.WebApp.Extra.EventSource.EventStream`

    Coverage: `dataEvent`/`namedEvent` split multi-line data on `\n`;
    `retryEvent`/`commentEvent` render their fixed SSE framing. -/

open Network.WebApp.Extra.EventSource Network.WebApp.Extra.EventSource.EventStream

namespace Tests.Network.WebApp.Extra.EventSource.EventStream

#guard (dataEvent "line1\nline2").eventData == ["line1", "line2"]
#guard (dataEvent "hi").render == "data: hi\n\n"

#guard (namedEvent "update" "payload").eventName == some "update"
#guard (namedEvent "update" "payload").render == "event: update\ndata: payload\n\n"

#guard retryEvent 5000 == "retry: 5000\n\n"
#guard commentEvent "ping" == ": ping\n\n"
#guard commentEvent == ": \n\n"

end Tests.Network.WebApp.Extra.EventSource.EventStream
