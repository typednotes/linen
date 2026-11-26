import Linen.Control.Resource
import Linen.Network.HTTP.Client.Connection
import Linen.Network.HTTP.Client.Response

/-! # Scoped HTTP/1.1 exchange
The runner fixes the request and owns its transport for the exchange.
-/

namespace Network.HTTP.Client
open Control.Resource

private opaque exchangeAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- State of a one-request HTTP exchange. -/
inductive ExchangeState where
  | ready
  | awaitingResponse
  | received

/-- A step in a single HTTP exchange with a privately owned transport. -/
abbrev ExchangeM :=
  Action exchangeAccess.val (fun _ : ExchangeState => Connection × Request)

namespace ExchangeM

/-- Send the request fixed by the runner, moving to the response phase. -/
def send : ExchangeM .ready .awaitingResponse Unit :=
  Program.step exchangeAccess.property fun current => do
    sendRequest current.1 current.2
    return ⟨(), current⟩

/-- Receive and decode the response before the transport closes. -/
def receive : ExchangeM .awaitingResponse .received Response :=
  Program.step exchangeAccess.property fun current => do
    return ⟨← receiveResponse current.1, current⟩

/-- Connect to the request's origin and run one exchange. The transport
closes on return or error. -/
def run (request : Request) (action : ExchangeM .ready .received α) : IO α := do
  let conn ← connect request.host request.port request.isSecure request.timeoutMillis
  try
    (·.1) <$> Program.run exchangeAccess.property action (conn, request)
  finally
    conn.connClose

/-- Send and receive the complete exchange, closing the connection at exit. -/
def request (req : Request) : IO Response :=
  run req (Program.ibind send fun () => receive)

end ExchangeM
end Network.HTTP.Client
