/-
  Tests for `Linen.Network.HTTP.Client.Conduit`.

  `httpSource`/`withResponse`/`httpSink` all perform real network IO (they
  connect and exchange bytes over the wire), so — like `Connection` and
  `Redirect`'s establishers — they're pinned at the type level rather than
  exercised against a live server.
-/
import Linen.Network.HTTP.Client.Conduit

open Network.HTTP.Client
open Data.Conduit

namespace Tests.Network.HTTP.Client.Conduit

/-! ### Signatures -/

unsafe example (req : Request) : ConduitT PEmpty ByteArray IO Response :=
  Network.HTTP.Client.Conduit.httpSource req

example : Request → (Response → IO α) → IO α := Network.HTTP.Client.Conduit.withResponse

unsafe example (req : Request) : ConduitT PEmpty PEmpty IO (Response × ByteArray) :=
  Network.HTTP.Client.Conduit.httpSink req

end Tests.Network.HTTP.Client.Conduit
