/-
  Tests for `Linen.Network.QUIC.Client`.

  `connect` takes a plain `ClientConfig` (no private constructor stands in the
  way), so — unlike `Connection`'s own operations — it can actually be *run*.
  Checked with `#eval` (a thrown error, or a wrongly-worded one, fails the
  build), as in the other IO stub tests.
-/
import Linen.Network.QUIC.Client

open Network.QUIC Network.QUIC.Client

namespace Tests.Network.QUIC.Client

#eval show IO Unit from do
  let cfg : ClientConfig := { serverName := "example.com" }
  try
    discard (connect cfg)
    throw (IO.userError "connect unexpectedly succeeded without a TLS 1.3 FFI backend")
  catch e =>
    let expected := "QUIC.Client.connect: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    unless toString e == expected do
      throw (IO.userError s!"unexpected error message: {toString e}")

end Tests.Network.QUIC.Client
