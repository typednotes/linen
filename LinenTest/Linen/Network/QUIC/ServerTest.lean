/-
  Tests for `Linen.Network.QUIC.Server`.

  `run` and `accept` both take a plain `ServerConfig`, so — like `Client.connect`
  — they can be run and their thrown error asserted with `#eval`.
-/
import Linen.Network.QUIC.Server

open Network.QUIC Network.QUIC.Server

namespace Tests.Network.QUIC.Server

#eval show IO Unit from do
  let cfg : ServerConfig := { tlsConfig := {} }
  try
    discard (accept cfg)
    throw (IO.userError "accept unexpectedly succeeded without a TLS 1.3 FFI backend")
  catch e =>
    let expected := "QUIC.Server.accept: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    unless toString e == expected do
      throw (IO.userError s!"unexpected error message: {toString e}")

#eval show IO Unit from do
  let cfg : ServerConfig := { tlsConfig := {} }
  try
    run cfg (fun _ => pure ())
    throw (IO.userError "run unexpectedly succeeded without a TLS 1.3 FFI backend")
  catch e =>
    let expected := "QUIC.Server.run: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)"
    unless toString e == expected do
      throw (IO.userError s!"unexpected error message: {toString e}")

end Tests.Network.QUIC.Server
