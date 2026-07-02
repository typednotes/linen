/-
  Linen.Network.QUIC.Server -- QUIC server operations

  Provides the server-side entry points for accepting QUIC connections.
  Ports Haskell's `Network.QUIC.Server` from the `quic` package.

  ## Design

  `run` creates a UDP socket, performs the QUIC handshake with each incoming
  client, and invokes the handler with the established `Connection`.
  Currently stubbed pending TLS 1.3 FFI integration.

  ## Guarantees

  - `run` is a blocking call that loops indefinitely (like Warp's `acceptLoop`)
  - Each connection is handled independently
-/

import Linen.Network.QUIC.Config
import Linen.Network.QUIC.Connection

namespace Network.QUIC.Server

-- NOTE: A real implementation would:
-- 1. Create a UDP socket bound to config.host:config.port
-- 2. Receive QUIC Initial packets
-- 3. Perform TLS 1.3 handshake via FFI (quiche_accept / ngtcp2_conn_server_new)
-- 4. Dispatch established connections to the handler

/-- Run a QUIC server that accepts connections and dispatches them to a handler.
    $$\text{run} : \text{ServerConfig} \to (\text{Connection} \to \text{IO}(\text{Unit})) \to \text{IO}(\text{Unit})$$
    This function blocks indefinitely, accepting connections in a loop.
    Currently stubbed: will error at runtime until TLS FFI is connected. -/
def run (_config : Network.QUIC.ServerConfig) (_handler : Network.QUIC.Connection → IO Unit) : IO Unit :=
  throw (IO.userError "QUIC.Server.run: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)")

/-- Accept a single QUIC connection.
    $$\text{accept} : \text{ServerConfig} \to \text{IO}(\text{Connection})$$
    Currently stubbed. -/
def accept (_config : Network.QUIC.ServerConfig) : IO Network.QUIC.Connection :=
  throw (IO.userError "QUIC.Server.accept: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)")

end Network.QUIC.Server
