/-
  Linen.Network.QUIC.Client -- QUIC client operations

  Provides the client-side entry point for connecting to a QUIC server.
  Ports Haskell's `Network.QUIC.Client` from the `quic` package.

  ## Design

  `connect` creates a UDP socket, performs the QUIC handshake with the server,
  and returns an established `Connection`.
  Currently stubbed pending TLS 1.3 FFI integration.

  ## Guarantees

  - `connect` returns only after the handshake is complete (connection is established)
  - The returned `Connection` is ready for stream operations
-/

import Linen.Network.QUIC.Config
import Linen.Network.QUIC.Connection

namespace Network.QUIC.Client

-- NOTE: A real implementation would:
-- 1. Create a UDP socket
-- 2. Send a QUIC Initial packet to serverName:port
-- 3. Perform TLS 1.3 handshake via FFI (quiche_connect / ngtcp2_conn_client_new)
-- 4. Return the established connection

/-- Connect to a QUIC server.
    $$\text{connect} : \text{ClientConfig} \to \text{IO}(\text{Connection})$$
    Performs the QUIC handshake and returns an established connection.
    Currently stubbed: will error at runtime until TLS FFI is connected. -/
def connect (_config : Network.QUIC.ClientConfig) : IO Network.QUIC.Connection :=
  throw (IO.userError "QUIC.Client.connect: not yet implemented (requires TLS 1.3 FFI to quiche or ngtcp2)")

end Network.QUIC.Client
