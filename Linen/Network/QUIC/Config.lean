/-
  Linen.Network.QUIC.Config -- QUIC server and client configuration

  Configuration structures for QUIC servers and clients. Ports Haskell's
  `Network.QUIC` (config) from the `quic` package.

  ## Design

  `ServerConfig` bundles TLS configuration (mandatory for QUIC, which always
  uses TLS 1.3), transport parameters, and bind address/port.
  `ClientConfig` bundles the server name for SNI, TLS config, and transport parameters.

  ## Guarantees

  - Port is `UInt16`, bounded to [0, 65535] by construction
  - `ServerConfig` requires `TLSConfig` (QUIC mandates TLS 1.3)
-/

import Linen.Network.QUIC.Types

namespace Network.QUIC

/-- Server configuration for a QUIC endpoint.
    $$\text{ServerConfig} = \{ \text{tlsConfig} : \text{TLSConfig},\; \text{port} : \text{UInt16},\; \ldots \}$$ -/
structure ServerConfig where
  /-- TLS configuration. Required: QUIC mandates TLS 1.3. -/
  tlsConfig : TLSConfig
  /-- Transport parameters advertised to clients. -/
  transportParams : TransportParams := {}
  /-- Host to bind to. Default: all interfaces. -/
  host : String := "0.0.0.0"
  /-- Port to listen on. Default: 443 (HTTPS). -/
  port : UInt16 := 443
  deriving Repr

/-- Client configuration for a QUIC connection.
    $$\text{ClientConfig} = \{ \text{serverName} : \text{String},\; \text{port} : \text{UInt16},\; \ldots \}$$ -/
structure ClientConfig where
  /-- TLS configuration. -/
  tlsConfig : TLSConfig := {}
  /-- Transport parameters advertised to the server. -/
  transportParams : TransportParams := {}
  /-- Server hostname for SNI. -/
  serverName : String
  /-- Server port. Default: 443 (HTTPS). -/
  port : UInt16 := 443
  deriving Repr

end Network.QUIC
