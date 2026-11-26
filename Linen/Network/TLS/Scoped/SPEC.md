# Scoped TLS sessions

- **S1. Handshake.** The runner owns the session from creation. It runs
  caller actions after a successful handshake and releases a session if
  the handshake fails. The client uses the context's certificate and
  hostname verification.
- **S2. Use.** Reads and writes use an established TLS session on a blocking
  socket. They follow the underlying TLS I/O error semantics.
- **S3. Ownership.** Early close consumes the usable state. The enclosing
  socket remains caller owned and must live until TLS cleanup finishes.
