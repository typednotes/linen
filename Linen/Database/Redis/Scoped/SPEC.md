# Scoped Redis connections

- **S1. Dispatch.** Use a regular or TLS connection pool to run requests.
  Commands require an open pool and preserve the existing reply decoder.
- **S2. Release.** Explicit close consumes the open scope. The runner
  disconnects at exit if the program leaves the pool open.
