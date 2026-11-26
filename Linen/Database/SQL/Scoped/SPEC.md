# Scoped PostgreSQL resources

- **S1. Connection.** The runner retains typed acquisition errors and
  releases any successful acquisition when its program ends. Statement
  and SQL actions require an open connection and retain session errors.
- **S2. Pool.** A pool borrows a connection for a single typed statement.
  The pool receives it back before any subsequent action and drains
  idle connections at scope exit.
