# PostgreSQL connection pool

- **S1. Capacity.** A pool reuses an idle connection or reserves a slot
  before opening a new connection. Idle connections, checked-out
  connections, and opening reservations count toward `maxSize`. A
  checkout without an available slot returns `.poolExhausted`.
- **S2. Accounting.** A failed open releases its reservation. The
  total-created count tracks successful opens. Statistics count opening
  reservations with checked-out connections.
- **S3. Session.** `use` checks out one connection and returns it after
  the session, including on application errors and IO exceptions. State
  transitions are synchronized at the pool. Opening and executing a
  session run outside the pool lock. A session that accesses the
  connection directly must keep the borrowed handle within that use.
- **S4. Draining.** `destroy` drains the idle connections present when it
  runs. Existing sessions can finish and return their connections. The
  pool remains reusable. Idle connections are retained until reused or
  drained, and the `idleTimeout` setting is reserved.
