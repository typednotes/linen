# Scoped SQLite resources

- **S1. Connection.** Query and execute actions require an open connection.
  The runner closes on return or exception.
- **S2. Cursor.** Bind before stepping, then inspect columns only on a row.
  Step may advance from ready or row to row or done. Reset makes a live
  statement ready. Finalize consumes its usable state.
- **S3. Nesting.** A statement is finalized before leaving its connection
  step, even on failure. The transaction bracket uses the existing
  SQLite commit and rollback behavior and requires an open connection
  when its body finishes.
