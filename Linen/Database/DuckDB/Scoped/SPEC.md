# Scoped DuckDB resources

- **S1. Connection.** A connection runner closes on return or failure.
  Typed query and execute actions require an open connection.
- **S2. Appender.** A nested appender accepts whole rows and closes and
  destroys before leaving the connection step. DuckDB validates types and
  shape. Closing may flush buffered rows even if the body failed.
- **S3. File and transaction scopes.** File operations stay inside their
  owning connection and file brackets. A transaction uses DuckDB's
  existing commit and rollback semantics.
