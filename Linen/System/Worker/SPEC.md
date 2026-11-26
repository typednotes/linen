# Scoped worker processes

- **S1. Requests.** Run a line-validated request on an open worker with
  the requested timeout. The underlying worker reports protocol and IO
  failure and stops on a failed request.
- **S2. Ownership.** The runner stops and reaps the worker at scope exit.
  Explicit stop consumes the open state, ruling out subsequent calls.
