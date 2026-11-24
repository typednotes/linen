# HTTP capability contract

- **S1 — Transport authority.** An `HTTP cap` request requires proofs that
  its method and URL are permitted. A typed operation's encoder also returns
  an `HTTP cap Response`, not an unchecked transport request. The operation
  layer cannot expand the encoder's method or URL authority.
- **S2 — Operation authority.** An `OperationHTTP policy` call requires
  evidence that `policy.allows op = true`, including when constructed
  directly. The input type depends on `op`. Denied operations cannot be
  constructed; a runtime caller can branch on the allowlist and pass the
  evidence from the allowed branch. This effect does not accept raw HTTP
  requests. Do not also give the restricted computation a raw HTTP effect
  or another transport if it must obey this policy.
- **S3 — Dispatch.** `runOperationHTTPWith` encodes each allowed call using
  the same policy that authorised it, then passes the resulting request once
  to the supplied transport. It returns the transport's response and
  propagates its errors. `runOperationHTTP` uses the ordinary HTTP transport.
- **S4 — Trust boundary.** The application defines which operations are
  read-only. The trusted encoder must derive the wire action from the
  operation, and must not let caller-controlled input override it through
  the body, query, or headers. The transport is trusted to send the request it
  is given. Linen does not classify AWS actions, sign requests, or replace
  provider-side IAM policy.

For AWS Query, for example, an application can allow `ListRoles` and
`GetCallerIdentity` while denying `CreateRole` and `AssumeRole`, even though
all use POST. The [offline test](../../../../../LinenTest/Linen/Control/Monad/Effect/HTTPTest.lean)
shows an allowlist, an operation-indexed input type, and a stub transport.
