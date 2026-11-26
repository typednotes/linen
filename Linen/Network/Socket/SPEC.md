# POSIX socket state and lifetime

- **S1. State tags.** The high-level API checks the argument's state index
  against the operation's signature. For example, `accept` requires
  `Socket .listening`, and `close` requires a proof of `state ≠ .closed`.
  A successful close returns `Socket .closed`. Lean erases the state index
  and the proof at runtime.
- **S2. Aliases.** Operations such as bind and close return a value with an
  updated tag for the same native handle. Earlier aliases retain their tags.
  An earlier alias may still satisfy an operation's signature while the
  underlying operation fails. Use the returned value after each transition.
  Raw FFI calls and explicit construction from a raw handle bypass the
  high-level signature checks.
- **S3. Closing.** On successful close the native shim marks the shared handle
  closed. Subsequent calls to `close` through earlier aliases succeed
  sequentially without another OS close. An `accept` through an earlier
  listener alias reports an error after close. Callers sharing handles across
  tasks must coordinate close with other operations.
- **S4. Scoped ownership.** `SocketM.run` acquires a fresh socket and keeps
  the handle private. A successful program ends closed. A failing program
  still closes the native handle. The cleanup is a no-op after explicit close.
- **S5. Scoped transitions.** Fixed transitions must compose in order.
  A non-blocking connect selects connected or connecting on success.
  Refusal throws. A nested accept owns the child until it closes and
  leaves the listener listening.
- **S6. Peer changes.** A peer can end its send side or reset the connection
  while the local scope is indexed as connected. `SocketM.recvOrClose`
  keeps the socket connected on data or `wouldBlock`. On EOF or error
  it closes the local socket before passing control to the closed branch.
  Use it for streams when both outcomes are terminal for the application.
  `SocketM.recv` keeps the scope available for applications that need
  different recovery or write after a peer half-close.
