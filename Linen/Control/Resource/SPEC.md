# Indexed resource programs

- **S1. Composition.** A program starts with its declared input world. Its
  successful result selects the world passed to the continuation. A fixed
  action selects the same output state for every value.
- **S2. Ownership.** An owned interpreter acquires once and releases once
  after successful acquisition, on ordinary return, explicit close, or
  an IO exception. An explicit close consumes the open state. The release
  operation must be terminal, even if it reports an error.
- **S3. Errors.** Acquisition errors stop execution without a release call.
  An IO error in the body aborts the rest of the program. A simultaneous
  cleanup failure preserves the original error. A cleanup error on a
  successful body is reported to the caller.
- **S4. Boundary.** Combinators may compose actions, lift effects with no
  access to the private resource, and produce values. Defining or
  interpreting primitives requires the resource's authorization proof.
  Implementers must keep the proof, native handles and aliased callbacks
  private. Each invocation operates on a single interpreter's world.
