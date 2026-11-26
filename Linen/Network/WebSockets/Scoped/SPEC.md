# Scoped WebSocket connection

- **S1. Use.** Application sends require the open state. Sending a close
  frame enters closing and permits continued receives.
- **S2. Close observation.** A receive may observe a close frame and enter
  closed. A payload preserves the prior state. This uses the observations
  of Linen's existing frame reader.
- **S3. Transport.** The client runner brackets TCP. The transport runner
  accepts framing callbacks and depends on their caller for transport
  shutdown. Neither runner returns the connection handle.
