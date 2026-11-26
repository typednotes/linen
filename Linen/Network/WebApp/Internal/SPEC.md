# Indexed WebApp response

- **S1. Application contract.** A successful application program transitions
  from pending to sent. Standard actions reject both a second response and a
  program that finishes without responding. IO lifts preserve state.
- **S2. Interpretation.** The server supplies and interprets the Green
  response callback. Framework code that directly runs a program or uses
  the middleware escape hatch takes responsibility for response semantics.
  A transport failure can abort the computation.
