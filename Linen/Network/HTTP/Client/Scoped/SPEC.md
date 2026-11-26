# Scoped HTTP/1.1 exchange

- **S1. Exchange.** A runner owns one request. The program sends it once,
  then receives a response. Receiving before send or sending again fails
  at type checking.
- **S2. Transport.** Connect to the request's origin. Close an acquired
  transport on return or exception. Request serialization, status, and
  body handling use the existing HTTP client.
