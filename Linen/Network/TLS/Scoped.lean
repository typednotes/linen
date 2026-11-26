import Linen.Control.Resource
import Linen.Network.TLS.Context

/-! # Scoped TLS sessions
The socket belongs to the caller. This module owns the TLS session established
over it and shuts that session down before returning.
-/

namespace Network.TLS
open Control.Resource

private opaque tlsAccess : {p : Prop // p} := ⟨True, trivial⟩

/-- A program over a private TLS session after a successful handshake. -/
abbrev TLSM := Owned tlsAccess.val TLSSession

namespace TLSM

private def run (sock : Socket.RawSocket) (acquire : IO TLSSession)
    (action : TLSM .open_ post α) : IO α :=
  Owned.run tlsAccess.property acquire TLS.close <|
    Program.ibind (Owned.use tlsAccess.property (handshake · sock)) fun () => action

/-- Establish a client session, including certificate and hostname verification,
and run the program. The caller must keep the underlying socket live. -/
def runClient (ctx : TLSContext) (sock : Socket.RawSocket) (hostname : String)
    (action : TLSM .open_ post α) : IO α :=
  run sock (newClientSession ctx sock hostname) action

/-- Establish a server session and run the program. The caller must keep the
underlying socket live. -/
def runServer (ctx : TLSContext) (sock : Socket.RawSocket) (action : TLSM .open_ post α) : IO α :=
  run sock (newServerSession ctx sock) action

/-- Read up to the supplied number of bytes from the TLS session on a blocking socket. -/
def read (maxLen : USize) : TLSM .open_ .open_ ByteArray :=
  Owned.use tlsAccess.property (TLS.read · maxLen)

/-- Write bytes to the TLS session on a blocking socket. -/
def write (data : ByteArray) : TLSM .open_ .open_ Unit :=
  Owned.use tlsAccess.property (TLS.write · data)

/-- Read the negotiated TLS protocol version. -/
def version : TLSM .open_ .open_ String :=
  Owned.use tlsAccess.property getVersion

/-- Read the negotiated ALPN protocol, if any. -/
def alpn : TLSM .open_ .open_ (Option String) :=
  Owned.use tlsAccess.property getAlpn

/-- Close the TLS session. The underlying socket remains owned by its caller. -/
def close : TLSM .open_ .closed Unit :=
  Owned.close tlsAccess.property TLS.close

end TLSM
end Network.TLS
