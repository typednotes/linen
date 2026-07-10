/-
  Linen.CDP.Domains.IO — the `IO` CDP domain

  Ports `CDP.Domains.IO` (see `docs/imports/cdp/dependencies.md`); naming
  conventions as in `CDP.Domains.Memory`'s docstring.

  Input/Output operations for streams produced by DevTools.

  NOTE: the CDP domain is named `IO`, clashing with Lean core's `IO` monad.
  The module lives under the fully-qualified namespace `CDP.Domains.IO` (as
  upstream's domain name requires), but nothing in this file needs the core
  `IO` monad — it is pure JSON-RPC parameter/response/command boilerplate —
  so there is no actual name resolution hazard: no bare `IO` identifier is
  ever written here that could be mistaken for `CDP.Domains.IO`.
-/
import Linen.CDP.Internal.Utils
import Linen.CDP.Domains.Runtime

namespace CDP.Domains.IO

open Data.Json (Value ToJSON FromJSON)
open CDP.Internal.Utils (Command)

-- ── Types ──

/-- This is either obtained from another method or specified as
    `blob:<uuid>` where `<uuid>` is an UUID of a Blob. -/
abbrev StreamHandle := String

-- ── `IO.close` ──

/-- Parameters of the `IO.close` command: close the stream, discard any
    temporary backing storage. -/
structure PClose where
  /-- Handle of the stream to close. -/
  handle : StreamHandle
  deriving Repr, BEq, DecidableEq

instance : ToJSON PClose where
  toJSON p := Data.Json.object [("handle", ToJSON.toJSON p.handle)]

instance : Command PClose where
  Response := Unit
  commandName _ := "IO.close"
  decodeResponse _ := .ok ()

-- ── `IO.read` ──

/-- Parameters of the `IO.read` command: read a chunk of the stream. -/
structure PRead where
  /-- Handle of the stream to read. -/
  handle : StreamHandle
  /-- Seek to the specified offset before reading (if not specified, proceed
      with offset following the last read). Some types of streams may only
      support sequential reads. -/
  offset : Option Int := none
  /-- Maximum number of bytes to read (left upon the agent discretion if not
      specified). -/
  size : Option Int := none
  deriving Repr, BEq, DecidableEq

instance : ToJSON PRead where
  toJSON p := Data.Json.object <|
    [("handle", ToJSON.toJSON p.handle)]
    ++ (p.offset.map fun v => ("offset", ToJSON.toJSON v)).toList
    ++ (p.size.map fun v => ("size", ToJSON.toJSON v)).toList

/-- Response of the `IO.read` command. -/
structure Read where
  /-- Set if the data is base64-encoded. -/
  base64Encoded : Option Bool := none
  /-- Data that were read. -/
  data : String
  /-- Set if the end-of-file condition occurred while reading. -/
  eof : Bool
  deriving Repr, BEq, DecidableEq

instance : FromJSON Read where
  parseJSON v := do
    .ok
      { base64Encoded := ← (← Value.getFieldOpt v "base64Encoded").mapM FromJSON.parseJSON
        data := ← Value.getField v "data" >>= FromJSON.parseJSON
        eof := ← Value.getField v "eof" >>= FromJSON.parseJSON }

instance : Command PRead where
  Response := Read
  commandName _ := "IO.read"
  decodeResponse := FromJSON.parseJSON

-- ── `IO.resolveBlob` ──

/-- Parameters of the `IO.resolveBlob` command: return the UUID of the Blob
    object specified by a remote object id. -/
structure PResolveBlob where
  /-- Object id of a Blob object wrapper. -/
  objectId : Runtime.RemoteObjectId
  deriving Repr, BEq, DecidableEq

instance : ToJSON PResolveBlob where
  toJSON p := Data.Json.object [("objectId", ToJSON.toJSON p.objectId)]

/-- Response of the `IO.resolveBlob` command. -/
structure ResolveBlob where
  /-- UUID of the specified Blob. -/
  uuid : String
  deriving Repr, BEq, DecidableEq

instance : FromJSON ResolveBlob where
  parseJSON v := do .ok { uuid := ← Value.getField v "uuid" >>= FromJSON.parseJSON }

instance : Command PResolveBlob where
  Response := ResolveBlob
  commandName _ := "IO.resolveBlob"
  decodeResponse := FromJSON.parseJSON

end CDP.Domains.IO
