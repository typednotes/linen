/-
  Linen.Network.WebApp.Logger — WebApp request/response logging

  Formats HTTP request/response data in Apache Combined Log Format.
  Ports Hale's `Network.Wai.Logger`.

  ## Format

  `host - - [date] "method path version" status size`

  ## Design
  Provides a pure formatting function and an IO-based logger that
  integrates with `System.Log.FastLogger`.
-/
import Linen.Network.WebApp
import Linen.Network.HTTP.Types.Header

namespace Network.WebApp.Logger

open Network.WebApp
open Network.HTTP.Types

/-- Format a request in Apache Combined Log Format.
    $$\text{apacheFormat} : \text{Request} \to \text{Status} \to \text{Option Nat} \to \text{String}$$
    The third parameter is the response body size (if known). -/
def apacheFormat (req : Request) (status : Status) (bodySize : Option Nat := none) : String :=
  let host := req.remoteHost.host
  let method := toString req.requestMethod
  let path := req.rawPathInfo ++ req.rawQueryString
  let version := toString req.httpVersion
  let code := status.statusCode
  let size := match bodySize with
    | some n => toString n
    | none => "-"
  s!"{host} - - \"{method} {path} {version}\" {code} {size}"

/-- Format with a date string prepended.
    $$\text{apacheFormatWithDate} : \text{String} \to \text{Request} \to \text{Status} \to \text{String}$$ -/
def apacheFormatWithDate (date : String) (req : Request) (status : Status)
    (bodySize : Option Nat := none) : String :=
  let host := req.remoteHost.host
  let method := toString req.requestMethod
  let path := req.rawPathInfo ++ req.rawQueryString
  let version := toString req.httpVersion
  let code := status.statusCode
  let size := match bodySize with
    | some n => toString n
    | none => "-"
  s!"{host} - - [{date}] \"{method} {path} {version}\" {code} {size}"

/-- A callback-based logger. Takes a date provider and output function. -/
structure ApacheLogger where
  /-- Provides the current date string for the log line. -/
  getDate : IO String
  /-- Writes a formatted log line to its destination. -/
  output : String → IO Unit

/-- Log a request/response pair. -/
def ApacheLogger.log (logger : ApacheLogger) (req : Request) (status : Status)
    (bodySize : Option Nat := none) : IO Unit := do
  let date ← logger.getDate
  let line := apacheFormatWithDate date req status bodySize
  logger.output (line ++ "\n")

end Network.WebApp.Logger
