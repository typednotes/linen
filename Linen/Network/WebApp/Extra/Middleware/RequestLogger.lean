/-
  Linen.Network.WebApp.Extra.Middleware.RequestLogger — HTTP request logging

  Logs HTTP requests to a configurable destination (stdout, file, etc.) in
  Apache Combined Log Format or a developer-friendly format. Ports
  `Network.Wai.Middleware.RequestLogger`.
-/
import Linen.Network.WebApp
import Linen.System.Log.FastLogger
import Linen.System.Console.Ansi

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Log output format. -/
inductive OutputFormat where
  /-- Apache Combined Log Format. -/
  | apache
  /-- Colorized developer-friendly format. -/
  | dev
deriving BEq

/-- Request logger settings. -/
structure RequestLoggerSettings where
  outputFormat : OutputFormat := .dev
  destination : System.Log.FastLogger.LogType := .stdout

/-- Format a request in Apache Combined Log Format:
    `host - - [request-line] status`. -/
private def formatApache (req : Request) (status : Status) : String :=
  let method := toString req.requestMethod
  let path := req.rawPathInfo ++ req.rawQueryString
  let version := toString req.httpVersion
  let host := req.remoteHost.host
  let code := status.statusCode
  s!"{host} - - \"{method} {path} {version}\" {code}\n"

/-- Format a request in developer-friendly format with colors. -/
private def formatDev (req : Request) (status : Status) : String :=
  let method := toString req.requestMethod
  let path := req.rawPathInfo ++ req.rawQueryString
  let code := status.statusCode
  let color := if code < 300 then System.Console.Ansi.Color.green
    else if code < 400 then System.Console.Ansi.Color.cyan
    else if code < 500 then System.Console.Ansi.Color.yellow
    else System.Console.Ansi.Color.red
  let statusStr := System.Console.Ansi.colored color (toString code)
  s!"{method} {path} {statusStr}\n"

/-- Request logging middleware. Logs each request after the response is
    sent.
    $$\text{logRequests} : \text{RequestLoggerSettings} \to \text{IO Middleware}$$ -/
def logRequests (settings : RequestLoggerSettings := {}) : IO Middleware := do
  let logger ← System.Log.FastLogger.newLoggerSet settings.destination
  return fun app req respond =>
    app req fun resp => do
      let fmt := match settings.outputFormat with
        | .apache => formatApache req resp.status
        | .dev => formatDev req resp.status
      System.Log.FastLogger.pushLogStr logger fmt
      respond resp

/-- Convenience: a simple stdout logger with dev format. -/
def logStdoutDev : IO Middleware :=
  logRequests { outputFormat := .dev, destination := .stdout }

end Network.WebApp.Extra.Middleware
