/-
  Linen.Network.WebApp.Extra.Middleware.RequestLogger.JSON — JSON request
  logging

  Ports `Network.Wai.Middleware.RequestLogger.JSON`.
-/
import Linen.Network.WebApp
import Linen.System.Log.FastLogger

namespace Network.WebApp.Extra.Middleware.RequestLogger

open Network.WebApp
open Network.HTTP.Types

/-- Format a request as a JSON log line. Built manually (no `Json` import)
    to keep this middleware dependency-free. -/
def formatJSON (req : Request) (status : Status) : String :=
  let method := toString req.requestMethod
  let path := req.rawPathInfo ++ req.rawQueryString
  let host := req.remoteHost.host
  let code := status.statusCode
  let ua := req.requestHeaderUserAgent.getD ""
  s!"\{\"method\":\"{method}\",\"path\":\"{path}\",\"status\":{code},\"host\":\"{host}\",\"userAgent\":\"{ua}\"}"

/-- JSON logging middleware.
    $$\text{logJSON} : \text{IO Middleware}$$ -/
def logJSON : IO Middleware := do
  let logger ← System.Log.FastLogger.newLoggerSet .stdout
  return fun app req respond =>
    app req fun resp => do
      let line := formatJSON req resp.status
      System.Log.FastLogger.pushLogStr logger (line ++ "\n")
      respond resp

end Network.WebApp.Extra.Middleware.RequestLogger
