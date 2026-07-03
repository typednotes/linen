import Linen.Network.WebApp.Logger

/-! ### Tests for `Linen.Network.WebApp.Logger`

    Coverage: `apacheFormat`/`apacheFormatWithDate` render the Apache
    Combined Log Format line, with and without a known body size;
    `ApacheLogger.log` drives `getDate`/`output` and appends a newline. -/

open Network.WebApp Network.WebApp.Logger
open Network.HTTP.Types

namespace Tests.Network.WebApp.Logger

def sampleReq : Request :=
  { defaultRequest with
    requestMethod := .standard .GET
    httpVersion := http11
    rawPathInfo := "/hello"
    rawQueryString := "?x=1"
    remoteHost := ⟨"203.0.113.5", 0⟩ }

#guard apacheFormat sampleReq status200
  == "203.0.113.5 - - \"GET /hello?x=1 HTTP/1.1\" 200 -"

#guard apacheFormat sampleReq status200 (some 42)
  == "203.0.113.5 - - \"GET /hello?x=1 HTTP/1.1\" 200 42"

#guard apacheFormatWithDate "01/Jan/2026:00:00:00 +0000" sampleReq status404
  == "203.0.113.5 - - [01/Jan/2026:00:00:00 +0000] \"GET /hello?x=1 HTTP/1.1\" 404 -"

#eval show IO Unit from do
  let out ← IO.mkRef ""
  let logger : ApacheLogger :=
    { getDate := pure "01/Jan/2026:00:00:00 +0000"
      output := fun s => out.set s }
  logger.log sampleReq status200 (some 5)
  let logged ← out.get
  unless logged == "203.0.113.5 - - [01/Jan/2026:00:00:00 +0000] \"GET /hello?x=1 HTTP/1.1\" 200 5\n" do
    throw (IO.userError s!"unexpected log line: {logged}")

end Tests.Network.WebApp.Logger
