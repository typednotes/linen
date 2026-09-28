/-
  Tests for `Linen.CDP.Endpoints`.

  Exercises the `FromJSON` instances with literal fixtures, `parseUri`
  against real `webSocketDebuggerUrl` shapes, and the HTTP endpoints
  end-to-end against a real server (started via `withApplication`) that
  serves fixed `/json/*` bodies.
-/
import Linen.CDP.Endpoints
import Linen.Network.WebApp.Server.WithApplication

open CDP.Endpoints
open Network.WebApp.Server
open Network.WebApp
open Network.HTTP.Types

namespace Tests.CDP.Endpoints

private def versionJson : String :=
  "{\"Browser\":\"Chrome/1.0\",\"Protocol-Version\":\"1.3\",\"User-Agent\":\"ua\"," ++
  "\"V8-Version\":\"v8\",\"WebKit-Version\":\"wk\"," ++
  "\"webSocketDebuggerUrl\":\"ws://127.0.0.1:9222/devtools/browser/abc\"}"

private def targetJson : String :=
  "{\"description\":\"\",\"devtoolsFrontendUrl\":\"/devtools/inspector.html\"," ++
  "\"id\":\"T1\",\"title\":\"t\",\"type\":\"page\",\"url\":\"about:blank\"," ++
  "\"webSocketDebuggerUrl\":\"ws://127.0.0.1:9222/devtools/page/T1\"}"

#guard match (Data.Json.Decode.decodeAs versionJson : Except String BrowserVersion) with
  | .ok v => v ==
      { browser := "Chrome/1.0", protocolVersion := "1.3", userAgent := "ua"
        v8Version := "v8", webKitVersion := "wk"
        webSocketDebuggerUrl := "ws://127.0.0.1:9222/devtools/browser/abc" }
  | .error _ => false

#guard match (Data.Json.Decode.decodeAs targetJson : Except String TargetInfo) with
  | .ok v => v ==
      { description := "", devtoolsFrontendUrl := "/devtools/inspector.html"
        id := "T1", title := "t", type := "page", url := "about:blank"
        webSocketDebuggerUrl := "ws://127.0.0.1:9222/devtools/page/T1" }
  | .error _ => false

#guard parseUri "ws://127.0.0.1:9222/devtools/browser/abc" ==
  some ("127.0.0.1", 9222, "/devtools/browser/abc")

#guard parseUri "not a uri" == none

private def app : Application := fun req respond =>
  match req.rawPathInfo with
  | "/json/version" => AppM.respondIO respond (pure (responseLBS status200 [] versionJson))
  | "/json/list" => AppM.respondIO respond (pure (responseLBS status200 [] s!"[{targetJson}]"))
  | "/json/new" => AppM.respondIO respond (pure (responseLBS status200 [] targetJson))
  | "/json/activate/T1" => AppM.respondIO respond (pure (responseLBS status200 [] "Target activated"))
  | _ => AppM.respondIO respond (pure (responseLBS status404 [] ""))

#eval show IO Unit from do
  withApplication (pure app) fun port => do
    let hostPort := (s!"http://127.0.0.1", port.toNat)
    let bv ← Endpoint.getEndpoint (ep := EPBrowserVersion) hostPort {}
    assert! bv.browser == "Chrome/1.0"

    let targets ← Endpoint.getEndpoint (ep := EPAllTargets) hostPort {}
    assert! targets.length == 1
    assert! (targets.head?.map (·.id)) == some "T1"

    let opened ← Endpoint.getEndpoint (ep := EPOpenNewTab) hostPort { url := "about:blank" }
    assert! opened.id == "T1"

    let () ← Endpoint.getEndpoint (ep := EPActivateTarget) hostPort { targetId := "T1" }

    let config : CDP.Internal.Utils.Config := { hostPort }
    let info ← connectToTab config "about:blank"
    assert! info.id == "T1"

end Tests.CDP.Endpoints
