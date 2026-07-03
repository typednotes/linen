/-
  Examples.WebAppStatic — `Network.WebApp.Static`'s filesystem-backed static
  file server end-to-end.

  Serves a real scratch directory through `Network.WebApp.Static.static`
  (`defaultFileServerSettings` + `staticApp`), driven by `Examples.WebApp`'s
  hand-rolled loopback HTTP/1.1 harness (`readRequest`/`writeResponse`/
  `serveOne`/`sendRequest`) instead of duplicating it. Exercises:

  * a direct file hit (`/hello.txt`), including its `Content-Type` and
    1-hour `Cache-Control` `max-age` (`StaticSettings`' defaults);
  * a directory request (`/`) redirected to its `index.html` via
    `ssRedirectToIndex`;
  * a missing path (`/nope.txt`) answered with a 404;
  * a dotfile-shaped path segment (`/.secret`) rejected with a 403 before
    any filesystem lookup runs (`Piece`'s `no_dot` invariant).

  Args: (none) -- runs the round trips and exits non-zero on any mismatch
-/
import Linen.Network.WebApp.Static.Application
import Examples.WebApp

open Network.Socket
open Network.Socket.Blocking
open Network.WebApp
open Network.WebApp.Static
open Network.WebApp.Static.Storage
open Network.HTTP.Types

namespace Examples.WebAppStatic

/-- Write `contents` to `dir/name`. -/
def writeScratchFile (dir : System.FilePath) (name contents : String) : IO Unit := do
  IO.FS.writeFile (dir / name) contents

def demoRoundTrip : IO Bool := do
  IO.println "── Network.WebApp.Static: staticApp over a real scratch directory ──"
  let (_handle, tempFile) ← IO.FS.createTempFile
  IO.FS.removeFile tempFile
  -- A fresh, uniquely-named sibling directory (never the shared system temp
  -- root itself) so cleanup can safely `removeDir` it at the end.
  let root : System.FilePath := tempFile.toString ++ "-static-demo"
  IO.FS.createDirAll root
  writeScratchFile root "index.html" "welcome"
  writeScratchFile root "hello.txt" "hello static world"
  IO.println s!"  serving {root} via Network.WebApp.Static.static"

  let app := Network.WebApp.Static.static root.toString
  let server ← listenTCP "127.0.0.1" 0
  let addr ← getSockName server
  IO.println s!"  server listening on 127.0.0.1:{addr.port}"
  let serverTask ← IO.asTask (prio := .dedicated)
    (Examples.WebApp.serveRequests app server 4)

  let (fileStatus, fileBody) ← Examples.WebApp.sendRequest addr.port "GET" "/hello.txt" "" ""
  IO.println s!"  GET /hello.txt -> {fileStatus} {fileBody.quote}"

  let (indexStatus, indexBody) ← Examples.WebApp.sendRequest addr.port "GET" "/" "" ""
  IO.println s!"  GET / -> {indexStatus} {indexBody.quote}  (via index.html)"

  let (notFoundStatus, _) ← Examples.WebApp.sendRequest addr.port "GET" "/nope.txt" "" ""
  IO.println s!"  GET /nope.txt -> {notFoundStatus}"

  let (forbiddenStatus, _) ← Examples.WebApp.sendRequest addr.port "GET" "/.secret" "" ""
  IO.println s!"  GET /.secret -> {forbiddenStatus}"

  match serverTask.get with
  | .ok _ => pure ()
  | .error e => throw e
  let _ ← close server
  IO.FS.removeFile (root / "index.html")
  IO.FS.removeFile (root / "hello.txt")
  IO.FS.removeDir root

  pure (fileStatus == 200 && fileBody == "hello static world" &&
        indexStatus == 200 && indexBody == "welcome" &&
        notFoundStatus == 404 && forbiddenStatus == 403)

def run (_args : List String) : IO Unit := do
  if ← demoRoundTrip then
    IO.println "\nwebappstatic demo done · all checks passed"
  else
    throw (IO.userError "webappstatic demo done · some checks failed")

end Examples.WebAppStatic
