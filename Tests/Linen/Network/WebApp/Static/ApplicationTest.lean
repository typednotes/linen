import Linen.Network.WebApp.Static.Application
import Linen.Control.Concurrent.Green

/-! ### Tests for `Linen.Network.WebApp.Static.Application`

    Exercises `staticApp` against an in-memory `StaticSettings` (no real
    filesystem needed — `ssLookupFile` is a plain pattern match), run via
    `Green.block` the same way `WebApp.InternalTest` runs `AppM` values:
    file hit, directory → index-file redirect, 404, and the 403 rejection
    of a dotfile-shaped path segment. -/

open Network.WebApp.Static
open Network.WebApp
open Network.HTTP.Types
open Control.Concurrent.Green

namespace Tests.Network.WebApp.Static.Application

/-- A `StaticSettings` backed by a tiny fixed in-memory "filesystem":
    `/index.html` is a file, `/dir` is a folder containing `/dir/index.html`,
    everything else is not found. -/
def fakeSettings : StaticSettings where
  ssLookupFile := fun pieces =>
    match pieces.map toString with
    | ["index.html"] =>
      pure (.lrFile { fileGetSize := 5,
                       fileToResponse := fun s h => .responseBuilder s h "hello".toUTF8,
                       fileName := unsafeToPiece "index.html",
                       fileGetMime := "text/html" })
    | ["dir"] => pure .lrFolder
    | ["dir", "index.html"] =>
      pure (.lrFile { fileGetSize := 3,
                       fileToResponse := fun s h => .responseBuilder s h "sub".toUTF8,
                       fileName := unsafeToPiece "index.html",
                       fileGetMime := "text/html" })
    | _ => pure .lrNotFound

/-- Run `staticApp fakeSettings` against a request with the given path
    segments, returning the `Response` it produced. -/
def runStatic (pathInfo : List String) : IO Response := do
  let tok ← Std.CancellationToken.new
  let captured ← IO.mkRef (none : Option Response)
  let respond : Response → Green ResponseReceived := fun resp =>
    (do captured.set (some resp); pure ResponseReceived.done : IO ResponseReceived)
  let req := { defaultRequest with pathInfo := pathInfo, rawPathInfo := "/" ++ "/".intercalate pathInfo }
  let _ ← Green.block ((staticApp fakeSettings req respond).run) tok
  match ← captured.get with
  | some resp => pure resp
  | none => throw (IO.userError "staticApp: respond was never called")

-- A direct file hit serves its body with a 200 and a Cache-Control header
-- (the 1-hour default from `StaticSettings.ssMaxAge`).
#eval show IO Unit from do
  let resp ← runStatic ["index.html"]
  unless resp.status.statusCode == 200 do
    throw (IO.userError s!"expected 200, got {resp.status.statusCode}")
  unless resp.headers.any (fun (n, v) => n == hCacheControl && v == "max-age=3600") do
    throw (IO.userError "expected a max-age=3600 Cache-Control header")

-- A folder request redirects to its index file.
#eval show IO Unit from do
  let resp ← runStatic ["dir"]
  unless resp.status.statusCode == 200 do
    throw (IO.userError s!"expected 200 (via index.html), got {resp.status.statusCode}")

-- A missing path is a 404.
#eval show IO Unit from do
  let resp ← runStatic ["nope.txt"]
  unless resp.status.statusCode == 404 do
    throw (IO.userError s!"expected 404, got {resp.status.statusCode}")

-- A dotfile-shaped path segment is rejected with 403 before any lookup runs.
#eval show IO Unit from do
  let resp ← runStatic [".secret"]
  unless resp.status.statusCode == 403 do
    throw (IO.userError s!"expected 403, got {resp.status.statusCode}")

end Tests.Network.WebApp.Static.Application
