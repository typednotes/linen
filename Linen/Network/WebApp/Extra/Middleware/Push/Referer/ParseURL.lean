/-
  Linen.Network.WebApp.Extra.Middleware.Push.Referer.ParseURL — URL parsing
  for push middleware

  Ports Hale's `Network.Wai.Middleware.Push.Referer.ParseURL`. Extracts path
  components from Referer header values for push prediction.
-/
namespace Network.WebApp.Extra.Middleware.Push.Referer

/-- Extract the path portion from a URL or Referer header value.
    "https://example.com/page?q=1" → "/page"
    "/page?q=1" → "/page"
    $$\text{extractPath} : \text{String} \to \text{String}$$ -/
def extractPath (url : String) : String :=
  -- Strip scheme and host if present
  let stripped :=
    if url.startsWith "https://" then
      let afterHost := (url.drop 8).toString
      match afterHost.splitOn "/" with
      | _ :: rest => "/" ++ "/".intercalate rest
      | _ => "/"
    else if url.startsWith "http://" then
      let afterHost := (url.drop 7).toString
      match afterHost.splitOn "/" with
      | _ :: rest => "/" ++ "/".intercalate rest
      | _ => "/"
    else url
  -- Strip query string
  match stripped.splitOn "?" with
  | p :: _ => p
  | [] => stripped

/-- Check if a URL path looks like a static resource (CSS, JS, image, etc.). -/
def isStaticResource (path : String) : Bool :=
  let exts := [".css", ".js", ".png", ".jpg", ".jpeg", ".gif", ".svg", ".ico",
               ".woff", ".woff2", ".ttf", ".eot", ".map", ".webp"]
  exts.any fun ext => path.endsWith ext

end Network.WebApp.Extra.Middleware.Push.Referer
