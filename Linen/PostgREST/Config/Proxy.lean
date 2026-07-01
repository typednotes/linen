/-
  PostgREST.Config -- Proxy configuration

  Handles the proxy URI configuration for PostgREST's OpenAPI spec
  generation.  When PostgREST sits behind a reverse proxy, the
  `server-proxy-uri` setting tells it the external base URL so that
  the generated OpenAPI spec references the correct server address.

  ## Haskell source
  - `PostgREST.Config` (postgrest package, proxy-related helpers)

  ## Design
  - `ProxyUri` decomposes a proxy URI into scheme, host, port, and
    base path:
    $$\text{ProxyUri} = \{ \text{scheme},\; \text{host},\;
      \text{port} : \mathbb{N},\; \text{basePath} \}$$
  - Parsing handles both `http://` and `https://` schemes, with
    optional port and path components
-/

namespace PostgREST.Config

-- ────────────────────────────────────────────────────────────────────
-- URI scheme
-- ────────────────────────────────────────────────────────────────────

/-- URI scheme (HTTP or HTTPS). -/
inductive UriScheme where
  | http
  | https
  deriving BEq, Repr

instance : ToString UriScheme where
  toString
    | .http  => "http"
    | .https => "https"

/-- The default port for a given scheme.
    $$\text{defaultPort}(\text{http}) = 80,\quad
      \text{defaultPort}(\text{https}) = 443$$ -/
def UriScheme.defaultPort : UriScheme -> Nat
  | .http  => 80
  | .https => 443

-- ────────────────────────────────────────────────────────────────────
-- Proxy URI
-- ────────────────────────────────────────────────────────────────────

/-- A parsed proxy URI for OpenAPI server specification.
    $$\text{ProxyUri} = \{ \text{scheme},\; \text{host},\;
      \text{port} : \mathbb{N},\; \text{basePath} : \text{String} \}$$ -/
structure ProxyUri where
  /-- The URI scheme (http or https). -/
  puScheme : UriScheme
  /-- The hostname or IP address. -/
  puHost : String
  /-- The port number. -/
  puPort : Nat
  /-- The base path prefix (e.g., `"/api/v1"`). -/
  puBasePath : String := "/"
  deriving BEq, Repr

/-- Reconstruct the full URI string from parts.
    $$\text{toUri}(p) = \text{scheme}\texttt{://}\text{host}[:\text{port}]\text{basePath}$$ -/
def ProxyUri.toUri (p : ProxyUri) : String :=
  let portStr := if p.puPort == p.puScheme.defaultPort then ""
    else s!":{p.puPort}"
  s!"{p.puScheme}://{p.puHost}{portStr}{p.puBasePath}"

instance : ToString ProxyUri := ⟨ProxyUri.toUri⟩

-- ────────────────────────────────────────────────────────────────────
-- Parsing
-- ────────────────────────────────────────────────────────────────────

/-- Parse a proxy URI string.
    $$\text{parse}(\texttt{https://api.example.com:8443/v1}) =
      \langle \text{https},\; \texttt{api.example.com},\; 8443,\; \texttt{/v1} \rangle$$

    Returns `none` if the URI cannot be parsed. Handles:
    - `http://` and `https://` schemes
    - Optional port (defaults to 80 or 443)
    - Optional path (defaults to `"/"`) -/
def ProxyUri.parse (uri : String) : Option ProxyUri := do
  let (scheme, rest) <-
    if uri.startsWith "https://" then
      some (UriScheme.https, (uri.drop 8).toString)
    else if uri.startsWith "http://" then
      some (UriScheme.http, (uri.drop 7).toString)
    else
      none
  -- Split host+port from path
  let (hostPort, path) :=
    match rest.splitOn "/" with
    | [] => (rest, "/")
    | [hp] => (hp, "/")
    | hp :: pathParts => (hp, "/" ++ String.intercalate "/" pathParts)
  -- Split host from port
  let (host, port) :=
    match hostPort.splitOn ":" with
    | [h] => (h, scheme.defaultPort)
    | [h, p] => match p.toNat? with
      | some n => (h, n)
      | none => (h, scheme.defaultPort)
    | _ => (hostPort, scheme.defaultPort)
  if host.isEmpty then none
  else some {
    puScheme := scheme
    puHost := host
    puPort := port
    puBasePath := path
  }

-- ────────────────────────────────────────────────────────────────────
-- OpenAPI server URL generation
-- ────────────────────────────────────────────────────────────────────

/-- Generate the OpenAPI server URL from configuration.
    If a proxy URI is configured, use it.  Otherwise, construct a
    URL from the server host and port. -/
def openApiServerUrl (proxyUri : Option ProxyUri) (host : String) (port : Nat) : String :=
  match proxyUri with
  | some proxy => proxy.toUri
  | none =>
    let hostStr := if host == "!4" then "0.0.0.0"
      else if host == "!6" then "[::]"
      else host
    s!"http://{hostStr}:{port}/"

end PostgREST.Config
