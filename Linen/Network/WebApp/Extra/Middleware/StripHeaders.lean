/-
  Linen.Network.WebApp.Extra.Middleware.StripHeaders — remove specified
  headers from responses

  Ports Hale's `Network.Wai.Middleware.StripHeaders`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp
open Network.HTTP.Types

/-- Remove the specified headers from all responses.
    $$\text{stripHeaders} : \text{List HeaderName} \to \text{Middleware}$$ -/
def stripHeaders (names : List HeaderName) : Middleware :=
  fun app req respond =>
    app req fun resp =>
      respond (resp.mapResponseHeaders (·.filter fun (n, _) => !names.contains n))

/-- Stripping no headers preserves response headers for builder responses. -/
theorem stripHeaders_nil_builder (s : Status) (h : ResponseHeaders) (b : ByteArray) :
    (Response.responseBuilder s h b).mapResponseHeaders
      (·.filter fun (n, _) => !([] : List HeaderName).contains n)
      = .responseBuilder s h b := by
  simp [Response.mapResponseHeaders, List.filter_eq_self]

/-- Stripping no headers preserves response headers for file responses. -/
theorem stripHeaders_nil_file (s : Status) (h : ResponseHeaders) (p : String)
    (fp : Option Network.Sendfile.FilePart) :
    (Response.responseFile s h p fp).mapResponseHeaders
      (·.filter fun (n, _) => !([] : List HeaderName).contains n)
      = .responseFile s h p fp := by
  simp [Response.mapResponseHeaders, List.filter_eq_self]

/-- Stripping no headers preserves response headers for stream responses. -/
theorem stripHeaders_nil_stream (s : Status) (h : ResponseHeaders) (b : StreamingBody) :
    (Response.responseStream s h b).mapResponseHeaders
      (·.filter fun (n, _) => !([] : List HeaderName).contains n)
      = .responseStream s h b := by
  simp [Response.mapResponseHeaders, List.filter_eq_self]

end Network.WebApp.Extra.Middleware
