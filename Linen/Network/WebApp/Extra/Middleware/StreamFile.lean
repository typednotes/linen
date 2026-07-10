/-
  Linen.Network.WebApp.Extra.Middleware.StreamFile — convert file responses
  to streaming

  Converts `.responseFile` responses into `.responseStream`, useful when the
  server doesn't support `sendfile(2)`. Ports
  `Network.Wai.Middleware.StreamFile`.
-/
import Linen.Network.WebApp

namespace Network.WebApp.Extra.Middleware

open Network.WebApp

/-- Convert file responses to streaming responses by reading the file
    content into memory.
    $$\text{streamFile} : \text{Middleware}$$ -/
def streamFile : Middleware :=
  fun app req respond =>
    app req fun resp =>
      match resp with
      | .responseFile status headers path _part =>
        respond (.responseStream status headers fun send flush => do
          let content ← IO.FS.readBinFile path
          send content
          flush)
      | other => respond other

end Network.WebApp.Extra.Middleware
