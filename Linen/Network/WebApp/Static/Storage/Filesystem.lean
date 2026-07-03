/-
  Linen.Network.WebApp.Static.Storage.Filesystem — filesystem-backed static
  file storage

  Provides `defaultFileServerSettings` which creates a `StaticSettings`
  that reads files from a given root directory on the filesystem.

  Ports Hale's `WaiAppStatic.Storage.Filesystem`.

  ## Design

  Uses Lean's `System.FilePath.metadata` for file stat operations and
  `Response.responseFile` for zero-copy file serving.

  ## Guarantees

  - Path traversal is prevented by the `Piece` type (no '.' prefix, no '/')
  - File metadata errors are caught and mapped to `lrNotFound`
  - MIME types are determined by `Network.Mime.defaultMimeLookup`
-/
import Linen.Network.WebApp.Static.Types

namespace Network.WebApp.Static.Storage

open Network.WebApp.Static
open Network.WebApp
open Network.HTTP.Types

/-- Join path pieces into a filesystem path relative to a root directory.
    $$\text{piecesToPath}(r, ps) = r \,/\!\!/\, p_1 \,/\!\!/\, \cdots \,/\!\!/\, p_n$$ -/
private def piecesToPath (root : String) (pieces : Pieces) : String :=
  root ++ "/" ++ "/".intercalate (pieces.map toString)

/-- Create StaticSettings backed by a filesystem directory.
    Files are served using `Response.responseFile` for efficient sendfile.
    MIME types are determined from file extensions.
    $$\text{defaultFileServerSettings} : \text{String} \to \text{StaticSettings}$$ -/
def defaultFileServerSettings (root : String) : StaticSettings where
  ssLookupFile := fun pieces => do
    let path := piecesToPath root pieces
    -- Attempt to stat the path; catch errors as "not found"
    let mdOpt ← do
      try
        let md ← System.FilePath.metadata path
        pure (some md)
      catch _ => pure none
    match mdOpt with
    | some md =>
      if md.type == .dir then
        pure .lrFolder
      else
        let size := md.byteSize.toNat
        let name := match pieces.getLast? with
          | some p => p
          | none => unsafeToPiece "unknown"
        let mime := Network.Mime.defaultMimeLookup name.val
        pure (.lrFile {
          fileGetSize := size
          fileToResponse := fun status headers =>
            .responseFile status
              ((hContentType, mime) :: headers)
              path none
          fileName := name
          fileGetMime := mime
        })
    | none => pure .lrNotFound

end Network.WebApp.Static.Storage
