/-
  Linen.Network.WebApp.Static.Types — static file serving types

  Core types for static file serving. The key type is `Piece`, a refined
  path segment that prevents directory traversal attacks.

  Ports Hale's `WaiAppStatic.Types`, under the renamed `WebApp.Static`
  namespace (Hale's `WaiAppStatic` → `WebApp.Static`, matching this
  project's `WAI → WebApp` rename).

  ## Guarantees
  - `Piece` cannot start with '.' (prevents dotfile serving)
  - `Piece` cannot contain '/' (prevents path traversal)
  - `toPiece` validates at construction time
-/
import Linen.Network.WebApp
import Linen.Network.Mime

namespace Network.WebApp.Static

open Network.WebApp
open Network.HTTP.Types

/-- Check whether a string starts with a dot. -/
private def startsDot (s : String) : Bool :=
  !s.isEmpty && s.front == '.'

/-- Check whether a string contains a slash. -/
private def containsSlash (s : String) : Bool :=
  s.any (· == '/')

/-- An individual component of a path. Validated to prevent path traversal.
    Invariant: no leading '.', no '/' character.
    $$\text{Piece} = \{ s : \text{String} \mid \neg\text{startsDot}(s) \land \neg\text{containsSlash}(s) \}$$ -/
structure Piece where
  val : String
  /-- The piece does not start with a dot. -/
  no_dot : startsDot val = false
  /-- The piece does not contain a slash. -/
  no_slash : containsSlash val = false
deriving Repr

instance : BEq Piece where beq a b := a.val == b.val
instance : ToString Piece where toString p := p.val

/-- Smart constructor: validates that the path segment is safe.
    Returns `none` if the segment starts with '.' or contains '/'.
    $$\text{toPiece} : \text{String} \to \text{Option Piece}$$ -/
def toPiece (t : String) : Option Piece :=
  if h1 : startsDot t = false then
    if h2 : containsSlash t = false then
      some ⟨t, h1, h2⟩
    else
      none
  else
    none

/-- Construct a `Piece` from a known-safe string literal.
    The proofs are discharged by `native_decide` — only use with
    compile-time-constant strings that are known not to start with '.'
    and not to contain '/'.
    $$\text{unsafeToPiece} : \text{String} \to \text{Piece}$$ -/
def unsafeToPiece (t : String)
    (h1 : startsDot t = false := by native_decide)
    (h2 : containsSlash t = false := by native_decide) : Piece :=
  ⟨t, h1, h2⟩

/-- Request path segments. The root path is the empty list. -/
abbrev Pieces := List Piece

/-- Convert text segments to validated Pieces.
    Returns `none` if any segment is invalid.
    $$\text{toPieces} : \text{List String} \to \text{Option Pieces}$$ -/
def toPieces (ts : List String) : Option Pieces :=
  ts.mapM toPiece

/-- Cache control configuration.
    $$\text{MaxAge} = \text{NoMaxAge} \mid \text{MaxAgeSeconds}\ \mathbb{N} \mid \text{MaxAgeForever} \mid \text{NoStore} \mid \text{NoCache}$$ -/
inductive MaxAge where
  /-- No cache-control header set. -/
  | noMaxAge
  /-- Set max-age to N seconds. -/
  | maxAgeSeconds (seconds : Nat)
  /-- Essentially infinite cache (~1 year / 31536000s). -/
  | maxAgeForever
  /-- cache-control: no-store -/
  | noStore
  /-- cache-control: no-cache -/
  | noCache
deriving BEq, Repr

/-- Just the name of a folder (validated path segment). -/
abbrev FolderName := Piece

/-- Information on an individual file.
    $$\text{File} = \{ \text{size} : \mathbb{N},\; \text{toResponse},\; \text{name} : \text{Piece},\; \text{mime} : \text{String} \}$$ -/
structure File where
  /-- Size of file in bytes. -/
  fileGetSize : Nat
  /-- How to construct a web-app response for this file.
      Takes the response status and additional headers. -/
  fileToResponse : Status → ResponseHeaders → Response
  /-- Last component of the filename. -/
  fileName : Piece
  /-- MIME type (e.g., "text/html", "application/json"). -/
  fileGetMime : String

/-- Result of looking up a path in the static file store.
    $$\text{LookupResult} = \text{File} \mid \text{Folder} \mid \text{NotFound} \mid \text{Redirect}$$ -/
inductive LookupResult where
  /-- Found a file. -/
  | lrFile (file : File)
  /-- Found a directory. -/
  | lrFolder
  /-- Path does not exist. -/
  | lrNotFound
  /-- Client should be redirected to a different path. -/
  | lrRedirect (pieces : Pieces)

/-- Settings for the static file server.
    Configures file lookup, MIME types, caching, and directory handling.
    $$\text{StaticSettings} = \{ \text{lookup}, \text{mime}, \text{maxAge}, \text{redirect}, \text{indices}, \text{listing} \}$$ -/
structure StaticSettings where
  /-- Look up a file or folder by path pieces.
      This is the core operation -- different backends (filesystem, embedded, etc.)
      provide different implementations. -/
  ssLookupFile : Pieces → IO LookupResult
  /-- Function to get MIME type from file name.
      Defaults to `Network.Mime.defaultMimeLookup`. -/
  ssGetMimeType : Piece → String := fun p => Network.Mime.defaultMimeLookup p.val
  /-- Cache duration for files. Defaults to 1 hour. -/
  ssMaxAge : MaxAge := .maxAgeSeconds 3600
  /-- Whether to redirect folders to their index file.
      When true, a request for `/dir/` will try to serve `/dir/index.html`. -/
  ssRedirectToIndex : Bool := true
  /-- Index file names to try, in order. -/
  ssIndices : List Piece := [unsafeToPiece "index.html"]
  /-- Optional directory listing handler.
      When `none`, directory requests return 404. -/
  ssListing : Option (Pieces → IO Response) := none

-- ── Proofs ──

/-- An empty string is always a valid piece (no dot, no slash).
    $$\text{toPiece}(\varepsilon) = \text{some}(\varepsilon)$$ -/
theorem empty_piece_valid : (toPiece "").isSome = true := by native_decide

/-- `toPiece` rejects dotfiles: any string starting with '.' returns `none`. -/
theorem toPiece_rejects_dot : (toPiece ".hidden").isNone = true := by native_decide

/-- `toPiece` rejects paths with slashes. -/
theorem toPiece_rejects_slash : (toPiece "a/b").isNone = true := by native_decide

/-- `toPiece` accepts simple filenames. -/
theorem toPiece_accepts_simple : (toPiece "index.html").isSome = true := by native_decide

end Network.WebApp.Static
