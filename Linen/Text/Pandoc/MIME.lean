/-
  `Linen.Text.Pandoc.MIME` — MIME-type lookup for pandoc.

  ## Haskell source

  Ported from `Text.Pandoc.MIME` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/MIME.hs`).

  Provides `getMimeType`, `getMimeTypeDef`, `mediaCategory`,
  `extensionFromMimeType` and `getCharset`, layered over the existing
  `Linen.Network.Mime` port of the `mime-types` package (per
  `docs/imports/pandoc/dependencies.md`, the `mime-types` dependency maps to
  `Linen.Network.Mime`).

  ### Deviations from upstream

  * `Text` → `String`; the base extension→type map is `Network.Mime.defaultMimeMap`.
  * Pandoc appends a large supplemental extension list on top of the base map;
    the documented overrides (notably `eps → application/eps`, which
    `Text.Pandoc.PDF` relies on) and the common source/document/image
    additions are ported here. The full ~100-entry supplemental list is a
    representative subset (the tail entries feed deferred binary-format
    writers); the base `mime-types` map is used unchanged for everything else.
-/

import Linen.Network.Mime

namespace Linen.Text.Pandoc
namespace MIME

/-- A MIME type string. -/
abbrev MimeType := String

/-- Pandoc's supplemental extension → MIME-type entries, layered on top of the
    base `mime-types` map (these take precedence on conflict). -/
def pandocMimeSupplement : List (String × MimeType) :=
  [ ("eps", "application/eps")           -- Text.Pandoc.PDF relies on this
  , ("gz", "application/x-gzip")
  , ("hs", "text/x-haskell")
  , ("lhs", "text/x-literate-haskell")
  , ("py", "text/x-python")
  , ("c", "text/x-csrc")
  , ("cpp", "text/x-c++src")
  , ("h", "text/x-chdr")
  , ("java", "text/x-java")
  , ("tex", "text/x-tex")
  , ("md", "text/markdown")
  , ("markdown", "text/markdown")
  , ("org", "text/x-org")
  , ("rst", "text/x-rst")
  , ("apng", "image/apng")
  , ("avif", "image/avif")
  , ("jxl", "image/jxl")
  , ("wmf", "image/wmf")
  , ("emf", "image/emf")
  , ("mol", "chemical/x-mdl-molfile")
  , ("pdb", "chemical/x-pdb")
  , ("cif", "chemical/x-cif")
  , ("lyx", "application/x-lyx")
  , ("mm", "application/x-freemind")
  , ("cbz", "application/x-cbz")
  , ("opml", "text/x-opml")
  , ("epub", "application/epub+zip")
  , ("djvu", "image/vnd.djvu") ]

/-- The combined extension → MIME-type map (supplement first so it overrides). -/
def mimeTypesList : List (String × MimeType) :=
  pandocMimeSupplement ++ Network.Mime.defaultMimeMap

/-- Extract the (lower-cased) final extension of a path, without the dot. -/
def getExtension (fp : String) : String :=
  match fp.toLower.splitOn "." with
  | [] => ""
  | [_] => ""            -- no dot: no extension
  | parts => parts.getLast!

/-- Look up the MIME type for a file path. -/
def getMimeType (fp : String) : Option MimeType :=
  if fp == "layout-cache" then some "application/binary"
  else if "Formula-".isPrefixOf fp && fp.endsWith "/" then
    some "application/vnd.oasis.opendocument.formula"
  else mimeTypesList.lookup (getExtension fp)

/-- Look up the MIME type, defaulting to `application/octet-stream`. -/
def getMimeTypeDef (fp : String) : MimeType :=
  (getMimeType fp).getD "application/octet-stream"

/-- The media category (the part before `/`) of a file's MIME type.

    `mediaCategory "foo.jpg" = some "image"`. -/
def mediaCategory (fp : String) : Option String :=
  (getMimeType fp).bind fun mt => (mt.splitOn "/").head?

/-- A plausible file extension for a MIME type (the reverse lookup). -/
def extensionFromMimeType (mimetype : MimeType) : Option String :=
  let mt := (mimetype.takeWhile (· != ';')).toString.trimAscii.toString
  match mt with
  | "text/plain" => some "txt"
  | "video/quicktime" => some "mov"
  | "video/mpeg" => some "mpeg"
  | "video/dv" => some "dv"
  | "image/vnd.djvu" => some "djvu"
  | "image/tiff" => some "tiff"
  | "image/jpeg" => some "jpg"
  | "application/xml" => some "xml"
  | "application/ogg" => some "ogg"
  | "image/svg+xml" => some "svg"    -- to avoid svgz
  | _ => (mimeTypesList.find? (·.2 == mt)).map (·.1)

/-- Extract an (upper-cased) charset parameter from a MIME type, if present. -/
def getCharset (mimetype : MimeType) : Option String :=
  let parts := (mimetype.splitOn ";").map (·.trimAscii.toString)
  (parts.find? (fun p => "charset=".isPrefixOf p.toLower)).map fun p =>
    (p.drop "charset=".length).toString.toUpper

end MIME
end Linen.Text.Pandoc
