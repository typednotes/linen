/-
  Tests for `Linen.Text.Pandoc.MIME`.
-/
import Linen.Text.Pandoc.MIME

namespace Tests.Linen.Text.Pandoc.MIME

open _root_.Linen.Text.Pandoc

-- ── getMimeType / getMimeTypeDef ──────────────────────────────────────

#guard MIME.getMimeType "photo.jpg" == some "image/jpeg"
#guard MIME.getMimeType "page.html" == some "text/html"
#guard MIME.getMimeType "noext" == none
#guard MIME.getMimeTypeDef "noext" == "application/octet-stream"

-- pandoc supplemental overrides
#guard MIME.getMimeType "figure.eps" == some "application/eps"
#guard MIME.getMimeType "Main.hs" == some "text/x-haskell"

-- special cases
#guard MIME.getMimeType "layout-cache" == some "application/binary"
#guard MIME.getMimeType "Formula-1/" == some "application/vnd.oasis.opendocument.formula"

-- ── mediaCategory ─────────────────────────────────────────────────────

#guard MIME.mediaCategory "foo.jpg" == some "image"
#guard MIME.mediaCategory "foo.html" == some "text"

-- ── extensionFromMimeType ─────────────────────────────────────────────

#guard MIME.extensionFromMimeType "text/plain" == some "txt"
#guard MIME.extensionFromMimeType "image/jpeg" == some "jpg"
#guard MIME.extensionFromMimeType "image/svg+xml" == some "svg"
#guard MIME.extensionFromMimeType "text/plain; charset=utf-8" == some "txt"

-- ── getCharset ────────────────────────────────────────────────────────

#guard MIME.getCharset "text/html; charset=utf-8" == some "UTF-8"
#guard MIME.getCharset "text/html" == none

end Tests.Linen.Text.Pandoc.MIME
