/-
  Tests for `Linen.Network.Mime`.

  All functions are pure, so every check is a `#guard` executed at build time.
-/
import Linen.Network.Mime

open Network.Mime

namespace Tests.Network.Mime

/-! ### fileNameExtensions — most-specific-first, multi-part aware -/

#guard fileNameExtensions "foo.tar.gz" == ["tar.gz", "gz"]
#guard fileNameExtensions "archive.tar.bz2" == ["tar.bz2", "bz2"]
#guard fileNameExtensions "index.html" == ["html"]
#guard fileNameExtensions "photo.JPEG" == ["jpeg"]          -- lower-cased
#guard fileNameExtensions "README" == []                    -- no extension
#guard fileNameExtensions "noext." == []                    -- trailing dot ⇒ dropped
#guard fileNameExtensions ".gitignore" == ["gitignore"]     -- leading dot
#guard fileNameExtensions "a.b.c.d" == ["b.c.d", "c.d", "d"]

/-! ### mimeByExt — first matching extension wins (most specific) -/

-- Multi-part extension is preferred over its suffix.
#guard mimeByExt defaultMimeMap defaultMimeType "backup.tar.gz" == "application/gzip"
#guard mimeByExt defaultMimeMap defaultMimeType "site.html" == "text/html"
-- Unknown extension falls back to the supplied default.
#guard mimeByExt defaultMimeMap defaultMimeType "mystery.qwerty" == "application/octet-stream"
#guard mimeByExt defaultMimeMap "x/y" "mystery.qwerty" == "x/y"
-- A custom map takes precedence over the defaults it is given.
#guard mimeByExt [("foo", "application/x-foo")] "d/d" "bar.foo" == "application/x-foo"

/-! ### defaultMimeLookup — common web content types -/

#guard defaultMimeLookup "style.css" == "text/css"
#guard defaultMimeLookup "app.js" == "application/javascript"
#guard defaultMimeLookup "data.json" == "application/json"
#guard defaultMimeLookup "logo.svg" == "image/svg+xml"
#guard defaultMimeLookup "clip.webm" == "video/webm"
#guard defaultMimeLookup "font.woff2" == "font/woff2"
#guard defaultMimeLookup "unknown.zzz" == "application/octet-stream"
#guard defaultMimeLookup "no-extension" == "application/octet-stream"

/-! ### defaults -/

#guard defaultMimeType == "application/octet-stream"
-- Every mapped MIME type is a non-empty `type/subtype` string.
#guard defaultMimeMap.all (fun (_, mt) => !mt.isEmpty && mt.any (· == '/'))

/-! ### Signatures -/

example : FileName → List Extension := fileNameExtensions
example : MimeMap → MimeType → FileName → MimeType := mimeByExt
example : FileName → MimeType := defaultMimeLookup

end Tests.Network.Mime
