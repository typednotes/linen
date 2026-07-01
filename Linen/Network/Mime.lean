/-
  Linen.Network.Mime — MIME type lookup

  Provides a default mapping from file extensions to MIME types,
  and functions to look up MIME types by file name.

  ## Design

  Mirrors Haskell's `Network.Mime` from the `mime-types` package.
  Uses a sorted association list for the MIME map, constructed once.

  ## Guarantees

  - `defaultMimeType` is always "application/octet-stream" (safe fallback)
  - `fileNameExtensions` correctly handles multi-part extensions (e.g., "tar.gz")
  - MIME types are always non-empty strings containing "/"
-/
namespace Network.Mime

-- ── Core types ──

/-- Individual MIME type to be served over the wire. -/
abbrev MimeType := String

/-- Path extension. May include multiple components, e.g., "tar.gz". -/
abbrev Extension := String

/-- File name (without directory path). -/
abbrev FileName := String

/-- Maps extensions to MIME types. -/
abbrev MimeMap := List (Extension × MimeType)

/-- The default fallback MIME type: "application/octet-stream". -/
def defaultMimeType : MimeType := "application/octet-stream"

-- ── Extension extraction ──

/-- Get a list of all file name extensions, from most specific to least.
    $$\text{fileNameExtensions}(\texttt{"foo.tar.gz"}) = [\texttt{"tar.gz"}, \texttt{"gz"}]$$

    Implemented by structural recursion on the dot-separated components of the
    (lower-cased) name: for `c₀.c₁.….cₙ` the extensions are the successive
    suffixes `c₁.….cₙ`, `c₂.….cₙ`, …, `cₙ`. A trailing empty component (from a
    name ending in `.`) is dropped, matching the reference implementation. -/
def fileNameExtensions (name : FileName) : List Extension :=
  match name.toLower.splitOn "." with
  | [] => []          -- `splitOn` never returns `[]`, but keep the match total
  | _ :: rest => go rest
where
  /-- Build the suffix extensions of a non-empty component list. -/
  go : List String → List Extension
    | [] => []
    | c :: cs =>
      let ext := ".".intercalate (c :: cs)
      -- An empty extension can only be a lone trailing "" component; stop there.
      if ext.isEmpty then [] else ext :: go cs

-- ── Lookup ──

/-- Look up a MIME type from the given map and default, by file name.
    Tries each extension from most to least specific.
    $$\text{mimeByExt}(m, d, f) = \text{first match in } m \text{ for extensions of } f$$ -/
def mimeByExt (mm : MimeMap) (default_ : MimeType) (name : FileName) : MimeType :=
  (fileNameExtensions name |>.findSome? (mm.lookup ·)).getD default_

-- ── Default MIME map ──

/-- A default mapping from filename extension to MIME type.
    Generated from Apache and nginx mime.types files + IANA registry.
    Covers the most common web content types. -/
def defaultMimeMap : MimeMap :=
  [ -- Text
    ("css", "text/css")
  , ("csv", "text/csv")
  , ("htm", "text/html")
  , ("html", "text/html")
  , ("ics", "text/calendar")
  , ("js", "application/javascript")
  , ("json", "application/json")
  , ("jsonld", "application/ld+json")
  , ("markdown", "text/markdown")
  , ("md", "text/markdown")
  , ("mjs", "application/javascript")
  , ("txt", "text/plain")
  , ("text", "text/plain")
  , ("xml", "application/xml")
  , ("yaml", "application/x-yaml")
  , ("yml", "application/x-yaml")
    -- Images
  , ("avif", "image/avif")
  , ("bmp", "image/bmp")
  , ("gif", "image/gif")
  , ("ico", "image/x-icon")
  , ("jpeg", "image/jpeg")
  , ("jpg", "image/jpeg")
  , ("png", "image/png")
  , ("svg", "image/svg+xml")
  , ("svgz", "image/svg+xml")
  , ("tif", "image/tiff")
  , ("tiff", "image/tiff")
  , ("webp", "image/webp")
    -- Audio
  , ("aac", "audio/aac")
  , ("flac", "audio/flac")
  , ("m4a", "audio/mp4")
  , ("mid", "audio/midi")
  , ("midi", "audio/midi")
  , ("mp3", "audio/mpeg")
  , ("oga", "audio/ogg")
  , ("ogg", "audio/ogg")
  , ("opus", "audio/opus")
  , ("wav", "audio/wav")
  , ("weba", "audio/webm")
    -- Video
  , ("3gp", "video/3gpp")
  , ("3g2", "video/3gpp2")
  , ("avi", "video/x-msvideo")
  , ("m4v", "video/mp4")
  , ("mkv", "video/x-matroska")
  , ("mov", "video/quicktime")
  , ("mp4", "video/mp4")
  , ("mpeg", "video/mpeg")
  , ("mpg", "video/mpeg")
  , ("ogv", "video/ogg")
  , ("ts", "video/mp2t")
  , ("webm", "video/webm")
  , ("wmv", "video/x-ms-wmv")
    -- Fonts
  , ("eot", "application/vnd.ms-fontobject")
  , ("otf", "font/otf")
  , ("ttf", "font/ttf")
  , ("woff", "font/woff")
  , ("woff2", "font/woff2")
    -- Archives
  , ("7z", "application/x-7z-compressed")
  , ("bz", "application/x-bzip")
  , ("bz2", "application/x-bzip2")
  , ("gz", "application/gzip")
  , ("rar", "application/vnd.rar")
  , ("tar", "application/x-tar")
  , ("tar.bz2", "application/x-bzip2")
  , ("tar.gz", "application/gzip")
  , ("xz", "application/x-xz")
  , ("zip", "application/zip")
  , ("zst", "application/zstd")
    -- Documents
  , ("doc", "application/msword")
  , ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
  , ("epub", "application/epub+zip")
  , ("odp", "application/vnd.oasis.opendocument.presentation")
  , ("ods", "application/vnd.oasis.opendocument.spreadsheet")
  , ("odt", "application/vnd.oasis.opendocument.text")
  , ("pdf", "application/pdf")
  , ("ppt", "application/vnd.ms-powerpoint")
  , ("pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation")
  , ("rtf", "application/rtf")
  , ("xls", "application/vnd.ms-excel")
  , ("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    -- Programming / Data
  , ("atom", "application/atom+xml")
  , ("jsonapi", "application/vnd.api+json")
  , ("map", "application/json")
  , ("rss", "application/rss+xml")
  , ("wasm", "application/wasm")
  , ("xhtml", "application/xhtml+xml")
    -- Binary / Other
  , ("bin", "application/octet-stream")
  , ("dmg", "application/x-apple-diskimage")
  , ("exe", "application/x-msdownload")
  , ("iso", "application/x-iso9660-image")
  , ("jar", "application/java-archive")
  , ("swf", "application/x-shockwave-flash")
  ]

/-- `mimeByExt` applied to `defaultMimeType` and `defaultMimeMap`.
    $$\text{defaultMimeLookup} : \text{FileName} \to \text{MimeType}$$ -/
def defaultMimeLookup (name : FileName) : MimeType :=
  mimeByExt defaultMimeMap defaultMimeType name

end Network.Mime
