/-
  `Linen.Text.Pandoc.MediaBag` — an in-memory collection of binary resources.

  ## Haskell source

  Ported from `Text.Pandoc.MediaBag` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/MediaBag.hs`).

  Provides the `MediaItem` record, the `MediaBag` (a map from normalised paths
  to items) with its monoid structure, and the accessors `insertMedia`,
  `lookupMedia`, `deleteMedia`, `mediaDirectory`, and `mediaItems`.

  ### Deviations from upstream

  * `Text` → `String`; lazy `ByteString` contents → `ByteArray`.
  * `insertMedia`'s upstream logic derives a SHA1-hash-based storage path for
    `data:` URIs and for non-relative / `..`-containing / percent-encoded
    paths (to avoid collisions and directory escapes). That hashing needs the
    (deferred) crypto layer; here `insertMedia` stores under the canonicalised
    original path directly. The MIME-type resolution (explicit type, else
    `getMimeTypeDef`, with the `.gz` special case) is ported faithfully.
  * `canonicalize` normalises `\` to `/` and leaves URIs untouched; it does not
    resolve percent-encoding (matching upstream's stated behaviour).
-/

import Linen.Text.Pandoc.MIME
import Linen.Text.Pandoc.URI
import Linen.Data.Map

namespace Linen.Text.Pandoc

open Data (Map)

/-- A single binary resource: its MIME type, stored path, and contents. -/
structure MediaItem where
  /-- The MIME type of the resource. -/
  mediaMimeType : MIME.MimeType
  /-- The stored path/name of the resource. -/
  mediaPath : String
  /-- The binary content. -/
  mediaContents : ByteArray
  deriving Inhabited

/-- A collection of binary resources keyed by normalised path. -/
structure MediaBag where
  /-- The underlying map from normalised path to item. -/
  unMediaBag : Map String MediaItem
  deriving Inhabited

namespace MediaBag

/-- The empty media bag. -/
def empty : MediaBag := ⟨Data.Map.empty⟩

/-- Normalise a path to `/`-separated form; URIs are left as-is. Percent
    encoding is not resolved. -/
def canonicalize (fp : String) : String :=
  if URI.isURI fp then fp
  else String.ofList (fp.toList.map (fun c => if c == '\\' then '/' else c))

/-- Look up the `MediaItem` for a path (canonicalised), or `none`. -/
def lookupMedia (fp : String) (bag : MediaBag) : Option MediaItem :=
  bag.unMediaBag.lookup (canonicalize fp)

/-- Remove the entry for a path (no-op if absent). -/
def deleteMedia (fp : String) (bag : MediaBag) : MediaBag :=
  ⟨bag.unMediaBag.delete (canonicalize fp)⟩

/-- Insert or replace a resource, resolving its MIME type (explicit, else by
    extension, with the `.gz` special case). -/
def insertMedia (fp : String) (mbMime : Option MIME.MimeType) (contents : ByteArray)
    (bag : MediaBag) : MediaBag :=
  let path := canonicalize fp
  let mime : MIME.MimeType :=
    match mbMime with
    | some m => m
    | none =>
        if fp.endsWith ".gz" then
          MIME.getMimeTypeDef (fp.dropRight 3)
        else
          MIME.getMimeTypeDef fp
  let item : MediaItem := { mediaMimeType := mime, mediaPath := path, mediaContents := contents }
  ⟨bag.unMediaBag.insert' path item⟩

/-- The stored entries as `(path, mimeType, byteLength)` triples. -/
def mediaDirectory (bag : MediaBag) : List (String × MIME.MimeType × Nat) :=
  bag.unMediaBag.toList'.map fun (_, item) =>
    (item.mediaPath, item.mediaMimeType, item.mediaContents.size)

/-- The stored entries as `(path, mimeType, contents)` triples. -/
def mediaItems (bag : MediaBag) : List (String × MIME.MimeType × ByteArray) :=
  bag.unMediaBag.toList'.map fun (_, item) =>
    (item.mediaPath, item.mediaMimeType, item.mediaContents)

end MediaBag

/-- `MediaBag`s combine by left-biased union (right argument wins on
    conflicts, matching `Data.Map`'s monoid). -/
instance : Append MediaBag where
  append a b := ⟨Data.Map.union b.unMediaBag a.unMediaBag⟩

instance : EmptyCollection MediaBag := ⟨MediaBag.empty⟩

end Linen.Text.Pandoc
