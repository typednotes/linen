import Linen.Codec.Picture.Metadata.Exif

/-!
  Port of `Codec.Picture.Metadata` from the `JuicyPixels` package (see
  `docs/imports/JuicyPixels/dependencies.md`, module 6 of 29). A common,
  type-safe "metadata" store attached to decoded images (`Keys a` maps a typed
  key to a value of type `a`), plus a handful of helpers for building the
  common metadata sets that a codec produces.

  Upstream encodes `Keys` as a GADT and `Elem k = forall a. (...) => !(k a)
  :=> a` as an existential pairing a `Keys a` with a value of that same `a`,
  with type-safe lookup/delete/insert driven by `keyEq :: Keys a -> Keys b ->
  Maybe (a :~: b)` (a type-equality witness obtained by pattern-matching both
  GADT values together). Lean supports the same idea directly: `Keys` below
  is a Lean indexed inductive family (an indexed `Type → Type`), `Elem` is a
  dependent record with an implicit existential type field, and `Keys.cast`
  below fuses upstream's `keyEq` with the `Refl`-guarded cast it exists to
  enable — matching two `Keys` values together on the same branch already
  unifies their index types for Lean, so the value can be handed back
  directly with no separate equality proof to thread through.

  Upstream's `Word` fields (`DpiX`/`DpiY`/`Width`/`Height` and the dpi
  conversion helpers) are ported as `Nat`, matching this library's general
  `Word → Nat` convention.

  `foldMap`'s upstream signature quantifies over an arbitrary `Monoid m`;
  Lean's standard library has no such general typeclass, so it is ported
  taking the monoid's `empty`/`append` operations as explicit arguments
  instead of a typeclass constraint.
-/

namespace Codec.Picture

-- ── Source format and colour space ──

/-- Type describing the original file format of the file. -/
inductive SourceFormat where
  | jpeg
  | gif
  | bitmap
  | tiff
  | png
  | hdr
  | tga
  deriving Repr, BEq

/-- The color space an image's pixel values are expressed in. -/
inductive ColorSpace where
  | sRGB
  | windowsBitmapColorSpace (v : ByteArray)
  | iccProfile (v : ByteArray)
  deriving BEq

-- ── Keys and values ──

/-- Encode values for unknown information. -/
inductive Value where
  | int (v : Int)
  | double (v : Float)
  | string (v : String)
  deriving BEq

/-- Store various additional information about an image. If something is not
    recognized, it can be stored under an `unknown` tag. -/
inductive Keys : Type → Type where
  | gamma : Keys Float
  | colorSpace : Keys ColorSpace
  | format : Keys SourceFormat
  | dpiX : Keys Nat
  | dpiY : Keys Nat
  | width : Keys Nat
  | height : Keys Nat
  | title : Keys String
  | description : Keys String
  | author : Keys String
  | copyright : Keys String
  | software : Keys String
  | comment : Keys String
  | disclaimer : Keys String
  | source : Keys String
  | warning : Keys String
  | exif (t : ExifTag) : Keys ExifData
  | unknown (name : String) : Keys Value

/-- If `k1` and `k2` are the same key, reinterpret a value held at `k2`'s type
    as one held at `k1`'s type. Matching `k1`/`k2` together on the same
    constructor already unifies their index types, so `v` can be returned
    unchanged. -/
def Keys.cast (k1 : Keys a) (k2 : Keys b) (v : b) : Option a :=
  match k1, k2 with
  | .gamma, .gamma => some v
  | .colorSpace, .colorSpace => some v
  | .format, .format => some v
  | .dpiX, .dpiX => some v
  | .dpiY, .dpiY => some v
  | .width, .width => some v
  | .height, .height => some v
  | .title, .title => some v
  | .description, .description => some v
  | .author, .author => some v
  | .copyright, .copyright => some v
  | .software, .software => some v
  | .comment, .comment => some v
  | .disclaimer, .disclaimer => some v
  | .source, .source => some v
  | .warning, .warning => some v
  | .exif t1, .exif t2 => if t1 == t2 then some v else none
  | .unknown s1, .unknown s2 => if s1 == s2 then some v else none
  | _, _ => none

-- ── Elements and the metadata store ──

/-- Element describing a metadata and its (typed) associated value. -/
structure Elem where
  {α : Type}
  key : Keys α
  value : α

/-- Dependent storage used for metadatas. All metadatas of a given kind are
    unique within this container.

    The current data structure is based on a list, so bad performance can be
    expected. -/
structure Metadatas where
  elems : List Elem

/-- Empty metadatas. -/
def Metadatas.empty : Metadatas := ⟨[]⟩

/-- Remove an element of the given key from the metadatas. If not present,
    does nothing. -/
def Metadatas.delete (k : Keys a) (m : Metadatas) : Metadatas :=
  ⟨m.elems.filter fun e => (Keys.cast k e.key e.value).isNone⟩

/-- Insert an already-built element, overwriting any element with the same
    key. -/
def Metadatas.insertElem (e : Elem) (m : Metadatas) : Metadatas :=
  ⟨e :: (m.delete e.key).elems⟩

/-- Insert an element in the metadatas; if an element with the same key is
    present, it is overwritten. -/
def Metadatas.insert (k : Keys a) (v : a) (m : Metadatas) : Metadatas :=
  m.insertElem ⟨k, v⟩

/-- Create metadatas with a single element. -/
def Metadatas.singleton (k : Keys a) (v : a) : Metadatas := ⟨[⟨k, v⟩]⟩

/-- Right-based union: elements of `m2` overwrite same-keyed elements of
    `m1`. -/
def Metadatas.union (m1 m2 : Metadatas) : Metadatas :=
  m2.elems.foldl (fun acc e => acc.insertElem e) m1

/-- Strict left fold of the metadatas. -/
def Metadatas.foldl' (f : acc → Elem → acc) (init : acc) (m : Metadatas) : acc :=
  m.elems.foldl f init

/-- `foldMap` equivalent for metadatas, taking the target monoid's `empty` and
    `append` operations explicitly (Lean has no general `Monoid` typeclass to
    quantify over, unlike upstream's `Monoid m` constraint). -/
def Metadatas.foldMap (empty : m) (append : m → m → m) (f : Elem → m) (md : Metadatas) : m :=
  md.foldl' (fun acc e => append acc (f e)) empty

/-- Extract all Exif-specific metadatas. -/
def Metadatas.extractExifMetas (m : Metadatas) : List (ExifTag × ExifData) :=
  m.elems.filterMap fun e =>
    match e with
    | ⟨.exif t, v⟩ => some (t, v)
    | _ => none

/-- Search a metadata with the given key. -/
def Metadatas.lookup (k : Keys a) (m : Metadatas) : Option a :=
  go m.elems
where
  go : List Elem → Option a
    | [] => none
    | e :: rest =>
      match Keys.cast k e.key e.value with
      | some v => some v
      | none => go rest

-- ── DPI conversion helpers ──

/-- Conversion from dots-per-meter to dots-per-inch. -/
def dotsPerMeterToDotPerInch (z : Nat) : Nat := z * 254 / 10000

/-- Conversion from dots-per-inch to dots-per-meter. -/
def dotPerInchToDotsPerMeter (z : Nat) : Nat := (z * 10000) / 254

/-- Conversion from dots-per-centimeter to dots-per-inch. -/
def dotsPerCentiMeterToDotPerInch (z : Nat) : Nat := z * 254 / 100

-- ── Metadata-set builders ──

/-- Create metadatas indicating the resolution, with `dpiX == dpiY`. -/
def mkDpiMetadata (w : Nat) : Metadatas := ⟨[⟨.dpiY, w⟩, ⟨.dpiX, w⟩]⟩

/-- Create metadatas holding width and height information. -/
def mkSizeMetadata (w h : Nat) : Metadatas := ⟨[⟨.width, w⟩, ⟨.height, h⟩]⟩

/-- Create simple metadatas with format, width & height. -/
def basicMetadata (f : SourceFormat) (w h : Nat) : Metadatas :=
  ⟨[⟨.format, f⟩, ⟨.width, w⟩, ⟨.height, h⟩]⟩

/-- Create simple metadatas with format, width, height, `dpiX` & `dpiY`. -/
def simpleMetadata (f : SourceFormat) (w h dpiX dpiY : Nat) : Metadatas :=
  ⟨[⟨.format, f⟩, ⟨.width, w⟩, ⟨.height, h⟩, ⟨.dpiX, dpiX⟩, ⟨.dpiY, dpiY⟩]⟩

end Codec.Picture
