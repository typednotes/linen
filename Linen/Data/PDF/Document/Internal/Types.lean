/-
  Data.PDF.Document.Internal.Types — internal type declarations

  Ports `Pdf.Document.Internal.Types` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/Internal/Types.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Internal/Types.hs`),
  module 2 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  ## Design

  - Upstream's `{-# OPTIONS_HADDOCK not-home #-}` pragma, plus its own
    curated export list (every type name, but never a data constructor),
    marks this module "internal": its constructors are meant to be built
    only by `Pdf.Document.Pdf`/`Document`/`Info` (this batch) and the next
    batch's `Catalog`/`PageNode`/`Page`/`FontDict`, with outward-facing
    consumers only ever seeing the *type* names re-exported from those
    higher modules, working through their accessor functions. Lean has no
    per-folder/per-Haddock-tag privacy boundary of that shape — `private`
    is file-scoped, and these constructors are genuinely built across
    several different files — so the fields below are ordinary public
    `structure` fields; "internal" is preserved here only as the same
    documentation-level boundary upstream itself draws (the module name and
    this doc-comment), not as an enforced Lean privacy mechanism.

  - Upstream's `Pdf = Pdf File (IORef ObjectCache)`, with
    `ObjectCache = (Bool, HashMap Ref Object)` (a "should we cache?" flag
    paired with the accumulated cache), maps `IORef` to Lean's `IO.Ref`
    (per `docs/imports/PdfToolboxDocument/dependencies.md`'s "External
    dependencies" note) and `HashMap Ref Object` to `Std.HashMap Ref Object`
    (`Data.PDF.Core.Object.Ref` already derives `Hashable`/`BEq`, so it is a
    valid `Std.HashMap` key exactly as it is a valid `HashMap` key upstream).

  - `Document`/`Catalog`/`Info`/`PageNode`/`Page` upstream are each a flat
    product of a `Pdf` handle, (except `Document`) a `Ref` identifying the
    underlying object, and its already-resolved `Dict`. Ported as ordinary
    `structure`s with named fields rather than upstream's positional data
    constructors — the same "named projection over positional
    pattern-match" choice `Data.PDF.Core.Types.Rectangle` already makes.

  - `FontDict = FontDict Pdf Dict` has no `Ref` field upstream (unlike its
    siblings above) — ported faithfully with no `ref` field either.

  - `PageTree = PageTreeNode PageNode | PageTreeLeaf Page` is ported as an
    ordinary two-constructor `inductive`, renamed to Lean's usual
    lower-case-after-dot constructor style (`.node`/`.leaf`) rather than
    carrying the `PageTree`-prefixed constructor names over verbatim (which
    Lean's anonymous-constructor dot notation makes redundant).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.File
import Std.Data.HashMap

namespace Data.PDF.Document.Internal.Types

open Data.PDF.Core.Object (Object Ref Dict)
open Data.PDF.Core.File (File)

/-! ── The per-file PDF handle ── -/

/-- Whether the object cache is currently enabled, and its accumulated
    contents. Mirrors upstream's `ObjectCache = (Bool, HashMap Ref Object)`. -/
abbrev ObjectCache := Bool × Std.HashMap Ref Object

/-- An open PDF file together with a mutable object cache. Mirrors
    upstream's `Pdf = Pdf File (IORef ObjectCache)` (see the module
    doc-comment for the `IORef` → `IO.Ref` substitution). -/
structure Pdf where
  /-- The underlying low-level file. -/
  file : File
  /-- The mutable object cache: whether caching is enabled, and the
      objects cached so far. -/
  cache : IO.Ref ObjectCache

/-! ── Document-level handles ── -/

/-- A PDF document: a `Pdf` handle plus its trailer dictionary. Mirrors
    upstream's `Document = Document Pdf Dict`. -/
structure Document where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The trailer dictionary. -/
  dict : Dict

/-- The document catalog (PDF32000-1:2008 §7.7.2). Mirrors upstream's
    `Catalog = Catalog Pdf Ref Dict`. -/
structure Catalog where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The catalog's own indirect reference. -/
  ref : Ref
  /-- The catalog's resolved dictionary. -/
  dict : Dict

/-- The document information dictionary (PDF32000-1:2008 §14.3.3). Mirrors
    upstream's `Info = Info Pdf Ref Dict`. -/
structure Info where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The info dictionary's own indirect reference. -/
  ref : Ref
  /-- The info dictionary's resolved dictionary. -/
  dict : Dict

/-- A page-tree node (PDF32000-1:2008 §7.7.3.2), containing pages or other
    nodes. Mirrors upstream's `PageNode = PageNode Pdf Ref Dict`. -/
structure PageNode where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The page-tree node's own indirect reference. -/
  ref : Ref
  /-- The page-tree node's resolved dictionary. -/
  dict : Dict

/-- A single PDF document page (PDF32000-1:2008 §7.7.3.3). Mirrors
    upstream's `Page = Page Pdf Ref Dict`. -/
structure Page where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The page's own indirect reference. -/
  ref : Ref
  /-- The page's resolved dictionary. -/
  dict : Dict

/-- A node of the page tree: either an interior node or a leaf page.
    Mirrors upstream's `PageTree = PageTreeNode PageNode | PageTreeLeaf
    Page` (see the module doc-comment for the constructor renaming). -/
inductive PageTree where
  /-- An interior page-tree node. -/
  | node (n : PageNode)
  /-- A leaf page. -/
  | leaf (p : Page)

/-- A font dictionary (PDF32000-1:2008 §9.6). Mirrors upstream's
    `FontDict = FontDict Pdf Dict` (no `Ref` field, unlike its siblings
    above). -/
structure FontDict where
  /-- The underlying PDF handle. -/
  pdf : Pdf
  /-- The font dictionary's resolved dictionary. -/
  dict : Dict

end Data.PDF.Document.Internal.Types
