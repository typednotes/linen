/-
  Data.PDF.Document.Page — PDF document pages

  Ports `Pdf.Document.Page` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox, `document/lib/Pdf/Document/Page.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/Page.hs`),
  module 10 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  `pageParentNode`/`pageContents`/`pageFontDicts` are single-indirection
  accessors, exactly like their counterparts in `Data.PDF.Document.PageNode`.
  `pageMediaBox`/`pageExtractText`/`pageExtractGlyphs` build on two more
  involved pieces: `mediaBoxRec` (ascend `/Parent` until a `/MediaBox` is
  found) and `dictXObjects` (resolve nested Form-XObject `/Resources`
  dictionaries into an in-memory `XObject` tree), both discussed below.

  ## Termination: `mediaBoxRec`'s untrusted `/Parent` ascent

  Exactly like `Data.PDF.Document.PageNode.pageNodePageByNum`'s descent
  through untrusted `/Kids`, upstream's `mediaBoxRec` ascends through
  untrusted `/Parent` references with no cycle guard — a malformed file
  whose `/Parent` chain cycles back on itself sends it into an infinite
  loop. Ported the same way: `mediaBoxRecFueled` takes a fuel parameter
  seeded from `Data.PDF.Document.PageNode.objectCountBound` (the trailer's
  `/Size`, a genuine file-derived bound on the number of distinct objects)
  and an explicit visited-`Ref` set, consuming one unit of fuel and marking
  the current node `visited` before ascending to its parent. A cycle is
  reported as a `corrupted` error instead of looping forever.

  ## Termination: `dictXObjects`'s untrusted, self-referential nested
  Form-XObject descent

  Upstream's `dictXObjects` builds a `Map Name XObject` from a
  `/Resources/XObject` dictionary, recursing into every Form XObject's own
  nested `/Resources/XObject` (`XObject` upstream is itself defined with an
  `xobjectChildren :: Map Name XObject` field — genuinely self-referential).
  Each XObject reference is, again, untrusted file content with no upstream
  cycle guard: a Form XObject whose own resources point back at an ancestor
  Form (or itself) sends upstream into an infinite loop building an
  infinitely-deep `XObject` value.

  Ported the same way as the other two cases: `dictXObjectsFueled`/
  `formXObjectFueled` below thread a fuel parameter (again seeded from
  `objectCountBound`) and a visited-`Ref` set through the resolution walk,
  consuming one unit of fuel per *newly visited* XObject reference. A
  reference that would revisit an already-`visited` `Ref` is reported as a
  `corrupted` "cycle detected" error, exactly like the other two cases —
  a deliberate, documented behavioural improvement over upstream's infinite
  loop on the same malformed input, not a change in behaviour on any
  acyclic/well-formed file.

  Once an `XObject` value is built this way, it is already a finite,
  ordinary (cycle-free-by-construction) in-memory Lean value — walking it at
  content-stream-processing time to service `Do` operators (`pageExtractGlyphs`'s
  local content-stream loop, below) is no longer a walk over untrusted `Ref`s
  at all, so it needs no fuel/visited-set of its own. It is instead written
  as an explicit stack-based iterative loop over `IO` actions, the same
  "no `partial`, no termination proof needed for an `IO`-driven traversal"
  idiom `Data.PDF.Content.Parser.readNextOperator` already establishes in
  this port for consuming a byte stream of caller-unknown length.

  ## Design: `combinedContent`

  Upstream's `combinedContent` is a hand-rolled `Streams.fromGenerator`
  producer/consumer coroutine, chaining a page's (possibly several)
  content streams into one lazily-read `InputStream`. Per
  `docs/imports/PdfToolboxDocument/dependencies.md`'s scope note, this is
  ported as a plain, eagerly-concatenated `Data.PDF.Stream.InputStream` —
  observationally equivalent for every consumer in this module, not a
  behaviour change (see `docs/imports/IoStreams/dependencies.md` for why
  `linen`'s streams are buffer-resident rather than incrementally
  generator-driven in general).
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Core.Types
import Linen.Data.PDF.Content
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Internal.Util
import Linen.Data.PDF.Document.Pdf
import Linen.Data.PDF.Document.PageNode
import Linen.Data.PDF.Document.FontDict
import Linen.Data.PDF.Stream
import Std.Data.HashMap

namespace Data.PDF.Document.Page

open Data.PDF.Core.Object (Name Dict Object Ref)
open Data.PDF.Core.Object.Util (refValue dictValue arrayValue nameValue streamValue)
open Data.PDF.Core.Exception (sure corrupted)
open Data.PDF.Core.Util (notice)
open Data.PDF.Core.Types (Rectangle rectangleFromArray)
open Data.PDF.Content
open Data.PDF.Document.Internal.Types (Pdf PageNode Page PageTree)
open Data.PDF.Document.PageNode (objectCountBound loadPageNode pageNodeParent)
open Data.PDF.Document.FontDict (FontDict)

export Data.PDF.Document.Internal.Types (Page)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── Single-indirection accessors ── -/

/-- This page's parent node. Mirrors upstream's `pageParentNode`. -/
def pageParentNode (page : Page) : IO PageNode := do
  let ref ← sure
    (notice (page.dict.get? (mkName "Parent") >>= refValue) "Parent should be a reference")
  match ← loadPageNode page.pdf ref with
  | .node n => pure n
  | .leaf _ => throw (corrupted "page parent should be a note, but leaf found")

/-- References to the page's content streams (`/Contents` may be a single
    indirect stream reference or an array of them). Mirrors upstream's
    `pageContents`. -/
def pageContents (page : Page) : IO (List Ref) :=
  Data.PDF.Core.Exception.message s!"contents for page: {reprStr page.ref}" do
    match page.dict.get? (mkName "Contents") with
    | none => pure []
    | some (.ref ref) => do
      let o ← Data.PDF.Document.Pdf.lookupObject page.pdf ref
      let o' ← Data.PDF.Document.Pdf.deref page.pdf o
      match o' with
      | .stream _ => pure [ref]
      | .array objs =>
        objs.toList.mapM fun obj => sure (notice (refValue obj) "Content should be a reference")
      | _ => throw (corrupted s!"Unexpected value in page content ref: {reprStr o'}")
    | some (.array objs) =>
      objs.toList.mapM fun obj => sure (notice (refValue obj) "Content should be a reference")
    | some _ => throw (corrupted "Unexpected value in page contents")

/-- The `(name, FontDict)` pairs of the page's `/Resources/Font` dictionary.
    Mirrors upstream's `pageFontDicts`. -/
def pageFontDicts (page : Page) : IO (List (Name × FontDict)) := do
  match page.dict.get? (mkName "Resources") with
  | none => pure []
  | some res => do
    let res' ← Data.PDF.Document.Pdf.deref page.pdf res
    let resDict ← sure (notice (dictValue res') "Resources should be a dictionary")
    match resDict.get? (mkName "Font") with
    | none => pure []
    | some fonts => do
      let fonts' ← Data.PDF.Document.Pdf.deref page.pdf fonts
      let fontsDict ← sure (notice (dictValue fonts') "Font should be a dictionary")
      fontsDict.toList.mapM fun (name, font) => do
        let font' ← Data.PDF.Document.Pdf.deref page.pdf font
        let fontDict ← sure (notice (dictValue font') "Each font should be a dictionary")
        Data.PDF.Document.Internal.Util.ensureType (mkName "Font") fontDict
        pure (name, ({ pdf := page.pdf, dict := fontDict } : FontDict))

/-! ── `pageMediaBox` (see the module doc-comment for the fuel/visited-set
     termination argument) ── -/

/-- One node's `(Pdf, Ref, Dict)` triple, whichever `PageTree` case it is. -/
private def treeTriple : PageTree → Pdf × Ref × Dict
  | .node n => (n.pdf, n.ref, n.dict)
  | .leaf p => (p.pdf, p.ref, p.dict)

/-- Ascend from `tree` until a `/MediaBox` is found (it is an inheritable
    attribute, PDF32000-1:2008 §7.7.3.4, Table 30). Mirrors upstream's
    `mediaBoxRec`, with the added `fuel`/`visited` cycle guard. -/
def mediaBoxRecFueled : Nat → Std.HashMap Ref Unit → PageTree → IO (Rectangle Float)
  | 0, _, tree =>
    let (_, ref, _) := treeTriple tree
    throw (corrupted
      s!"pageMediaBox: exceeded the file's declared object count ascending from {reprStr ref}")
  | fuel + 1, visited, tree => do
    let (pdf, ref, dict) := treeTriple tree
    if visited.contains ref then
      throw (corrupted s!"pageMediaBox: cycle detected in /Parent chain at {reprStr ref}")
    let visited' := visited.insert ref ()
    match dict.get? (mkName "MediaBox") with
    | some box => do
      let box' ← Data.PDF.Document.Pdf.deref pdf box
      let arr ← sure (notice (arrayValue box') "MediaBox should be an array")
      sure (rectangleFromArray arr)
    | none => do
      let parent ← match tree with
        | .node n => do
          match ← pageNodeParent n with
          | none => throw (corrupted "Media box not found")
          | some p => pure (PageTree.node p)
        | .leaf p => PageTree.node <$> pageParentNode p
      mediaBoxRecFueled fuel visited' parent

/-- The page's media box (PDF32000-1:2008 §7.7.3.4, Table 30), inherited
    from an ancestor page-tree node if not set directly. Mirrors upstream's
    `pageMediaBox`. -/
def pageMediaBox (page : Page) : IO (Rectangle Float) := do
  let fuel ← objectCountBound page.pdf
  mediaBoxRecFueled fuel ({} : Std.HashMap Ref Unit) (.leaf page)

/-! ── Nested Form XObjects (see the module doc-comment for the
     fuel/visited-set termination argument) ── -/

/-- A resolved Form XObject (PDF32000-1:2008 §8.10.2): its already-decoded
    content stream, how to decode glyphs drawn by its own fonts, and its
    own nested Form XObjects (genuinely self-referential, exactly as
    upstream's `XObject` record is). Mirrors upstream's `XObject`.

    `children` is a `List (Name × XObject)` association list rather than a
    `Std.HashMap Name XObject` — `Std.HashMap` is itself a structure carrying
    a well-formedness proof, and nesting a self-reference through it this way
    does not go through Lean's nested-inductive support (unlike `List`, which
    is a plain inductive the compiler already knows how to nest). This is a
    representation choice only: every use site below converts to/from a
    `Std.HashMap` with `Std.HashMap.ofList`/`Std.HashMap.toList` at the
    (finite, already-resolved) point of use, so there is no behavioural
    difference from a genuine map. -/
structure XObject where
  /-- The Form XObject's decoded content stream. -/
  content : ByteArray
  /-- How to decode a string drawn with one of this XObject's own fonts
      into glyphs. -/
  glyphDecoder : GlyphDecoder
  /-- This XObject's own nested Form XObjects, by name. -/
  children : List (Name × XObject)

mutual
  /-- Resolve every Form XObject reachable from `dict`'s `/Resources/XObject`
      entry (dropping any entry that isn't a Form XObject, or that would
      revisit an already-`visited` reference — see the module doc-comment).
      Mirrors upstream's `dictXObjects`. -/
  def dictXObjectsFueled (fuel : Nat) (visited : Std.HashMap Ref Unit) (pdf : Pdf) (dict : Dict) :
      IO (Std.HashMap Name XObject) := do
    match dict.get? (mkName "Resources") with
    | none => pure {}
    | some res => do
      let resObj ← Data.PDF.Document.Pdf.deref pdf res
      let resDict ← sure (notice (dictValue resObj) "Resources should be a dict")
      match resDict.get? (mkName "XObject") with
      | none => pure {}
      | some xo => do
        let xoObj ← Data.PDF.Document.Pdf.deref pdf xo
        let xosDict ← sure (notice (dictValue xoObj) "XObject should be a dict")
        xosDict.toList.foldlM
          (fun acc (name, o) => do
            let ref ← sure (notice (refValue o) "Not a ref")
            match ← formXObjectFueled fuel visited pdf ref with
            | none => pure acc
            | some xobj => pure (acc.insert name xobj))
          ({} : Std.HashMap Name XObject)

  /-- Resolve one candidate Form XObject reference: `none` if it turns out
      not to be a Form XObject; throws `corrupted` if `ref` is already
      `visited` (a cyclic nested-XObject graph — see the module
      doc-comment). Consumes one unit of `fuel` when it does recurse into
      the Form's own `/Resources`. -/
  def formXObjectFueled : Nat → Std.HashMap Ref Unit → Pdf → Ref → IO (Option XObject)
    | 0, _, _, ref =>
      throw (corrupted
        s!"dictXObjects: exceeded the file's declared object count resolving {reprStr ref}")
    | fuel + 1, visited, pdf, ref => do
      if visited.contains ref then
        throw (corrupted s!"dictXObjects: cycle detected in nested Form XObjects at {reprStr ref}")
      else do
        let visited' := visited.insert ref ()
        let v ← Data.PDF.Document.Pdf.lookupObject pdf ref
        let s ← sure (notice (streamValue v) "Not a stream")
        match (s.dict.get? (mkName "Subtype")).bind nameValue with
        | some n =>
          if n == mkName "Form" then do
            let is ← Data.PDF.Document.Pdf.streamContent pdf ref s
            let parts ← Data.PDF.Stream.toList is
            let cont := parts.foldl (· ++ ·) ByteArray.empty
            let fontDicts ← pageFontDicts { pdf := pdf, ref := ref, dict := s.dict }
            let fontInfos ← fontDicts.mapM fun (name, fd) => do
              let fi ← Data.PDF.Document.FontDict.fontDictLoadInfo fd
              pure (name, fi)
            let fontInfoMap : Std.HashMap Name Data.PDF.Content.FontInfo := Std.HashMap.ofList fontInfos
            let decoder : GlyphDecoder := fun fontName str =>
              match fontInfoMap.get? fontName with
              | none => []
              | some fi => Data.PDF.Content.fontInfoDecodeGlyphs fi str
            let children ← dictXObjectsFueled fuel visited' pdf s.dict
            pure (some { content := cont, glyphDecoder := decoder, children := children.toList })
          else
            pure none
        | none => pure none
end

/-- The page's own nested Form XObjects (`/Resources/XObject`). Mirrors
    upstream's `pageXObjects`. -/
def pageXObjects (page : Page) : IO (Std.HashMap Name XObject) := do
  let fuel ← objectCountBound page.pdf
  dictXObjectsFueled fuel ({} : Std.HashMap Ref Unit) page.pdf page.dict

/-! ── Combining a page's content streams ── -/

/-- Concatenate every content stream in `refs` into one input stream (see
    the module doc-comment for why this is eager rather than
    generator-driven, unlike upstream's `combinedContent`). -/
private def combinedContent (pdf : Pdf) (refs : List Ref) : IO Data.PDF.Stream.InputStream := do
  let chunks ← refs.mapM fun ref => do
    let o ← Data.PDF.Document.Pdf.lookupObject pdf ref
    match streamValue o with
    | some s => Data.PDF.Document.Pdf.streamContent pdf ref s
    | none => throw (corrupted "Page content is not a stream")
  let bytes ← chunks.mapM fun is => do
    let parts ← Data.PDF.Stream.toList is
    pure (parts.foldl (· ++ ·) ByteArray.empty)
  Data.PDF.Stream.fromByteString (bytes.foldl (· ++ ·) ByteArray.empty)

/-! ── Extracting text ── -/

/-- One frame of the content-stream-processing stack below: the input
    stream currently being read, the Form XObjects reachable by name from
    it, and the `GlyphDecoder` to restore once this frame's stream is
    exhausted (mirrors the `gdec'`/`{p with prGlyphDecoder = ...}` dance in
    upstream's local `loop`/`processDo`). -/
private structure Frame where
  stream : Data.PDF.Stream.InputStream
  xobjects : Std.HashMap Name XObject
  savedGlyphDecoder : GlyphDecoder

/-- Walk the page's content stream, dispatching every operator to
    `Data.PDF.Content.Processor.processOp`, and every `Do` operator naming
    a known Form XObject to that XObject's own (already-finite, see the
    module doc-comment) content stream instead. See the module doc-comment
    for why this needs no fuel/visited-set of its own: it walks an
    already-built, finite, cycle-free `XObject` value, not untrusted `Ref`s. -/
private def runContentLoop (root : Data.PDF.Stream.InputStream)
    (rootXObjects : Std.HashMap Name XObject) (initial : Data.PDF.Content.Processor) :
    IO Data.PDF.Content.Processor := do
  let mut p := initial
  let initialFrame : Frame :=
    { stream := root, xobjects := rootXObjects, savedGlyphDecoder := initial.prGlyphDecoder }
  let mut stack : List Frame := [initialFrame]
  let mut more := true
  while more do
    match stack with
    | [] => more := false
    | frame :: rest =>
      match ← Data.PDF.Content.readNextOperator frame.stream with
      | none =>
        p := { p with prGlyphDecoder := frame.savedGlyphDecoder }
        stack := rest
      | some (op, args) =>
        if op == (.Do : Op) then
          match args with
          | [.name name] =>
            match frame.xobjects.get? name with
            | none => stack := frame :: rest
            | some xobj => do
              let is ← Data.PDF.Stream.fromByteString xobj.content
              p := { p with prGlyphDecoder := xobj.glyphDecoder }
              let childFrame : Frame :=
                { stream := is, xobjects := Std.HashMap.ofList xobj.children,
                  savedGlyphDecoder := frame.savedGlyphDecoder }
              stack := childFrame :: frame :: rest
          | _ => stack := frame :: rest
        else
          match Data.PDF.Content.processOp (op, args) p with
          | .error err => throw (Data.PDF.Core.Exception.unexpected err)
          | .ok p' =>
            p := p'
            stack := frame :: rest
  pure p

/-- Extract every `Span` of glyphs drawn on the page, walking its content
    stream (and any Form XObjects it draws). Mirrors upstream's
    `pageExtractGlyphs`. -/
def pageExtractGlyphs (page : Page) : IO (List Span) := do
  let fontDicts ← pageFontDicts page
  let fontInfos ← fontDicts.mapM fun (name, fd) => do
    let fi ← Data.PDF.Document.FontDict.fontDictLoadInfo fd
    pure (name, fi)
  let fontInfoMap : Std.HashMap Name Data.PDF.Content.FontInfo := Std.HashMap.ofList fontInfos
  let decoder : GlyphDecoder := fun fontName str =>
    match fontInfoMap.get? fontName with
    | none => []
    | some fi => Data.PDF.Content.fontInfoDecodeGlyphs fi str
  let xobjects ← pageXObjects page
  let contents ← pageContents page
  let is ← combinedContent page.pdf contents
  let p ← runContentLoop is xobjects { mkProcessor with prGlyphDecoder := decoder }
  pure p.prSpans.reverse

/-! ── Converting glyphs to text ── -/

/-- One step of `glyphsToText`'s left fold: append `glyphs`' own text to
    `res`, inserting a space or newline first if the gap since the previous
    span looks large enough to need one. Mirrors the body of upstream's
    `glyphsToText` (its local `step`). -/
private def glyphsToTextStep
    (acc : (Data.PDF.Content.Vector Float × Bool) × String) (glyphs : List Glyph) :
    (Data.PDF.Content.Vector Float × Bool) × String :=
  match glyphs with
  | [] => acc
  | x :: xs =>
    let ((lastPos, wasSpace), res) := acc
    let lastGlyph := xs.getLastD x
    let x1 := x.glyphTopLeft.x
    let y1 := x.glyphTopLeft.y
    let x2 := lastGlyph.glyphBottomRight.x
    let y2 := lastGlyph.glyphTopLeft.y
    let space :=
      if (lastPos.y - y1).abs < 1.8 then
        if wasSpace || (lastPos.x - x1).abs < 1.8 then "" else " "
      else "\n"
    let txt := String.join ((x :: xs).filterMap (·.glyphText))
    let endWithSpace := lastGlyph.glyphText == some " "
    ((⟨x2, y2⟩, endWithSpace), res ++ space ++ txt)

/-- Convert `Span`s of glyphs to text, trying to add spaces/newlines between
    spans that look far enough apart in glyph space to need one. Mirrors
    upstream's `glyphsToText`. -/
def glyphsToText (spans : List Span) : Data.Text :=
  ((spans.map (·.spGlyphs)).foldl glyphsToTextStep ((⟨0, 0⟩, false), "")).2

/-- Extract the page's text, trying to add spaces between glyphs that don't
    otherwise appear as actual characters in the content stream. Mirrors
    upstream's `pageExtractText`. -/
def pageExtractText (page : Page) : IO Data.Text :=
  glyphsToText <$> pageExtractGlyphs page

end Data.PDF.Document.Page
