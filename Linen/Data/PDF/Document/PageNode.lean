/-
  Data.PDF.Document.PageNode — page-tree nodes

  Ports `Pdf.Document.PageNode` from Hackage's `pdf-toolbox-document`
  (https://github.com/Yuras/pdf-toolbox,
  `document/lib/Pdf/Document/PageNode.hs`, fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/document/lib/Pdf/Document/PageNode.hs`),
  module 8 of the `pdf-toolbox-document` import documented in
  `docs/imports/PdfToolboxDocument/dependencies.md`.

  `pageNodeNKids`/`pageNodeParent`/`pageNodeKids` read one node's `/Count`,
  `/Parent` and `/Kids` entries (PDF32000-1:2008 §7.7.3.2, Table 29) —
  each following at most one indirect reference, no recursion. `loadPageNode`
  resolves a `Ref` to either a `PageNode` or a `Page`, dispatching on the
  resolved dictionary's `/Type`.

  ## Termination: `pageNodePageByNum`'s untrusted tree descent

  Upstream's `pageNodePageByNum` walks down the page tree (via
  `pageNodeKids`/`loadPageNode`) to find the page at a given 0-based index,
  using each node's `/Count` to decide whether the target page is inside
  that child subtree or a later sibling. The `/Kids` and `/Count` entries it
  follows are untrusted, attacker-controlled file content — nothing stops a
  malformed file from making a `/Kids` entry point back at an ancestor
  (directly or through several nodes), and upstream has no cycle guard at
  all, so it simply recurses forever on such input.

  Per `docs/imports/PdfToolboxDocument/dependencies.md`'s termination
  strategy for this batch, `pageNodePageByNum` is ported against a fuel
  parameter *and* an explicit visited-`Ref` set, together turning "the set
  of distinct object references in a file is finite" into a real,
  data-derived decreasing measure rather than an arbitrary bound:

  - The fuel is seeded, once, from `objectCountBound` below — the trailer's
    own `/Size` entry, i.e. a value the file itself declares as one more
    than its highest object number. This is not a magic constant: it is a
    genuine upper bound on how many *distinct* object references the file
    can contain, taken directly from the file's own mandatory metadata.
  - `pageNodePageByNumFueled`/`loop` consume one unit of fuel each time they
    descend into a *new* page-tree node (`fuel + 1 → fuel`, an ordinary
    structurally-decreasing `Nat` pattern, so Lean's termination checker
    accepts the definition with no extra proof obligation); walking across
    *siblings* at the same tree depth costs no fuel, since `loop` recurses
    structurally on the (already fully materialized, finite) `List Ref` of
    kids instead.
  - The `visited` set records every node `Ref` seen so far. Before
    descending into a child, we check it is not already `visited`; if it
    is, the walk throws a "cycle detected" `corrupted` error instead of
    looping. Since a legitimate (acyclic) page tree can never revisit a
    node, this only ever fires on malformed input — a deliberate, documented
    behavioural improvement over upstream's infinite loop on the same input,
    not a change in behaviour on any well-formed file.
  - Should a pathological file manage to visit `objectCountBound` many
    *distinct* nodes without the `visited` check ever firing (impossible for
    a real file, since it would mean visiting more distinct objects than
    the file declares having), the fuel itself reaches `0` first and the
    walk still terminates, with a clear "exceeded object count" error
    rather than a silent non-terminating loop.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Document.Internal.Types
import Linen.Data.PDF.Document.Internal.Util
import Linen.Data.PDF.Document.Pdf
import Std.Data.HashMap

namespace Data.PDF.Document.PageNode

open Data.PDF.Core.Object (Name Ref)
open Data.PDF.Core.Object.Util (refValue dictValue arrayValue intValue)
open Data.PDF.Core.Exception (sure corrupted)
open Data.PDF.Core.Util (notice)
open Data.PDF.Document.Internal.Types (Pdf PageNode Page PageTree)
open Data.PDF.Document.Internal.Util (ensureType dictionaryType)

export Data.PDF.Document.Internal.Types (PageNode PageTree)

private def mkName (s : String) : Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── A data-derived bound on the number of distinct objects ── -/

/-- An upper bound on the number of distinct object references the file can
    contain: the last trailer's mandatory `/Size` entry (PDF32000-1:2008
    §7.5.5) — "the number of entries in the file's cross-reference table",
    i.e. one more than the highest object number in use. See the module
    doc-comment for why this is the fuel seed for every untrusted-graph
    walk in this batch, rather than an arbitrary constant. -/
def objectCountBound (pdf : Pdf) : IO Nat := do
  let dict ← Data.PDF.Core.File.lastTrailer pdf.file
  let n ← sure (notice (dict.get? (mkName "Size") >>= intValue) "trailer: Size should be an integer")
  pure n.toNat

/-! ── Single-step accessors (no recursion) ── -/

/-- The total number of leaf pages in the subtree rooted at this node,
    including deep children. Mirrors upstream's `pageNodeNKids`. -/
def pageNodeNKids (node : PageNode) : IO Int :=
  sure (notice (node.dict.get? (mkName "Count") >>= intValue) "Count should be an integer")

/-- This node's parent, if any. Mirrors upstream's `pageNodeParent`. -/
def pageNodeParent (node : PageNode) : IO (Option PageNode) := do
  match node.dict.get? (mkName "Parent") with
  | none => pure none
  | some (.ref ref) => do
    let obj ← Data.PDF.Document.Pdf.deref node.pdf (.ref ref)
    let d ← sure (notice (dictValue obj) "Parent should be a dictionary")
    ensureType (mkName "Pages") d
    pure (some { pdf := node.pdf, ref := ref, dict := d })
  | some _ => throw (corrupted "Parent should be an indirect ref")

/-- References to all immediate children of this node. Mirrors upstream's
    `pageNodeKids`. -/
def pageNodeKids (node : PageNode) : IO (List Ref) := do
  let raw ← sure (notice (node.dict.get? (mkName "Kids")) "Page node should have Kids")
  let obj ← Data.PDF.Document.Pdf.deref node.pdf raw
  let arr ← sure (notice (arrayValue obj) "Kids should be an array")
  arr.toList.mapM fun k => sure (notice (refValue k) "each kid should be a reference")

/-- Load a page-tree node by reference, dispatching on its resolved
    dictionary's `/Type` (`"Pages"` for an interior node, `"Page"` for a
    leaf). Mirrors upstream's `loadPageNode`. -/
def loadPageNode (pdf : Pdf) (ref : Ref) : IO PageTree := do
  let obj ← Data.PDF.Document.Pdf.lookupObject pdf ref
  let obj' ← Data.PDF.Document.Pdf.deref pdf obj
  let d ← sure (notice (dictValue obj') "page should be a dictionary")
  let nodeType ← sure (dictionaryType d)
  if nodeType == mkName "Pages" then
    pure (.node { pdf := pdf, ref := ref, dict := d })
  else if nodeType == mkName "Page" then
    pure (.leaf { pdf := pdf, ref := ref, dict := d })
  else
    throw (corrupted s!"Unexpected page tree node type: {reprStr nodeType}")

/-! ── `pageNodePageByNum`'s untrusted tree descent (see the module
     doc-comment for the fuel/visited-set termination argument) ── -/

mutual
  /-- Find the page at 0-based index `num` within the subtree rooted at
      `node`, consuming one unit of `fuel` and marking `node.ref` as
      `visited` before descending into any child. Mirrors upstream's
      `pageNodePageByNum`, with the added `fuel`/`visited` guard. -/
  def pageNodePageByNumFueled : Nat → Std.HashMap Ref Unit → PageNode → Int → IO Page
    | 0, _, node, _ =>
      throw (corrupted
        s!"pageNodePageByNum: exceeded the file's declared object count while searching from {reprStr node.ref}")
    | fuel + 1, visited, node, num => do
      if visited.contains node.ref then
        throw (corrupted s!"pageNodePageByNum: cycle detected at {reprStr node.ref}")
      let visited' := visited.insert node.ref ()
      let kids ← pageNodeKids node
      loopSiblings fuel visited' node.pdf kids num

  /-- Walk the given siblings (a finite, already-materialized `List Ref` of
      one node's `Kids`, so this recursion is ordinary structural recursion
      on the list — no untrusted-graph issue here), descending into a child
      subtree (consuming fuel, via `pageNodePageByNumFueled`) exactly when
      `num` falls within it. Mirrors the body of upstream's `pageNodePageByNum`
      (its local `loop`). -/
  def loopSiblings (fuel : Nat) (visited : Std.HashMap Ref Unit) (pdf : Pdf) :
      List Ref → Int → IO Page
    | [], _ => throw (corrupted "Page not found")
    | x :: xs, i => do
      let kid ← loadPageNode pdf x
      match kid with
      | .node n => do
        let nkids ← pageNodeNKids n
        if i < nkids then
          pageNodePageByNumFueled fuel visited n i
        else
          loopSiblings fuel visited pdf xs (i - nkids)
      | .leaf page =>
        if i == 0 then pure page else loopSiblings fuel visited pdf xs (i - 1)
end

/-- Find the page at 0-based index `num` within `node`'s subtree.

    Note: as upstream itself notes, this is not efficient for files with a
    lot of pages, since it re-traverses the tree on every call — use
    `pageNodeNKids`/`pageNodeKids`/`loadPageNode` directly for efficient
    traversal. Mirrors upstream's `pageNodePageByNum`; see the module
    doc-comment for the added, deliberate cycle detection. -/
def pageNodePageByNum (node : PageNode) (num : Int) : IO Page := do
  let fuel ← objectCountBound node.pdf
  pageNodePageByNumFueled fuel ({} : Std.HashMap Ref Unit) node num

end Data.PDF.Document.PageNode
