/-
  `Linen.Text.Pandoc.Builder` — the monoidal `Blocks`/`Inlines` builder DSL.

  ## Haskell source

  Ported from `Text.Pandoc.Builder` in the `pandoc-types` package
  (v1.23.1, `src/Text/Pandoc/Builder.hs`).

  Provides the `Many` wrapper (`Inlines`/`Blocks`) with its smart-concatenating
  monoid, the document/inline/block smart constructors that every reader emits
  into, and the table-normalisation helpers (`normalizeTableHead`/`Body`/
  `Foot`, `placeRowSection`, `clipRows`).

  ### Deviations from upstream

  * `Many` wraps a `List` rather than `Data.Sequence.Seq`.  The `Inlines`
    `Semigroup` still performs the same boundary "meld" (merging adjacent
    `Space`/`SoftBreak`/`Str`/`Emph`/… ).
  * Upstream's table normalisation streams cells against `repeat emptyCell`
    (an infinite list); Lean has no infinite lists, so the cell stream is
    padded with a finite `replicate twidth emptyCell`, which is always enough
    to fill a `twidth`-column row.  `placeRowSection` recurses on the finite
    overhang list (`termination_by` its length), using a small
    `dropAtMostWhile` length lemma to discharge the decrease.
  * `IsString Inlines` becomes a `Coe String Inlines`.
-/

import Linen.Text.Pandoc.Definition

namespace Linen.Text.Pandoc

open Data (Map)

-- ── The `Many` wrapper ────────────────────────────────────────────────

/-- A sequence of AST fragments (a list, in this port).  `Inlines`/`Blocks`
    are `Many Inline`/`Many Block`. -/
structure Many (α : Type) where
  unMany : List α
  deriving Repr, BEq, Inhabited

/-- The inline builder type. -/
abbrev Inlines := Many Inline
/-- The block builder type. -/
abbrev Blocks := Many Block

namespace Many

/-- The underlying list. -/
def toList (m : Many α) : List α := m.unMany
/-- A one-element `Many`. -/
def singleton (x : α) : Many α := ⟨[x]⟩
/-- Build a `Many` from a list. -/
def fromList (l : List α) : Many α := ⟨l⟩
/-- Is the `Many` empty? -/
def isNull (m : Many α) : Bool := m.unMany.isEmpty

end Many

-- ── Monoid structure ──────────────────────────────────────────────────

instance : Inhabited (Many α) := ⟨⟨[]⟩⟩

/-- `Blocks` concatenate plainly. -/
instance : Append Blocks where append a b := ⟨a.unMany ++ b.unMany⟩

/-- Merge two adjacent inlines at a concatenation boundary, mirroring
    upstream's `Inlines` `Semigroup`. -/
private def meld : Inline → Inline → List Inline
  | .Space, .Space => [.Space]
  | .Space, .SoftBreak => [.SoftBreak]
  | .SoftBreak, .Space => [.SoftBreak]
  | .Str t1, .Str t2 => [.Str (t1 ++ t2)]
  | .Emph i1, .Emph i2 => [.Emph (i1 ++ i2)]
  | .Underline i1, .Underline i2 => [.Underline (i1 ++ i2)]
  | .Strong i1, .Strong i2 => [.Strong (i1 ++ i2)]
  | .Subscript i1, .Subscript i2 => [.Subscript (i1 ++ i2)]
  | .Superscript i1, .Superscript i2 => [.Superscript (i1 ++ i2)]
  | .Strikeout i1, .Strikeout i2 => [.Strikeout (i1 ++ i2)]
  | .Space, .LineBreak => [.LineBreak]
  | .LineBreak, .Space => [.LineBreak]
  | .SoftBreak, .LineBreak => [.LineBreak]
  | .LineBreak, .SoftBreak => [.LineBreak]
  | .SoftBreak, .SoftBreak => [.SoftBreak]
  | x, y => [x, y]

/-- `Inlines` concatenate with a boundary meld. -/
instance : Append Inlines where
  append a b :=
    match a.unMany.reverse, b.unMany with
    | [], _ => b
    | _, [] => a
    | x :: revInit, y :: ys => ⟨revInit.reverse ++ meld x y ++ ys⟩

/-- Trim leading and trailing spaces and softbreaks from an `Inlines`. -/
def trimInlines (i : Inlines) : Inlines :=
  let isSp : Inline → Bool := fun x => x matches .Space | .SoftBreak
  ⟨((i.unMany.dropWhile isSp).reverse.dropWhile isSp).reverse⟩

-- ── Document builders ─────────────────────────────────────────────────

/-- A document from a `Blocks` with empty metadata. -/
def doc (b : Blocks) : Pandoc := ⟨nullMeta, b.toList⟩

/-- Things that can be turned into a `MetaValue`. -/
class ToMetaValue (α : Type) where
  toMetaValue : α → MetaValue

instance : ToMetaValue MetaValue := ⟨id⟩
instance : ToMetaValue Blocks := ⟨fun b => .MetaBlocks b.toList⟩
instance : ToMetaValue Inlines := ⟨fun i => .MetaInlines i.toList⟩
instance : ToMetaValue Bool := ⟨.MetaBool⟩
instance : ToMetaValue String := ⟨.MetaString⟩
instance [ToMetaValue α] : ToMetaValue (List α) := ⟨fun xs => .MetaList (xs.map ToMetaValue.toMetaValue)⟩
instance [ToMetaValue α] : ToMetaValue (Map String α) :=
  ⟨fun m => .MetaMap (m.toList'.map (fun kv => (kv.1, ToMetaValue.toMetaValue kv.2)))⟩

/-- Structures carrying document metadata. -/
class HasMeta (α : Type) where
  setMeta {β : Type} [ToMetaValue β] : String → β → α → α
  deleteMeta : String → α → α

instance : HasMeta Meta where
  setMeta key val m := ⟨m.unMeta.insert' key (ToMetaValue.toMetaValue val)⟩
  deleteMeta key m := ⟨m.unMeta.delete key⟩

instance : HasMeta Pandoc where
  setMeta key val d := ⟨⟨d.docMeta.unMeta.insert' key (ToMetaValue.toMetaValue val)⟩, d.blocks⟩
  deleteMeta key d := ⟨⟨d.docMeta.unMeta.delete key⟩, d.blocks⟩

/-- Set the document title. -/
def setTitle (t : Inlines) : Pandoc → Pandoc := HasMeta.setMeta "title" t
/-- Set the document authors. -/
def setAuthors (as : List Inlines) : Pandoc → Pandoc := HasMeta.setMeta "author" as
/-- Set the document date. -/
def setDate (d : Inlines) : Pandoc → Pandoc := HasMeta.setMeta "date" d

-- ── Inline list builders ──────────────────────────────────────────────

private def isSpaceChar (c : Char) : Bool := c == ' ' || c == '\r' || c == '\n' || c == '\t'
private def isNewlineChar (c : Char) : Bool := c == '\r' || c == '\n'

/-- Take a maximal run of characters of the same space-category. -/
private def takeCat (cat : Bool) : List Char → List Char × List Char
  | [] => ([], [])
  | c :: cs =>
    if isSpaceChar c == cat then
      let p := takeCat cat cs
      (c :: p.1, p.2)
    else ([], c :: cs)

private theorem takeCat_snd_le (cat : Bool) (l : List Char) :
    (takeCat cat l).2.length ≤ l.length := by
  induction l with
  | nil => simp [takeCat]
  | cons c cs ih =>
    simp only [takeCat]
    split
    · simpa using Nat.le_succ_of_le ih
    · simp

/-- Split a character list into maximal same-category runs. -/
private def textRuns : List Char → List (List Char)
  | [] => []
  | c :: cs =>
    (c :: (takeCat (isSpaceChar c) cs).1) :: textRuns (takeCat (isSpaceChar c) cs).2
  termination_by l => l.length
  decreasing_by
    have h := takeCat_snd_le (isSpaceChar c) cs
    simp_wf
    omega

private def convRun (run : List Char) : Inline :=
  if run.all isSpaceChar then
    if run.any isNewlineChar then .SoftBreak else .Space
  else .Str (String.ofList run)

/-- Convert a `String` to `Inlines`, turning interword spaces into `Space`s or
    `SoftBreak`s.  For a `Str` with literal spaces, use `str`. -/
def text (s : String) : Inlines := ⟨(textRuns s.toList).map convRun⟩

/-- A literal `Str`. -/
def str (t : String) : Inlines := Many.singleton (.Str t)

def emph (i : Inlines) : Inlines := Many.singleton (.Emph i.toList)
def underline (i : Inlines) : Inlines := Many.singleton (.Underline i.toList)
def strong (i : Inlines) : Inlines := Many.singleton (.Strong i.toList)
def strikeout (i : Inlines) : Inlines := Many.singleton (.Strikeout i.toList)
def superscript (i : Inlines) : Inlines := Many.singleton (.Superscript i.toList)
def subscript (i : Inlines) : Inlines := Many.singleton (.Subscript i.toList)
def smallcaps (i : Inlines) : Inlines := Many.singleton (.SmallCaps i.toList)

private def quoted (qt : QuoteType) (i : Inlines) : Inlines := Many.singleton (.Quoted qt i.toList)
def singleQuoted (i : Inlines) : Inlines := quoted .SingleQuote i
def doubleQuoted (i : Inlines) : Inlines := quoted .DoubleQuote i

def cite (cts : List Citation) (i : Inlines) : Inlines := Many.singleton (.Cite cts i.toList)

/-- Inline code with attributes. -/
def codeWith (attr : Attr) (s : String) : Inlines := Many.singleton (.Code attr s)
/-- Plain inline code. -/
def code (s : String) : Inlines := codeWith nullAttr s

def space : Inlines := Many.singleton .Space
def softbreak : Inlines := Many.singleton .SoftBreak
def linebreak : Inlines := Many.singleton .LineBreak

/-- Inline math. -/
def math (s : String) : Inlines := Many.singleton (.Math .InlineMath s)
/-- Display math. -/
def displayMath (s : String) : Inlines := Many.singleton (.Math .DisplayMath s)

def rawInline (format : String) (s : String) : Inlines := Many.singleton (.RawInline ⟨format⟩ s)

def linkWith (attr : Attr) (url title : String) (x : Inlines) : Inlines :=
  Many.singleton (.Link attr x.toList (url, title))
def link (url title : String) (x : Inlines) : Inlines := linkWith nullAttr url title x

def imageWith (attr : Attr) (url title : String) (x : Inlines) : Inlines :=
  Many.singleton (.Image attr x.toList (url, title))
def image (url title : String) (x : Inlines) : Inlines := imageWith nullAttr url title x

def note (b : Blocks) : Inlines := Many.singleton (.Note b.toList)
def spanWith (attr : Attr) (i : Inlines) : Inlines := Many.singleton (.Span attr i.toList)

-- ── Block list builders ───────────────────────────────────────────────

def para (i : Inlines) : Blocks := Many.singleton (.Para i.toList)
def plain (i : Inlines) : Blocks := if i.isNull then ⟨[]⟩ else Many.singleton (.Plain i.toList)
def lineBlock (ls : List Inlines) : Blocks := Many.singleton (.LineBlock (ls.map Many.toList))

/-- A code block with attributes. -/
def codeBlockWith (attr : Attr) (s : String) : Blocks := Many.singleton (.CodeBlock attr s)
/-- A plain code block. -/
def codeBlock (s : String) : Blocks := codeBlockWith nullAttr s

def rawBlock (format : String) (s : String) : Blocks := Many.singleton (.RawBlock ⟨format⟩ s)
def blockQuote (b : Blocks) : Blocks := Many.singleton (.BlockQuote b.toList)

/-- Ordered list with explicit attributes. -/
def orderedListWith (attrs : ListAttributes) (items : List Blocks) : Blocks :=
  Many.singleton (.OrderedList attrs (items.map Many.toList))
/-- Ordered list with default attributes. -/
def orderedList (items : List Blocks) : Blocks :=
  orderedListWith (1, .DefaultStyle, .DefaultDelim) items

def bulletList (items : List Blocks) : Blocks := Many.singleton (.BulletList (items.map Many.toList))

def definitionList (items : List (Inlines × List Blocks)) : Blocks :=
  Many.singleton (.DefinitionList (items.map (fun p => (p.1.toList, p.2.map Many.toList))))

def headerWith (attr : Attr) (level : Int) (i : Inlines) : Blocks :=
  Many.singleton (.Header level attr i.toList)
def header (level : Int) (i : Inlines) : Blocks := headerWith nullAttr level i

def horizontalRule : Blocks := Many.singleton .HorizontalRule

def cellWith (attr : Attr) (a : Alignment) (r : RowSpan) (c : ColSpan) (b : Blocks) : Cell :=
  .Cell attr a r c b.toList
def cell (a : Alignment) (r : RowSpan) (c : ColSpan) (b : Blocks) : Cell := cellWith nullAttr a r c b
/-- A 1×1 cell with default alignment. -/
def simpleCell (b : Blocks) : Cell := cell .AlignDefault 1 1 b
/-- A 1×1 empty cell. -/
def emptyCell : Cell := simpleCell ⟨[]⟩

def figureWith (attr : Attr) (capt : Caption) (b : Blocks) : Blocks :=
  Many.singleton (.Figure attr capt b.toList)
def figure (capt : Caption) (b : Blocks) : Blocks := figureWith nullAttr capt b

def caption (short : Option ShortCaption) (b : Blocks) : Caption := .Caption short b.toList
def simpleCaption (b : Blocks) : Caption := caption none b
def emptyCaption : Caption := simpleCaption ⟨[]⟩

def divWith (attr : Attr) (b : Blocks) : Blocks := Many.singleton (.Div attr b.toList)

-- ── Table processing ──────────────────────────────────────────────────

private def getRowSpan : Cell → Int | .Cell _ _ h _ _ => h
private def setColSpanC (w : Int) : Cell → Cell | .Cell a al h _ b => .Cell a al h w b
private def getColSpanC : Cell → Int | .Cell _ _ _ w _ => w

/-- Drop at most `n` leading elements while they satisfy `p`; return the count
    dropped and the remaining suffix. -/
private def dropAtMostWhile (p : Int → Bool) : Nat → List Int → Nat × List Int
  | _, [] => (0, [])
  | 0, l => (0, l)
  | n + 1, x :: xs =>
    if p x then ((dropAtMostWhile p n xs).1 + 1, (dropAtMostWhile p n xs).2)
    else (0, x :: xs)

private theorem dropAtMostWhile_len (p : Int → Bool) (n : Nat) (l : List Int) :
    (dropAtMostWhile p n l).1 + (dropAtMostWhile p n l).2.length = l.length := by
  induction n generalizing l with
  | zero => cases l <;> simp [dropAtMostWhile]
  | succ n ih =>
    cases l with
    | nil => simp [dropAtMostWhile]
    | cons x xs =>
      cases hpx : p x with
      | true => simp only [dropAtMostWhile, hpx, if_true, List.length_cons]; have := ih xs; omega
      | false => simp [dropAtMostWhile, hpx]

set_option linter.unusedVariables false in
/-- Lay a list of cells on a single grid row, given the previous row's overhang
    (`oldHang`).  Returns the current row's overhang, the placed (dimension-
    adjusted) cells, and the unused cells. -/
private def placeRowSection (oldHang : List Int) (cells : List Cell) :
    List Int × List Cell × List Cell :=
  match oldHang with
  | [] => ([], [], cells)
  | o :: os =>
    if o > 1 then
      let res := placeRowSection os cells
      ((o - 1) :: res.1, res.2.1, res.2.2)
    else
      match cells with
      | [] => ([], [], [])
      | c :: cells' =>
        match hd : dropAtMostWhile (· == 1) (max 1 (getColSpanC c)).toNat (o :: os) with
        | (0, _) => ([], [], c :: cells')
        | (k + 1, rest) =>
          let w'' := min (Int.ofNat (k + 1)) (max 1 (getColSpanC c))
          let res := placeRowSection rest cells'
          (List.replicate w''.toNat (getRowSpan c) ++ res.1, setColSpanC w'' c :: res.2.1, res.2.2)
  termination_by oldHang.length
  decreasing_by
    · simp_wf
    · have hlen := dropAtMostWhile_len (· == 1) (max 1 (getColSpanC c)).toNat (o :: os)
      rw [hd] at hlen
      simp_wf
      simp only [List.length_cons] at *
      omega

/-- Ensure each cell's height lies between 1 and its distance to the end of the
    section. -/
def clipRows (rows : List Row) : List Row :=
  let total : Int := Int.ofNat rows.length
  let heights := (List.range rows.length).map (fun i => total - Int.ofNat i)
  let clipH (high : Int) : Cell → Cell := fun c =>
    let h := getRowSpan c
    setRowSpan (min high (max 1 h)) c
  (heights.zip rows).map (fun p => match p.2 with | .Row attr cells => .Row attr (cells.map (clipH p.1)))
where
  setRowSpan (h : Int) : Cell → Cell | .Cell a al _ w b => .Cell a al h w b

/-- Place a list of already-clipped rows on a grid `twidth` columns wide,
    padding each row's cells with empty cells. -/
private def normalizeHeaderRows (twidth : Nat) : List Int → List Row → List Row
  | _, [] => []
  | oldHang, .Row attr cells :: rs =>
    let res := placeRowSection oldHang (cells ++ List.replicate twidth emptyCell)
    .Row attr res.2.1 :: normalizeHeaderRows twidth res.1 rs

private def normalizeHeaderSection (twidth : Nat) (rows : List Row) : List Row :=
  normalizeHeaderRows twidth (List.replicate twidth 1) (clipRows rows)

private def normalizeBodyRows (twidth : Nat) : List Int → List Int → List Row → List Row
  | _, _, [] => []
  | headHang, bodyHang, .Row attr cells :: rs =>
    let padded := cells ++ List.replicate twidth emptyCell
    let resHead := placeRowSection headHang padded
    let resBody := placeRowSection bodyHang resHead.2.2
    .Row attr (resHead.2.1 ++ resBody.2.1) :: normalizeBodyRows twidth resHead.1 resBody.1 rs

private def normalizeBodySection (twidth : Nat) (rhc : Int) (rows : List Row) : List Row :=
  let rbc := twidth - rhc.toNat
  normalizeBodyRows twidth (List.replicate rhc.toNat 1) (List.replicate rbc 1) (clipRows rows)

/-- Normalize a `TableHead`. -/
def normalizeTableHead (twidth : Nat) : TableHead → TableHead
  | .TableHead attr rows => .TableHead attr (normalizeHeaderSection twidth rows)

/-- Normalize a `TableBody`. -/
def normalizeTableBody (twidth : Nat) : TableBody → TableBody
  | .TableBody attr rhc th tb =>
    let rhc' := max 0 (min (Int.ofNat twidth) rhc)
    .TableBody attr rhc' (normalizeHeaderSection twidth th) (normalizeBodySection twidth rhc' tb)

/-- Normalize a `TableFoot`. -/
def normalizeTableFoot (twidth : Nat) : TableFoot → TableFoot
  | .TableFoot attr rows => .TableFoot attr (normalizeHeaderSection twidth rows)

/-- Table builder with attributes, performing head/body/foot normalisation.
    The number of columns is the length of the `ColSpec` list. -/
def tableWith (attr : Attr) (capt : Caption) (specs : List ColSpec)
    (th : TableHead) (tbs : List TableBody) (tf : TableFoot) : Blocks :=
  let twidth := specs.length
  Many.singleton (.Table attr capt specs
    (normalizeTableHead twidth th)
    (tbs.map (normalizeTableBody twidth))
    (normalizeTableFoot twidth tf))

/-- Table builder with default attributes. -/
def table (capt : Caption) (specs : List ColSpec)
    (th : TableHead) (tbs : List TableBody) (tf : TableFoot) : Blocks :=
  tableWith nullAttr capt specs th tbs tf

/-- A simple table without a caption. -/
def simpleTable (headers : List Blocks) (rows : List (List Blocks)) : Blocks :=
  let numcols := ((headers :: rows).map (·.length)).foldl max 0
  let defaults : ColSpec := (.AlignDefault, .ColWidthDefault)
  let toRow : List Blocks → Row := fun l => .Row nullAttr (l.map simpleCell)
  let th : TableHead := .TableHead nullAttr (if headers.isEmpty then [] else [toRow headers])
  let tb : TableBody := .TableBody nullAttr 0 [] (rows.map toRow)
  let tf : TableFoot := .TableFoot nullAttr []
  table emptyCaption (List.replicate numcols defaults) th [tb] tf

/-- A simple figure from attributes, caption inlines, an image path and title. -/
def simpleFigureWith (attr : Attr) (figureCaption : Inlines) (url title : String) : Blocks :=
  figureWith nullAttr (simpleCaption (plain figureCaption))
    (plain (imageWith attr url title ⟨[]⟩))
def simpleFigure (figureCaption : Inlines) (url title : String) : Blocks :=
  simpleFigureWith nullAttr figureCaption url title

-- ── IsString ──────────────────────────────────────────────────────────

instance : Coe String Inlines := ⟨text⟩

end Linen.Text.Pandoc
