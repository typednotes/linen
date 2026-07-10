/-
  Data.PDF.Content.Processor — interpret content-stream operators, tracking
  graphics state

  Ports `Pdf.Content.Processor` from Hackage's `pdf-toolbox-content`
  (https://github.com/Yuras/pdf-toolbox, `content/lib/Pdf/Content/Processor.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/content/lib/Pdf/Content/Processor.hs`),
  module 11 of the `pdf-toolbox-content` import documented in
  `docs/imports/PdfToolboxContent/dependencies.md`. Upstream's own doc-comment
  flags this module "pretty experimental" — carried forward as-is.

  `processOp` walks one already-parsed `Operator` (an
  `Data.PDF.Content.Ops.Op` together with its operands) against a
  `Processor`'s current graphics state, updating text position/matrices and
  collecting `Span`s of positioned `Glyph`s as `Tj`/`TJ` operators are seen.

  ## Termination: inlining upstream's `processOp`-through-`processOp` calls

  Upstream's `Op_TD`, `Op_T_star` and `Op_apostrophe` cases implement
  themselves by calling `processOp` again with a *different*, synthetically
  built operator (`TD` via `TL` then `Td`; `T_star` via `TD`; `apostrophe`
  via `T_star` then `Tj`). Every such call targets a fixed, statically-known
  `Op` case rather than ever looping back to the same or a "larger" case —
  the call graph is `apostrophe → {T_star, Tj}`, `T_star → TD`,
  `TD → {TL, Td}`, a finite DAG with no cycle — so this is genuinely
  terminating, but not *structurally* so: the recursive argument is a freshly
  built `Operator`, not a subterm of the one just matched, so Lean's
  structural-recursion checker has no automatic decreasing measure to find
  for a literal transcription.

  Rather than force a hand-written well-founded `termination_by` rank on
  `Op` purely to justify a transcription, the shared logic each case
  delegates to is factored into ordinary (non-recursive-through-`processOp`)
  private helpers — `applyLeadingAndCheck`/`setTd`/`tjOp` below — that
  `processOp`'s `TD`/`T_star`/`apostrophe` cases call directly instead of
  routing back through `processOp` itself. `processOp` therefore has no
  self- or mutual recursion at all: every case is a direct, non-recursive
  computation, needing no termination argument beyond Lean's ordinary
  (always-terminating) pattern-matching. The externally observable
  behaviour — including the exact order in which operand-shape/value checks
  run, and so which error message surfaces first on malformed input — is
  preserved by construction; see each helper's doc-comment for the specific
  correspondence.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Content.Ops
import Linen.Data.PDF.Content.Transform
import Linen.Data.Text

namespace Data.PDF.Content.Processor

open Data.PDF.Core.Object
open Data.PDF.Core.Object.Util (realValue nameValue)
open Data.PDF.Core.Util (notice)
open Data.PDF.Content.Ops
open Data.PDF.Content.Transform

/-! ── Glyphs ── -/

/-- A decoded glyph. Mirrors upstream's `Glyph` record. -/
structure Glyph where
  /-- The code as read from the content stream. -/
  glyphCode : Int
  /-- Top-left corner of the glyph's bounding box. -/
  glyphTopLeft : Vector Float
  /-- Bottom-right corner of the glyph's bounding box. -/
  glyphBottomRight : Vector Float
  /-- Text extracted from the glyph, if any. -/
  glyphText : Option Data.Text
deriving BEq, Repr

/-- Given a font name and a string, decode it into glyphs and their widths.
    Mirrors upstream's `GlyphDecoder = Name -> ByteString -> [(Glyph, Double)]`.

    Note: it should not try to position or scale glyphs to user space —
    bounding boxes should be defined in glyph space.

    Note: a glyph's width is the distance between its origin and the next
    glyph's origin, so it generally can't be calculated from a bounding box.

    Note: `Processor` doesn't actually care about a glyph's bounding box, so
    a `GlyphDecoder` may return anything it wants there. -/
abbrev GlyphDecoder := Name → Data.ByteString → List (Glyph × Float)

/-! ── Graphics state ── -/

/-- The graphics state maintained while walking a content stream. Mirrors
    upstream's `GraphicsState` record. -/
structure GraphicsState where
  /-- Are we currently inside a text object (between `BT`/`ET`)? -/
  gsInText : Bool
  /-- The current transformation matrix. -/
  gsCurrentTransformMatrix : Transform Float
  /-- The current font, if `Tf` has been seen. -/
  gsFont : Option Name
  /-- The current font size, if `Tf` has been seen. -/
  gsFontSize : Option Float
  /-- The text matrix. Only meaningful inside a text object. -/
  gsTextMatrix : Transform Float
  /-- The text line matrix. Only meaningful inside a text object. -/
  gsTextLineMatrix : Transform Float
  /-- The text leading (line spacing). -/
  gsTextLeading : Float
  /-- The character spacing. -/
  gsTextCharSpacing : Float
  /-- The word spacing. -/
  gsTextWordSpacing : Float
deriving Repr

/-- The empty (initial) graphics state. Mirrors upstream's
    `initialGraphicsState`. -/
def initialGraphicsState : GraphicsState where
  gsInText := false
  gsCurrentTransformMatrix := identity
  gsFont := none
  gsFontSize := none
  gsTextMatrix := identity
  gsTextLineMatrix := identity
  gsTextLeading := 0
  gsTextCharSpacing := 0
  gsTextWordSpacing := 0

/-! ── Spans and the processor itself ── -/

/-- Glyphs drawn in one shot (by a single `Tj`, or one string entry of a
    `TJ`). Mirrors upstream's `Span` record. -/
structure Span where
  /-- The glyphs drawn. -/
  spGlyphs : List Glyph
  /-- The font they were drawn with. -/
  spFontName : Name

/-- A processor maintains the graphics state accumulated while walking a
    content stream, together with the `Span`s of glyphs drawn so far.
    Mirrors upstream's `Processor` record. -/
structure Processor where
  /-- The current graphics state. -/
  prState : GraphicsState
  /-- The `q`/`Q` graphics-state stack. -/
  prStateStack : List GraphicsState
  /-- How to decode a font's strings into glyphs. -/
  prGlyphDecoder : GlyphDecoder
  /-- Every `Span` drawn so far, most recent first. -/
  prSpans : List Span

/-- A processor in its initial state, with a `GlyphDecoder` that decodes
    nothing (returns no glyphs for any input). Mirrors upstream's
    `mkProcessor`. -/
def mkProcessor : Processor where
  prState := initialGraphicsState
  prStateStack := []
  prGlyphDecoder := fun _ _ => []
  prSpans := []

/-! ── Shared helpers ── -/

/-- Fail unless the processor's `gsInText` matches `inText`. Mirrors
    upstream's `ensureInTextObject`. -/
def ensureInTextObject (inText : Bool) (p : Processor) : Except String Unit :=
  if inText == p.prState.gsInText then .ok ()
  else
    .error s!"ensureInTextObject: expected: {inText}, found: {p.prState.gsInText}"

/-- The `Td`-operator's actual state update: prepend a translation by
    `(tx, ty)` to the text line matrix, and set the text matrix to match.
    Mirrors the body of upstream's `Op_Td` case (shared, via this helper,
    with the `TD`/`T_star` cases that ultimately perform the same update). -/
def setTd (tx ty : Float) (gs : GraphicsState) : GraphicsState :=
  let tm := translate tx ty gs.gsTextLineMatrix
  { gs with gsTextMatrix := tm, gsTextLineMatrix := tm }

/-- The shared `TD`-prefix shared by `Op_TD` and `Op_T_star`: set the text
    leading to `-ty` (mirrors upstream's `Op_TL` sub-call), then check
    `ensureInTextObject true` on the resulting processor (mirrors upstream's
    `Op_Td` sub-call's own check, run against the leading-updated state).
    Returns the leading-updated processor on success. -/
def applyLeadingAndCheck (ty : Float) (p : Processor) : Except String Processor := do
  let p' := { p with prState := { p.prState with gsTextLeading := -ty } }
  ensureInTextObject true p'
  pure p'

/-- `Op_TD`'s full body, given already-parsed operands: mirrors upstream's
    `Op_TD` case (itself `Op_TL` then `Op_Td`) via `applyLeadingAndCheck`
    (the `TL` step, plus `Td`'s `ensureInTextObject` check) followed by
    `setTd` (the rest of `Td`'s body) — preserving the exact order in which
    `ty`, then the in-text-object check, then `tx`, are validated. -/
def doTD (txo tyo : Object) (p : Processor) : Except String Processor := do
  let ty ← notice (realValue tyo) "TD: y should be a real value"
  let p' ← applyLeadingAndCheck ty p
  let tx ← notice (realValue txo) "Td: x should be a real value"
  pure { p' with prState := setTd tx ty p'.prState }

/-- `Op_T_star`'s full body: mirrors upstream's `Op_T_star` case (its own
    `ensureInTextObject` check, then a delegated `Op_TD` call with
    `(tx, ty) = (0, -leading)`) via `applyLeadingAndCheck`/`setTd` directly,
    rather than routing through `doTD`'s `Object`-typed operand parsing
    (unnecessary here since both operands are synthesized, always-valid
    numbers — see the module doc-comment). -/
def doT_star (p : Processor) : Except String Processor := do
  ensureInTextObject true p
  let l := p.prState.gsTextLeading
  let p' ← applyLeadingAndCheck (-l) p
  pure { p' with prState := setTd 0 (-l) p'.prState }

/-! ── Positioning glyphs ── -/

/-- Position a run of decoded glyphs (with their widths) along the text
    matrix, applying the current transformation matrix and font size to
    each glyph's bounding box, and advancing the text matrix by each
    glyph's width (plus char/word spacing). Mirrors upstream's
    `positionGlyghs`; structurally recursive on the glyph list. -/
def positionGlyphs (fontSize : Float) (ctm textMatrix : Transform Float)
    (charSpacing wordSpacing : Float) : List (Glyph × Float) → Transform Float × List Glyph :=
  go textMatrix []
where
  go (tm : Transform Float) (acc : List Glyph) : List (Glyph × Float) → Transform Float × List Glyph
    | [] => (tm, acc.reverse)
    | (g, width) :: gs =>
      let scaled := scale fontSize fontSize
      let combined := multiply tm ctm
      let g' := { g with
        glyphTopLeft := transform combined (transform scaled g.glyphTopLeft),
        glyphBottomRight := transform combined (transform scaled g.glyphBottomRight) }
      let spacing := charSpacing + if g.glyphText == some (Data.Text.singleton ' ') then wordSpacing else 0
      let tm' := translate (width * fontSize + spacing) 0 tm
      go tm' (g' :: acc) gs

/-! ── Text showing (`Tj`/`TJ`) ── -/

/-- `Op_Tj`'s full body, given an already-parsed string operand. Mirrors
    upstream's `Op_Tj` case. -/
def doTj (str : Data.ByteString) (p : Processor) : Except String Processor := do
  let gstate := p.prState
  let fontName ← notice gstate.gsFont "Op_Tj: font not set"
  let fontSize ← notice gstate.gsFontSize "Op_Tj: font size not set"
  let (tm, glyphs) := positionGlyphs fontSize gstate.gsCurrentTransformMatrix gstate.gsTextMatrix
      gstate.gsTextCharSpacing gstate.gsTextWordSpacing (p.prGlyphDecoder fontName str)
  let sp : Span := { spGlyphs := glyphs, spFontName := fontName }
  pure { p with prSpans := sp :: p.prSpans, prState := { gstate with gsTextMatrix := tm } }

/-- `Op_Tj`'s case-dispatch, given a raw operand list: succeeds only on
    exactly one string operand, mirroring upstream's `Op_Tj` clauses
    (the pattern-matching `[String str]` case, and the wrong-shape
    catch-all). Shared with `Op_apostrophe` (see `doApostrophe`), which
    upstream implements by delegating to this same dispatch. -/
def tjOp (args : List Object) (p : Processor) : Except String Processor :=
  match args with
  | [.string str] => doTj str p
  | _ => .error s!"Op_Tj: wrong number of agruments:{reprStr args}"

/-- `Op_TJ`'s full body, given an already-parsed array operand. Each array
    entry is either a string (positioned via `positionGlyphs`, exactly as
    `Op_Tj` would) or a number (a bare text-space adjustment); any other
    entry is skipped. Mirrors upstream's `Op_TJ` case, including its
    left-to-right accumulation order for the resulting `Span`s (see the
    module's `prSpans` field doc: most recent first). -/
def doTJ (array : Array Object) (p : Processor) : Except String Processor := do
  let gstate := p.prState
  let fontName ← notice gstate.gsFont "Op_Tj: font not set"
  let fontSize ← notice gstate.gsFontSize "Op_Tj: font size not set"
  let rec loop (tm : Transform Float) (acc : List (List Glyph)) :
      List Object → Transform Float × List (List Glyph)
    | [] => (tm, acc)
    | .string str :: rest =>
      let (tm', gs) := positionGlyphs fontSize gstate.gsCurrentTransformMatrix tm
          gstate.gsTextCharSpacing gstate.gsTextWordSpacing (p.prGlyphDecoder fontName str)
      loop tm' (gs :: acc) rest
    | .number n :: rest =>
      let d := n.toRealFloat
      loop (translate (-d * fontSize / 1000) 0 tm) acc rest
    | _ :: rest => loop tm acc rest
  let (textMatrix, groups) := loop gstate.gsTextMatrix [] array.toList
  let spans := groups.map (fun gs => Span.mk gs fontName)
  pure { p with prSpans := spans ++ p.prSpans, prState := { gstate with gsTextMatrix := textMatrix } }

/-! ── The operator interpreter ── -/

/-- Process one content-stream operator against a `Processor`, returning
    the updated `Processor` or an error. Mirrors upstream's `processOp`; see
    the module doc-comment for how the `TD`/`T_star`/`apostrophe` cases
    avoid upstream's self-referential `processOp` calls. Any operator not
    handled here (most of the Annex A operator list — graphics/path/color
    operators that don't affect text extraction — matching upstream's own
    scope) leaves the processor unchanged. -/
def processOp (operator : Operator) (p : Processor) : Except String Processor :=
  match operator with
  | (.q, []) => .ok { p with prStateStack := p.prState :: p.prStateStack }
  | (.q, args) => .error s!"Op_q: wrong number of arguments: {reprStr args}"
  | (.Q, []) =>
    match p.prStateStack with
    | [] => .error "Op_Q: state is empty"
    | x :: xs => .ok { p with prState := x, prStateStack := xs }
  | (.Q, args) => .error s!"Op_Q: wrong number of arguments: {reprStr args}"
  | (.BT, []) => do
    ensureInTextObject false p
    pure { p with
      prState := { p.prState with gsInText := true, gsTextMatrix := identity, gsTextLineMatrix := identity } }
  | (.BT, args) => .error s!"Op_BT: wrong number of arguments: {reprStr args}"
  | (.ET, []) => do
    ensureInTextObject true p
    pure { p with prState := { p.prState with gsInText := false } }
  | (.ET, args) => .error s!"Op_ET: wrong number of arguments: {reprStr args}"
  | (.Td, [txo, tyo]) => do
    ensureInTextObject true p
    let tx ← notice (realValue txo) "Td: x should be a real value"
    let ty ← notice (realValue tyo) "Td: y should be a real value"
    pure { p with prState := setTd tx ty p.prState }
  | (.Td, args) => .error s!"Op_Td: wrong number of arguments: {reprStr args}"
  | (.TD, [txo, tyo]) => doTD txo tyo p
  | (.TD, args) => .error s!"Op_TD: wrong number of arguments: {reprStr args}"
  | (.Tm, [a', b', c', d', e', f']) => do
    ensureInTextObject true p
    let a ← notice (realValue a') "Tm: a should be a real value"
    let b ← notice (realValue b') "Tm: b should be a real value"
    let c ← notice (realValue c') "Tm: c should be a real value"
    let d ← notice (realValue d') "Tm: d should be a real value"
    let e ← notice (realValue e') "Tm: e should be a real value"
    let f ← notice (realValue f') "Tm: f should be a real value"
    let tm : Transform Float := ⟨a, b, c, d, e, f⟩
    pure { p with prState := { p.prState with gsTextMatrix := tm, gsTextLineMatrix := tm } }
  | (.Tm, args) => .error s!"Op_Tm: wrong number of arguments: {reprStr args}"
  | (.T_star, []) => doT_star p
  | (.T_star, args) => .error s!"Op_T_star: wrong number of arguments: {reprStr args}"
  | (.TL, [lo]) => do
    let l ← notice (realValue lo) "TL: l should be a real value"
    pure { p with prState := { p.prState with gsTextLeading := l } }
  | (.TL, args) => .error s!"Op_TL: wrong number of arguments: {reprStr args}"
  | (.cm, [a', b', c', d', e', f']) => do
    let a ← notice (realValue a') "cm: a should be a real value"
    let b ← notice (realValue b') "cm: b should be a real value"
    let c ← notice (realValue c') "cm: c should be a real value"
    let d ← notice (realValue d') "cm: d should be a real value"
    let e ← notice (realValue e') "cm: e should be a real value"
    let f ← notice (realValue f') "cm: f should be a real value"
    let ctm := multiply (Transform.mk a b c d e f) p.prState.gsCurrentTransformMatrix
    pure { p with prState := { p.prState with gsCurrentTransformMatrix := ctm } }
  | (.cm, args) => .error s!"Op_cm: wrong number of arguments: {reprStr args}"
  | (.Tf, [fontO, szO]) => do
    let font ← notice (nameValue fontO) "Tf: font should be a name"
    let sz ← notice (realValue szO) "Tf: size should be a real value"
    pure { p with prState := { p.prState with gsFont := some font, gsFontSize := some sz } }
  | (.Tf, args) => .error s!"Op_Tf: wrong number of agruments: {reprStr args}"
  | (.Tj, args) => tjOp args p
  | (.TJ, [.array array]) => doTJ array p
  | (.TJ, args) => .error s!"Op_TJ: wrong number of agruments:{reprStr args}"
  | (.Tc, [o]) => do
    let spacing ← notice (realValue o) "Tc: spacing should be a real value"
    pure { p with prState := { p.prState with gsTextCharSpacing := spacing } }
  | (.Tc, args) => .error s!"Op_Tc: wrong number of agruments:{reprStr args}"
  | (.Tw, [o]) => do
    let spacing ← notice (realValue o) "Tw: spacing should be a real value"
    pure { p with prState := { p.prState with gsTextWordSpacing := spacing } }
  | (.Tw, args) => .error s!"Op_Tw: wrong number of agruments:{reprStr args}"
  | (.apostrophe, [o]) => do
    let p' ← doT_star p
    tjOp [o] p'
  | (.apostrophe, args) => .error s!"Op_apostrophe: wrong number of agruments:{reprStr args}"
  | (_, _) => .ok p

end Data.PDF.Content.Processor
