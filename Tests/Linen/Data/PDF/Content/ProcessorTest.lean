/-
  Tests for `Linen.Data.PDF.Content.Processor`.

  All of `processOp`, its helpers, and `positionGlyphs` are pure `Except`
  computations, so every test here is a `#guard`.
-/
import Linen.Data.PDF.Content.Processor

open Data.PDF.Core.Object Data.PDF.Content.Ops Data.PDF.Content.Transform Data.PDF.Content.Processor

namespace Tests.Data.PDF.Content.Processor

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

private def str (s : String) : Object := .string (Data.ByteString.pack s.toUTF8.toList)

-- A `GlyphDecoder` that decodes every byte of a string to a fixed-width,
-- textless glyph — enough to exercise `Tj`/`TJ` without depending on any
-- real font data.
private def testDecoder : GlyphDecoder := fun _ bs =>
  bs.unpack.map (fun c =>
    ({ glyphCode := (c.toNat : Int), glyphTopLeft := ⟨0, 0⟩, glyphBottomRight := ⟨1, 1⟩, glyphText := none },
     (1 : Float)))

private def testProcessor : Processor := { mkProcessor with prGlyphDecoder := testDecoder }

-- ── `q`/`Q` ── the graphics-state stack

#guard match processOp (.q, []) mkProcessor with
  | .ok p => p.prStateStack.length == 1
  | .error _ => false

#guard match processOp (.q, [.number 1]) mkProcessor with
  | .error _ => true
  | .ok _ => false

#guard match processOp (.Q, []) mkProcessor with
  | .error _ => true -- the stack is empty
  | .ok _ => false

#guard match processOp (.q, []) mkProcessor >>= processOp (.Q, []) with
  | .ok p => p.prStateStack.isEmpty
  | .error _ => false

-- `Q` restores the exact state pushed by the matching `q` (here, a change
-- to `gsTextLeading` made between `q` and `Q` is undone).
#guard match processOp (.q, []) mkProcessor >>= processOp (.TL, [.number 12]) >>= processOp (.Q, []) with
  | .ok p => p.prState.gsTextLeading == 0
  | .error _ => false

-- ── `BT`/`ET` ── entering/leaving a text object

#guard match processOp (.BT, []) mkProcessor with
  | .ok p => p.prState.gsInText
  | .error _ => false

#guard match processOp (.BT, []) mkProcessor >>= processOp (.BT, []) with
  | .error _ => true -- already in a text object
  | .ok _ => false

#guard match processOp (.ET, []) mkProcessor with
  | .error _ => true -- not in a text object yet
  | .ok _ => false

#guard match processOp (.BT, []) mkProcessor >>= processOp (.ET, []) with
  | .ok p => !p.prState.gsInText
  | .error _ => false

-- ── `Td`/`TD`/`T*`/`Tm` ── text positioning

#guard match processOp (.BT, []) mkProcessor >>= processOp (.Td, [.number 3, .number 4]) with
  | .ok p => p.prState.gsTextMatrix == translate 3 4 identity
  | .error _ => false

-- `Td` outside a text object is rejected.
#guard match processOp (.Td, [.number 3, .number 4]) mkProcessor with
  | .error _ => true
  | .ok _ => false

-- `TD tx ty` sets the leading to `-ty` and moves exactly as `Td tx ty` would.
#guard match processOp (.BT, []) mkProcessor >>= processOp (.TD, [.number 3, .number (-4)]) with
  | .ok p => p.prState.gsTextLeading == 4 ∧ p.prState.gsTextMatrix == translate 3 (-4) identity
  | .error _ => false

-- `T*` moves down by the current leading, with no horizontal movement.
#guard
  match processOp (.BT, []) mkProcessor >>= processOp (.TL, [.number 5]) >>= processOp (.T_star, []) with
  | .ok p => p.prState.gsTextMatrix == translate 0 (-5) identity
  | .error _ => false

#guard
  match processOp (.BT, []) mkProcessor >>=
      processOp (.Tm, [.number 1, .number 0, .number 0, .number 1, .number 5, .number 6]) with
  | .ok p => p.prState.gsTextMatrix == (⟨1, 0, 0, 1, 5, 6⟩ : Transform Float)
  | .error _ => false

-- ── `cm` ── the current transformation matrix

#guard match processOp (.cm, [.number 2, .number 0, .number 0, .number 2, .number 0, .number 0])
    mkProcessor with
  | .ok p => p.prState.gsCurrentTransformMatrix == scale 2 2
  | .error _ => false

-- ── `Tf`/`Tc`/`Tw` ── font, character and word spacing

#guard match processOp (.Tf, [.name (mkName "F1"), .number 12]) mkProcessor with
  | .ok p => p.prState.gsFont == some (mkName "F1") ∧ p.prState.gsFontSize == some 12
  | .error _ => false

#guard match processOp (.Tc, [.number 1]) mkProcessor with
  | .ok p => p.prState.gsTextCharSpacing == 1
  | .error _ => false

#guard match processOp (.Tw, [.number 2]) mkProcessor with
  | .ok p => p.prState.gsTextWordSpacing == 2
  | .error _ => false

-- ── `Tj`/`TJ`/`'`── showing text

#guard match processOp (.Tf, [.name (mkName "F1"), .number 1]) testProcessor >>= processOp (.Tj, [str "ab"]) with
  | .ok p => match p.prSpans with
    | [sp] => sp.spGlyphs.length == 2
    | _ => false
  | .error _ => false

-- `Tj` without a font set is rejected.
#guard match processOp (.Tj, [str "a"]) testProcessor with
  | .error _ => true
  | .ok _ => false

-- `TJ` with a string and a numeric adjustment yields one span (for the
-- string) and moves the text matrix by the adjustment.
#guard
  match processOp (.Tf, [.name (mkName "F1"), .number 1]) testProcessor >>=
      processOp (.TJ, [.array #[str "a", .number 500]]) with
  | .ok p => p.prSpans.length == 1
  | .error _ => false

-- `'` (apostrophe) moves to the next line (as `T*`) and shows the text (as
-- `Tj`), in that order.
#guard
  match processOp (.BT, []) testProcessor >>= processOp (.Tf, [.name (mkName "F1"), .number 1]) >>=
      processOp (.TL, [.number 5]) >>= processOp (.apostrophe, [str "a"]) with
  -- `'` moves down by the leading first (its text matrix's `f` component is
  -- `-5` before `Tj` further advances it horizontally), then shows the text.
  | .ok p => p.prState.gsTextMatrix.f == -5 ∧ p.prSpans.length == 1
  | .error _ => false

-- ── Unhandled operators leave the processor unchanged ──
-- (Neither `Processor` — it carries a function field, `prGlyphDecoder` —
-- nor `GraphicsState` derive `BEq`, so this compares the individual
-- primitive fields that could plausibly have changed instead.)

#guard match processOp (.f, []) mkProcessor with
  | .ok p => p.prState.gsInText == mkProcessor.prState.gsInText ∧
      p.prState.gsTextLeading == mkProcessor.prState.gsTextLeading ∧
      p.prStateStack.length == mkProcessor.prStateStack.length ∧
      p.prSpans.length == mkProcessor.prSpans.length
  | .error _ => false

end Tests.Data.PDF.Content.Processor
