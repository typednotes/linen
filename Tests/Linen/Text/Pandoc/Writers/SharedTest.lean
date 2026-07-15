/-
  Tests for `Linen.Text.Pandoc.Writers.Shared`.
-/
import Linen.Text.Pandoc.Writers.Shared

namespace Tests.Linen.Text.Pandoc.Writers.Shared

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.Writers.Shared
open _root_.Text.DocLayout (render literal empty Doc)

-- ── Template-context field helpers ────────────────────────────────────

#guard getField "a" [("a", "1"), ("b", "2")] == some "1"
#guard getField "z" ([] : Context String) == (none : Option String)
#guard defField "a" "9" [("a", "1")] == [("a", "1")]        -- kept
#guard defField "c" "3" [("a", "1")] == [("a", "1"), ("c", "3")]
#guard resetField "a" "9" [("a", "1")] == [("a", "9")]      -- overwritten
#guard setField "a" "9" [("a", "1")] == [("a", "9")]

-- template-free: metaToContext yields the empty context
#guard Id.run (metaToContext (m := Id) (α := String) ({} : WriterOptions)
    (fun _ => pure empty) (fun _ => pure empty) nullMeta) == ([] : Context String)

-- ── Metadata lookups ──────────────────────────────────────────────────

private def m1 : Meta := ⟨Data.Map.empty.insert' "t" (.MetaString "hi")⟩
private def m2 : Meta := ⟨Data.Map.empty.insert' "t" (.MetaInlines [.Str "a"])⟩

#guard lookupMetaString "t" m1 == "hi"
#guard lookupMetaString "missing" m1 == ""
#guard lookupMetaBool "t" m1 == true
#guard lookupMetaBool "missing" m1 == false
#guard lookupMetaInlines "t" m2 == [Inline.Str "a"]
#guard lookupMetaBlocks "t" m1 == [Block.Plain [.Str "hi"]]

-- getLang prefers the writer variable, else the metadata field
#guard getLang ({ writerVariables := [("lang", "en")] } : WriterOptions) nullMeta == some "en"
#guard getLang ({} : WriterOptions)
    ⟨Data.Map.empty.insert' "lang" (.MetaString "fr")⟩ == some "fr"
#guard getLang ({} : WriterOptions) nullMeta == (none : Option String)

-- ── HTML attribute helpers ────────────────────────────────────────────

#guard htmlAlignmentToString .AlignLeft == some "left"
#guard htmlAlignmentToString .AlignDefault == (none : Option String)
#guard formatKey "custom" == "data-custom"
#guard formatKey "aria-hidden" == "aria-hidden"
#guard formatKey "a:b" == "a:b"

/-- info: true -/
#guard_msgs in
#eval (render none (tagWithAttrs "div" ("i1", ["c"], [])) : String)
  == "<div id=\"i1\" class=\"c\">"

/-- info: true -/
#guard_msgs in
#eval (render none (htmlAttrs ("", [], [("k", "v")])) : String) == " data-k=\"v\""

-- htmlAddStyle inserts/updates a CSS declaration in the style attribute
#guard htmlAddStyle ("color", "red") [("style", "width: 1px;")]
  == [("style", "color: red; width: 1px;")]
#guard htmlAddStyle ("color", "red") []
  == [("style", "color: red;")]

-- ── Math helpers ──────────────────────────────────────────────────────

#guard isDisplayMath (.Math .DisplayMath "x") == true
#guard isDisplayMath (.Math .InlineMath "x") == false
#guard isDisplayMath (.Span nullAttr [.Math .DisplayMath "x"]) == true

-- a Para mixing display math and text becomes a math Div
#guard (match fixDisplayMath (.Para [.Str "a", .Math .DisplayMath "x"]) with
        | .Div (_, cls, _) _ => cls == ["math"]
        | _ => false)
-- a plain paragraph is left untouched
#guard fixDisplayMath (.Para [.Str "a"]) == Block.Para [.Str "a"]

-- ── Typographic helpers ───────────────────────────────────────────────

#guard unsmartify ({} : WriterOptions) "“hi”" == "\"hi\""
#guard unsmartify ({} : WriterOptions) "a–b" == "a--b"          -- en dash → --
#guard unsmartify ({} : WriterOptions) "a—b" == "a---b"         -- em dash → ---

#guard toSuperscript '2' == some '²'
#guard toSuperscript '0' == some '⁰'
#guard toSuperscript 'q' == (none : Option Char)
#guard toSubscript '2' == some '₂'
#guard toSuperscriptInline [.Str "12"] == some [Inline.Str "¹²"]
#guard toSuperscriptInline [.Str "ab"] == (none : Option (List Inline))

-- ── Block predicates ──────────────────────────────────────────────────

#guard endsWithPlain [.Para [], .Plain []] == true
#guard endsWithPlain [.Para []] == false
#guard endsWithPlain [.BulletList [[.Plain []]]] == true
#guard endsWithPlain ([] : List Block) == false

-- ── removeLinks ───────────────────────────────────────────────────────

#guard removeLinks [.Link nullAttr [.Str "x"] ("u", "")]
  == [Inline.Span nullAttr [.Str "x"]]

-- ── toLegacyTable ─────────────────────────────────────────────────────

private def hcell (s : String) : Cell := .Cell nullAttr .AlignDefault 1 1 [.Plain [.Str s]]

private def specs2 : List ColSpec :=
  [(.AlignDefault, .ColWidthDefault), (.AlignRight, .ColWidthDefault)]

private def hd2 : TableHead := .TableHead nullAttr [.Row nullAttr [hcell "H1", hcell "H2"]]
private def body2 : TableBody :=
  .TableBody nullAttr 0 [] [.Row nullAttr [hcell "a", hcell "b"]]

private def legacy := toLegacyTable emptyCaption specs2 hd2 [body2] (.TableFoot nullAttr [])

#guard legacy.2.1 == [Alignment.AlignDefault, Alignment.AlignRight]       -- aligns
#guard legacy.2.2.2.1 == [[Block.Plain [.Str "H1"]], [Block.Plain [.Str "H2"]]]  -- header cells
#guard legacy.2.2.2.2 == [[[Block.Plain [.Str "a"]], [Block.Plain [.Str "b"]]]]  -- body rows

-- ── gridTable ─────────────────────────────────────────────────────────

private unsafe def cellDoc (_ : WriterOptions) (bs : List Block) : Id (Doc String) :=
  pure (literal (_root_.Linen.Text.Pandoc.Shared.stringify bs))

-- the rendered grid table contains the border/bar glyphs and header rule
/-- info: true -/
#guard_msgs in
#eval (let s := render (some 40)
        (Id.run (gridTable ({} : WriterOptions) cellDoc emptyCaption specs2 hd2 [body2]
          (.TableFoot nullAttr [])));
       s.contains '+' && s.contains '|' && s.contains '=')

end Tests.Linen.Text.Pandoc.Writers.Shared
