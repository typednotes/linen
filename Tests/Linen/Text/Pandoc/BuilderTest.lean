/-
  Tests for `Linen.Text.Pandoc.Builder`: the monoidal `Inlines`/`Blocks` DSL.
-/
import Linen.Text.Pandoc.Builder

namespace Tests.Linen.Text.Pandoc.Builder

open _root_.Linen.Text.Pandoc

-- ── Inline builders and the smart `Inlines` monoid ────────────────────

#guard (str "hi").toList == [Inline.Str "hi"]
#guard (emph (str "x")).toList == [Inline.Emph [.Str "x"]]
#guard (code "f").toList == [Inline.Code nullAttr "f"]

-- adjacent `Str`s / spaces are melded on append
#guard (str "a" ++ str "b").toList == [Inline.Str "ab"]
#guard (space ++ space).toList == [Inline.Space]
#guard (emph (str "x") ++ emph (str "y")).toList == [Inline.Emph [.Str "x", .Str "y"]]
#guard (str "a" ++ str "b" ++ str "c").toList == [Inline.Str "abc"]

-- `text` splits interword spaces / newlines
#guard (text "a b").toList == [Inline.Str "a", .Space, .Str "b"]
#guard (text "a\nb").toList == [Inline.Str "a", .SoftBreak, .Str "b"]
#guard (text "abc").toList == [Inline.Str "abc"]

-- `trimInlines` drops leading/trailing space & softbreak
#guard (trimInlines (space ++ str "x" ++ space)).toList == [Inline.Str "x"]

-- ── Block builders ────────────────────────────────────────────────────

#guard (para (str "hi")).toList == [Block.Para [.Str "hi"]]
#guard (plain (Many.fromList [])).toList == ([] : List Block)   -- empty plain vanishes
#guard (bulletList [para (str "a"), para (str "b")]).toList ==
  [Block.BulletList [[.Para [.Str "a"]], [.Para [.Str "b"]]]]
#guard (header 1 (str "T")).toList == [Block.Header 1 nullAttr [.Str "T"]]

-- ── Documents and metadata ────────────────────────────────────────────

#guard doc (para (str "hi")) == ⟨nullMeta, [.Para [.Str "hi"]]⟩
#guard docTitle (setTitle (str "My Title") (doc (para (str "b")))).docMeta == [Inline.Str "My Title"]
#guard isNullMeta (HasMeta.deleteMeta "title" (setTitle (str "x") (doc (Many.fromList [])))).docMeta == true

-- ── Table normalisation ───────────────────────────────────────────────

-- a 2-column simple table has one header row and one body row of two cells
private def st : Blocks := simpleTable [plain (str "H1"), plain (str "H2")]
                                       [[plain (str "a"), plain (str "b")]]

private def tableColumns : Block → Nat
  | .Table _ _ specs _ _ _ => specs.length
  | _ => 0

#guard (st.toList.headD .HorizontalRule |> tableColumns) == 2

end Tests.Linen.Text.Pandoc.Builder
