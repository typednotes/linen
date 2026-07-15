/-
  Tests for `Linen.Text.Pandoc.Definition`.

  Structural helpers and the `ToJSON`/`FromJSON` bridge are checked with
  `#guard`.  JSON round-trips compare via `Except.toOption` (the AST derives
  `BEq`, not `DecidableEq`).
-/
import Linen.Text.Pandoc.Definition

namespace Tests.Linen.Text.Pandoc.Definition

open _root_.Linen.Text.Pandoc
open Data.Json

-- ── Attribute / metadata helpers ──────────────────────────────────────

#guard nullAttr == ("", [], [])
#guard isNullMeta nullMeta == true

private def sampleMeta : Meta :=
  ⟨Data.Map.fromList [("title", MetaValue.MetaString "Hello"),
                      ("author", MetaValue.MetaInlines [.Str "Ada"])]⟩

#guard isNullMeta sampleMeta == false
#guard docTitle sampleMeta == [Inline.Str "Hello"]
#guard docAuthors sampleMeta == [[Inline.Str "Ada"]]
#guard (lookupMeta "title" sampleMeta).isSome == true
#guard (lookupMeta "missing" sampleMeta).isNone == true

-- Format equality is case-insensitive
#guard (Format.mk "HTML" == Format.mk "html") == true

-- ── JSON round-trips ──────────────────────────────────────────────────

private def roundtripInline (i : Inline) : Bool :=
  ((FromJSON.parseJSON (ToJSON.toJSON i) : Except String Inline).toOption) == some i

private def roundtripBlock (b : Block) : Bool :=
  ((FromJSON.parseJSON (ToJSON.toJSON b) : Except String Block).toOption) == some b

#guard roundtripInline (.Str "x")
#guard roundtripInline .Space
#guard roundtripInline (.Emph [.Str "a", .Space, .Str "b"])
#guard roundtripInline (.Quoted .DoubleQuote [.Str "q"])
#guard roundtripInline (.Link ("i", ["c"], [("k", "v")]) [.Str "l"] ("u", "t"))
#guard roundtripInline (.Note [.Para [.Str "n"]])
#guard roundtripInline (.Cite [.mk "id" [.Str "p"] [] .NormalCitation 0 0] [.Str "c"])
#guard roundtripInline (.Math .InlineMath "x^2")

#guard roundtripBlock (.Para [.Str "hi"])
#guard roundtripBlock .HorizontalRule
#guard roundtripBlock (.CodeBlock ("", ["lean"], []) "code")
#guard roundtripBlock (.BulletList [[.Plain [.Str "a"]], [.Plain [.Str "b"]]])
#guard roundtripBlock (.OrderedList (1, .Decimal, .Period) [[.Para [.Str "x"]]])
#guard roundtripBlock (.Header 2 nullAttr [.Str "h"])

-- A table round-trips (exercises Caption/Row/Cell/TableHead/…)
private def sampleTable : Block :=
  .Table nullAttr (.Caption none [])
    [(.AlignLeft, .ColWidthDefault)]
    (.TableHead nullAttr [.Row nullAttr [.Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "h"]]]])
    [.TableBody nullAttr 0 [] [.Row nullAttr [.Cell nullAttr .AlignDefault 1 1 [.Plain [.Str "d"]]]]]
    (.TableFoot nullAttr [])

#guard roundtripBlock sampleTable

-- Full document round-trip
private def sampleDoc : Pandoc := ⟨sampleMeta, [.Para [.Str "hi"], sampleTable]⟩

#guard ((FromJSON.parseJSON (ToJSON.toJSON sampleDoc) : Except String Pandoc).toOption) == some sampleDoc

-- The encoded document carries the API version tag
#guard pandocTypesVersion == [1, 23, 1]

end Tests.Linen.Text.Pandoc.Definition
