/-
  Tests for `Linen.Text.Pandoc.Walk`: bottom-up `walk`/`walkM` and `query`.
-/
import Linen.Text.Pandoc.Walk

namespace Tests.Linen.Text.Pandoc.Walk

open _root_.Linen.Text.Pandoc

private def doc1 : Pandoc :=
  ⟨nullMeta, [.Para [.Str "a", .Space, .Emph [.Str "a"]], .BlockQuote [.Plain [.Str "a"]]]⟩

private def doc1b : Pandoc :=
  ⟨nullMeta, [.Para [.Str "b", .Space, .Emph [.Str "b"]], .BlockQuote [.Plain [.Str "b"]]]⟩

private def relabel : Inline → Inline
  | .Str "a" => .Str "b"
  | x => x

-- `walk` replaces every matching inline throughout the structure
#guard (walk relabel doc1 : Pandoc) == doc1b

-- `walkM` in the identity monad agrees with `walk`
#guard Id.run (walkM (m := Id) (fun i => pure (relabel i)) doc1) == doc1b

-- `query` collects a contribution from every inline (pre-order)
private def strings (i : Inline) : List String :=
  match i with | .Str s => [s] | _ => []

#guard (query strings doc1 : List String) == ["a", "a", "a"]

-- Query over a block: count blocks
private def blockTag (b : Block) : List String :=
  match b with | .Para _ => ["para"] | .BlockQuote _ => ["quote"] | .Plain _ => ["plain"] | _ => []

#guard (query blockTag doc1 : List String) == ["para", "quote", "plain"]

-- Walk over metadata inlines
private def metaDoc : Pandoc :=
  ⟨⟨Data.Map.fromList [("title", MetaValue.MetaInlines [.Str "a"])]⟩, []⟩

#guard (query strings metaDoc : List String) == ["a"]
#guard docTitle (walk relabel metaDoc : Pandoc).docMeta == [Inline.Str "b"]

end Tests.Linen.Text.Pandoc.Walk
