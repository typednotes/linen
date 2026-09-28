/-
  Tests for `Linen.Text.Pandoc.Generic`.

  `bottomUp`/`bottomUpM`/`queryWith` are total, so they use `#guard`.
  `topDown` is `unsafe` (see the module note), so it is exercised with
  `#guard_msgs in #eval`.
-/
import Linen.Text.Pandoc.Generic

namespace Tests.Linen.Text.Pandoc.Generic

open _root_.Linen.Text.Pandoc

private def doc1 : Pandoc :=
  ⟨nullMeta, [.Para [.Str "a", .Space, .Emph [.Str "a"]]]⟩

private def doc1b : Pandoc :=
  ⟨nullMeta, [.Para [.Str "b", .Space, .Emph [.Str "b"]]]⟩

private def relabel : Inline → Inline
  | .Str "a" => .Str "b"
  | x => x

#guard (bottomUp relabel doc1 : Pandoc) == doc1b
#guard Id.run (bottomUpM (m := Id) (fun i => pure (relabel i)) doc1) == doc1b

private def strings (i : Inline) : List String :=
  match i with | .Str s => [s] | _ => []

#guard (queryWith strings doc1 : List String) == ["a", "a"]

-- `topDown` (unsafe) reaches the same fixed point for this monotone rename
/-- info: true -/
#guard_msgs in #eval ((topDown relabel doc1 : Pandoc) == doc1b)

end Tests.Linen.Text.Pandoc.Generic
