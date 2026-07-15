/-
  `Linen.Text.DocLayout.HasChars` — a class of string-like types.

  ## Haskell source

  Ported from `Text.DocLayout.HasChars` in the `doclayout` package
  (v0.5.0.3, `src/Text/DocLayout/HasChars.hs`).

  `HasChars` abstracts over string-like types that can be folded over their
  characters, tested for emptiness, replicated, split into lines, and turned
  into an output builder.  Upstream provides instances for strict `Text`,
  `String`, lazy `Text`, and the sibling `Attr`/`Attributed` types; since Lean
  has a single `String` type the three text instances collapse into one
  `HasChars String`.

  Upstream's `build :: a -> Data.Text.Lazy.Builder.Builder` becomes
  `build : a → String` (Lean has no lazy-`Text` builder; a `String` is the
  natural accumulator).  The superclass constraints `(IsString a, Semigroup a,
  Monoid a, Show a)` are expressed with Lean's `IsString`/`Append`/`Inhabited`
  (there is no core `Monoid`/`Semigroup`/`Show`; `Append`+`Inhabited` play the
  monoid role, matching `Attributed`).
-/

import Linen.Text.DocLayout.Attributed
import Linen.Data.String

namespace Text.DocLayout

/- ── The `HasChars` class ─────────────────────────────────────────── -/

/-- A type whose values are made of characters: foldable over `Char`,
testable for emptiness, replicable, splittable into lines, and renderable to
an output `String`.  Corresponds to `Text.DocLayout.HasChars`. -/
class HasChars (a : Type) [IsString a] [Append a] [Inhabited a] where
  /-- Right fold over the characters. -/
  foldrChar : {β : Type} → (Char → β → β) → β → a → β
  /-- Left fold over the characters. -/
  foldlChar : {β : Type} → (β → Char → β) → β → a → β
  /-- `replicateChar n c` is `c` repeated `n` times. -/
  replicateChar : Nat → Char → a
  /-- Is the value empty (contains no characters)? -/
  isNull : a → Bool
  /-- Split on newline characters (`'\n'`). -/
  splitLines : a → List a
  /-- Render to an output `String` (upstream: a lazy-`Text` builder). -/
  build : a → String

/- ── `String` instance ────────────────────────────────────────────── -/

/-- Strict/lazy `Text` and `String` upstream instances all collapse to this
single `HasChars String`. -/
instance : HasChars String where
  foldrChar f z s := s.toList.foldr f z
  foldlChar f z s := s.toList.foldl f z
  replicateChar n c := String.ofList (List.replicate n c)
  isNull s := s.isEmpty
  -- `lines . (++ "\n")` upstream; `splitOn "\n"` is the exact Lean equivalent.
  splitLines s := s.splitOn "\n"
  build s := s

/- ── `Attr` instance ──────────────────────────────────────────────── -/

/-- All methods delegate to the wrapped payload; `splitLines` re-attaches the
chunk's link/font to each resulting line. -/
instance [IsString a] [Append a] [Inhabited a] [HasChars a] : HasChars (Attr a) where
  foldrChar f z x := HasChars.foldrChar f z x.value
  foldlChar f z x := HasChars.foldlChar f z x.value
  replicateChar n c := IsString.fromString (String.ofList (List.replicate n c))
  isNull x := HasChars.foldrChar (fun _ _ => false) true x.value
  splitLines x := (HasChars.splitLines x.value).map (fun v => ⟨x.link, x.font, v⟩)
  build x := HasChars.build x.value

/- ── `Attributed` instance ────────────────────────────────────────── -/

/-- Worker for `Attributed.splitLines`.  Threads a reversed list of completed
lines `lns` and the current in-progress line `cur` (both as lists of `Attr`
chunks) across the run of chunks, splitting each chunk and merging boundary
lines with adjacent chunks.  Mirrors the `go` where-clause upstream (structural
recursion on the chunk list — no `partial`). -/
private def splitLinesGo [IsString a] [Append a] [Inhabited a] [HasChars a]
    (lns : List (List (Attr a))) (cur : List (Attr a)) :
    List (Attr a) → List (List (Attr a))
  | [] => cur :: lns
  | x :: xs =>
    match HasChars.splitLines x with
    | [] => splitLinesGo (cur :: lns) [] xs
    | [k1] => splitLinesGo lns (cur ++ [k1]) xs
    | k1 :: ks =>
      -- `ks` is non-empty here.  `end` = last piece (new current line),
      -- `most` = the middle pieces, each its own completed line.
      match (ks.map (fun k => [k])).reverse with
      | [] => splitLinesGo lns (cur ++ [k1]) xs        -- unreachable
      | endSeg :: most => splitLinesGo (most ++ (cur ++ [k1]) :: lns) endSeg xs

/-- Folds/splits over the concatenated characters of every chunk, preserving
attributes across `splitLines`. -/
instance [IsString a] [Append a] [Inhabited a] [HasChars a] : HasChars (Attributed a) where
  foldrChar f z x := x.chunks.foldr (fun c acc => HasChars.foldrChar f acc c.value) z
  foldlChar f z x := x.chunks.foldl (fun acc c => HasChars.foldlChar f acc c.value) z
  replicateChar n c := IsString.fromString (String.ofList (List.replicate n c))
  isNull x := x.chunks.all (fun c => HasChars.isNull c.value)
  splitLines x := (splitLinesGo [] [] x.chunks).reverse.map (fun seg => (⟨seg⟩ : Attributed a))
  build x := String.join (x.chunks.map (fun c => HasChars.build c.value))

end Text.DocLayout
