/-
  `Linen.Text.Pandoc.Sources` — multi-source parser input.

  ## Haskell source

  Ported from `Text.Pandoc.Sources` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Sources.hs`).

  Provides the `Sources` type (a list of `(SourcePos, Text)` chunks that the
  parsec readers stream over), the `ToSources` class, and the pure helpers
  `sourcesToText`, `initialSourceName`, and `ensureFinalNewlines`.

  ### Deviations from upstream

  * `Text` → `String`.
  * Upstream additionally makes `Sources` a `parsec` `Stream` instance and
    exposes the stream-manipulating parser actions `addToSources`,
    `addToInput`, and re-implementations of the `Text.Parsec.Char` primitives
    (`satisfy`/`oneOf`/`char`/…) generalised over `UpdateSourcePos`. `linen`'s
    parsing layer (`Linen.Text.Pandoc.Parsing`) is built directly over Lean's
    `Std.Internal.Parsec` string parser rather than a bespoke `Sources`
    stream, so those stream/parser actions live there (or are scoped out with
    the parser's own deviation note); this module keeps the pure `Sources`
    data structure and its conversions, which is what `sourcesToText`/
    `toSources` callers need.
-/

import Linen.Text.Pandoc.Logging

namespace Linen.Text.Pandoc

/-- A parser input made of one or more text chunks, each tagged with the
    source position at which it begins. Text chunks are assumed to use `\n`
    line endings. -/
structure Sources where
  /-- The underlying list of position/text pairs. -/
  unSources : List (SourcePos × String)
  deriving Repr, BEq, Inhabited

namespace Sources

/-- Strip carriage returns from a string (readers assume `\n` endings). -/
def stripCR (s : String) : String := String.ofList (s.toList.filter (· != '\r'))

/-- The concatenation of all text chunks, discarding position info. -/
def sourcesToText (s : Sources) : String :=
  String.join (s.unSources.map (·.2))

/-- The source name (file) of the first chunk, or `""` if empty. -/
def initialSourceName (s : Sources) : String :=
  match s.unSources with
  | [] => ""
  | (pos, _) :: _ => pos.name

/-- Count how many trailing `\n` characters a string ends with. -/
private def trailingNewlines (s : String) : Nat :=
  (s.toList.reverse.takeWhile (· == '\n')).length

/-- Pad the last chunk so the whole input ends with at least `n` newlines. On
    empty input, produce a single chunk of `n` newlines at the initial
    position. -/
def ensureFinalNewlines (n : Nat) (s : Sources) : Sources :=
  match s.unSources.reverse with
  | [] => ⟨[({} , String.ofList (List.replicate n '\n'))]⟩
  | (pos, t) :: revInit =>
      let cnt := trailingNewlines t
      if cnt >= n then s
      else
        let t' := t ++ String.ofList (List.replicate (n - cnt) '\n')
        ⟨(revInit.reverse) ++ [(pos, t')]⟩

end Sources

instance : Append Sources where
  append a b := ⟨a.unSources ++ b.unSources⟩

/-- Things that can be converted to `Sources`. -/
class ToSources (α : Type) where
  /-- Convert to a `Sources`. -/
  toSources : α → Sources

/-- A single-chunk `Sources` from a string, stripping `\r`, at an unnamed
    initial position. -/
instance : ToSources String where
  toSources s := ⟨[({}, Sources.stripCR s)]⟩

/-- A multi-chunk `Sources`, one chunk per `(filename, text)` pair, each with
    a trailing `\n` appended and `\r` stripped. -/
instance : ToSources (List (String × String)) where
  toSources files := ⟨files.map fun (fp, t) =>
    ({ name := fp : SourcePos }, Sources.stripCR t ++ "\n")⟩

instance : ToSources Sources where
  toSources s := s

/-- `String` literals build a single-chunk `Sources`. -/
instance : Coe String Sources := ⟨ToSources.toSources⟩

end Linen.Text.Pandoc
