/-
  Linen.Data.String — Haskell's `Data.String` API

  Adds what Lean core's `String` lacks:

  * `IsString` — overloaded string literals (no core equivalent).
  * `String.words` / `String.unwords` / `String.unlines` — whitespace/line
    tokenising and joining, built on core `String.intercalate`.

  **Not re-declared** (already in core): `lines` is `s.splitOn "\n"` — or core
  `String.lines`, which additionally strips `\r` and follows Haskell's
  trailing-newline rule (returning slices).
-/

/-- Overloaded string literals: a type `α` with an `IsString` instance can be
    built from a `String` (Haskell's `Data.String.IsString`; Lean core has no
    equivalent).
    $$\text{fromString} : \text{String} \to \alpha$$ -/
class IsString (α : Type u) where
  /-- Convert a `String` to the target type. -/
  fromString : String → α

instance : IsString String where
  fromString := id

namespace String

/-! ── Tokenising ── -/

/-- Split a string into words on runs of whitespace, dropping empty fields.
    $$\text{words}(\text{"hello   world"}) = [\text{"hello"}, \text{"world"}]$$ -/
def words (s : String) : List String :=
  let rec go (acc : List Char) (rest : List Char) (result : List String) : List String :=
    match rest with
    | [] => (if acc.isEmpty then result else String.ofList acc.reverse :: result).reverse
    | c :: cs =>
      if c.isWhitespace then
        if acc.isEmpty then go [] cs result
        else go [] cs (String.ofList acc.reverse :: result)
      else go (c :: acc) cs result
  go [] s.toList []

/-! ── Joining ── -/

/-- Join lines with `'\n'`, adding a trailing newline when non-empty
    (Haskell `unlines`).
    $$\text{unlines}([\text{"a"}, \text{"b"}]) = \text{"a\\nb\\n"}$$ -/
def unlines (ls : List String) : String :=
  String.intercalate "\n" ls ++ if ls.isEmpty then "" else "\n"

/-- Join words with a single space (Haskell `unwords`).
    $$\text{unwords}([\text{"hello"}, \text{"world"}]) = \text{"hello world"}$$ -/
def unwords (ws : List String) : String :=
  String.intercalate " " ws

/-! ── Laws ── -/

/-- `unwords` of the empty list is empty. -/
theorem unwords_nil : unwords [] = "" := rfl

/-- `unlines` of the empty list is empty. -/
theorem unlines_nil : unlines [] = "" := rfl

end String
