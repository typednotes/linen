/-
  `Linen.Text.Pandoc.Translations` — localized UI terms.

  ## Haskell source

  Ported from `Text.Pandoc.Translations` and `Text.Pandoc.Translations.Types`
  in the `pandoc` package (v3.10, `src/Text/Pandoc/Translations.hs`,
  `src/Text/Pandoc/Translations/Types.hs`).

  Provides the `Term` enumeration of translatable UI terms, the `Translations`
  map from terms to their localized strings, `lookupTerm`, `readTranslations`,
  and a minimal `Lang` (language tag) type.

  ### Deviations from upstream

  * `Text` → `String`. `Term`'s `Ord` (needed to key a `Data.Map`) is derived
    from an explicit index (`Term.toNat`).
  * `Lang` comes from `unicode-collation`'s `Text.Collate.Lang` upstream (a
    deferred dependency); a minimal record with the fields pandoc actually
    uses (`langLanguage`, `langScript`, `langRegion`) plus `renderLang` is
    provided here.
  * `readTranslations` parses a **bounded YAML subset** — the flat
    `Term: value` mapping that pandoc's bundled `translations/*.yaml` files
    are — over a small line parser, rather than a full YAML engine (the `yaml`
    dependency is deferred per `docs/imports/pandoc/dependencies.md`).
  * The `PandocMonad`-using helpers `getTranslations`/`setTranslations`/
    `translateTerm` depend on the (later) `Class.PandocMonad` layer and on
    data-file loading; they live in `Linen.Text.Pandoc.Class.PandocMonad`
    (`setTranslations`/`translateTerm`), keeping this module free of the monad
    dependency so it can precede `CommonState`.
-/

import Linen.Data.Map

namespace Linen.Text.Pandoc

open Data (Map)

/-- A translatable UI term. -/
inductive Term where
  | Abstract | Appendix | Bibliography | Cc | Chapter | Contents | Encl
  | Figure | Glossary | Index | Listing | ListOfFigures | ListOfTables
  | Page | Part | Preface | Proof | References | See | SeeAlso | Table | To
  deriving Repr, DecidableEq, Inhabited

namespace Term

/-- An index for each term, giving a total order (for use as a map key) and
    round-tripping to/from the constructor name. -/
def toNat : Term → Nat
  | Abstract => 0 | Appendix => 1 | Bibliography => 2 | Cc => 3 | Chapter => 4
  | Contents => 5 | Encl => 6 | Figure => 7 | Glossary => 8 | Index => 9
  | Listing => 10 | ListOfFigures => 11 | ListOfTables => 12 | Page => 13
  | Part => 14 | Preface => 15 | Proof => 16 | References => 17 | See => 18
  | SeeAlso => 19 | Table => 20 | To => 21

/-- The constructor name of a term (matching upstream's derived `Show`). -/
def name : Term → String
  | Abstract => "Abstract" | Appendix => "Appendix" | Bibliography => "Bibliography"
  | Cc => "Cc" | Chapter => "Chapter" | Contents => "Contents" | Encl => "Encl"
  | Figure => "Figure" | Glossary => "Glossary" | Index => "Index"
  | Listing => "Listing" | ListOfFigures => "ListOfFigures"
  | ListOfTables => "ListOfTables" | Page => "Page" | Part => "Part"
  | Preface => "Preface" | Proof => "Proof" | References => "References"
  | See => "See" | SeeAlso => "SeeAlso" | Table => "Table" | To => "To"

/-- All terms, in index order. -/
def all : List Term :=
  [ Abstract, Appendix, Bibliography, Cc, Chapter, Contents, Encl, Figure,
    Glossary, Index, Listing, ListOfFigures, ListOfTables, Page, Part,
    Preface, Proof, References, See, SeeAlso, Table, To ]

/-- Parse a term from its constructor name. -/
def ofName? (s : String) : Option Term := all.find? (fun t => t.name == s)

end Term

instance : Ord Term where compare a b := compare a.toNat b.toNat
instance : ToString Term := ⟨Term.name⟩

/-- A map from terms to their localized strings. -/
structure Translations where
  /-- The underlying term → text map. -/
  unTranslations : Map Term String
  deriving Inhabited

namespace Translations

/-- The empty translation table. -/
def empty : Translations := ⟨Data.Map.empty⟩

/-- Look up the translation for a term, if present. -/
def lookupTerm (t : Term) (tr : Translations) : Option String :=
  tr.unTranslations.lookup t

/-- Build translations from an association list of `(Term, text)`. -/
def fromList (l : List (Term × String)) : Translations := ⟨Data.Map.fromList l⟩

end Translations

instance : EmptyCollection Translations := ⟨Translations.empty⟩

/-- `Translations` combine by left-biased union. -/
instance : Append Translations where
  append a b := ⟨Data.Map.union b.unTranslations a.unTranslations⟩

/-- Parse the flat `Term: value` YAML subset pandoc's translation files use.
    Blank lines and `#` comments are ignored; each other line must be
    `Key: value` with `Key` a valid `Term` name, otherwise it is skipped. -/
def readTranslations (input : String) : Except String Translations := do
  let stripComment (l : String) : String :=
    (l.takeWhile (· != '#')).toString
  let entries := (input.splitOn "\n").filterMap fun rawLine =>
    let line := (stripComment rawLine).trimAscii.toString
    if line == "" then none
    else match line.splitOn ":" with
      | key :: rest =>
          match Term.ofName? key.trimAscii.toString with
          | some t =>
              let val := (":".intercalate rest).trimAscii.toString
              -- strip optional surrounding quotes
              let vchars := val.toList
              let quoted := vchars.length >= 2 &&
                ((val.startsWith "\"" && val.endsWith "\"") || (val.startsWith "'" && val.endsWith "'"))
              let val := if quoted then String.ofList (vchars.drop 1).dropLast else val
              some (t, val)
          | none => none
      | [] => none
  .ok (Translations.fromList entries)

-- ── Language tags ─────────────────────────────────────────────────────

/-- A BCP 47 language tag (minimal port of `Text.Collate.Lang`). -/
structure Lang where
  /-- The primary language subtag (e.g. `"en"`). -/
  langLanguage : String := ""
  /-- The script subtag, if any (e.g. `"Latn"`). -/
  langScript : Option String := none
  /-- The region subtag, if any (e.g. `"US"`). -/
  langRegion : Option String := none
  /-- Any variant subtags. -/
  langVariants : List String := []
  deriving Repr, BEq, Inhabited

/-- Render a `Lang` back to a hyphen-joined BCP 47 tag. -/
def renderLang (l : Lang) : String :=
  let parts := [l.langLanguage]
    ++ (l.langScript.map (fun s => [s])).getD []
    ++ (l.langRegion.map (fun s => [s])).getD []
    ++ l.langVariants
  "-".intercalate (parts.filter (· != ""))

/-- Parse a BCP 47 tag into a `Lang` (best-effort: language, optional
    4-letter script, optional 2-letter/3-digit region, then variants). -/
def parseLang (s : String) : Except String Lang :=
  match (s.splitOn "-").filter (· != "") with
  | [] => .error "empty language tag"
  | lang :: rest =>
      let isScript (x : String) := x.length == 4
      let isRegion (x : String) := x.length == 2 || x.length == 3
      let (script, rest₁) := match rest with
        | x :: xs => if isScript x then (some x, xs) else (none, rest)
        | [] => (none, [])
      let (region, rest₂) := match rest₁ with
        | x :: xs => if isRegion x then (some x, xs) else (none, rest₁)
        | [] => (none, [])
      .ok { langLanguage := lang.toLower, langScript := script,
            langRegion := region.map String.toUpper, langVariants := rest₂ }

end Linen.Text.Pandoc
