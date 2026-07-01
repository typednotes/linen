/-
  `Options.Applicative.Types` — Haskell `optparse-applicative`'s core types

  Ports the foundational types of Haskell's `optparse-applicative`
  (`Options.Applicative.Types`): readers, modifiers, option specifications,
  and a composable parser type for command-line arguments.

  ## Design note

  Haskell's `optparse-applicative` represents `Parser` as a free applicative
  functor GADT (`ap : Parser (β → α) → Parser β → Parser α`). Lean 4's strict
  positivity checker rejects that shape as an inductive constructor, so
  `Parser` is instead a **functional** representation: a function from the
  remaining argument list to a parsed value and the arguments left over.
  Option metadata for help-text generation is tracked separately in a
  `descrs` field rather than embedded in the GADT.

  https://hackage.haskell.org/package/optparse-applicative
-/

namespace Options.Applicative

-- ── Readers ──────────────────────────────────────────

/-- A reader that parses a string value into a typed result.
    $$\text{ReadM}\ \alpha := \text{String} \to \text{Except String}\ \alpha$$ -/
abbrev ReadM (α : Type) := String → Except String α

instance : Inhabited (ReadM α) where
  default := fun _ => .error "no reader"

-- ── Modifiers ──────────────────────────────────────────

/-- Option visibility and characteristics: long/short names, help text,
    metavar, and visibility flags. -/
structure Mod where
  /-- Long option name (e.g. `"output"` for `--output`). -/
  long : Option String := none
  /-- Short option character (e.g. `'o'` for `-o`). -/
  short : Option Char := none
  /-- Help text displayed in usage. -/
  help : Option String := none
  /-- Metavar displayed in usage (e.g. `FILE` in `--output FILE`). -/
  metavar : Option String := none
  /-- Whether this option is hidden from help. -/
  hidden : Bool := false
  /-- Whether to show the default value in help text. -/
  showDefault : Bool := false
  deriving Inhabited, Repr

/-- Combine two `Mod` values, with the right-hand side taking precedence for
    fields it sets.
    $$\text{Mod} \text{ is a monoid under } (\text{++}), \text{right-biased} $$ -/
instance : Append Mod where
  append a b := {
    long := b.long <|> a.long
    short := b.short <|> a.short
    help := b.help <|> a.help
    metavar := b.metavar <|> a.metavar
    hidden := a.hidden || b.hidden
    showDefault := a.showDefault || b.showDefault
  }

/-- Modifier configuration for `ParserInfo`: description, header, footer,
    and failure behaviour. -/
structure InfoMod where
  /-- Program description. -/
  description : Option String := none
  /-- Header text shown before usage. -/
  header : Option String := none
  /-- Footer text shown after options. -/
  footer : Option String := none
  /-- Whether to show the full description in help. -/
  fullDesc : Bool := true
  /-- Exit code on parse failure. -/
  failureCode : Nat := 1
  deriving Inhabited, Repr

-- ── Option descriptions ──────────────────────────────────────────

/-- Description of a single option, used for generating help text. Kept
    separate from the parsing logic so that `Parser` can stay a plain
    function type. -/
inductive OptDescr where
  /-- A named option (`--long` / `-s`). -/
  | optionDescr (mods : Mod)
  /-- A flag (`--long` / `-s`, no value). -/
  | flagDescr (mods : Mod)
  /-- A positional argument. -/
  | argDescr (mods : Mod)
  /-- A subcommand group. -/
  | cmdDescr (commands : List (String × Option String))
  deriving Inhabited, Repr

-- ── Parser ──────────────────────────────────────────

/-- A composable command-line parser: a function from the remaining argument
    list to either an error or a parsed value paired with the arguments left
    over. Option descriptions are tracked separately for help generation.
    $$\text{Parser}\ \alpha := \{\, \text{run} : \text{List String} \to
    \text{Except String}\ (\alpha \times \text{List String}),\
    \text{descrs} : \text{List OptDescr} \,\}$$ -/
structure Parser (α : Type) where
  /-- Run the parser on an argument list, returning the result and the
      remaining arguments. -/
  run : List String → Except String (α × List String)
  /-- Option descriptions for help-text generation. -/
  descrs : List OptDescr := []

/-- A `Parser` together with metadata for help-text generation.
    $$\text{ParserInfo}\ \alpha := (\text{Parser}\ \alpha, \text{description},
    \text{header}, \text{footer}, \ldots)$$ -/
structure ParserInfo (α : Type) where
  /-- The underlying parser. -/
  parser : Parser α
  /-- Program description. -/
  description : Option String := none
  /-- Header text shown before usage. -/
  header : Option String := none
  /-- Footer text shown after options. -/
  footer : Option String := none
  /-- Whether to show the full description. -/
  fullDesc : Bool := true
  /-- Exit code on parse failure. -/
  failureCode : Nat := 1

end Options.Applicative
