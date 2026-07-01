/-
  `Options.Applicative.Builder` — builder combinators for `Options.Applicative`

  A fluent API for constructing command-line parsers from individual option
  specifications, on top of `Options.Applicative.Types`. Mirrors Haskell's
  `Options.Applicative.Builder`:
  https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Builder.html

  $$\text{strOption} : \text{Mod} \to \text{Parser String}$$
  $$\text{option} : \text{ReadM}\ \alpha \to \text{Mod} \to \text{Parser}\ \alpha$$
-/
import Linen.Options.Applicative.Types

namespace Options.Applicative

-- ── Modifier constructors ──────────────────────────────────────────

/-- Set the long option name (e.g. `"output"` for `--output`).
    $$\text{long} : \text{String} \to \text{Mod}$$ -/
def long (name : String) : Mod :=
  { long := some name }

/-- Set the short option character (e.g. `'o'` for `-o`).
    $$\text{short} : \text{Char} \to \text{Mod}$$ -/
def short (c : Char) : Mod :=
  { short := some c }

/-- Set the help text for an option.
    $$\text{help} : \text{String} \to \text{Mod}$$ -/
def help (text : String) : Mod :=
  { help := some text }

/-- Set the metavar for an option (displayed in usage, e.g. `FILE`).
    $$\text{metavar} : \text{String} \to \text{Mod}$$ -/
def metavar (name : String) : Mod :=
  { metavar := some name }

/-- Mark an option as hidden from help output.
    $$\text{hidden} : \text{Mod}$$ -/
def hidden : Mod :=
  { hidden := true }

/-- Mark that the default value should be shown in help.
    $$\text{showDefault} : \text{Mod}$$ -/
def showDefault : Mod :=
  { showDefault := true }

-- ── Readers ──────────────────────────────────────────

/-- Identity reader that returns the string as-is.
    $$\text{str} : \text{ReadM String}$$ -/
def str : ReadM String :=
  fun s => .ok s

/-- Construct a reader from an `Except`-returning function.
    $$\text{eitherReader} : (\text{String} \to \text{Except String}\ \alpha) \to \text{ReadM}\ \alpha$$ -/
def eitherReader (f : String → Except String α) : ReadM α :=
  f

/-- Types that can be parsed from a string representation, used by the
    `auto` reader. -/
class FromString (α : Type) where
  /-- Parse a string into a value, or return `none` on failure. -/
  fromString? : String → Option α

instance : FromString String where
  fromString? s := some s

instance : FromString Nat where
  fromString? s := s.toNat?

instance : FromString Int where
  fromString? s := s.toInt?

instance : FromString Bool where
  fromString? s :=
    let s' := s.trimAscii.toString.toLower
    if s' == "true" || s' == "yes" || s' == "1" then some true
    else if s' == "false" || s' == "no" || s' == "0" then some false
    else none

/-- Automatic reader using `FromString` and `ToString` instances.
    $$\text{auto} : [\text{ToString}\ \alpha] \to [\text{FromString}\ \alpha] \to \text{ReadM}\ \alpha$$ -/
def auto [ToString α] [FromString α] : ReadM α :=
  fun s =>
    match FromString.fromString? s with
    | some a => .ok a
    | none => .error s!"cannot parse value '{s}'"

-- ── Internal argument matching helpers ──────────────────────────────────────────

/-- Try to consume a named option (`--long value`, `--long=value`, `-s value`)
    from the argument list. Returns the value and remaining args on success. -/
private def matchOption (mods : Mod) (args : List String) : Option (String × List String) :=
  match args with
  | [] => none
  | arg :: rest =>
    match mods.long with
    | some l =>
      if arg == s!"--{l}" then
        match rest with
        | val :: rest' => some (val, rest')
        | [] => none
      else if arg.startsWith s!"--{l}=" then
        some ((arg.drop (l.length + 3)).toString, rest)
      else
        match mods.short with
        | some c =>
          if arg == s!"-{c}" then
            match rest with
            | val :: rest' => some (val, rest')
            | [] => none
          else none
        | none => none
    | none =>
      match mods.short with
      | some c =>
        if arg == s!"-{c}" then
          match rest with
          | val :: rest' => some (val, rest')
          | [] => none
        else none
      | none => none

/-- Try to match a flag (no value) from the argument list. -/
private def matchFlag (mods : Mod) (args : List String) : Option (List String) :=
  match args with
  | [] => none
  | arg :: rest =>
    let matchLong := match mods.long with
      | some l => arg == s!"--{l}"
      | none => false
    let matchShort := match mods.short with
      | some c => arg == s!"-{c}"
      | none => false
    if matchLong || matchShort then some rest
    else none

/-- Scan through args looking for a named option, consuming it from wherever
    it appears (not just the head). Returns `(value, remaining args)`. -/
private def scanOption (mods : Mod) : List String → Option (String × List String)
  | [] => none
  | arg :: rest =>
    match matchOption mods (arg :: rest) with
    | some result => some result
    | none =>
      match scanOption mods rest with
      | some (val, rest') => some (val, arg :: rest')
      | none => none

/-- Scan through args looking for a flag, consuming it from wherever it
    appears. Returns the remaining args. -/
private def scanFlag (mods : Mod) : List String → Option (List String)
  | [] => none
  | arg :: rest =>
    match matchFlag mods (arg :: rest) with
    | some rest' => some rest'
    | none =>
      match scanFlag mods rest with
      | some rest' => some (arg :: rest')
      | none => none

/-- Name of an option for error messages. -/
private def optName (mods : Mod) : String :=
  mods.long.map (s!"--{·}") |>.getD
    (mods.short.map (s!"-{·}") |>.getD "option")

-- ── Option builders ──────────────────────────────────────────

/-- Build a typed option using the given reader. The option can appear
    anywhere in the argument list (scanned).
    $$\text{option} : \text{ReadM}\ \alpha \to \text{Mod} \to \text{Parser}\ \alpha$$ -/
def option (reader : ReadM α) (mods : Mod) : Parser α :=
  { run := fun args =>
      match scanOption mods args with
      | some (val, rest) =>
        match reader val with
        | .ok v => .ok (v, rest)
        | .error e => .error s!"Invalid value for {optName mods}: {e}"
      | none => .error s!"Missing required option: {optName mods}"
    descrs := [.optionDescr mods] }

/-- Build a string-valued option.
    $$\text{strOption} : \text{Mod} \to \text{Parser String}$$ -/
def strOption (mods : Mod) : Parser String :=
  option str mods

/-- Build a boolean switch (defaults to `false`, active when present).
    $$\text{switch} : \text{Mod} \to \text{Parser Bool}$$ -/
def switch (mods : Mod) : Parser Bool :=
  { run := fun args =>
      match scanFlag mods args with
      | some rest => .ok (true, rest)
      | none => .ok (false, args)
    descrs := [.flagDescr mods] }

/-- Build a flag with explicit inactive/active values.
    $$\text{flag} : \alpha \to \alpha \to \text{Mod} \to \text{Parser}\ \alpha$$ -/
def flag (inactive active : α) (mods : Mod) : Parser α :=
  { run := fun args =>
      match scanFlag mods args with
      | some rest => .ok (active, rest)
      | none => .ok (inactive, args)
    descrs := [.flagDescr mods] }

/-- Build a flag with no default (must be explicitly provided, or combined
    with an alternative via `<|>`).
    $$\text{flag'} : \alpha \to \text{Mod} \to \text{Parser}\ \alpha$$ -/
def flag' (active : α) (mods : Mod) : Parser α :=
  { run := fun args =>
      match scanFlag mods args with
      | some rest => .ok (active, rest)
      | none => .error s!"Flag not found: {optName mods}"
    descrs := [.flagDescr mods] }

/-- Build a positional argument parser. Consumes the first non-flag argument.
    $$\text{argument} : \text{ReadM}\ \alpha \to \text{Mod} \to \text{Parser}\ \alpha$$ -/
def argument (reader : ReadM α) (mods : Mod) : Parser α :=
  { run := fun args =>
      let rec findArg (before : List String) : List String → Except String (α × List String)
        | [] =>
          let name := mods.metavar.getD "ARG"
          .error s!"Missing required argument: {name}"
        | arg :: rest =>
          if arg.startsWith "-" then
            findArg (before ++ [arg]) rest
          else
            match reader arg with
            | .ok v => .ok (v, before ++ rest)
            | .error e =>
              let name := mods.metavar.getD "ARG"
              .error s!"Invalid value for {name}: {e}"
      findArg [] args
    descrs := [.argDescr mods] }

/-- Build a subcommand parser. The first positional argument selects the
    command.
    $$\text{subparser} : \text{List}\ (\text{String} \times \text{ParserInfo}\ \alpha) \to \text{Parser}\ \alpha$$ -/
def subparser (cmds : List (String × ParserInfo α)) : Parser α :=
  { run := fun args =>
      match args with
      | [] => .error s!"Missing command. Available: {", ".intercalate (cmds.map Prod.fst)}"
      | cmd :: rest =>
        match cmds.find? (fun (name, _) => name == cmd) with
        | some (_, info) => info.parser.run rest
        | none => .error s!"Unknown command: '{cmd}'. Available: {", ".intercalate (cmds.map Prod.fst)}"
    descrs := [.cmdDescr (cmds.map fun (name, info) => (name, info.description))] }

-- ── Parser combinators ──────────────────────────────────────────

/-- Applicative pure: a parser that always succeeds with the given value,
    consuming no arguments.
    $$\text{pure} : \alpha \to \text{Parser}\ \alpha$$ -/
instance : Pure Parser where
  pure a := { run := fun args => .ok (a, args) }

/-- Functor map for `Parser`.
    $$\text{map} : (\alpha \to \beta) \to \text{Parser}\ \alpha \to \text{Parser}\ \beta$$ -/
instance : Functor Parser where
  map f p := {
    run := fun args => do
      let (a, rest) ← p.run args
      .ok (f a, rest)
    descrs := p.descrs
  }

/-- Applicative sequencing for `Parser`.
    $$\text{seq} : \text{Parser}\ (\alpha \to \beta) \to (\text{Unit} \to \text{Parser}\ \alpha) \to \text{Parser}\ \beta$$ -/
instance : Seq Parser where
  seq pf px := {
    run := fun args => do
      let (f, rest) ← pf.run args
      let (x, rest') ← (px ()).run rest
      .ok (f x, rest')
    descrs := pf.descrs ++ (px ()).descrs
  }

/-- `SeqLeft` for `Parser`: run both, keep the left result. -/
instance : SeqLeft Parser where
  seqLeft pa pb := {
    run := fun args => do
      let (a, rest) ← pa.run args
      let (_, rest') ← (pb ()).run rest
      .ok (a, rest')
    descrs := pa.descrs ++ (pb ()).descrs
  }

/-- `SeqRight` for `Parser`: run both, keep the right result. -/
instance : SeqRight Parser where
  seqRight pa pb := {
    run := fun args => do
      let (_, rest) ← pa.run args
      let (b, rest') ← (pb ()).run rest
      .ok (b, rest')
    descrs := pa.descrs ++ (pb ()).descrs
  }

/-- Applicative instance for `Parser`. -/
instance : Applicative Parser where

/-- Alternative: try the left parser, fall back to the right on failure.
    $$\text{orElse} : \text{Parser}\ \alpha \to (\text{Unit} \to \text{Parser}\ \alpha) \to \text{Parser}\ \alpha$$ -/
instance : OrElse (Parser α) where
  orElse pa pb := {
    run := fun args =>
      match pa.run args with
      | .ok result => .ok result
      | .error _ => (pb ()).run args
    descrs := pa.descrs ++ (pb ()).descrs
  }

/-- Set a default value for a parser: if it fails, return the default.
    $$\text{withDefault} : \alpha \to \text{Parser}\ \alpha \to \text{Parser}\ \alpha$$ -/
def withDefault (dflt : α) (p : Parser α) : Parser α :=
  { run := fun args =>
      match p.run args with
      | .ok result => .ok result
      | .error _ => .ok (dflt, args)
    descrs := p.descrs }

/-- Build a typed option with a default value.
    $$\text{optionWithDefault} : \text{ReadM}\ \alpha \to \text{Mod} \to \alpha \to \text{Parser}\ \alpha$$ -/
def optionWithDefault (reader : ReadM α) (mods : Mod) (dflt : α) : Parser α :=
  withDefault dflt (option reader mods)

/-- Build a string-valued option with a default value.
    $$\text{strOptionWithDefault} : \text{Mod} \to \text{String} \to \text{Parser String}$$ -/
def strOptionWithDefault (mods : Mod) (dflt : String) : Parser String :=
  withDefault dflt (strOption mods)

/-- Create a command entry for use with `subparser`.
    $$\text{command} : \text{String} \to \text{ParserInfo}\ \alpha \to \text{String} \times \text{ParserInfo}\ \alpha$$ -/
def command (name : String) (info : ParserInfo α) : String × ParserInfo α :=
  (name, info)

end Options.Applicative
