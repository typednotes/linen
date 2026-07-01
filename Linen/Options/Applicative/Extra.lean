/-
  `Options.Applicative.Extra` — execution and help generation

  The high-level API for running parsers against command-line arguments and
  generating help text, on top of `Options.Applicative.Types` and
  `Options.Applicative.Builder`. Mirrors Haskell's `Options.Applicative.Extra`:
  https://hackage.haskell.org/package/optparse-applicative/docs/Options-Applicative-Extra.html

  $$\text{execParser} : \text{ParserInfo}\ \alpha \to \text{List String} \to \text{IO}\ \alpha$$
-/
import Linen.Options.Applicative.Types
import Linen.Options.Applicative.Builder

namespace Options.Applicative

-- ── Help text rendering ──────────────────────────────────────────

/-- Render the option name portion of help for a `Mod`.
    Returns a string like `"--output, -o FILE"`. -/
private def renderOptionName (mods : Mod) : String := Id.run do
  let mut parts : List String := []
  if let some l := mods.long then
    parts := parts ++ [s!"--{l}"]
  if let some s := mods.short then
    parts := parts ++ [s!"-{s}"]
  let name := ", ".intercalate parts
  if let some m := mods.metavar then
    return s!"{name} {m}"
  return name

/-- Pad a string to a given width with trailing spaces. -/
private def padRight (s : String) (width : Nat) : String :=
  if s.length >= width then s
  else s ++ String.ofList (List.replicate (width - s.length) ' ')

/-- Render a single option description as a help line: `(name, helpText,
    isHidden)` triples. -/
private def renderOptDescr (descr : OptDescr) : List (String × String × Bool) :=
  match descr with
  | .optionDescr mods =>
    [(renderOptionName mods, mods.help.getD "", mods.hidden)]
  | .flagDescr mods =>
    [(renderOptionName mods, mods.help.getD "", mods.hidden)]
  | .argDescr mods =>
    let name := mods.metavar.getD "ARG"
    [(name, mods.help.getD "", mods.hidden)]
  | .cmdDescr commands =>
    commands.map fun (name, desc) => (name, desc.getD "", false)

/-- Collect all command descriptions from option descriptors. -/
private def collectCmdDescrs (descrs : List OptDescr) : List (String × String) :=
  descrs.flatMap fun
    | .cmdDescr cmds => cmds.map fun (n, d) => (n, d.getD "")
    | _ => []

/-- Generate help text for a `ParserInfo`.
    $$\text{renderHelp} : \text{ParserInfo}\ \alpha \to \text{String}$$ -/
def renderHelp (pinfo : ParserInfo α) : String := Id.run do
  let mut lines : List String := []
  -- Header
  if let some h := pinfo.header then
    lines := lines ++ [h, ""]
  -- Description
  if let some d := pinfo.description then
    lines := lines ++ [d, ""]
  -- Usage line
  lines := lines ++ ["Usage: <program> [OPTIONS]", ""]
  -- Collect all descriptions
  let allDescrs := pinfo.parser.descrs.flatMap renderOptDescr
  let visibleDescrs := allDescrs.filter fun (_, _, h) => !h
  -- Separate command descriptions
  let cmdDescrs := collectCmdDescrs pinfo.parser.descrs
  let cmdNames := cmdDescrs.map Prod.fst
  let optDescrs := visibleDescrs.filter fun (name, _, _) =>
    !cmdNames.any (· == name)
  -- Options section
  if !optDescrs.isEmpty then
    lines := lines ++ ["Available options:"]
    let maxNameWidth := optDescrs.foldl (fun acc (name, _, _) =>
      Nat.max acc name.length) 0
    let colWidth := Nat.max (maxNameWidth + 2) 24
    for (name, helpText, _) in optDescrs do
      if helpText.isEmpty then
        lines := lines ++ [s!"  {name}"]
      else
        lines := lines ++ [s!"  {padRight name colWidth}{helpText}"]
  -- Subcommands section
  if !cmdDescrs.isEmpty then
    lines := lines ++ ["", "Available commands:"]
    let maxCmdWidth := cmdDescrs.foldl (fun acc (name, _) =>
      Nat.max acc name.length) 0
    let colWidth := Nat.max (maxCmdWidth + 2) 24
    for (name, desc) in cmdDescrs do
      if desc.isEmpty then
        lines := lines ++ [s!"  {name}"]
      else
        lines := lines ++ [s!"  {padRight name colWidth}{desc}"]
  -- Footer
  if let some f := pinfo.footer then
    lines := lines ++ ["", f]
  return "\n".intercalate lines

-- ── Helper and info combinators ──────────────────────────────────────────

/-- Check if `--help` or `-h` appears in the argument list. -/
private def hasHelpFlag (args : List String) : Bool :=
  args.any fun a => a == "--help" || a == "-h"

/-- A parser that recognises `--help` / `-h` and returns the identity
    function. When combined with `<*>`, it passes through the original value
    unchanged.
    $$\text{helper} : \text{Parser}\ (\alpha \to \alpha)$$ -/
def helper : Parser (α → α) :=
  flag id id ({ long := some "help", short := some 'h',
                help := some "Show this help text" } : Mod)

/-- Wrap a parser with info metadata.
    $$\text{info} : \text{Parser}\ \alpha \to \text{InfoMod} \to \text{ParserInfo}\ \alpha$$ -/
def info (p : Parser α) (mods : InfoMod := {}) : ParserInfo α :=
  { parser := helper <*> p
    description := mods.description
    header := mods.header
    footer := mods.footer
    fullDesc := mods.fullDesc
    failureCode := mods.failureCode }

/-- Build a hidden subparser (not shown in help).
    $$\text{hsubparser} : \text{List (String} \times \text{ParserInfo}\ \alpha\text{)} \to \text{Parser}\ \alpha$$ -/
def hsubparser (cmds : List (String × ParserInfo α)) : Parser α :=
  { run := fun args =>
      match args with
      | [] => .error s!"Missing command. Available: {", ".intercalate (cmds.map Prod.fst)}"
      | cmd :: rest =>
        match cmds.find? (fun (name, _) => name == cmd) with
        | some (_, pinfo) => pinfo.parser.run rest
        | none => .error s!"Unknown command: '{cmd}'. Available: {", ".intercalate (cmds.map Prod.fst)}"
    descrs := [] }  -- hidden: no descriptions

-- ── Parser execution ──────────────────────────────────────────

/-- Parse the given argument list using the `ParserInfo`, printing help and
    exiting on failure or when `--help` is requested.

    Intended to be called from `main`:
    ```lean
    def main (args : List String) : IO Unit := do
      let opts ← execParser myParserInfo args
      ...
    ```

    $$\text{execParser} : \text{ParserInfo}\ \alpha \to \text{List String} \to \text{IO}\ \alpha$$ -/
def execParser (pinfo : ParserInfo α) (args : List String) : IO α := do
  -- Check for help flag
  if hasHelpFlag args then
    IO.println (renderHelp pinfo)
    IO.Process.exit 0
  -- Run the parser
  match pinfo.parser.run args with
  | .ok (result, _) => return result
  | .error e => do
    IO.eprintln s!"Error: {e}"
    IO.eprintln ""
    IO.eprintln (renderHelp pinfo)
    IO.Process.exit pinfo.failureCode.toUInt8

/-- Pure variant of `execParser` that returns an `Except` instead of
    performing IO. Useful for testing.
    $$\text{execParserPure} : \text{ParserInfo}\ \alpha \to \text{List String} \to \text{Except String}\ \alpha$$ -/
def execParserPure (pinfo : ParserInfo α) (args : List String) : Except String α :=
  match pinfo.parser.run args with
  | .ok (result, _) => .ok result
  | .error e => .error e

end Options.Applicative
