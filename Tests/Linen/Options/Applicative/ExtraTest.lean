/-
  Tests for `Linen.Options.Applicative.Extra`.

  `Parser`/`Except` results carry no `BEq` here (see `TypesTest`), so
  round-trips are asserted by pattern-matching the result rather than
  comparing whole values.
-/
import Linen.Options.Applicative.Extra

open Options.Applicative

namespace Tests.Options.Applicative.Extra

/-! ### `helper` -/

#guard (match (helper (α := Nat)).run ["--help"] with | .ok (f, []) => f 7 == 7 | _ => false)
#guard (match (helper (α := Nat)).run [] with | .ok (f, []) => f 7 == 7 | _ => false)

/-! ### `info` -/

def nameParser : Parser String := strOption (long "name")

def nameInfo : ParserInfo String :=
  info nameParser { description := some "greet someone" }

#guard nameInfo.description == some "greet someone"

#guard (match nameInfo.parser.run ["--name", "bob"] with
  | .ok ("bob", []) => true | _ => false)
#guard (match nameInfo.parser.run ["--name", "bob", "--help"] with
  | .ok ("bob", []) => true | _ => false)

/-! ### `execParserPure` -/

#guard (match execParserPure nameInfo ["--name", "bob"] with
  | .ok "bob" => true | _ => false)
#guard (match execParserPure nameInfo [] with | .error _ => true | _ => false)

/-! ### `hsubparser` -/

def greetInfo : ParserInfo String :=
  { parser := strOption (long "name"), description := some "greet someone" }

def hcmds := [("greet", greetInfo)]

#guard (match (hsubparser hcmds).run ["greet", "--name", "bob"] with
  | .ok ("bob", []) => true | _ => false)
#guard (match (hsubparser hcmds).run ["unknown"] with | .error _ => true | _ => false)
#guard (match (hsubparser hcmds).run [] with | .error _ => true | _ => false)
#guard (hsubparser hcmds).descrs.isEmpty

/-! ### `renderHelp` -/

def fullInfo : ParserInfo String :=
  info nameParser
    { description := some "a greeter", header := some "greet-cli",
      footer := some "see also: --help" }

#guard ((renderHelp fullInfo).splitOn "greet-cli").length > 1
#guard ((renderHelp fullInfo).splitOn "a greeter").length > 1
#guard ((renderHelp fullInfo).splitOn "--name").length > 1
#guard ((renderHelp fullInfo).splitOn "see also: --help").length > 1

def hiddenInfo : ParserInfo String :=
  { parser := strOption (long "secret" ++ hidden), description := none }

#guard ((renderHelp hiddenInfo).splitOn "--secret").length == 1

def cmdInfo : ParserInfo String :=
  { parser := subparser [command "greet" greetInfo], description := none }

#guard ((renderHelp cmdInfo).splitOn "Available commands:").length > 1
#guard ((renderHelp cmdInfo).splitOn "greet").length > 1

end Tests.Options.Applicative.Extra
