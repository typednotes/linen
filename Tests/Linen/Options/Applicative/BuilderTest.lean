/-
  Tests for `Linen.Options.Applicative.Builder`.

  `Parser`/`Except` results carry no `BEq` here (see `TypesTest`), so
  round-trips are asserted by pattern-matching the `run` result rather than
  comparing whole values.
-/
import Linen.Options.Applicative.Builder

open Options.Applicative

namespace Tests.Options.Applicative.Builder

/-! ### Modifier constructors -/

#guard (long "output").long == some "output"
#guard (short 'o').short == some 'o'
#guard (help "the output file").help == some "the output file"
#guard (metavar "FILE").metavar == some "FILE"
#guard hidden.hidden == true
#guard showDefault.showDefault == true

/-! ### Readers -/

#guard (match str "hi" with | .ok "hi" => true | _ => false)

def nonEmpty : ReadM String := eitherReader fun s =>
  if s.isEmpty then .error "empty" else .ok s

#guard (match nonEmpty "x" with | .ok "x" => true | _ => false)
#guard (match nonEmpty "" with | .error "empty" => true | _ => false)

#guard (FromString.fromString? "42" : Option Nat) == some 42
#guard (FromString.fromString? "abc" : Option Nat) == none
#guard (FromString.fromString? "yes" : Option Bool) == some true
#guard (FromString.fromString? "0" : Option Bool) == some false
#guard (FromString.fromString? "??" : Option Bool) == none

#guard (match (auto : ReadM Nat) "7" with | .ok 7 => true | _ => false)
#guard (match (auto : ReadM Nat) "x" with | .error _ => true | _ => false)

/-! ### Option / flag / argument builders -/

def outMod : Mod := long "output" ++ short 'o'

#guard (match (strOption outMod).run ["--output", "out.txt"] with
  | .ok ("out.txt", []) => true | _ => false)
#guard (match (strOption outMod).run ["-o", "out.txt", "extra"] with
  | .ok ("out.txt", ["extra"]) => true | _ => false)
#guard (match (strOption outMod).run [] with | .error _ => true | _ => false)
#guard (match (option auto (long "count")).run ["--count", "3"] with
  | .ok (3, []) => true | _ => false)

def verboseMod : Mod := long "verbose"

#guard (match (switch verboseMod).run ["--verbose"] with | .ok (true, []) => true | _ => false)
#guard (match (switch verboseMod).run [] with | .ok (false, []) => true | _ => false)

#guard (match (flag "off" "on" verboseMod).run ["--verbose"] with
  | .ok ("on", []) => true | _ => false)
#guard (match (flag "off" "on" verboseMod).run [] with | .ok ("off", []) => true | _ => false)

#guard (match (flag' "on" verboseMod).run ["--verbose"] with | .ok ("on", []) => true | _ => false)
#guard (match (flag' "on" verboseMod).run [] with | .error _ => true | _ => false)

#guard (match (argument str (metavar "NAME")).run ["hello"] with
  | .ok ("hello", []) => true | _ => false)
#guard (match (argument str (metavar "NAME")).run [] with
  | .error "Missing required argument: NAME" => true | _ => false)
#guard (match (argument str (metavar "NAME")).run ["--flag", "hello"] with
  | .ok ("hello", ["--flag"]) => true | _ => false)

/-! ### Applicative combinators -/

structure Config where
  name : String
  count : Nat

def configParser : Parser Config :=
  Config.mk <$> strOption (long "name") <*> option auto (long "count")

#guard (match configParser.run ["--name", "bob", "--count", "3"] with
  | .ok (⟨"bob", 3⟩, []) => true | _ => false)

#guard (match (pure 5 : Parser Nat).run ["x"] with | .ok (5, ["x"]) => true | _ => false)

/-! ### Alternatives and defaults -/

def pA : Parser String := strOption (long "a")
def pB : Parser String := strOption (long "b")

#guard (match (pA <|> pB).run ["--b", "val"] with | .ok ("val", []) => true | _ => false)
#guard (match (pA <|> pB).run [] with | .error _ => true | _ => false)

#guard (match (withDefault "def" pA).run [] with | .ok ("def", []) => true | _ => false)
#guard (match (optionWithDefault auto (long "n") 99).run [] with | .ok (99, []) => true | _ => false)
#guard (match (strOptionWithDefault (long "x") "def").run [] with | .ok ("def", []) => true | _ => false)

/-! ### Subcommands -/

def greetInfo : ParserInfo String :=
  { parser := strOption (long "name"), description := some "greet someone" }

def cmds := [command "greet" greetInfo]

#guard (match (subparser cmds).run ["greet", "--name", "bob"] with
  | .ok ("bob", []) => true | _ => false)
#guard (match (subparser cmds).run ["unknown"] with | .error _ => true | _ => false)
#guard (match (subparser cmds).run [] with | .error _ => true | _ => false)

end Tests.Options.Applicative.Builder
