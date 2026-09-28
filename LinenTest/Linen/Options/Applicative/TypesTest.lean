/-
  Tests for `Linen.Options.Applicative.Types`.

  These are pure type/combinator definitions, so behaviour is asserted with
  `#guard`. Neither `Except` (core) nor `OptDescr`/`Parser` (this module)
  carry a `BEq` instance here, so checks either pattern-match directly or
  inspect individual fields rather than whole-value equality.
-/
import Linen.Options.Applicative.Types

open Options.Applicative

namespace Tests.Options.Applicative.Types

/-! ### ReadM -/

def yesNo : ReadM Bool := fun s =>
  if s == "yes" then .ok true
  else if s == "no" then .ok false
  else .error s!"expected yes/no, got {s}"

#guard (match yesNo "yes" with | .ok true => true | _ => false)
#guard (match yesNo "no" with | .ok false => true | _ => false)
#guard (match yesNo "maybe" with | .error "expected yes/no, got maybe" => true | _ => false)
#guard (match (default : ReadM Nat) "42" with | .error "no reader" => true | _ => false)

/-! ### Mod: right-biased combination -/

def modA : Mod := { long := some "a", hidden := false }
def modB : Mod := { long := some "b", hidden := true }

#guard (modA ++ modB).long == some "b"                    -- right-hand side wins
#guard (modA ++ modB).hidden == true                       -- either side sets it
#guard ((modA : Mod) ++ ({} : Mod)).long == some "a"        -- empty right side keeps left
#guard ((default : Mod) ++ (default : Mod)).long == none

/-! ### OptDescr -/

#guard (match OptDescr.optionDescr modA with | .optionDescr m => m.long == some "a" | _ => false)
#guard (match OptDescr.flagDescr modA with | .flagDescr _ => true | _ => false)
#guard (match OptDescr.argDescr modA with | .optionDescr _ => false | _ => true)
#guard (match OptDescr.cmdDescr [("run", some "run the thing")] with
  | .cmdDescr cs => cs == [("run", some "run the thing")]
  | _ => false)

/-! ### Parser: a hand-built one-argument parser -/

def takeOne : Parser String where
  run
    | []      => .error "expected an argument"
    | a :: rest => .ok (a, rest)

#guard (match takeOne.run ["x", "y"] with | .ok ("x", ["y"]) => true | _ => false)
#guard (match takeOne.run [] with | .error "expected an argument" => true | _ => false)
#guard takeOne.descrs.isEmpty

/-! ### ParserInfo: defaults -/

def info : ParserInfo String := { parser := takeOne, description := some "demo" }

#guard info.description == some "demo"
#guard info.header == none
#guard info.fullDesc == true
#guard info.failureCode == 1

end Tests.Options.Applicative.Types
