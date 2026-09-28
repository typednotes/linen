/-
  Tests for `Linen.Data.Ini`.

  Pure, so everything is checked with `#guard`. The headline document is a real
  `~/.aws/credentials` / `~/.aws/config` pair, since reading those is what the
  module was written for — including the `[profile name]` header form that
  `aws configure` writes, whose section name contains a space.
-/
import Linen.Data.Ini

open Data.Ini

namespace Tests.Data.Ini

/-- Parse, or a sentinel that makes a failure obvious in the assertion. -/
private def ini (s : String) : Ini := (parse s).toOption.getD {}

private def failed (s : String) : Bool := (parse s).toOption.isNone

/-! ### Sections and pairs -/

#guard (ini "[a]\nx = 1").lookup "a" "x" == some "1"
#guard (ini "[a]\nx: 1").lookup "a" "x" == some "1"        -- colon separator
#guard (ini "[a]\nx=1").lookup "a" "x" == some "1"          -- no spaces
#guard (ini "[a]\n  x  =  1  ").lookup "a" "x" == some "1"  -- trimmed both sides
#guard (ini "[a]\nx =").lookup "a" "x" == some ""           -- empty value is legal
#guard (ini "[a]\nx = 1").lookup "a" "missing" == none
#guard (ini "[a]\nx = 1").lookup "missing" "x" == none

/-! ### Comments and blank lines -/

#guard (ini "; lead\n[a]\n# note\nx = 1\n\n").lookup "a" "x" == some "1"
#guard (ini "  ; indented comment\n[a]\nx = 1").lookup "a" "x" == some "1"
-- Not stripped mid-line: passwords and URL fragments contain both characters.
#guard (ini "[a]\nx = a#b").lookup "a" "x" == some "a#b"
#guard (ini "[a]\nx = a;b").lookup "a" "x" == some "a;b"

/-! ### Values containing separators

  The *first* separator splits, so the rest belongs to the value. -/

#guard (ini "[a]\nurl = https://h:443/p").lookup "a" "url" == some "https://h:443/p"
#guard (ini "[a]\nx = a=b=c").lookup "a" "x" == some "a=b=c"
#guard (ini "[a]\nx = k:v").lookup "a" "x" == some "k:v"
-- `=` before `:` and vice versa: whichever comes first wins.
#guard (ini "[a]\nk:x = y").lookup "a" "k" == some "x = y"
#guard (ini "[a]\nk=x: y").lookup "a" "k" == some "x: y"

/-! ### Globals -/

#guard (ini "x = 1\n[a]\ny = 2").lookupGlobal "x" == some "1"
#guard (ini "x = 1\n[a]\ny = 2").lookup "a" "y" == some "2"
#guard (ini "[a]\ny = 2").lookupGlobal "y" == none      -- inside a section, not global

/-! ### Section names -/

#guard (ini "[a]\n[b]\n[c]").sectionNames == ["a", "b", "c"]
#guard (ini "[ spaced ]\nx=1").lookup "spaced" "x" == some "1"   -- header trimmed
#guard (ini "[profile dev]\nregion = eu-west-1").lookup "profile dev" "region"
     == some "eu-west-1"                                          -- as `aws config` writes it
#guard (ini "").sectionNames == []

/-! ### Duplicates

  Last wins on lookup; all are kept so `render` does not silently drop lines. -/

#guard (ini "[a]\nx = 1\nx = 2").lookup "a" "x" == some "2"
#guard (ini "[a]\nx = 1\n[a]\nx = 2").lookup "a" "x" == some "2"   -- split section merges
#guard (ini "[a]\nx = 1\n[b]\nx = 9\n[a]\ny = 2").lookup "a" "x" == some "1"
#guard (ini "[a]\nx = 1\n[a]\ny = 2").keys "a" == ["x", "y"]
#guard (ini "[a]\nx = 1\n[a]\nx = 2").sectionNames == ["a"]        -- deduplicated

/-! ### Errors

  A malformed line is reported, not skipped: a typo in a credentials file
  should surface rather than look like a missing key. -/

#guard failed "[unterminated"
#guard failed "[]"                    -- empty section name
#guard failed "[  ]"                  -- whitespace-only section name
#guard failed "novalue"               -- no separator at all
#guard failed "[a]\n= 1"              -- empty key
#guard !(failed "")                   -- an empty document is fine
#guard !(failed "\n\n  \n")           -- as is a blank one

/-! ### A real AWS credentials file -/

private def awsCredentials : String :=
  "# managed by aws configure\n\
[default]\n\
aws_access_key_id = AKIAIOSFODNN7EXAMPLE\n\
aws_secret_access_key = wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY\n\
\n\
[staging]\n\
aws_access_key_id = AKIAI44QH8DHBEXAMPLE\n\
aws_secret_access_key = je7MtGbClwBF/2Zp9Utk/h3yCo8nvbEXAMPLEKEY\n\
aws_session_token = FQoGZXIvYXdzEBYaDDRw==\n"

#guard (ini awsCredentials).sectionNames == ["default", "staging"]
#guard (ini awsCredentials).lookup "default" "aws_access_key_id" == some "AKIAIOSFODNN7EXAMPLE"
-- The secret contains '/', which must survive untouched.
#guard (ini awsCredentials).lookup "default" "aws_secret_access_key"
     == some "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
#guard (ini awsCredentials).lookup "staging" "aws_session_token" == some "FQoGZXIvYXdzEBYaDDRw=="
#guard (ini awsCredentials).lookup "default" "aws_session_token" == none
#guard (ini awsCredentials).keys "staging"
     == ["aws_access_key_id", "aws_secret_access_key", "aws_session_token"]

/-! ### Rendering and round-trip -/

#guard render { globals := [], sections := [("a", [("x", "1")])] } == "[a]\nx = 1\n"
#guard render { globals := [("g", "0")], sections := [] } == "g = 0\n"
#guard render {} == ""

-- Parsing a rendered document gives the same document back: comments, blank
-- lines and spacing are lost, but no pair is.
#guard ini (render (ini awsCredentials)) == ini awsCredentials
#guard ini (render (ini "x = 1\n[a]\ny = 2")) == ini "x = 1\n[a]\ny = 2"

/-! ### Signatures -/

example : String → Except String Ini := parse
example : Ini → String := render
example : Ini → String → String → Option String := Ini.lookup
example : Ini → String → Option (List (String × String)) := Ini.sectionPairs

end Tests.Data.Ini
