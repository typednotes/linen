/-
  Tests for `Linen.Data.Yaml`.

  Pure, so everything is checked with `#guard`. The headline document is a real
  `~/.config/scw/config.yaml`, since reading that is what the module was
  written for.

  `Value` has no `BEq` (it is recursive over lists of itself), so assertions go
  through the accessors — which is also how a caller reads a config, so the
  tests exercise the API rather than the representation.
-/
import Linen.Data.Yaml

open Data.Yaml

namespace Tests.Data.Yaml

private def v (s : String) : Value := (parse s).toOption.getD .null
private def failed (s : String) : Bool := (parse s).toOption.isNone

/-- A scalar at a dotted path, as text. -/
private def at? (s path : String) : Option String :=
  ((v s).path? path).bind (·.asString?)

/-! ### Core-schema scalar resolution -/

#guard at? "a: hello" "a" == some "hello"
#guard ((v "a: 1").get? "a").bind (·.asInt?) == some 1
#guard ((v "a: -42").get? "a").bind (·.asInt?) == some (-42)
#guard ((v "a: 0x1f").get? "a").bind (·.asInt?) == some 31        -- hex
#guard ((v "a: 0o17").get? "a").bind (·.asInt?) == some 15        -- octal
#guard ((v "a: true").get? "a").bind (·.asBool?) == some true
#guard ((v "a: True").get? "a").bind (·.asBool?) == some true
#guard ((v "a: FALSE").get? "a").bind (·.asBool?) == some false

-- Null spellings all collapse to the same node.
#guard ((v "a: null").get? "a").bind (·.asString?) == none
#guard ((v "a: ~").get? "a").bind (·.asString?) == none
#guard ((v "a:").get? "a").bind (·.asString?) == none
-- ...but the key is still present: it must not silently vanish.
#guard (v "a:").keys == ["a"]
#guard (v "a: 1\nempty:\nb: 2").keys == ["a", "empty", "b"]

-- Anything not matching a core-schema tag stays a string.
#guard at? "a: 1.2.3" "a" == some "1.2.3"       -- a version, not a float
#guard at? "a: 12:30" "a" == some "12:30"       -- a time, not a mapping
#guard at? "a: yes" "a" == some "yes"           -- YAML 1.1 boolean, not 1.2
#guard at? "a: on" "a" == some "on"
#guard at? "a: 007" "a" == some "7"             -- resolves as an integer

/-! ### Quoting -/

#guard at? "a: 'raw'" "a" == some "raw"
#guard at? "a: \"esc\\ttab\"" "a" == some "esc\ttab"
#guard at? "a: '123'" "a" == some "123"          -- quoted digits stay a string
#guard at? "a: \"a: b\"" "a" == some "a: b"      -- a colon inside quotes
#guard at? "a: 'it # hash'" "a" == some "it # hash"

/-! ### Comments

  `#` starts a comment only at line start or after whitespace, and never
  inside quotes — a URL fragment must survive. -/

#guard at? "# lead\na: 1  # trailing" "a" == some "1"
#guard at? "a: http://h/#frag" "a" == some "http://h/#frag"
#guard (v "# only a comment").keys == []

/-! ### Block mappings and sequences -/

#guard (v "a: 1\nb: 2").keys == ["a", "b"]
#guard at? "a:\n  b:\n    c: deep" "a.b.c" == some "deep"

private def seqOf (s p : String) : List String :=
  ((((v s).path? p).bind (·.asSeq?)).getD []).filterMap (·.asString?)

#guard seqOf "xs:\n  - 1\n  - 2" "xs" == ["1", "2"]
-- A sequence at the *same* column as its key is legal, and a later key at that
-- column must end it.
#guard seqOf "xs:\n- 1\n- 2" "xs" == ["1", "2"]
#guard (v "xs:\n- 1\n- 2\nafter: 9").keys == ["xs", "after"]
#guard at? "xs:\n- 1\nafter: 9" "after" == some "9"

-- A top-level sequence.
#guard (((v "- a\n- b").asSeq?).getD []).filterMap (·.asString?) == ["a", "b"]

-- The compact `- key: value` form, and nested inline dashes.
#guard ((((v "top:\n  - name: a\n  - name: b").path? "top").bind (·.asSeq?)).getD []).filterMap
         (fun e => (e.get? "name").bind (·.asString?)) == ["a", "b"]
#guard ((((v "- - a\n- b").asSeq?).getD []).length) == 2

/-! ### Flow collections -/

#guard at? "m: {a: 1}" "m.a" == some "1"
#guard at? "m: {a: 1, b: 2}" "m.b" == some "2"
#guard seqOf "xs: [1, 2, 3]" "xs" == ["1", "2", "3"]
#guard at? "m: {a: {b: c}}" "m.a.b" == some "c"                 -- nested flow
#guard seqOf "m: {xs: [1, 2]}" "m.xs" == ["1", "2"]
#guard at? "m: {a: 'x, y'}" "m.a" == some "x, y"                 -- comma inside quotes
#guard (((v "xs: []").path? "xs").bind (·.asSeq?)) == some []

/-! ### Block scalars

  Indentation comes from the block's first line, not the key's column. -/

#guard at? "lit: |\n  one\n  two" "lit" == some "one\ntwo\n"
#guard at? "lit: |-\n  one\n  two" "lit" == some "one\ntwo"      -- chomped
#guard at? "fold: >\n  one\n  two" "fold" == some "one two\n"     -- folded
-- Deeper nesting, and the block ends when indentation drops.
#guard at? "deep:\n    a: |\n      x\n      y\nafter: 1" "deep.a" == some "x\ny\n"
#guard at? "deep:\n    a: |\n      x\nafter: 1" "after" == some "1"

/-! ### Documents -/

#guard (parseDocuments "---\na: 1\n---\nb: 2").toOption.map (·.length) == some 2
#guard (parseDocuments "a: 1").toOption.map (·.length) == some 1
#guard (parseDocuments "").toOption.map (·.length) == some 0
#guard failed "---\na: 1\n---\nb: 2"       -- `parse` insists on one document

/-! ### Deliberately rejected

  These produce an error rather than a wrong parse. Silently reading `*base`
  as the string `"*base"` would be worse than refusing it. -/

#guard failed "x: &anchor 1"       -- anchors
#guard failed "x: *alias"          -- aliases
#guard failed "<<: base"           -- merge keys
#guard failed "x: !!str 1"         -- custom tags
#guard failed "a:\n\tb: 1"         -- tab indentation

/-! ### A real Scaleway CLI config -/

private def scwConfig : String :=
  "# This file was created by the Scaleway CLI\n\
access_key: SCWXXXXXXXXXXXXXXXXX\n\
secret_key: 7f0a4e33-1234-5678-9abc-def012345678\n\
default_organization_id: 951df375-e094-4d26-97c1-ba548eeb9c42\n\
default_project_id: 951df375-e094-4d26-97c1-ba548eeb9c42\n\
default_region: fr-par\n\
default_zone: fr-par-1\n\
api_url: https://api.scaleway.com\n\
insecure: false\n"

#guard at? scwConfig "access_key" == some "SCWXXXXXXXXXXXXXXXXX"
#guard at? scwConfig "secret_key" == some "7f0a4e33-1234-5678-9abc-def012345678"
#guard at? scwConfig "default_region" == some "fr-par"
-- The URL keeps its `//` and its colon.
#guard at? scwConfig "api_url" == some "https://api.scaleway.com"
#guard ((v scwConfig).get? "insecure").bind (·.asBool?) == some false
#guard (v scwConfig).get? "missing" == none

/-! ### Signatures -/

example : String → Except String Value := parse
example : String → Except String (List Value) := parseDocuments
example : String → Value := resolveScalar
example : Value → String → Option Value := Value.get?
example : Value → String → Option Value := Value.path?

end Tests.Data.Yaml
