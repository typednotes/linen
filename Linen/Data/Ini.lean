/-
  Linen.Data.Ini — INI configuration files

  Reads and writes the `[section]` / `key = value` format used by countless
  tools' configuration files — `~/.aws/credentials`, `~/.gitconfig`,
  `pip.conf`, systemd units, and so on.

  ## Provenance
  Ported from Hackage's `ini` package, which likewise models a file as an
  optional set of top-level pairs plus named sections.

  ## There is no INI standard
  Every implementation differs, so the accepted dialect is stated explicitly:

  * Sections are `[name]` on a line of their own; the name is trimmed, and may
    contain spaces (`[profile dev]`, as `~/.aws/config` writes them).
  * Pairs are `key = value` or `key: value`; the first separator wins, so a
    value may itself contain `=` or `:` (URLs and connection strings do).
  * Keys and values are trimmed. A value may be empty.
  * `;` and `#` begin a comment when they start a line. They are *not*
    recognised mid-line: `key = a#b` has the value `a#b`, because values like
    passwords and URL fragments legitimately contain both characters.
  * Pairs before any section header go in `globals`.
  * Duplicate keys: the last wins on lookup, but all are kept, so `render`
    round-trips a file faithfully rather than quietly dropping lines.

  Not supported: line continuations, `[[nested]]` sections, quoted values with
  escape sequences, and mid-line comments — none of which the format's common
  dialects agree on either.

  ## Structure
  Parsing is a fold over lines, and each line is independent, so it is
  structurally recursive with no lookahead and no termination argument.
-/

namespace Data.Ini

-- ── The document ──

/-- A parsed INI file: pairs appearing before any header, then named sections
    in the order they appeared. -/
structure Ini where
  /-- Pairs before the first `[section]` header. -/
  globals  : List (String × String) := []
  /-- Named sections, in file order, each with its pairs in file order. -/
  sections : List (String × List (String × String)) := []
  deriving Repr, DecidableEq, BEq, Inhabited

-- ── Lookup ──

/-- The pairs of a named section, or `none` if there is no such section.

    Duplicate section headers are merged, so a key is found wherever in the
    file it was written. -/
def Ini.sectionPairs (ini : Ini) (name : String) : Option (List (String × String)) :=
  let matching := ini.sections.filter (·.1 == name)
  if matching.isEmpty then none else some (matching.flatMap (·.2))

/-- Look a key up within a section. The last occurrence wins, matching how
    most readers of these files behave. -/
def Ini.lookup (ini : Ini) (sect key : String) : Option String :=
  (ini.sectionPairs sect).bind fun pairs =>
    (pairs.reverse.find? (·.1 == key)).map (·.2)

/-- Look a key up among the pairs written before any section header. -/
def Ini.lookupGlobal (ini : Ini) (key : String) : Option String :=
  (ini.globals.reverse.find? (·.1 == key)).map (·.2)

/-- Every section name, in file order and without duplicates. -/
def Ini.sectionNames (ini : Ini) : List String :=
  ini.sections.foldl (fun acc (n, _) => if acc.contains n then acc else acc ++ [n]) []

/-- Every key in a section, in file order. -/
def Ini.keys (ini : Ini) (sect : String) : List String :=
  ((ini.sectionPairs sect).getD []).map (·.1)

-- ── Parsing ──

private def trim (s : String) : String := s.trimAscii.toString

/-- Split a pair on the first `=` or `:`, whichever comes first.

    Splitting on the *first* separator is what lets a value contain more of
    them, which URLs and connection strings routinely do. -/
private def splitPair (line : String) : Option (String × String) :=
  let cs := line.toList
  let idxEq := cs.findIdx? (· == '=')
  let idxCo := cs.findIdx? (· == ':')
  let idx := match idxEq, idxCo with
    | some a, some b => some (min a b)
    | some a, none   => some a
    | none,   some b => some b
    | none,   none   => none
  idx.map fun i => (trim (String.ofList (cs.take i)), trim (String.ofList (cs.drop (i + 1))))

/-- What the fold carries: the section being filled (`none` before the first
    header), the pairs gathered for it, and the completed sections. All
    reversed until the end. -/
private structure PState where
  current  : Option String := none
  pending  : List (String × String) := []
  globals  : List (String × String) := []
  sections : List (String × List (String × String)) := []

/-- Close the section in progress, if any. -/
private def PState.close (st : PState) : PState :=
  match st.current with
  | none      => { st with globals := st.globals ++ st.pending.reverse, pending := [] }
  | some name => { st with sections := (name, st.pending.reverse) :: st.sections, pending := [] }

/-- Consume one line. Lines are independent, so this never recurses. -/
private def stepLine (st : PState) (raw : String) : Except String PState :=
  let line := trim raw
  if line.isEmpty then .ok st
  else if line.startsWith ";" || line.startsWith "#" then .ok st
  else if line.startsWith "[" then
    if line.endsWith "]" then
      let name := trim ((line.drop 1).dropEnd 1).toString
      if name.isEmpty then .error "empty section name"
      else
        let closed := st.close
        .ok { closed with current := some name }
    else .error s!"unterminated section header: {line}"
  else
    match splitPair line with
    | some (k, v) =>
      if k.isEmpty then .error s!"empty key in: {line}"
      else .ok { st with pending := (k, v) :: st.pending }
    | none => .error s!"expected 'key = value' or a [section] header, got: {line}"

/-- Parse an INI document. Errors name the offending line rather than
    silently skipping it — a typo in a credentials file should be reported,
    not treated as absent. -/
def parse (input : String) : Except String Ini := do
  let final ← (input.splitOn "\n").foldlM stepLine {}
  let closed := final.close
  return { globals := closed.globals, sections := closed.sections.reverse }

-- ── Rendering ──

/-- Render back to INI text. Globals first, then sections in order, so a
    parsed file round-trips modulo comments, blank lines and whitespace. -/
def render (ini : Ini) : String :=
  let pair (kv : String × String) : String := s!"{kv.1} = {kv.2}\n"
  let globals := String.join (ini.globals.map pair)
  let sections := String.join (ini.sections.map fun (name, pairs) =>
    s!"[{name}]\n" ++ String.join (pairs.map pair))
  if ini.globals.isEmpty then sections
  else globals ++ (if ini.sections.isEmpty then "" else "\n") ++ sections

end Data.Ini
