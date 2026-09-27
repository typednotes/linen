/-
  `Data.Name` — reading `Lean.Name`s back from their dotted syntax, exactly

  `Name.toString` writes a name in Lean's dotted syntax (`Foo.bar`,
  `double.1`, `«a.b».c`), but its own documentation warns that names with
  numeric components or `»` may not round-trip, and `String.toName` reaches
  `unreachable!` on a component such as `1a`. `Data.Name.parse` is a total
  reader of that syntax: components separated by `.`, each `«…»`-escaped text,
  a number, or text not starting with a digit — malformed input is an error,
  never a panic. `Data.Name.roundTrips` says whether a name's string form reads
  back as itself (so callers can fall back to a structural form when not).
-/

namespace Data.Name

/-- The state of `parse`: the component being read (reversed), whether we
    are inside `«…»`, whether the current component was escaped, and the
    components read so far with their escapedness. -/
private structure LabelState where
  cur : List Char := []
  inEscape : Bool := false
  escaped : Bool := false
  comps : Array (String × Bool) := #[]

/-- Read one character of a dotted label. -/
private def labelStep (st : LabelState) (c : Char) : Except String LabelState :=
  if st.inEscape then
    if c == '»' then .ok { st with inEscape := false }
    else .ok { st with cur := c :: st.cur }
  else if c == '«' then
    if st.cur.isEmpty && !st.escaped then .ok { st with inEscape := true, escaped := true }
    else .error "misplaced `«`"
  else if c == '.' then
    if st.cur.isEmpty && !st.escaped then .error "empty component"
    else .ok { comps := st.comps.push (String.ofList st.cur.reverse, st.escaped) }
  else if st.escaped then .error "text after `»`"
  else .ok { st with cur := c :: st.cur }

/-- One component: escaped text is a string; unescaped digits are a number;
    anything else must not start with a digit. -/
private def labelComponent (n : Lean.Name) : String × Bool → Except String Lean.Name
  | (s, true)  => .ok (.str n s)
  | (s, false) =>
    if s.all Char.isDigit then .ok (.num n s.toNat!)
    else if s.front.isDigit then .error s!"component `{s}` starts with a digit"
    else .ok (.str n s)

/-- Parse a label in Lean's dotted syntax: components separated by `.`, each
    either `«…»`-escaped text, a number, or text not starting with a digit.
    Total: malformed input is an error, never a panic. -/
def parse (s : String) : Except String Lean.Name := do
  let st ← s.toList.foldlM labelStep {}
  if st.inEscape then throw s!"label `{s}`: unterminated `«`"
  if st.cur.isEmpty && !st.escaped then throw s!"label `{s}`: empty component"
  let comps := st.comps.push (String.ofList st.cur.reverse, st.escaped)
  comps.foldlM labelComponent .anonymous |>.mapError (s!"label `{s}`: " ++ ·)

/-- Whether `n`'s dotted string form reads back as `n`. -/
def roundTrips (n : Lean.Name) : Bool :=
  match parse n.toString with
  | .ok n' => n' == n
  | .error _ => false

end Data.Name
