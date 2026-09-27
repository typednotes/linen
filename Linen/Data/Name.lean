/-
  `Data.Name` — reading `Lean.Name`s back from their dotted syntax, exactly

  `Name.toString` writes a name in Lean's dotted syntax (`Foo.bar`,
  `double.1`, `«a.b».c`), but its own documentation warns that names with
  numeric components or `»` may not round-trip, and `String.toName` reaches
  `unreachable!` on a component such as `1a`. The total reader of that syntax
  is Lean core's own `Lean.Syntax.decodeNameLit` — what the elaborator uses for
  name literals — so this module does not re-implement it: `Data.Name.parse`
  is that reader with an error message, and `Data.Name.roundTrips` says whether
  a name's string form reads back as itself (so callers can fall back to a
  structural form when not).
-/

namespace Data.Name

/-- Parse a name in Lean's dotted syntax — components separated by `.`, each
    an identifier, a number, or `«…»`-escaped text — as a name literal
    (`Lean.Syntax.decodeNameLit`). Total: malformed input is an error, never a
    panic. -/
def parse (s : String) : Except String Lean.Name :=
  match Lean.Syntax.decodeNameLit ("`" ++ s) with
  | some n => .ok n
  | none => .error s!"`{s}` is not a name in Lean's dotted syntax"

/-- Whether `n`'s dotted string form reads back as `n`. -/
def roundTrips (n : Lean.Name) : Bool :=
  match parse n.toString with
  | .ok n' => n' == n
  | .error _ => false

end Data.Name
