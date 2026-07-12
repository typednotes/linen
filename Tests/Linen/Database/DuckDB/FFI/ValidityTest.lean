/-
  Tests for `Linen.Database.DuckDB.FFI.Validity`.

  Creates a real `Vector`, forces a writable validity mask via
  `Database.DuckDB.FFI.Vector.ensureValidityWritable`, and exercises
  `rowIsValid`/`setRowValidity`/`setRowInvalid`/`setRowValid` on it.
-/
import Linen.Database.DuckDB.FFI.Validity
import Linen.Database.DuckDB.FFI.Vector
import Linen.Database.DuckDB.FFI.LogicalTypes

open Database.DuckDB.FFI.Validity
open Database.DuckDB.FFI.Vector (createVector destroy ensureValidityWritable getValidity)
open Database.DuckDB.FFI.LogicalTypes (create)
open Database.DuckDB.FFI.Types

namespace Tests.Database.DuckDB.FFI.Validity

#eval show IO Unit from do
  let ty ← create .integer
  let vec ← createVector ty 8

  ensureValidityWritable vec
  let validityOpt ← getValidity vec
  let validity ← match validityOpt with
    | some v => pure v
    | none => throw (IO.userError "expected a writable validity mask after ensureValidityWritable")

  -- Freshly-ensured masks report every row valid.
  let row0Valid ← rowIsValid validity 0
  unless row0Valid do throw (IO.userError "expected row 0 to be valid by default")

  setRowInvalid validity 2
  let row2Valid ← rowIsValid validity 2
  unless !row2Valid do throw (IO.userError "expected row 2 to be invalid after setRowInvalid")

  setRowValid validity 2
  let row2ValidAgain ← rowIsValid validity 2
  unless row2ValidAgain do throw (IO.userError "expected row 2 to be valid after setRowValid")

  setRowValidity validity 5 false
  let row5Valid ← rowIsValid validity 5
  unless !row5Valid do throw (IO.userError "expected row 5 to be invalid after setRowValidity false")

  setRowValidity validity 5 true
  let row5ValidAgain ← rowIsValid validity 5
  unless row5ValidAgain do throw (IO.userError "expected row 5 to be valid after setRowValidity true")

  destroy vec
  Database.DuckDB.FFI.LogicalTypes.destroy ty

end Tests.Database.DuckDB.FFI.Validity
