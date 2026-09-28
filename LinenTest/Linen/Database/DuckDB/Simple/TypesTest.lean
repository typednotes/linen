/-
  Tests for `Linen.Database.DuckDB.Simple.Types`.

  Exercises `FormatError`'s `ToString`, the reused `Only`/`Cons`/`:.`
  surface, and `UUID`'s byte/string conversions (round trip, and against a
  known real-world UUID literal).
-/
import Linen.Database.DuckDB.Simple.Types

open Database.DuckDB.Simple

namespace Tests.Database.DuckDB.Simple.Types

-- `FormatError`.
#guard
  toString ({ message := "bad parameter count", query := Query.mk "SELECT ?", params := ["1", "2"] } :
      FormatError) ==
    "duckdb-simple: format error: bad parameter count (query: SELECT ?)"

-- `Only`/`Cons` are the exact same type reused from `sqlite-simple`'s port
-- (see the module doc's precedence note), not re-declared here. `:.` is
-- notation for the `Cons` *type* (e.g. `Int :. String`), not a value
-- constructor, so values are built with ordinary structure syntax.
#guard ({ fromOnly := 42 } : Only Nat).fromOnly == 42
#guard ({ car := (1 : Nat), cdr := "hello" } : Nat :. String).car == 1
#guard ({ car := (1 : Nat), cdr := "hello" } : Nat :. String).cdr == "hello"

-- `UUID`: byte round trip.
#guard
  let u : UUID := { hi := 0x0123456789abcdef, lo := 0xfedcba9876543210 }
  UUID.ofBytes? u.toBytes == some u

-- `UUID.ofBytes?` rejects anything but exactly 16 bytes.
#guard UUID.ofBytes? (ByteArray.mk #[1, 2, 3]) == none

-- `UUID`: canonical string round trip.
#guard
  let u : UUID := { hi := 0x0123456789abcdef, lo := 0xfedcba9876543210 }
  UUID.ofCanonicalString? (UUID.toCanonicalString u) == some u

-- A known real-world UUID literal renders to the expected canonical form.
#guard
  let u : UUID := { hi := 0x550e8400e29b41d4, lo := 0xa716446655440000 }
  UUID.toCanonicalString u == "550e8400-e29b-41d4-a716-446655440000"

-- Parsing accepts the same literal back, dashes and all.
#guard
  UUID.ofCanonicalString? "550e8400-e29b-41d4-a716-446655440000" ==
    some { hi := 0x550e8400e29b41d4, lo := 0xa716446655440000 }

-- Malformed input (wrong length, or a non-hex character) is rejected.
#guard UUID.ofCanonicalString? "not-a-uuid" == none
#guard UUID.ofCanonicalString? "550e8400-e29b-41d4-a716-44665544000" == none -- one digit short

end Tests.Database.DuckDB.Simple.Types
