/-
  Linen.Database.DuckDB.Simple.ToRow — the `ToRow` class

  Module #9 of `docs/imports/duckdb-simple/dependencies.md`, on #7
  (`Linen.Database.DuckDB.Simple.ToField`) and #4 (`…Types`, for
  `Only`/`Cons`).

  ## Design

  `ToRow` renders a whole collection of query parameters (one call's worth)
  into a flat `Array FieldBinding`, one entry per `ToField.toField` call —
  the DuckDB-`Simple` counterpart to `Linen.Database.SQLite.Simple.ToRow`,
  ported fresh into this package's own `FieldBinding`/`ToField` (per
  `docs/imports/duckdb-simple/dependencies.md`'s precedence note: the two
  upstream packages share no code) rather than imported. See that module's
  doc for the full rationale behind dropping `GToRow`/`ToRow [a]` and
  keeping `ToRow Unit`, all of which apply here unchanged.

  ### Tuple-arity cutoff

  Upstream's `duckdb-simple` `ToRow` only goes up to arity **5**. This port
  keeps the established **arity-7** cutoff anyway, for the same reason
  `Linen.Database.DuckDB.Simple.FromRow`'s module doc gives for overriding
  upstream's mismatched `FromRow` ceiling of 10: one shared cutoff across
  both of this batch's row modules, rather than each mirroring its own
  package's mismatched upstream ceiling. `Cons`/`(:.)` composition reaches
  further still (e.g. `(a,b,c,d,e,f,g) :. (h,i,j)` reaches arity 10 with no
  dedicated instance).

  ## Haskell source
  - `Database.DuckDB.Simple.ToRow` (`duckdb-simple` package, version 0.1.5.1)
-/

import Linen.Database.DuckDB.Simple.ToField
import Linen.Database.DuckDB.Simple.Types

namespace Database.DuckDB.Simple

-- ────────────────────────────────────────────────────────────────────
-- The `ToRow` class
-- ────────────────────────────────────────────────────────────────────

/-- A collection type that can be rendered into a flat array of
    `FieldBinding` query parameters. Instances are provided for `Unit`,
    `Only`, tuples up to arity 7 (see the module doc), and `Cons`. -/
class ToRow (α : Type u) where
  /-- Render every field of `a` as a query-parameter binding, in order. -/
  toRow : α → Array FieldBinding

export ToRow (toRow)

-- ────────────────────────────────────────────────────────────────────
-- `Unit` / `Only` / tuple / `Cons` instances
-- ────────────────────────────────────────────────────────────────────

instance : ToRow Unit where
  toRow _ := #[]

instance [ToField a] : ToRow (Only a) where
  toRow r := #[toField r.fromOnly]

instance [ToField a] [ToField b] : ToRow (a × b) where
  toRow := fun (x, y) => #[toField x, toField y]

instance [ToField a] [ToField b] [ToField c] : ToRow (a × b × c) where
  toRow := fun (x, y, z) => #[toField x, toField y, toField z]

instance [ToField a] [ToField b] [ToField c] [ToField d] : ToRow (a × b × c × d) where
  toRow := fun (w, x, y, z) => #[toField w, toField x, toField y, toField z]

instance [ToField a] [ToField b] [ToField c] [ToField d] [ToField e] :
    ToRow (a × b × c × d × e) where
  toRow := fun (v, w, x, y, z) => #[toField v, toField w, toField x, toField y, toField z]

instance [ToField a] [ToField b] [ToField c] [ToField d] [ToField e] [ToField f] :
    ToRow (a × b × c × d × e × f) where
  toRow := fun (u, v, w, x, y, z) =>
    #[toField u, toField v, toField w, toField x, toField y, toField z]

instance [ToField a] [ToField b] [ToField c] [ToField d] [ToField e] [ToField f] [ToField g] :
    ToRow (a × b × c × d × e × f × g) where
  toRow := fun (t, u, v, w, x, y, z) =>
    #[toField t, toField u, toField v, toField w, toField x, toField y, toField z]

instance [ToRow a] [ToRow b] : ToRow (Cons a b) where
  toRow r := toRow r.car ++ toRow r.cdr

end Database.DuckDB.Simple
