/-
  Linen.Database.SQLite.Simple.ToRow — the `ToRow` class

  Module #13 of `docs/imports/sqlite-simple/dependencies.md`, on module #4
  (`Linen.Database.SQLite`, for `SQLData`), module #5 (`…Types`, for
  `Only`/`(:.)`), and module #10 (`…ToField`).

  ## Design

  `ToRow` renders a whole collection of parameters (one query's worth) into
  a flat `Array SQLData`, one entry per `ToField.toField` call, for
  `Database.SQLite3.bind`.

  ### Tuple-arity cutoff

  Matches `…FromRow`'s own cutoff at **arity 7**, for the same reason:
  upstream itself switches from a `deriving`-shorthand block to
  hand-written instances exactly at that point (`deriving instance
  (ToField a, …, ToField g) => ToRow (a,…,g)` is upstream's *last*
  `deriving` line; arities 8–10 are hand-written `instance … where toRow
  … = […]` blocks). Lean has no `deriving`-via-`Generic` mechanism to
  produce even the first seven automatically, so every arity here is
  hand-written regardless — but 7 is kept as the break point anyway, to
  mirror `…FromRow`'s cutoff and because a caller needing more can reach
  for `(:.)` instead (e.g. `(a,b,c,d,e,f,g) :. (h,i,j)` covers 10 without a
  dedicated instance).

  ### Omissions

  - **`GToRow`/generic derivation** — dropped for the same reason
    `…FromRow`'s module doc drops `GFromRow`: no structural `Generic`
    analogue in Lean; write `toRow x := #[toField x.a, toField x.b]`
    directly instead.
  - **`ToRow [a]`** (upstream: render a homogeneous list, `map toField`) —
    dropped as a variable-arity instance, orthogonal to this module's
    fixed-arity tuple/`(:.)` scope (see `…FromRow`'s module doc for the same
    call on its dual). `Array.map toField` covers the same use case
    directly against `ToField` without a class instance.
  - **`ToRow Unit`** (upstream: `deriving instance ToRow ()`, the
    zero-parameter row) — kept: it is width-0, not a variable-arity case,
    and costs nothing to include.

  ## Haskell source
  - `Database.SQLite.Simple.ToRow` (`sqlite-simple` package)
-/

import Linen.Database.SQLite
import Linen.Database.SQLite.Simple.Types
import Linen.Database.SQLite.Simple.ToField

namespace Database.SQLite.Simple

open Database.SQLite3 (SQLData)

-- ────────────────────────────────────────────────────────────────────
-- The `ToRow` class
-- ────────────────────────────────────────────────────────────────────

/-- A collection type that can be rendered into a flat array of `SQLData`
    query parameters. Instances are provided for `Unit`, `Only`, tuples up
    to arity 7 (see the module doc), and `(:.)`. -/
class ToRow (α : Type u) where
  /-- Render every field of `a` as a query parameter, in order. -/
  toRow : α → Array SQLData

export ToRow (toRow)

-- ────────────────────────────────────────────────────────────────────
-- `Unit` / `Only` / tuple / `(:.)` instances
-- ────────────────────────────────────────────────────────────────────

instance : ToRow Unit where
  toRow _ := #[]

instance [ToField a] : ToRow (Types.Only a) where
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

instance [ToRow a] [ToRow b] : ToRow (Types.Cons a b) where
  toRow r := toRow r.car ++ toRow r.cdr

end Database.SQLite.Simple
