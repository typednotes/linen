/-
  Linen.Database.SQLite.Simple.FromRow — the `FromRow` class, `RowParser`

  Module #12 of `docs/imports/sqlite-simple/dependencies.md`, on module #6
  (`…Ok`), module #9 (`…Internal`, for `Field`), and module #11
  (`…FromField`).

  ## Design

  ### `RowParser`

  Upstream's `RowParser` is `ReaderT Int (StateT (Int, [SQLData])) Ok` — a
  reader of the row's total column count, layered over state tracking the
  current column index and the not-yet-consumed `SQLData` values, over the
  error-accumulating `Ok`. This port collapses that stack into one direct
  state-passing function, `Array Field → Nat → Ok (α × Nat)`: a `RowParser`
  reads the row's already-decoded `Field`s (produced by
  `Linen.Database.SQLite.Simple.Internal.currentRowFields`) starting at a
  given column, and returns the parsed value together with the new column
  position (or an `Ok.errors` failure). This is equivalent in observable
  behaviour — reader/state/error stacks and one function threading the same
  three pieces of information are the same computation, just associated
  differently — and mirrors this module's own `FieldParser := Field → Ok α`
  (from `…FromField`) in shape. `RowParser` has `Functor`/`Applicative`
  instances (no `Monad`; nothing below needs `>>=`, only `<$>`/`<*>`, which
  is all upstream's own tuple instances use too).

  `fieldWith`/`field`/`numFieldsRemaining`/`returnRowError` are upstream's
  own combinators of the same names, ported directly onto this
  representation.

  ### Tuple-arity cutoff

  Upstream provides `FromRow` instances for tuples up to arity 10. This port
  goes up to **arity 7** — the same point where upstream's *sibling*
  `ToRow` module itself switches from a `deriving`-shorthand block to
  individually hand-written instances (see `…ToRow`'s module doc). Lean has
  no `deriving`-via-`Generic` mechanism for these tuple instances (see
  below), so every arity here is hand-written regardless; 7 is kept as the
  cutoff anyway, both to match `ToRow`'s natural break point and because
  `sqlite-simple` rows of more than 7 independently-typed columns are rare
  in practice — a longer row is easily assembled from smaller pieces via the
  `Cons`/`(:.)` instance below (e.g. `(a, b, c, d, e, f, g) :. (h, i, j)`
  reaches arity 10 and beyond without a dedicated instance).

  ### Omissions

  - **`GFromRow`/generic derivation.** Upstream's `GFromRow` piggybacks on
    GHC's `Generic` typeclass to derive `FromRow` for an arbitrary
    product-shaped record. Lean has no structural `Generic` analogue for
    ad hoc user types; callers needing a `FromRow User` instance write
    `fromRow := User.mk <$> field <*> field` directly (upstream's own
    manual style, still fully supported), the same substitution
    `Linen.Data.PDF.Core.Name`'s dropped `IsString` instance uses elsewhere
    in this codebase (prefer the direct idiom over the class-based
    metaprogramming trick).
  - **`FromRow [a]`** (upstream: consume every remaining column as a
    homogeneous list). Dropped: it is a strictly *variable*-arity
    instance, orthogonal to the fixed-arity tuple/`(:.)` instances this
    module's task is scoped to (see the dependency plan's own "up to the
    usual small arity" phrasing); a caller needing it can write
    `numFieldsRemaining >>= fun n => ...` directly against the `RowParser`
    combinators exported here.

  ## Haskell source
  - `Database.SQLite.Simple.FromRow` (`sqlite-simple` package)
-/

import Linen.Database.SQLite.Simple.Ok
import Linen.Database.SQLite.Simple.Internal
import Linen.Database.SQLite.Simple.FromField

namespace Database.SQLite.Simple

-- ────────────────────────────────────────────────────────────────────
-- `RowParser`
-- ────────────────────────────────────────────────────────────────────

/-- A parser that consumes zero or more consecutive `Field`s from a result
    row, threading the current column position and accumulating `Ok`
    failures (see the module doc for how this represents upstream's
    `ReaderT`/`StateT`/`Ok` stack). -/
structure RowParser (α : Type u) where
  /-- Run the parser against a row's `Field`s starting at the given column,
      returning the parsed value and the column position just past what it
      consumed. -/
  run : Array Field → Nat → Ok (α × Nat)

namespace RowParser

instance : Functor RowParser where
  map f p := ⟨fun fields col =>
    match p.run fields col with
    | .ok (a, col') => .ok (f a, col')
    | .errors es => .errors es⟩

instance : Applicative RowParser where
  pure a := ⟨fun _ col => .ok (a, col)⟩
  seq pf px := ⟨fun fields col =>
    match pf.run fields col with
    | .errors es => .errors es
    | .ok (f, col') =>
      match (px ()).run fields col' with
      | .errors es => .errors es
      | .ok (a, col'') => .ok (f a, col'')⟩

end RowParser

/-- Consume one column, decoding it with a caller-supplied `FieldParser`
    rather than an ambient `FromField` instance. -/
def fieldWith (p : FieldParser α) : RowParser α :=
  ⟨fun fields col =>
    if h : col < fields.size then
      match p fields[col] with
      | .ok a => .ok (a, col + 1)
      | .errors es => .errors es
    else
      .errors #[s!"column index {col} out of bounds (row has {fields.size} columns)"]⟩

/-- Consume one column, decoding it via its `FromField` instance. -/
def field [FromField α] : RowParser α := fieldWith fromField

/-- The number of columns not yet consumed by this parser. -/
def numFieldsRemaining : RowParser Nat :=
  ⟨fun fields col => .ok (fields.size - col, col)⟩

/-- Fail the row parse with a message, without consuming a column. -/
def returnRowError (msg : String) : RowParser α :=
  ⟨fun _ _ => .errors #[msg]⟩

-- ────────────────────────────────────────────────────────────────────
-- The `FromRow` class
-- ────────────────────────────────────────────────────────────────────

/-- A collection type that can be decoded from a sequence of `Field`s (a
    full result row). Instances are provided for tuples up to arity 7 (see
    the module doc) and for `Only`/`(:.)`. -/
class FromRow (α : Type u) where
  /-- Parse an entire row. -/
  fromRow : RowParser α

export FromRow (fromRow)

/-- Decode a full row of `Field`s via its `FromRow` instance, failing if any
    column is left unconsumed... no wait, matching upstream: unconsumed
    trailing columns are simply ignored, only under-consumption (running out
    of columns mid-parse) is reported, via `fieldWith`'s own bounds check. -/
def runFromRow [FromRow α] (fields : Array Field) : Ok α :=
  match (fromRow (α := α)).run fields 0 with
  | .ok (a, _) => .ok a
  | .errors es => .errors es

-- ────────────────────────────────────────────────────────────────────
-- `Only` / tuple / `(:.)` instances
-- ────────────────────────────────────────────────────────────────────

instance [FromField a] : FromRow (Types.Only a) where
  fromRow := Types.Only.mk <$> field

instance [FromField a] [FromField b] : FromRow (a × b) where
  fromRow := Prod.mk <$> field <*> field

instance [FromField a] [FromField b] [FromField c] : FromRow (a × b × c) where
  fromRow := (fun x y z => (x, y, z)) <$> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] :
    FromRow (a × b × c × d) where
  fromRow := (fun x y z w => (x, y, z, w)) <$> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] :
    FromRow (a × b × c × d × e) where
  fromRow := (fun v w x y z => (v, w, x, y, z)) <$> field <*> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] [FromField f] :
    FromRow (a × b × c × d × e × f) where
  fromRow := (fun u v w x y z => (u, v, w, x, y, z)) <$>
    field <*> field <*> field <*> field <*> field <*> field

instance [FromField a] [FromField b] [FromField c] [FromField d] [FromField e] [FromField f]
    [FromField g] : FromRow (a × b × c × d × e × f × g) where
  fromRow := (fun t u v w x y z => (t, u, v, w, x, y, z)) <$>
    field <*> field <*> field <*> field <*> field <*> field <*> field

instance [FromRow a] [FromRow b] : FromRow (Types.Cons a b) where
  fromRow := Types.Cons.mk <$> fromRow <*> fromRow

end Database.SQLite.Simple
