/-
  Linen.Database.SQLite.Simple.Types — `Query`, `Null`, `Only`, row-cons

  The foundational types shared across the `sqlite-simple` port (module #5
  of `docs/imports/sqlite-simple/dependencies.md`), with no dependency on any
  other `sqlite-simple` module.

  ## Design

  - `Null` is a placeholder value used by `ToField`/`FromField` (ported
    later) to represent SQL `NULL`; matching upstream, no two `Null`s ever
    compare equal.
  - `Query` wraps a `String` SQL statement. Upstream's `IsString` instance
    (letting an `OverloadedStrings` literal serve as a `Query` directly) has
    no direct Lean counterpart, since Lean has no string-literal-overloading
    class; the substitute is a plain `Coe String Query` instance, which lets
    an ordinary string literal be used wherever a `Query` is expected (the
    same "coercion instead of a literal-overloading class" substitution
    `Linen.Data.PDF.Core.Name`'s module doc uses for its own dropped
    `IsString` instance).
  - `Only` is upstream's separate `Only` package, folded into this module per
    `docs/imports/sqlite-simple/dependencies.md`'s precedence note: a trivial
    1-constructor tuple wrapper used to let a single scalar act as a
    `FromRow`/`ToRow` instance without a bespoke newtype at every call site.
  - `Cons`/`(:.)` is upstream's row-cons operator, used to build ad hoc
    composite rows (e.g. `Int :. String`) out of two independently-decodable
    parts; ported as a plain two-field structure with an infix notation for
    the type former, mirroring how upstream's `data h :. t = h :. t` uses the
    same token for both the type and its single data constructor.
-/

namespace Database.SQLite.Simple.Types

-- ────────────────────────────────────────────────────────────────────
-- Null
-- ────────────────────────────────────────────────────────────────────

/-- A placeholder for the SQL `NULL` value. -/
inductive Null where
  | null
deriving Repr, Inhabited

/-- No two `Null` values are ever considered equal, matching upstream's
    deliberately perverse `Eq Null` instance (`_ == _ = False`). -/
instance : BEq Null where
  beq _ _ := false

-- ────────────────────────────────────────────────────────────────────
-- Query
-- ────────────────────────────────────────────────────────────────────

/-- A SQL query string, wrapped to discourage building one by ad hoc string
    concatenation (a common source of SQL-injection bugs). -/
structure Query where
  fromQuery : String
deriving BEq, Ord, Repr, Inhabited

namespace Query

/-- Build a `Query` from a plain string (upstream's `IsString.fromString`). -/
@[inline] def ofString (s : String) : Query := ⟨s⟩

instance : Coe String Query := ⟨ofString⟩

instance : ToString Query where
  toString q := q.fromQuery

/-- The empty query (upstream's `Monoid` identity). -/
def empty : Query := ⟨""⟩

/-- Concatenate two queries' underlying text (upstream's `Semigroup`/
    `Monoid` `(<>)`). -/
def append (a b : Query) : Query := ⟨a.fromQuery ++ b.fromQuery⟩

instance : Append Query := ⟨append⟩

end Query

-- ────────────────────────────────────────────────────────────────────
-- Only
-- ────────────────────────────────────────────────────────────────────

/-- A one-field tuple wrapper, letting a single scalar value serve as a
    complete row without a bespoke newtype at every call site. -/
structure Only (α : Type u) where
  fromOnly : α
deriving BEq, Ord, Repr, Inhabited

-- ────────────────────────────────────────────────────────────────────
-- Row-cons
-- ────────────────────────────────────────────────────────────────────

/-- Composes two independently-decodable row pieces into one, so a `FromRow`/
    `ToRow` instance can be assembled out of parts without a dedicated
    record type, e.g. `Int :. String`. -/
structure Cons (h t : Type u) where
  car : h
  cdr : t
deriving BEq, Ord, Repr, Inhabited

@[inherit_doc] infixr:35 " :. " => Cons

end Database.SQLite.Simple.Types
