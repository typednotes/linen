/-
  Tests for `Linen.Control.Monad.Effect.PostgreSQL`.

  Covers all three parts of the capability, as `FileSystemTest` covers two:

  - **Connection target** — `host`/`port`/`database`/`user` are fields of the
    capability, and `Capability.settings` derives the connection string from them
    alone. There is nothing to reject here, so the tests assert the positive
    fact: the string a capability yields, and that no other is reachable.
  - **Statement kinds** — which operations are allowed. A withheld kind makes the
    call fail to elaborate, asserted with `#guard_msgs`.
  - **Table scope** — which tables those statements may touch. A table outside
    the capability's list makes the obligation `cap.permits q = true`
    *unsatisfiable*, which is asserted directly: proving it equals `false` shows
    no proof of `= true` can exist. That is a stronger statement than matching an
    error message, and it does not rot across compiler versions the way a message
    match does.

  Plus the rendered SQL for every statement shape, through `dryRun`, so no test
  needs a live PostgreSQL instance.
-/
import Linen.Control.Monad.Effect.PostgreSQL

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.PostgreSQL

namespace Tests.Control.Monad.Effect.PostgreSQL

abbrev orders : Table := { name := "orders" }
abbrev lineItems : Table := { name := "line_items" }
abbrev users : Table := { name := "users" }
abbrev auditLog : Table := { schema := "audit", name := "log" }

-- ── Identifier quoting and rendering ────────────────────────────────────────

#guard quoteIdent "orders" == "\"orders\""
#guard quoteIdent "we\"ird" == "\"we\"\"ird\""
#guard orders.render == "\"public\".\"orders\""
#guard auditLog.render == "\"audit\".\"log\""

-- A table is two components, not one dotted string, for the same reason a path
-- is a component list: `public.orders_archive` has `public.orders` as a string
-- prefix but is a different table.
#guard orders != ({ name := "orders_archive" } : Table)

-- Every value is a bound parameter; none is ever interpolated into the text.
#guard Query.render (.select orders [] none)
  == ("SELECT * FROM \"public\".\"orders\"", #[])
#guard Query.render (.select orders ["id", "total"] none)
  == ("SELECT \"id\", \"total\" FROM \"public\".\"orders\"", #[])
#guard Query.render (.select orders ["id"] (some (.eq "status" (.text "paid"))))
  == ("SELECT \"id\" FROM \"public\".\"orders\" WHERE \"status\" = $1", #[some "paid"])
#guard Query.render (.insert orders ["id", "total"] [.text "1", .null])
  == ("INSERT INTO \"public\".\"orders\" (\"id\", \"total\") VALUES ($1, $2)",
      #[some "1", none])
#guard Query.render (.update orders [("total", .text "9")] (some (.eq "id" (.text "1"))))
  == ("UPDATE \"public\".\"orders\" SET \"total\" = $1 WHERE \"id\" = $2",
      #[some "9", some "1"])
#guard Query.render (.delete orders (some (.eq "id" (.text "7"))))
  == ("DELETE FROM \"public\".\"orders\" WHERE \"id\" = $1", #[some "7"])

-- Parameter numbering continues across a compound condition, and across the
-- `SET` list of an `UPDATE` into its `WHERE`.
#guard Query.render (.select orders [] (some (.and (.eq "a" (.text "1")) (.eq "b" (.text "2")))))
  == ("SELECT * FROM \"public\".\"orders\" WHERE (\"a\" = $1 AND \"b\" = $2)",
      #[some "1", some "2"])
#guard Query.render (.select orders [] (some (.or (.eq "a" (.text "1"))
        (.and (.eq "b" (.text "2")) (.eq "c" .null)))))
  == ("SELECT * FROM \"public\".\"orders\" WHERE (\"a\" = $1 OR (\"b\" = $2 AND \"c\" = $3))",
      #[some "1", some "2", none])
#guard Query.render (.update orders [("x", .text "1"), ("y", .text "2")]
        (some (.eq "id" (.text "3"))))
  == ("UPDATE \"public\".\"orders\" SET \"x\" = $1, \"y\" = $2 WHERE \"id\" = $3",
      #[some "1", some "2", some "3"])

-- A quote inside an identifier is doubled rather than escaping the quoting.
#guard Query.render (.select { name := "we\"ird" } [] none)
  == ("SELECT * FROM \"public\".\"we\"\"ird\"", #[])

-- `op` and `table` are projections of the value, which is what makes the scope
-- check decidable — the rendered SQL is derived from the same value, so the two
-- cannot disagree.
#guard (Query.select orders [] none).op == Op.select
#guard (Query.insert orders [] []).op == Op.insert
#guard (Query.update orders [] none).op == Op.update
#guard (Query.delete orders none).op == Op.delete
#guard (Query.delete auditLog none).table == auditLog

-- ── The connection target ───────────────────────────────────────────────────

abbrev shopRead : Capability := readOnly "shop" "analytics"

-- The capability names the instance, the database and the role; the handler
-- builds its connection string from these and nothing else, so a computation
-- typed `Eff [PostgreSQL shopRead] α` has no way to reach another database or
-- authenticate as another user. There is no obligation to discharge because
-- there is no term that could express the alternative.
#guard (shopRead.settings "secret").connString
  == "host=localhost port=5432 user=analytics password=secret dbname=shop"

abbrev remote : Capability :=
  reporting "warehouse" "reader" [orders] (host := "db.internal") (port := 6543)

#guard (remote.settings "pw").connString
  == "host=db.internal port=6543 user=reader password=pw dbname=warehouse"

-- Two capabilities over the same instance differ in the database and role they
-- can ever reach.
#guard shopRead.database != remote.database
#guard shopRead.user != remote.user

-- ── Statement-kind obligations ──────────────────────────────────────────────

abbrev app : Capability := appWriter "shop" "svc"

-- A granted permission is discharged by `rfl` on a concrete capability.
example : shopRead.canSelect = true := rfl
example : app.canInsert = true := rfl
example : app.canUpdate = true := rfl

-- A withheld permission is provably absent, not merely unproven.
example : shopRead.canInsert = false := rfl
example : shopRead.canDelete = false := rfl
example : app.canDelete = false := rfl

-- The instances exist exactly when the corresponding bit is set.
example : CanSelect shopRead := inferInstance
example : CanInsert app := inferInstance
example : CanUpdate app := inferInstance

-- The proof carried by an instance really is the field equation — this is why an
-- instance cannot be forged for a capability lacking the bit.
example : CanSelect.proof (cap := shopRead) = (rfl : shopRead.canSelect = true) := rfl

-- `allows` agrees with the bits.
#guard shopRead.allows .select
#guard !shopRead.allows .delete
#guard app.allows .update
#guard !app.allows .delete

-- ── Positive: permitted statements elaborate ────────────────────────────────

-- `cap` is inferred from the row rather than supplied — what the `outParam` on
-- `HasPostgreSQL` buys — and it works inside `do`-notation.
example : Eff [PostgreSQL shopRead] ResultSet := select orders ["id", "total"]

example : Eff [PostgreSQL app] Nat := insertInto orders ["id"] [.text "1"]

example : Eff [PostgreSQL app] Nat :=
  update orders [("total", .text "9")] (some (.eq "id" (.text "1")))

example : Eff [PostgreSQL app] Nat := do
  let _ ← select orders ["id"]
  let _ ← insertInto orders ["id"] [.text "2"]
  update orders [("total", .text "0")] none

-- ── Negative: withheld statement kinds do not elaborate ─────────────────────

-- Inserting under a read-only capability. `PostgreSQL` *is* in the row, so a
-- name-only effect row would admit this call; the capability value rejects it.
-- Two errors follow, not one: `permits` folds the statement kind in alongside
-- the table scope, so a withheld kind trips the obligation as well as the
-- instance. That is what lets a `ScopedQuery` be evidence for both halves at
-- once, which `commandAt` relies on.
/--
error: failed to synthesize instance of type class
  CanInsert shopRead

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  shopRead.permits (Query.insert orders ["id"] [Value.text "1"]) = true
is false
-/
#guard_msgs in
#check (insertInto (effs := [PostgreSQL shopRead]) orders ["id"] [.text "1"])

-- Deleting under a capability that may select, insert and update: the four
-- statement kinds are split within a single effect.
/--
error: failed to synthesize instance of type class
  CanDelete app

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  app.permits (Query.delete orders none) = true
is false
-/
#guard_msgs in
#check (deleteFrom (effs := [PostgreSQL app]) orders)

-- The permission is provably absent, independently of any error message.
theorem no_delete_under_app : app.permits (.delete orders none) ≠ true := by decide
theorem no_insert_under_read :
    shopRead.permits (.insert orders ["id"] [.text "1"]) ≠ true := by decide

-- ── Table scope ─────────────────────────────────────────────────────────────

abbrev rep : Capability := reporting "shop" "analytics" [orders, lineItems]

-- Inside the scope: the obligation holds, so the operations elaborate.
#guard rep.permits (.select orders ["id"] none)
#guard rep.permits (.select lineItems [] none)
#guard rep.permitsTable orders
#guard rep.permitsTable lineItems

example : Eff [PostgreSQL rep] ResultSet := select orders ["id"]
example : Eff [PostgreSQL rep] ResultSet :=
  select lineItems ["sku"] (some (.eq "order_id" (.text "1")))
example : Eff [PostgreSQL rep] (Array (Option String)) := do
  let a ← select orders ["id"]
  let b ← select lineItems ["sku"]
  pure (a.rows.flatten ++ b.rows.flatten)

-- Outside the scope the obligation is provably *false*, so no proof of `= true`
-- exists and the call cannot be written at all. `SELECT` is granted here — it is
-- the *argument* that is refused, which is the half no type-level row can reach.
#guard rep.allows .select
#guard !rep.permitsTable users
#guard !rep.permits (.select users [] none)
example : rep.permits (.select users [] none) = false := rfl
theorem no_select_unlisted_table :
    rep.permits (.select users [] none) ≠ true := by decide

-- A different schema is a different table, even under the same name.
#guard !rep.permitsTable { schema := "archive", name := "orders" }
theorem no_cross_schema :
    rep.permits (.select { schema := "archive", name := "orders" } [] none) ≠ true := by decide

-- An empty `tables` means unrestricted, so a permission-only capability admits
-- any table — this is what keeps `readOnly` usable.
#guard shopRead.tables.isEmpty
#guard shopRead.permitsTable users
example (t : Table) : shopRead.permitsTable t = true := rfl

-- Two capabilities granting the *same statement kinds* but differing in table
-- scope: the distinction a type-level effect row cannot draw at all.
example : shopRead.canSelect = rep.canSelect := rfl
example : shopRead.permits (.select users [] none) = true := rfl
example : rep.permits (.select users [] none) = false := rfl

-- ── Runtime-validated queries ───────────────────────────────────────────────

-- A query assembled at runtime — a table chosen from user input, say — cannot be
-- checked by `decide`; `check?` validates it and hands back the evidence.
#guard (ScopedQuery.check? rep (.select orders [] none)).isSome
#guard (ScopedQuery.check? rep (.select users [] none)).isNone
#guard (ScopedQuery.check? rep (.delete orders none)).isNone

-- The evidence a `ScopedQuery` carries is exactly the operations' obligation, so
-- `selectAt` needs no `decide`.
example (sq : ScopedQuery rep) (h : sq.query.op = .select) :
    Eff [PostgreSQL rep] ResultSet := selectAt sq h

-- Choosing a table at runtime and refusing the ones outside the capability.
def readTable (name : String) : Eff [PostgreSQL rep] (Option ResultSet) :=
  match h : ScopedQuery.check? rep (.select { name := name } [] none) with
  | some sq => some <$> selectAt sq (by rw [ScopedQuery.check?_eq h]; rfl)
  | none    => pure none

-- ── End to end: the SQL a computation would send ────────────────────────────

/-- Read the open orders, then record that they were seen. -/
def sweep : Eff [PostgreSQL app] Nat := do
  let _ ← select orders ["id", "total"] (some (.eq "status" (.text "open")))
  let _ ← insertInto orders ["status"] [.text "swept"]
  update orders [("status", .text "seen")] (some (.eq "status" (.text "open")))

#guard renderedBy sweep ==
  [ ("SELECT \"id\", \"total\" FROM \"public\".\"orders\" WHERE \"status\" = $1",
     #[some "open"])
  , ("INSERT INTO \"public\".\"orders\" (\"status\") VALUES ($1)", #[some "swept"])
  , ("UPDATE \"public\".\"orders\" SET \"status\" = $1 WHERE \"status\" = $2",
     #[some "seen", some "open"]) ]

-- `dryRun` answers reads with an empty result set and writes with `0`, so the
-- computation's own value comes back too.
#guard (dryRun sweep).1 == 0
#guard (dryRun (select (cap := rep) orders ["id"])).1 == ResultSet.empty
#guard (dryRun (cap := rep) (pure 7 : Eff [PostgreSQL rep] Nat)) == (7, [])

end Tests.Control.Monad.Effect.PostgreSQL
