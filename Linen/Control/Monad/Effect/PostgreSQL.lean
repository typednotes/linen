/-
  `Control.Monad.Effect.PostgreSQL` — a capability-restricted PostgreSQL effect

  ## Not a Haskell port

  Like `Control.Monad.Effect.FileSystem` and `Control.Monad.Effect.HTTP`, this
  module is `linen`-original: it has no counterpart in `freer-simple` (or in
  `polysemy`, `effectful`, or `fused-effects`), and is not part of the
  `FreerSimple` import's topological checklist. It is documented as such in
  `docs/imports/FreerSimple/dependencies.md`.

  ## What it adds over an effect row

  `Eff [PostgreSQL cap] α` may talk to exactly one database, on one instance, as
  one role, running only the statement kinds and touching only the tables that
  `cap` names. A type-level effect row can say "this computation uses a
  database"; it cannot say *which* database, *as whom*, or "may read `orders` but
  never write it, and never see `users` at all".

  The restriction comes in three parts, of decreasing strength:

  1. **Connection target** — `host`, `port`, `database`, `user` are fields of the
     capability, and the handler builds its connection string from them alone.
     This is not a proof obligation but a *structural* guarantee: the effect
     carries no connection, so no term of type `Eff [PostgreSQL cap] α` can name
     another database or another role. There is nothing to check because there is
     nothing to say.
  2. **Statement kinds** — `canSelect`/`canInsert`/`canUpdate`/`canDelete`, each
     demanded as a `Prop`-class instance carrying a real proof.
  3. **Table scope** — which tables those statements may touch, demanded as a
     proof obligation that `decide` discharges at the call site.

  ## Why queries are an AST, not strings

  The same reason `FileSystem.Path` is a component list rather than a `String`:
  a scope check has to *reduce*. A `String` of SQL is opaque to `decide` — there
  is no way to ask it which table it touches without parsing it, and no way to
  parse it at elaboration time.

  So a `Query` is structured, and `Query.op`/`Query.table` are projections of the
  value. That buys three things at once:

  - The obligation `cap.permits q = true` reduces, so it is checked when the code
    is elaborated rather than when it runs.
  - The rendered SQL *cannot disagree* with what was checked, because it is
    derived from the same value. A `{ op := .select, sql := "DROP TABLE users" }`
    record is not expressible here.
  - Values are always bound as `$n` parameters, never interpolated, so there is
    no injection surface either.

  There is deliberately **no** `rawSql` escape hatch. Adding one would weaken
  every guarantee above from "cannot be written" to "cannot be written without
  lying", which is not a guarantee. A capability system whose first workaround is
  a string is a capability system in name only.

  ## Soundness and the trust boundary

  Every `PostgreSQL` request carries its proofs, so the handler re-checks
  nothing. What the capability constrains is the *computation*: a term of type
  `Eff [PostgreSQL cap] α` names no connection, so it cannot name another
  database or role. Binding it to a real server is the handler's job, and the
  handler is the trusted boundary — `runPostgreSQL` derives the connection from
  `cap`, but `runSession` hands back a `Session`, and whoever runs that
  `Session` chooses the connection. That is the same trust `runFileSystem`
  places in `IO.FS`; prefer `runPostgreSQL` unless you are deliberately
  composing with an existing session. `dryRun` interprets the same computation
  purely, into the SQL it *would* send — which is what the tests use, so they
  need no live instance.

  ## Backend

  `linen` already has a libpq binding and a `hasql`-style session layer, so per
  AGENTS.md's reuse precedence the handler goes through
  `Database.SQL.Session.query` rather than re-implementing parameter passing and
  error handling on raw `LibPQ.execParams`.
-/
import Linen.Control.Monad.Effect
import Linen.Database.PostgreSQL.LibPQ
import Linen.Database.SQL.Connection
import Linen.Database.SQL.Session

namespace Control.Monad.Effect.PostgreSQL

open Data.OpenUnion Control.Monad.Effect
open Database.PostgreSQL.LibPQ
open Database.SQL.Session (Session SessionError)

-- ── Query syntax ────────────────────────────────────────────────────────────

/-- A table, qualified by schema.

    Two `String` fields rather than one dotted name, for the same reason a path
    is a component list: `"public.orders_archive"` has `"public.orders"` as a
    string prefix but is a different table. -/
structure Table where
  /-- The schema; PostgreSQL's default is `public`. -/
  schema : String := "public"
  /-- The table name. -/
  name : String
  deriving DecidableEq, BEq, Repr

/-- The kind of statement a query is. Each kind is separately grantable. -/
inductive Op
  /-- `SELECT` — reads rows. -/
  | select
  /-- `INSERT` — adds rows. -/
  | insert
  /-- `UPDATE` — modifies existing rows. -/
  | update
  /-- `DELETE` — removes rows. -/
  | delete
  deriving DecidableEq, BEq, Repr

/-- A literal appearing in a query.

    Always sent as a bound parameter, never rendered into the SQL text, so this
    type is the whole of the value language and there is no injection surface. -/
inductive Value
  /-- A text value; PostgreSQL casts it to the column's type. -/
  | text (s : String)
  /-- `NULL`. -/
  | null
  deriving DecidableEq, BEq, Repr

/-- The parameter libpq receives for this value. -/
def Value.toParam : Value → Option String
  | .text s => some s
  | .null   => none

/-- A `WHERE` clause. -/
inductive Cond
  /-- `column = value`. -/
  | eq (column : String) (value : Value)
  /-- Conjunction. -/
  | and (left right : Cond)
  /-- Disjunction. -/
  | or (left right : Cond)
  deriving DecidableEq, BEq, Repr

/-- A statement.

    The constructor fixes the `Op`, and the `Table` is a field, so both are
    projections of the value rather than facts about a string — which is what
    makes `Capability.permits` decidable. -/
inductive Query
  /-- `SELECT columns FROM table [WHERE ...]`; `columns = []` means `*`. -/
  | select (table : Table) (columns : List String) (whre : Option Cond)
  /-- `INSERT INTO table (columns) VALUES (...)`. -/
  | insert (table : Table) (columns : List String) (values : List Value)
  /-- `UPDATE table SET ... [WHERE ...]`. -/
  | update (table : Table) (assignments : List (String × Value)) (whre : Option Cond)
  /-- `DELETE FROM table [WHERE ...]`. -/
  | delete (table : Table) (whre : Option Cond)
  deriving DecidableEq, BEq, Repr

/-- Which kind of statement this is. -/
def Query.op : Query → Op
  | .select .. => .select
  | .insert .. => .insert
  | .update .. => .update
  | .delete .. => .delete

/-- Which table this statement touches. -/
def Query.table : Query → Table
  | .select t .. => t
  | .insert t .. => t
  | .update t .. => t
  | .delete t .. => t

-- ── Rendering ───────────────────────────────────────────────────────────────

/-- Quote an SQL identifier, doubling any embedded quote.

    Identifiers cannot be parameters, so they are quoted rather than bound; the
    doubling is what makes the quoting total rather than merely usual. -/
def quoteIdent (s : String) : String :=
  "\"" ++ s.replace "\"" "\"\"" ++ "\""

/-- Render a table as `"schema"."name"`. -/
def Table.render (t : Table) : String :=
  quoteIdent t.schema ++ "." ++ quoteIdent t.name

/-- Render a condition, numbering its parameters from `next`, and return the
    parameters it bound.

    Structurally recursive on the condition. -/
def Cond.render : Cond → Nat → String × List (Option String)
  | .eq col v, next => (s!"{quoteIdent col} = ${next}", [Value.toParam v])
  | .and l r, next =>
      let (ls, lp) := l.render next
      let (rs, rp) := r.render (next + lp.length)
      (s!"({ls} AND {rs})", lp ++ rp)
  | .or l r, next =>
      let (ls, lp) := l.render next
      let (rs, rp) := r.render (next + lp.length)
      (s!"({ls} OR {rs})", lp ++ rp)

/-- Render the optional `WHERE` clause of a statement. -/
def renderWhere : Option Cond → Nat → String × List (Option String)
  | none, _      => ("", [])
  | some c, next => let (s, ps) := c.render next; (" WHERE " ++ s, ps)

/-- Render a statement to parameterised SQL.

    Every value becomes a `$n` placeholder, so the returned array is exactly what
    `execParams` expects and no value is ever interpolated into the text. -/
def Query.render : Query → String × Array (Option String)
  | .select t cols w =>
      let colText := if cols.isEmpty then "*" else ", ".intercalate (cols.map quoteIdent)
      let (whereText, ps) := renderWhere w 1
      (s!"SELECT {colText} FROM {t.render}{whereText}", ps.toArray)
  | .insert t cols vs =>
      let colText := ", ".intercalate (cols.map quoteIdent)
      let holes := ", ".intercalate ((List.range vs.length).map (fun i => s!"${i + 1}"))
      (s!"INSERT INTO {t.render} ({colText}) VALUES ({holes})",
       (vs.map Value.toParam).toArray)
  | .update t sets w =>
      let setText := ", ".intercalate
        (sets.zipIdx.map (fun (a : (String × Value) × Nat) => s!"{quoteIdent a.1.1} = ${a.2 + 1}"))
      let (whereText, wps) := renderWhere w (sets.length + 1)
      (s!"UPDATE {t.render} SET {setText}{whereText}",
       ((sets.map (fun (a : String × Value) => Value.toParam a.2)) ++ wps).toArray)
  | .delete t w =>
      let (whereText, ps) := renderWhere w 1
      (s!"DELETE FROM {t.render}{whereText}", ps.toArray)

-- ── Results ─────────────────────────────────────────────────────────────────

/-- A decoded result set: libpq's text-format values, `none` for `NULL`. -/
structure ResultSet where
  /-- Column names, in order. -/
  columns : Array String
  /-- Rows, each as many entries as `columns`. -/
  rows : Array (Array (Option String))
  deriving Repr, Inhabited, BEq

/-- The empty result set, which `dryRun` answers `SELECT`s with. -/
def ResultSet.empty : ResultSet := { columns := #[], rows := #[] }

/-- Decode a libpq result into a `ResultSet`. -/
def ResultSet.ofPgResult (r : PgResult) : IO ResultSet := do
  let nf ← nfields r
  let nt ← ntuples r
  let columns ← (List.range nf.toNat).toArray.mapM fun c => fname r c.toUInt32
  let rows ← (List.range nt.toNat).toArray.mapM fun row =>
    (List.range nf.toNat).toArray.mapM fun col => do
      if ← getIsNull r row.toUInt32 col.toUInt32 then
        pure none
      else
        pure (some (← getvalue r row.toUInt32 col.toUInt32))
  return { columns, rows }

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- What a computation is permitted to do to a PostgreSQL instance: where it may
    connect, as whom, which statement kinds it may run, and against which tables.

    `host`/`port`/`database`/`user` pin the connection — the handler builds its
    connection string from these and nothing else, so they restrict the
    computation structurally rather than by a proof obligation. Permission fields
    default to `false`, so `{ canSelect := true, .. }` denies writing by
    construction. `tables` defaults to `[]`, meaning *no table restriction*.

    Declare capability constants with `abbrev`, not `def`: instance resolution
    does not unfold a non-reducible `def`, so `def myCap` leaves
    `CanSelect myCap` unresolvable. -/
structure Capability where
  /-- The server host. -/
  host : String := "localhost"
  /-- The server port. -/
  port : UInt16 := 5432
  /-- The one database this capability opens. -/
  database : String
  /-- The one role it authenticates as. -/
  user : String
  /-- May run `SELECT`. -/
  canSelect : Bool := false
  /-- May run `INSERT`. -/
  canInsert : Bool := false
  /-- May run `UPDATE`. -/
  canUpdate : Bool := false
  /-- May run `DELETE`. -/
  canDelete : Bool := false
  /-- Tables the capability is confined to; `[]` means every table. -/
  tables : List Table := []
  deriving DecidableEq, Repr

/-- Does this capability grant statements of kind `op`? -/
def Capability.allows (cap : Capability) : Op → Bool
  | .select => cap.canSelect
  | .insert => cap.canInsert
  | .update => cap.canUpdate
  | .delete => cap.canDelete

/-- May this capability touch `t` at all? -/
def Capability.permitsTable (cap : Capability) (t : Table) : Bool :=
  cap.tables.isEmpty || cap.tables.contains t

/-- May this capability run `q`?

    Both halves at once: the statement kind must be granted and the table must be
    in scope. Stating it this way makes a rejection provable as a single
    `cap.permits q = false`, whichever half failed. -/
def Capability.permits (cap : Capability) (q : Query) : Bool :=
  cap.allows q.op && cap.permitsTable q.table

/-- `cap` grants `SELECT`. Carries the proof, so it cannot be forged. -/
class CanSelect (cap : Capability) : Prop where
  /-- Evidence that the select bit is set. -/
  proof : cap.canSelect = true

/-- `cap` grants `INSERT`. Carries the proof, so it cannot be forged. -/
class CanInsert (cap : Capability) : Prop where
  /-- Evidence that the insert bit is set. -/
  proof : cap.canInsert = true

/-- `cap` grants `UPDATE`. Carries the proof, so it cannot be forged. -/
class CanUpdate (cap : Capability) : Prop where
  /-- Evidence that the update bit is set. -/
  proof : cap.canUpdate = true

/-- `cap` grants `DELETE`. Carries the proof, so it cannot be forged. -/
class CanDelete (cap : Capability) : Prop where
  /-- Evidence that the delete bit is set. -/
  proof : cap.canDelete = true

instance instCanSelect {h : String} {p : UInt16} {d u : String}
    {i up del : Bool} {ts : List Table} :
    CanSelect ⟨h, p, d, u, true, i, up, del, ts⟩ := ⟨rfl⟩
instance instCanInsert {h : String} {p : UInt16} {d u : String}
    {s up del : Bool} {ts : List Table} :
    CanInsert ⟨h, p, d, u, s, true, up, del, ts⟩ := ⟨rfl⟩
instance instCanUpdate {h : String} {p : UInt16} {d u : String}
    {s i del : Bool} {ts : List Table} :
    CanUpdate ⟨h, p, d, u, s, i, true, del, ts⟩ := ⟨rfl⟩
instance instCanDelete {h : String} {p : UInt16} {d u : String}
    {s i up : Bool} {ts : List Table} :
    CanDelete ⟨h, p, d, u, s, i, up, true, ts⟩ := ⟨rfl⟩

-- ── Scoped queries, for statements not known statically ─────────────────────

/-- A query together with a proof that `cap` allows running it.

    For queries built at compile time the obligation on each operation is
    discharged by `decide` and this type is not needed. It exists for queries
    assembled at runtime — a table chosen from user input, say: `check?`
    validates one and hands back the evidence, so the operations still cannot be
    reached without it. -/
structure ScopedQuery (cap : Capability) where
  /-- The statement. -/
  query : Query
  /-- Evidence that `cap` permits it. -/
  inScope : cap.permits query = true

/-- Validate a runtime query against `cap`, returning the evidence on success. -/
def ScopedQuery.check? (cap : Capability) (q : Query) : Option (ScopedQuery cap) :=
  if h : cap.permits q = true then some ⟨q, h⟩ else none

/-- A validated query is the query that was validated.

    Needed at the call site: `selectAt` still asks for `sq.query.op = .select`,
    and without this the statement kind of a runtime-checked query would be
    opaque even though it was fixed when `check?` was called. -/
theorem ScopedQuery.check?_eq {cap : Capability} {q : Query} {sq : ScopedQuery cap}
    (h : ScopedQuery.check? cap q = some sq) : sq.query = q := by
  unfold ScopedQuery.check? at h
  split at h
  · exact (congrArg ScopedQuery.query (Option.some.inj h)).symm
  · exact absurd h (by simp)

-- ── The effect ──────────────────────────────────────────────────────────────

/-- PostgreSQL statements available under the capability `cap`.

    Each constructor takes a proof that `cap` grants the statement kind *and* a
    proof that `cap` allows the query, so both the permission and the table scope
    are part of what it means for the request to exist. Reading and writing are
    separate constructors because they answer with different types. -/
inductive PostgreSQL (cap : Capability) : Type → Type where
  /-- Run a `SELECT`, answering with its rows. -/
  | query (hp : cap.canSelect = true) (q : Query) (hop : q.op = .select)
      (hs : cap.permits q = true) : PostgreSQL cap ResultSet
  /-- Run a statement that modifies rows, answering with how many it affected. -/
  | command (q : Query) (hop : q.op ≠ .select) (hp : cap.allows q.op = true)
      (hs : cap.permits q = true) : PostgreSQL cap Nat

/-- Locates a `PostgreSQL` effect in the row and recovers *which* capability it
    carries.

    `cap` is an `outParam`: it is an output of resolving against `effs`, not
    something the caller must supply. That is what keeps both the permission
    obligations and the table-scope obligation solvable inside `do`-notation,
    where `cap` would otherwise remain a metavariable. -/
class HasPostgreSQL (effs : List (Type → Type)) (cap : outParam Capability) where
  /-- Inject a statement into the row. -/
  inject : {α : Type} → PostgreSQL cap α → Union effs α

/-- The PostgreSQL effect is the row's head. -/
instance instHasPostgreSQLHere {cap : Capability} {effs : List (Type → Type)} :
    HasPostgreSQL (PostgreSQL cap :: effs) cap where
  inject e := .here e

/-- The PostgreSQL effect is somewhere in the row's tail. -/
instance instHasPostgreSQLThere {cap : Capability} {eff : Type → Type}
    {effs : List (Type → Type)} [HasPostgreSQL effs cap] :
    HasPostgreSQL (eff :: effs) cap where
  inject e := .there (HasPostgreSQL.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- `SELECT columns FROM table [WHERE ...]`; `columns = []` selects `*`.

    Requires `CanSelect cap` and a proof that `cap` allows the resulting query,
    both resolved from the capability the row carries. Under a capability without
    the select bit, or for a table outside its `tables`, this call does not
    elaborate. -/
def select {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] [perm : CanSelect cap]
    (table : Table) (columns : List String := []) (whre : Option Cond := none)
    (hs : cap.permits (.select table columns whre) = true := by decide) :
    Eff effs ResultSet :=
  .impure (h.inject (.query perm.proof (.select table columns whre) rfl hs)) .protect

/-- `INSERT INTO table (columns) VALUES (...)`, answering with the row count.

    Requires `CanInsert cap` and table scope. -/
def insertInto {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] [perm : CanInsert cap]
    (table : Table) (columns : List String) (values : List Value)
    (hs : cap.permits (.insert table columns values) = true := by decide) :
    Eff effs Nat :=
  .impure (h.inject (.command (.insert table columns values) (by simp [Query.op])
    perm.proof hs)) .protect

/-- `UPDATE table SET ... [WHERE ...]`, answering with the row count.

    Requires `CanUpdate cap` and table scope. -/
def update {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] [perm : CanUpdate cap]
    (table : Table) (assignments : List (String × Value)) (whre : Option Cond := none)
    (hs : cap.permits (.update table assignments whre) = true := by decide) :
    Eff effs Nat :=
  .impure (h.inject (.command (.update table assignments whre) (by simp [Query.op])
    perm.proof hs)) .protect

/-- `DELETE FROM table [WHERE ...]`, answering with the row count.

    Requires `CanDelete cap` and table scope. A capability granting select,
    insert and update but not delete makes this call fail to elaborate — the
    distinction a type-level effect row cannot draw. -/
def deleteFrom {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] [perm : CanDelete cap]
    (table : Table) (whre : Option Cond := none)
    (hs : cap.permits (.delete table whre) = true := by decide) :
    Eff effs Nat :=
  .impure (h.inject (.command (.delete table whre) (by simp [Query.op])
    perm.proof hs)) .protect

-- ── Operations on runtime-validated queries ─────────────────────────────────

/-- Run a `SELECT` validated at runtime.

    The `ScopedQuery` supplies the scope evidence, so no `decide` is involved and
    the query need not be statically known; `hop` still pins it to a `SELECT`, so
    this cannot smuggle a write past `CanSelect`. -/
def selectAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] [perm : CanSelect cap] (sq : ScopedQuery cap)
    (hop : sq.query.op = .select) : Eff effs ResultSet :=
  .impure (h.inject (.query perm.proof sq.query hop sq.inScope)) .protect

/-- Run a modifying statement validated at runtime.

    The permission comes from the query's own kind via `cap.allows`, which
    `sq.inScope` already establishes — a scoped query is evidence for both halves,
    so no separate `Can*` instance is needed here. -/
def commandAt {effs : List (Type → Type)} {cap : Capability}
    [h : HasPostgreSQL effs cap] (sq : ScopedQuery cap)
    (hop : sq.query.op ≠ .select) : Eff effs Nat :=
  .impure (h.inject (.command sq.query hop
    (by have := sq.inScope; simp [Capability.permits] at this; exact this.1)
    sq.inScope)) .protect

-- ── Handlers ────────────────────────────────────────────────────────────────

open Database.SQL.Connection in
/-- The libpq connection string this capability authorises, and no other.

    Derived from `cap`'s own fields, which is the whole of the database/user
    restriction: a computation cannot supply a different one because it never
    supplies one at all. -/
def Capability.settings (cap : Capability) (password : String) : Settings :=
  Settings.components (host := cap.host) (port := cap.port.toNat)
    (user := cap.user) (password := password) (database := cap.database)

/-- Interpret a computation into a `Session` on an already-open connection.

    Performs no permission or scope check: every request already carries both
    proofs, so reaching this point means they were established at construction.
    The handler only renders the query and dispatches it.

    The connection comes from whoever runs the resulting `Session`, so this
    function does *not* itself enforce the capability's database and role —
    `runPostgreSQL` is what ties those to `cap`. Use this one only to compose
    with a session you are already running, and pass it a connection acquired
    from `cap.settings`. -/
def runSession (cap : Capability) {α : Type} : Eff [PostgreSQL cap] α → Session α :=
  interpretM fun
    | .query _ q _ _ => do
        let (sql, params) := q.render
        let result ← Database.SQL.Session.Session.query sql params
        liftM (ResultSet.ofPgResult result)
    | .command q _ _ _ => do
        let (sql, params) := q.render
        let result ← Database.SQL.Session.Session.query sql params
        let affected ← liftM (cmdTuples result)
        return affected.toNat?.getD 0

open Database.SQL.Connection in
/-- Run a computation against the one database the capability names.

    Acquires a connection from `cap.settings`, runs, and releases. This is the
    handler to reach for: it cannot be pointed at another database or another
    role without changing `cap`, which changes the type of every computation it
    runs. -/
def runPostgreSQL (cap : Capability) (password : String) {α : Type}
    (action : Eff [PostgreSQL cap] α) : IO (Except SessionError α) := do
  match ← acquire (cap.settings password) with
  | .error (.cantConnect msg) => return .error (.connectionError msg)
  | .ok conn =>
      try
        Database.SQL.Session.Session.run (runSession cap action) conn
      finally
        release conn

/-- Interpret a computation purely, into the SQL it would send.

    Answers every `SELECT` with an empty result set and every modifying statement
    with `0` rows affected, collecting the rendered statements in order. Tests use
    this, so they assert the real SQL and its parameters without a live server —
    the analogue of `Trace.runTracePure`.

    Structurally recursive: the recursive call's argument applies the very
    continuation being destructed, for which `Eff`'s `brecOn` supplies the
    hypothesis. No `partial`, no fuel. -/
def dryRun {cap : Capability} {α : Type} :
    Eff [PostgreSQL cap] α → α × List (String × Array (Option String)) :=
  go []
where
  /-- The accumulator carries the statements seen so far, most recent first. -/
  go (acc : List (String × Array (Option String))) :
      Eff [PostgreSQL cap] α → α × List (String × Array (Option String))
    | .protect a  => (a, acc.reverse)
    | .impure u k => match u with
      | .here e => match e with
        | .query _ q _ _   => go (q.render :: acc) (k ResultSet.empty)
        | .command q _ _ _ => go (q.render :: acc) (k 0)
      | .there u' => u'.elim0

/-- The statements a computation would send, discarding its result. -/
def renderedBy {cap : Capability} {α : Type}
    (action : Eff [PostgreSQL cap] α) : List (String × Array (Option String)) :=
  (dryRun action).2

-- ── Common capabilities ─────────────────────────────────────────────────────

/-- Read-only access to every table of one database, as one role. -/
abbrev readOnly (database user : String) (host : String := "localhost")
    (port : UInt16 := 5432) : Capability :=
  { host := host, port := port, database := database, user := user
  , canSelect := true }

/-- Read-only access confined to a list of tables.

    The interesting shape: this and `readOnly` grant the same *statement kinds*
    and still differ in which *arguments* they admit. -/
abbrev reporting (database user : String) (tables : List Table)
    (host : String := "localhost") (port : UInt16 := 5432) : Capability :=
  { host := host, port := port, database := database, user := user
  , canSelect := true, tables := tables }

/-- Read and write, but **not** delete — the permission split a type-level effect
    row cannot express. -/
abbrev appWriter (database user : String) (tables : List Table := [])
    (host : String := "localhost") (port : UInt16 := 5432) : Capability :=
  { host := host, port := port, database := database, user := user
  , canSelect := true, canInsert := true, canUpdate := true, tables := tables }

end Control.Monad.Effect.PostgreSQL
