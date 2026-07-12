/-
  Linen.Database.SQLite.Simple.Function — user-defined scalar SQL functions

  Module #16 of `docs/imports/sqlite-simple/dependencies.md`, on module #3
  (`Linen.Database.SQLite.Direct`), module #4 (`Linen.Database.SQLite`, for
  `createFunction`/`deleteFunction`/`funcArgValue`/`funcResultValue`), module
  #9 (`…Internal`, for `Connection`/`Field`), module #10 (`…ToField`), and
  module #11 (`…FromField`).

  ## The arity problem, and how this port resolves it

  Upstream's `Function` typeclass is a genuinely polymorphic-arity
  dispatcher, built on GHC's overlapping-instance extensions:

  ```haskell
  class Function a where
    apply :: a -> FunctionArgs -> FunctionResult
  instance ToField a             => Function a          -- 0-ary result
  instance ToField a             => Function (IO a)      -- IO-wrapped result
  instance (FromField a, Function b) => Function (a -> b) -- one more argument
  ```

  `instance Function a` and `instance Function (a -> b)` overlap at the
  *type* level for any `a` that is itself a function type, resolved only by
  GHC's `OVERLAPPING`/left-to-right-instance-search extensions — there is no
  Lean analogue: Lean's typeclass resolution has no notion of one instance
  head being "more specific" than another when both could apply, and a
  naive port would make every arity's instance ambiguous with every other.

  This is the same shape of problem already solved for `FromRow`/`ToRow` (see
  those modules' own docs): upstream itself resolves arbitrary tuple arity
  through a mechanism (`Generic`-deriving there, `Function`'s recursive
  instance here) that Lean cannot replicate, so both ports substitute a
  **fixed arity cutoff** with one hand-written definition per arity. `ToRow`/
  `FromRow` cut off at 7 (matching upstream's own `deriving`-block cutoff);
  there is no analogous upstream cutoff to mirror here (upstream's own
  mechanism is unbounded), so this port instead uses the same cutoff already
  established for this codebase's other C-callback-arity-bounded surface —
  `lean_apply_4`, used by the `xFunc` trampoline itself
  (`ffi/sqlite3_shim.c`), applies exactly 4 arguments (3 real + the `IO`
  world token) to the *registered* closure, but that closure is always
  `FuncContext → FuncArgs → UInt32 → IO Unit` regardless of the *user*
  function's arity — the real limiting factor is purely how many
  `FromField`-decoded arguments this module chooses to unpack from
  `FuncArgs` by hand. **0 to 3 arguments** is supported below (`createFunction0`
  through `createFunction3`); a caller needing more can register directly
  against `Database.SQLite3.createFunction` (module #4) and decode
  `FuncArgs` itself the same way these helpers do.

  ## Design

  Each `createFunctionN` wraps a plain Lean function of `N` `FromField`
  arguments returning `IO r` (`ToField r`) into the
  `FuncContext → FuncArgs → UInt32 → IO Unit` shape
  `Database.SQLite3.createFunction` (module #4) expects: it decodes each
  argument via `funcArgValue` (module #4) wrapped as a `Field` (with a
  synthetic column index and no name, matching how `Internal.currentRowFields`
  already builds one for a normal query column) so `FromField.fromField` can
  be reused unmodified, applies the user's function, and reports the result
  via `funcResultValue`. A `FromField`/`ToField` conversion failure — or any
  other exception the user's function itself throws — simply propagates as
  a thrown `IO.userError`; per `ffi/sqlite3_shim.c`'s own trampoline doc,
  SQLite then sees `SQL NULL` reported for that row, exactly matching
  upstream's own catch-all `-> funcResultNull` fallback for an exception
  escaping a registered function.

  `deleteFunction` is a thin re-export of `Database.SQLite3.deleteFunction`
  under a `Connection`-taking signature, for symmetry with `createFunctionN`
  (upstream's own `deleteFunction` is likewise arity-independent — nothing
  about the arity dispatch problem above applies to removal).

  ## Haskell source
  - `Database.SQLite.Simple.Function` (`sqlite-simple` package)
-/

import Linen.Database.SQLite.Simple.Internal
import Linen.Database.SQLite.Simple.ToField
import Linen.Database.SQLite.Simple.FromField

namespace Database.SQLite.Simple

open Database.SQLite3.Bindings.Types (FuncContext FuncArgs)

-- ────────────────────────────────────────────────────────────────────
-- Argument decoding
-- ────────────────────────────────────────────────────────────────────

/-- Decode argument `idx` of a scalar-function call via `FromField`,
    throwing `IO.userError` on failure (see the module doc: this is what
    ultimately surfaces to SQLite as a reported `NULL` for the row). -/
private def decodeArg [FromField a] (args : FuncArgs) (idx : UInt32) : IO a := do
  let value ← Database.SQLite3.funcArgValue args idx
  let field : Field := { result := value, column := idx.toNat }
  match FromField.fromField field with
  | .ok a => pure a
  | .errors es => throw (IO.userError s!"function argument {idx} conversion failed: {es}")

/-- Report `r` as the result of a scalar-function call via `ToField`. -/
private def setResult [ToField r] (ctx : FuncContext) (r : r) : IO Unit :=
  Database.SQLite3.funcResultValue ctx (ToField.toField r)

-- ────────────────────────────────────────────────────────────────────
-- Registration, by arity (see the module doc for the 0..3 cutoff)
-- ────────────────────────────────────────────────────────────────────

/-- Register a nullary scalar SQL function. -/
def createFunction0 [ToField r] (conn : Connection) (name : String) (deterministic : Bool)
    (f : IO r) : IO Unit :=
  Database.SQLite3.createFunction conn.connectionHandle name 0 deterministic
    fun ctx _args _argc => do setResult ctx (← f)

/-- Register a unary scalar SQL function. -/
def createFunction1 [FromField a] [ToField r] (conn : Connection) (name : String)
    (deterministic : Bool) (f : a → IO r) : IO Unit :=
  Database.SQLite3.createFunction conn.connectionHandle name 1 deterministic
    fun ctx args _argc => do
      let x ← decodeArg args 0
      setResult ctx (← f x)

/-- Register a binary scalar SQL function. -/
def createFunction2 [FromField a] [FromField b] [ToField r] (conn : Connection) (name : String)
    (deterministic : Bool) (f : a → b → IO r) : IO Unit :=
  Database.SQLite3.createFunction conn.connectionHandle name 2 deterministic
    fun ctx args _argc => do
      let x ← decodeArg args 0
      let y ← decodeArg args 1
      setResult ctx (← f x y)

/-- Register a ternary scalar SQL function. -/
def createFunction3 [FromField a] [FromField b] [FromField c] [ToField r]
    (conn : Connection) (name : String) (deterministic : Bool) (f : a → b → c → IO r) :
    IO Unit :=
  Database.SQLite3.createFunction conn.connectionHandle name 3 deterministic
    fun ctx args _argc => do
      let x ← decodeArg args 0
      let y ← decodeArg args 1
      let z ← decodeArg args 2
      setResult ctx (← f x y z)

/-- Remove a scalar function registered with `nArg` arguments. -/
def deleteFunction (conn : Connection) (name : String) (nArg : Int32) : IO Unit :=
  Database.SQLite3.deleteFunction conn.connectionHandle name nArg

end Database.SQLite.Simple
