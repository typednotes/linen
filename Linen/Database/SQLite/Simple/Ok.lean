/-
  Linen.Database.SQLite.Simple.Ok — the error-accumulating `Ok` type

  Module #6 of `docs/imports/sqlite-simple/dependencies.md`. Upstream's `Ok`
  is essentially `Either [SomeException] a`, used throughout `FromField`/
  `FromRow` so a failed conversion can report *why*, and so several
  independent failures (e.g. across the columns of a row) can be collected
  rather than only the first.

  ## Design

  Lean has no open, extensible exception hierarchy to accumulate (`Except`'s
  error type is whatever the caller chooses, not a universal `SomeException`),
  so per `docs/imports/sqlite-simple/dependencies.md`'s precedence note this
  is ported around plain `String` messages instead: `Ok α` is either `.ok a`
  or `.errors es` for `es : Array String`, isomorphic to `Except (Array
  String) α` (see `toExcept`/`ofExcept` below for the conversion). A fresh
  inductive type (rather than a bare `abbrev` for `Except (Array String) α`)
  is used so this module can give it upstream's error-*accumulating*
  `Alternative`/`Applicative` behaviour, which differs from `Except`'s own
  (an `Except` short-circuits on its first error; `Ok`'s `Alternative`
  instance concatenates the error lists of two failed alternatives, and its
  `Applicative` instance still short-circuits on the first failing argument,
  exactly matching upstream).

  Upstream's `MonadThrow`/`MonadFail` instances (which build an exception
  value from a `String`/thrown value) collapse into `Ok`'s own error
  representation being `String` already — `Ok.errors #[msg]` is directly the
  substitute for `throwM`/`fail msg`, so no separate class instance is
  needed. Upstream's `ManyErrors` (an `Exception` wrapping the accumulated
  list, for re-throwing as a single exception) has no Lean counterpart to
  re-throw into and is dropped; callers needing the raw messages use
  `toExcept`.
-/

namespace Database.SQLite.Simple

/-- An error-accumulating result: either a successful value, or a
    non-empty-in-practice list of error messages gathered from one or more
    failed conversions. -/
inductive Ok (α : Type u) where
  | ok (a : α)
  | errors (es : Array String)
deriving Repr, Inhabited

namespace Ok

/-- Two `errors` results are considered equal regardless of their message
    lists, matching upstream's deliberately coarse `Eq (Ok a)` instance. -/
instance [BEq α] : BEq (Ok α) where
  beq
    | .errors _, .errors _ => true
    | .ok a, .ok b => a == b
    | _, _ => false

instance : Functor Ok where
  map f
    | .ok a => .ok (f a)
    | .errors es => .errors es

instance : Applicative Ok where
  pure a := .ok a
  seq f x :=
    match f with
    | .errors es => .errors es
    | .ok f =>
      match x () with
      | .errors es => .errors es
      | .ok a => .ok (f a)

/-- `empty` is the identity of `<|>`: no errors at all. `<|>` prefers the
    first successful side, and concatenates both message lists when both
    sides fail — matching upstream's `Alternative Ok` exactly. -/
instance : Alternative Ok where
  failure := .errors #[]
  orElse a b :=
    match a with
    | .ok _ => a
    | .errors as =>
      match b () with
      | .ok r => .ok r
      | .errors bs => .errors (as ++ bs)

instance : Monad Ok where
  bind
    | .errors es, _ => .errors es
    | .ok a, f => f a

/-- Fail with a single error message — the substitute for upstream's
    `MonadFail`/`MonadThrow` instances (see the module doc). -/
def fail (msg : String) : Ok α := .errors #[msg]

/-- Convert to a plain `Except`, collapsing the accumulated messages into its
    single error value. -/
def toExcept : Ok α → Except (Array String) α
  | .ok a => .ok a
  | .errors es => .error es

/-- The inverse of `toExcept`. -/
def ofExcept : Except (Array String) α → Ok α
  | .ok a => .ok a
  | .error es => .errors es

-- ── Proofs ──

theorem toExcept_ofExcept (e : Except (Array String) α) :
    (ofExcept e).toExcept = e := by
  cases e <;> rfl

end Ok
end Database.SQLite.Simple
