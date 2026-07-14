/-
  Linen.Control.Exception.Lens — prisms over `IO.Error`, plus IO-specialized
  `throwing`/`catching`/`handling`/`trying`

  Port of Hackage's `lens-5.3.6`'s `Control.Exception.Lens` (fetched and read
  via the real source, not recalled from memory). Upstream's real module
  provides two things over the `base`/`exceptions`-package `SomeException`
  hierarchy:

  1. `AsIOException`-style `Prism' SomeException e` instances (`_IOException`,
     plus, via `mkErrorTypePrisms`-generated Template Haskell, one prism per
     `GHC.IO.Exception.IOErrorType` constructor: `_AlreadyExists`,
     `_NoSuchThing`, `_ResourceBusy`, …).
  2. Re-exports of `Control.Monad.Error.Lens`'s `throwing`/`catching`/…,
     specialized from an arbitrary `MonadError e m` down to `MonadIO
     m`/`IO` via `exceptions`'s `MonadThrow`/`MonadCatch`.

  **Substitution (`SomeException` hierarchy → `IO.Error`).** `linen` has no
  extensible-exception hierarchy — `Linen.Control.Exception` builds directly
  on Lean core's `IO.Error` (`Init/System/IOError.lean`), a single **closed**
  inductive with one constructor per POSIX error class (`IO.Error.
  alreadyExists`, `.otherError`, `.resourceBusy`, …, plus `.unexpectedEof`
  and `.userError`), rather than GHC's open `SomeException`/`Typeable`
  hierarchy where a program can throw any type in the `Exception` class. This
  module therefore ports upstream's *shape* — "one prism per exception-ish
  case" — as one `Prism' IO.Error _` per `IO.Error` constructor, matching
  upstream's own `IOErrorType`-indexed prisms (`_AlreadyExists`,
  `_NoSuchThing`, …) almost one-for-one, since Lean's `IO.Error` is itself
  already modelled on Haskell's `System.IO.Error.IOErrorType`
  (`Init/System/IOError.lean`'s own comment: "Imitates the structure of
  IOErrorType in Haskell"). Upstream's separate `_IOException :: Prism'
  SomeException IOException` step (unwrapping `SomeException` down to
  `IOException` before the per-`IOErrorType` prisms apply) has no
  counterpart: `linen`'s `IO` already throws `IO.Error` directly, with no
  outer `SomeException` wrapper to unwrap first.

  **Substitution (`MonadThrow`/`MonadCatch` → Lean's `MonadExcept`).** Lean's
  `IO` gets `MonadExcept IO.Error IO` for free (`IO := EIO IO.Error`, and
  `EIO ε` has a `MonadExceptOf ε` instance — see `Linen.Control.Exception`'s
  own module doc comment table). `throwing`/`catching`/`handling`/`trying`
  therefore need no new IO-specific definitions at all: they are simply
  `Linen.Control.Monad.Error.Lens`'s combinators, already generic in any
  `MonadExcept ε m`, instantiated at `ε := IO.Error`, `m := IO` — re-exported
  here via `export` so callers working with `IO.Error` prisms never need to
  import `Control.Monad.Error.Lens` themselves, mirroring how upstream's own
  `Control.Exception.Lens` re-exports `throwing`/`catching`/… from
  `Control.Monad.Error.Lens` rather than redefining them.

  **Scope note (async exceptions, `Handler`, `catches`, `bracket`
  variants).** Upstream also ports `mapException`, `fromException`-based
  `trying`/`catching` for arbitrary `Exception e => e`, GHC's
  asynchronous-exception mask-aware combinators, and an `Exception`-class
  `Handler`/`catches` pair. None of these have a faithful counterpart:
  `Linen.Control.Exception`'s own module doc comment already notes Lean
  exposes no async-exception mechanism, and `IO.Error` (being closed, not an
  open `Exception` class) has no `fromException`/`toException` step to
  generalize over in the first place — skipped, matching that module's own
  documented scope. -/

import Linen.Control.Lens.Prism
import Linen.Control.Monad.Error.Lens

open Control.Lens

namespace Control.Exception.Lens

export Control.Monad.Error.Lens (throwing throwing_ catching catching_ handling handling_ trying)

-- ── per-constructor prisms over `IO.Error` ──────

/-- `_AlreadyExists :: Prism' IO.Error (Option String × UInt32 × String)`:
    focus on `IO.Error.alreadyExists`'s `(filename, osCode, details)`
    payload — upstream's `_AlreadyExists :: Prism' IOErrorType ()`
    specialized here to carry the constructor's actual fields, since Lean's
    `IO.Error.alreadyExists` (unlike GHC's field-free `AlreadyExists`
    `IOErrorType` case) stores its POSIX details directly on the
    constructor. -/
def _AlreadyExists : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .alreadyExists p.1 p.2.1 p.2.2) (fun e => match e with
    | .alreadyExists f c d => some (f, c, d)
    | _ => none)

/-- `_OtherError :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.otherError`. -/
def _OtherError : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .otherError p.1 p.2) (fun e => match e with
    | .otherError c d => some (c, d)
    | _ => none)

/-- `_ResourceBusy :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.resourceBusy`. -/
def _ResourceBusy : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .resourceBusy p.1 p.2) (fun e => match e with
    | .resourceBusy c d => some (c, d)
    | _ => none)

/-- `_ResourceVanished :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.resourceVanished`. -/
def _ResourceVanished : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .resourceVanished p.1 p.2) (fun e => match e with
    | .resourceVanished c d => some (c, d)
    | _ => none)

/-- `_UnsupportedOperation :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.unsupportedOperation`. -/
def _UnsupportedOperation : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .unsupportedOperation p.1 p.2) (fun e => match e with
    | .unsupportedOperation c d => some (c, d)
    | _ => none)

/-- `_HardwareFault :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.hardwareFault`. -/
def _HardwareFault : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .hardwareFault p.1 p.2) (fun e => match e with
    | .hardwareFault c d => some (c, d)
    | _ => none)

/-- `_UnsatisfiedConstraints :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.unsatisfiedConstraints`. -/
def _UnsatisfiedConstraints : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .unsatisfiedConstraints p.1 p.2) (fun e => match e with
    | .unsatisfiedConstraints c d => some (c, d)
    | _ => none)

/-- `_IllegalOperation :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.illegalOperation`. -/
def _IllegalOperation : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .illegalOperation p.1 p.2) (fun e => match e with
    | .illegalOperation c d => some (c, d)
    | _ => none)

/-- `_ProtocolError :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.protocolError`. -/
def _ProtocolError : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .protocolError p.1 p.2) (fun e => match e with
    | .protocolError c d => some (c, d)
    | _ => none)

/-- `_TimeExpired :: Prism' IO.Error (UInt32 × String)`: focus on
    `IO.Error.timeExpired`. -/
def _TimeExpired : Prism' IO.Error (UInt32 × String) :=
  prism' (fun p => .timeExpired p.1 p.2) (fun e => match e with
    | .timeExpired c d => some (c, d)
    | _ => none)

/-- `_Interrupted :: Prism' IO.Error (String × UInt32 × String)`: focus on
    `IO.Error.interrupted`'s `(filename, osCode, details)` payload. -/
def _Interrupted : Prism' IO.Error (String × UInt32 × String) :=
  prism' (fun p => .interrupted p.1 p.2.1 p.2.2) (fun e => match e with
    | .interrupted f c d => some (f, c, d)
    | _ => none)

/-- `_NoFileOrDirectory :: Prism' IO.Error (String × UInt32 × String)`: focus
    on `IO.Error.noFileOrDirectory`. -/
def _NoFileOrDirectory : Prism' IO.Error (String × UInt32 × String) :=
  prism' (fun p => .noFileOrDirectory p.1 p.2.1 p.2.2) (fun e => match e with
    | .noFileOrDirectory f c d => some (f, c, d)
    | _ => none)

/-- `_InvalidArgument :: Prism' IO.Error (Option String × UInt32 × String)`:
    focus on `IO.Error.invalidArgument`. -/
def _InvalidArgument : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .invalidArgument p.1 p.2.1 p.2.2) (fun e => match e with
    | .invalidArgument f c d => some (f, c, d)
    | _ => none)

/-- `_PermissionDenied :: Prism' IO.Error (Option String × UInt32 × String)`:
    focus on `IO.Error.permissionDenied`. -/
def _PermissionDenied : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .permissionDenied p.1 p.2.1 p.2.2) (fun e => match e with
    | .permissionDenied f c d => some (f, c, d)
    | _ => none)

/-- `_ResourceExhausted :: Prism' IO.Error (Option String × UInt32 ×
    String)`: focus on `IO.Error.resourceExhausted`. -/
def _ResourceExhausted : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .resourceExhausted p.1 p.2.1 p.2.2) (fun e => match e with
    | .resourceExhausted f c d => some (f, c, d)
    | _ => none)

/-- `_InappropriateType :: Prism' IO.Error (Option String × UInt32 ×
    String)`: focus on `IO.Error.inappropriateType`. -/
def _InappropriateType : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .inappropriateType p.1 p.2.1 p.2.2) (fun e => match e with
    | .inappropriateType f c d => some (f, c, d)
    | _ => none)

/-- `_NoSuchThing :: Prism' IO.Error (Option String × UInt32 × String)`:
    focus on `IO.Error.noSuchThing`. -/
def _NoSuchThing : Prism' IO.Error (Option String × UInt32 × String) :=
  prism' (fun p => .noSuchThing p.1 p.2.1 p.2.2) (fun e => match e with
    | .noSuchThing f c d => some (f, c, d)
    | _ => none)

/-- `_UnexpectedEof :: Prism' IO.Error Unit`: focus on
    `IO.Error.unexpectedEof`, Lean's nullary end-of-file case (no direct
    upstream `IOErrorType` counterpart carries no fields either — `EOF` maps
    to `System.IO.Error.isEOFError`, not a dedicated constructor upstream,
    but the shape here is the same "recognise a fieldless case" prism as
    `Linen.Control.Lens.Prism`'s own `_Nothing`). -/
def _UnexpectedEof : Prism' IO.Error Unit :=
  prism' (fun _ => .unexpectedEof) (fun e => match e with
    | .unexpectedEof => some ()
    | _ => none)

/-- `_UserError :: Prism' IO.Error String`: focus on `IO.Error.userError`'s
    message — the case produced by `IO.userError`/the `Coe String IO.Error`
    instance, matching upstream's own `userError :: String -> IOError`
    smart constructor. -/
def _UserError : Prism' IO.Error String :=
  prism' .userError (fun e => match e with
    | .userError m => some m
    | _ => none)

/-! ── Note (batch D, `System.IO.Error.Lens`, #62) ──

Hackage's `lens-5.3.6` also ships a separate module, `System.IO.Error.Lens`,
which (per its real source) provides `Prism' IOError ()`-shaped predicates
over GHC's `System.IO.Error` — one per `IOErrorType` classifier
(`_AlreadyExists`, `_NoSuchThing`, `_ResourceBusy`, …), i.e. exactly the same
set of cases as `Control.Exception.Lens`'s own `mkErrorTypePrisms`-derived
prisms above, just accessed through `System.IO.Error`'s naming rather than
`GHC.IO.Exception`'s.

That distinction has no counterpart here: `linen`'s single closed `IO.Error`
sum type (`Init/System/IOError.lean`) already *is* the per-constructor
representation both upstream modules' prisms key off of — there is no
separate `GHC.IO.Exception.IOErrorType` classifier layered on top of a
mutable `IOException` record for `System.IO.Error.Lens`'s prisms to target
instead. Porting `System.IO.Error.Lens` as its own file
(`Linen.System.IO.Error.Lens`) would therefore duplicate every prism already
given above verbatim. Per this batch's own scope note for exactly this case,
no such file is created; `_AlreadyExists` .. `_UserError` above already give
`System.IO.Error.Lens`'s full capability. -/

end Control.Exception.Lens
