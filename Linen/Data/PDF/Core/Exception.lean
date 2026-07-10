/-
  Data.PDF.Core.Exception — structured PDF parse-error reporting

  Ports `Pdf.Core.Exception` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Exception.hs`),
  module 2 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  Upstream distinguishes two exception types via `Control.Exception`'s
  extensible-exception hierarchy:

  - `Corrupted message details` — the PDF file itself is malformed.
  - `Unexpected message details` — an internal invariant was violated
    (probably an API-misuse bug, not a file problem).

  Both carry a general `message` plus a `details` list that grows as the
  exception is rethrown up the call stack with more context (`message`
  below).

  ## Design

  Lean's `IO.Error` is a **closed** inductive (`Init.System.IOError`), not an
  open, `Typeable`-indexed hierarchy the way Haskell's `SomeException` is —
  there is no Lean equivalent of `catches [Handler ..., Handler ...]`
  dispatching on the *type* of the thrown value. So `Corrupted`/`Unexpected`
  are represented here as a small tagged `Exc` record that gets *rendered*
  into an `IO.Error.userError` string at the point it's actually thrown
  (`corrupted`/`unexpected` below); `message` degrades to prepending context
  to whatever `userError` string is currently in flight, regardless of which
  of the two tags produced it — the closest total, honest analogue of
  upstream's typed rethrow-with-context idiom that `IO.Error` allows. -/
namespace Data.PDF.Core.Exception

/-- Which of the two upstream exception constructors produced this
    exception: a corrupted file, or an internal (API-misuse) invariant
    violation. -/
inductive Kind where
  /-- The PDF file is malformed. -/
  | corrupted
  /-- Something unexpected happened — probably an API misuse, not a file
      problem. -/
  | unexpected
deriving BEq, Repr

/-- A structured PDF exception: a `Kind` tag, a general `message`, and a
    list of `details` accumulated as the exception propagates
    (`message` prepends to this list on rethrow). -/
structure Exc where
  /-- Which upstream exception constructor this represents. -/
  kind : Kind
  /-- The general message. -/
  message : String
  /-- Context accumulated by successive `message` wraps, innermost first. -/
  details : List String
deriving Repr

/-- Render an `Exc` to a single human-readable string: the tag, the general
    message, then every accumulated detail. -/
def render (e : Exc) : String :=
  let tag := match e.kind with
    | .corrupted => "Corrupted"
    | .unexpected => "Unexpected"
  if e.details.isEmpty then
    s!"{tag}: {e.message}"
  else
    s!"{tag}: {e.message} ({String.intercalate "; " e.details})"

/-- Throw a "file is corrupted" error, e.g. `sure`/parsing failures below. -/
def corrupted (message : String) (details : List String := []) : IO.Error :=
  .userError (render ⟨.corrupted, message, details⟩)

/-- Throw an "unexpected/internal invariant violated" error. -/
def unexpected (message : String) (details : List String := []) : IO.Error :=
  .userError (render ⟨.unexpected, message, details⟩)

/-- We are sure the `Except` is `.ok`; otherwise throw `corrupted`.
    Mirrors upstream's `sure :: Either String a -> IO a`. -/
def sure (e : Except String α) : IO α :=
  match e with
  | .ok a => pure a
  | .error err => throw (corrupted err)

/-- Catch any error thrown by `action` and prepend `msg` to it as extra
    context before rethrowing, then run `action`. Mirrors upstream's
    `message :: String -> IO a -> IO a`, specialised to `IO.Error.userError`
    messages (see the module doc-comment for why: `IO.Error` isn't an open
    hierarchy, so the `Corrupted`/`Unexpected` tag itself can't be recovered
    from an already-thrown error — only its rendered string can). -/
def message (msg : String) (action : IO α) : IO α :=
  MonadExcept.tryCatch action fun e =>
    match e with
    | .userError s => throw (IO.Error.userError s!"{msg}: {s}")
    | other => throw other

end Data.PDF.Core.Exception
