/-
  `Control.Monad.Effect.Writer` — the writer effect over `Eff`

  ## Haskell source

  https://hackage.haskell.org/package/freer-simple-1.2.1.1/docs/Control-Monad-Freer-Writer.html
  module #6 of the `FreerSimple` import (see
  `docs/imports/FreerSimple/dependencies.md`).

  ## Substitutions / deviations

  - **No `Monoid` typeclass.** Upstream's `runWriter` requires `Monoid w`. Neither
    Lean's standard library nor `linen` has a general `Monoid` class, so this
    module follows the two precedents already set in this codebase: the general
    `runWriter` takes the monoid's unit and append as explicit arguments (as
    `Codec.Picture.Metadata` does for its `foldMap`), and `runWriterAppend`
    offers the common case through `[Append ω] [Inhabited ω]` (as
    `Data.Foldable.foldMap` does). Nothing is lost: `runWriter empty append` is
    exactly upstream's `runWriter` at the monoid `(empty, append)`.
-/
import Linen.Control.Monad.Effect

namespace Control.Monad.Effect.Writer

open Data.OpenUnion Control.Monad.Effect

-- ── The effect ──────────────────────────────────────────────────────────────

/-- The writer effect: a single request contributing an output of type `ω`. -/
inductive Writer (ω : Type) : Type → Type where
  /-- Append `w` to the accumulated output. -/
  | tell : ω → Writer ω Unit

/-- Locates a `Writer` effect in the row and recovers *which* output type it
    accumulates.

    `ω` is an `outParam` for the same reason as `Error`'s `ε`: `tell w` pins `ω`
    from its argument, but a row with several effects would otherwise leave
    `Member (Writer ?ω) effs` stalled. -/
class HasWriter (effs : List (Type → Type)) (ω : outParam Type) where
  /-- Inject a writer request into the row. -/
  inject : {α : Type} → Writer ω α → Union effs α

/-- The writer effect is the row's head. -/
instance instHasWriterHere {ω : Type} {effs : List (Type → Type)} :
    HasWriter (Writer ω :: effs) ω where
  inject e := .here e

/-- The writer effect is somewhere in the row's tail. -/
instance instHasWriterThere {ω : Type} {eff : Type → Type}
    {effs : List (Type → Type)} [HasWriter effs ω] :
    HasWriter (eff :: effs) ω where
  inject e := .there (HasWriter.inject e)

-- ── Operations ──────────────────────────────────────────────────────────────

/-- Contribute `w` to the accumulated output. -/
def tell {ω : Type} {effs : List (Type → Type)} [hs : HasWriter effs ω]
    (w : ω) : Eff effs Unit :=
  .impure (hs.inject (.tell w)) .protect

-- ── Handlers ────────────────────────────────────────────────────────────────

/-- Accumulate every `tell` with `append`, starting from `empty`, returning the
    value alongside the accumulated output.

    `empty`/`append` stand in for upstream's `Monoid w` constraint. Written as a
    direct recursion rather than via `interpret` because the handler carries the
    accumulator between requests. -/
def runWriter {ω : Type} {effs : List (Type → Type)} {α : Type}
    (empty : ω) (append : ω → ω → ω) :
    Eff (Writer ω :: effs) α → Eff effs (α × ω) :=
  go empty
where
  /-- Accumulate from `acc`. -/
  go {α : Type} (acc : ω) : Eff (Writer ω :: effs) α → Eff effs (α × ω)
    | .protect a  => .protect (a, acc)
    | .impure u k => match u with
      | .here e   => match e with
        | .tell w => go (append acc w) (k ())
      | .there u' => .impure u' (fun b => go acc (k b))

/-- `runWriter` at the `[Append ω] [Inhabited ω]` monoid, covering the common
    cases (`List`, `String`, `Array`) without naming the operations. -/
def runWriterAppend {ω : Type} [Append ω] [Inhabited ω]
    {effs : List (Type → Type)} {α : Type} :
    Eff (Writer ω :: effs) α → Eff effs (α × ω) :=
  runWriter default (· ++ ·)

/-- Run a writer computation, keeping only the accumulated output. -/
def execWriter {ω : Type} {effs : List (Type → Type)} {α : Type}
    (empty : ω) (append : ω → ω → ω) (m : Eff (Writer ω :: effs) α) :
    Eff effs ω :=
  Prod.snd <$> runWriter empty append m

/-- Run a writer computation, keeping only its value. -/
def evalWriter {ω : Type} {effs : List (Type → Type)} {α : Type}
    (empty : ω) (append : ω → ω → ω) (m : Eff (Writer ω :: effs) α) :
    Eff effs α :=
  Prod.fst <$> runWriter empty append m

end Control.Monad.Effect.Writer
