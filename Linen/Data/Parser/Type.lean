/-
  Linen.Data.Parser.Type — the backtracking streaming-parser type (`Parser`)

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Parser.Type`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Parser/Type.hs),
  module #25 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  A `Parser a m b` is a *fold that can fail and backtrack*. It generalizes
  `Fold` (#14): where a fold's step always makes progress on every input, a
  parser's step returns a richer `Step` command telling the driver how to move
  the stream position (forward, stay, or backtrack) and whether the parse has
  succeeded (`SDone`), can accept more input (`SPartial`), needs more input
  (`SContinue`), or failed (`SError`). This buffering/replay machinery is what
  lets parsers `alt`ernate and take-while.

  The parser is represented as three actions over an existentially hidden state
  `s` (exactly as `Fold`):

  * `initial : m (Initial s b)` — start (`IPartial`/`IDone`/`IError`);
  * `step : s → a → m (Step s b)` — consume one element;
  * `extract : s → m (Final s b)` — read the result at end of input.

  ## Step / stream-position semantics (upstream `Step`)

  The count `n : Int` in `SPartial n`, `SContinue n`, `SDone n` adjusts the
  stream position: new position = current position + `n`. `n = 1` consumes the
  current element, `n = 0` re-presents it, `n < 0` backtracks. `SPartial`
  additionally drops the backtracking buffer (cannot backtrack before this
  point); `SContinue` retains it. This is streamly-0.3.1's *new* `Step` shape
  (`SPartial`/`SContinue`/`SDone`/`SError`); the older `Partial`/`Continue`/
  `Done` are only deprecated pattern synonyms upstream and are **not** ported.

  ## Substitutions / deviations

  - **`Fuse` annotations dropped** (GHC-plugin no-ops; see the plan's
    `fusion-plugin-types` drop). The local fusion-state records
    (`SeqParseState`/`SeqAState`/`AltParseState`/`Fused3`) are plain inductives.
  - **`undefined` step/extract in `fromPure`/`fromEffect`/`die`/`dieM` made
    total.** Upstream builds `Parser undefined (pure (IDone b)) undefined`; the
    `undefined` fields are unreachable (initial is `IDone`/`IError`). Per
    AGENTS.md (no partial landmines) they are given total, sensible bodies.
  - **`concatMap` and the `Monad`/`MonadFail`/`MonadIO` instances dropped — a
    universe wall.** Upstream's `concatMap` state `ConcatParseState` has a
    constructor `forall s. ConcatParseR (s -> a -> m (Step s b)) …` that
    existentially quantifies over another `Type u` inside the *state* type; that
    pushes the state into `Type (u+1)`, but a `Parser`'s state field is
    `s : Type u`. This is a genuine universe-polymorphism limit (a parser whose
    residual continuation type is chosen at run time), exactly like the dropped
    `Fold.duplicate`, so `concatMap`/`>>=` are omitted rather than weakened. The
    `Functor`, `Applicative` (`splitWith`/`split_`) and `Alternative`
    (`die`/`alt`) instances are all universe-clean and are kept.
  - **`splitMany`/`splitSome` are `unsafe`.** They contain the mutually
    recursive `handleCollect`/`runCollectorWith` loop (upstream flags "there is
    mutual recursion here"): a parser that repeatedly returns `IDone` without
    consuming input loops with no structural/well-founded measure. Per AGENTS.md
    this uses `unsafe` (the sanctioned alternative to `partial`/fuel — the same
    call `Stream.Type`/`Stream.Eliminate` make), never `sorry`.
  - **`noErrorUnsafeSplitWith`/`noErrorUnsafeSplit_`/`noErrorUnsafeConcatMap`,
    `splitManyPost`, `localReaderT`, the deprecated `Partial`/`Continue`/`Done`
    pattern synonyms, and `negateDirection`** are deferred: they are peripheral
    performance variants / `ReaderT`-specific / deprecated-compat helpers, out
    of this batch's primary scope (matching the plan's own scoping convention).
  - **`ParseError`/`ParseErrorPos`** are ported as plain data (Lean has no GHC
    `Exception` class); parser drivers surface failure via `Except String` (see
    `Linen.Data.Parser`).
-/

import Linen.Data.Bifunctor
import Linen.Data.Fold.Type

namespace Data.Parser

open Data.Fold (Fold)

-- ── The Initial / Step / Final command types ─────────────────────────────────

/-- The result of a parser's `initial` action: wait for input (`IPartial`),
    return a result with no input (`IDone`), or fail (`IError`). -/
inductive Initial (s b : Type u) where
  | IPartial : s → Initial s b
  | IDone : b → Initial s b
  | IError : String → Initial s b
  deriving Repr, BEq

/-- The result of a parser's `step`. The `Int` count adjusts the stream
    position (see the module header):

    * `SPartial n s` — a result is available; move by `n`, drop the backtrack
      buffer before the new position.
    * `SContinue n s` — need more input; move by `n`, retain the buffer.
    * `SDone n b` — finished with result `b`; final position moves by `n`.
    * `SError err` — failed without a result. -/
inductive Step (s b : Type u) where
  | SPartial : Int → s → Step s b
  | SContinue : Int → s → Step s b
  | SDone : Int → b → Step s b
  | SError : String → Step s b
  deriving Repr, BEq

/-- The result of a parser's `extract` (end of input): finished (`FDone`), still
    needs input (`FContinue`), or failed (`FError`). -/
inductive Final (s b : Type u) where
  | FDone : Int → b → Final s b
  | FContinue : Int → s → Final s b
  | FError : String → Final s b
  deriving Repr, BEq

-- ── Functor / Bifunctor on Initial ───────────────────────────────────────────

namespace Initial

/-- `first` maps on `IPartial`, `second` maps on `IDone`. -/
@[inline] def bimap (f : s → t) (g : b → c) : Initial s b → Initial t c
  | .IPartial a => .IPartial (f a)
  | .IDone b => .IDone (g b)
  | .IError err => .IError err

/-- Map over the state (`IPartial`) only. -/
@[inline] def mapFst (f : s → t) : Initial s b → Initial t b
  | .IPartial a => .IPartial (f a)
  | .IDone b => .IDone b
  | .IError err => .IError err

/-- Map over the result (`IDone`) only. -/
@[inline] def mapSnd (g : b → c) : Initial s b → Initial s c
  | .IPartial a => .IPartial a
  | .IDone b => .IDone (g b)
  | .IError err => .IError err

instance : Data.Bifunctor Initial where
  bimap := bimap
  mapFst := mapFst
  mapSnd := mapSnd

instance : Functor (Initial s) where
  map := mapSnd

end Initial

-- ── Functor / Bifunctor on Step ──────────────────────────────────────────────

namespace Step

/-- Map first function over the state and second over the result. -/
@[inline] def bimap (f : s → t) (g : b → c) : Step s b → Step t c
  | .SPartial n s => .SPartial n (f s)
  | .SContinue n s => .SContinue n (f s)
  | .SDone n b => .SDone n (g b)
  | .SError err => .SError err

/-- Map over the state only. -/
@[inline] def mapFst (f : s → t) : Step s b → Step t b
  | .SPartial n s => .SPartial n (f s)
  | .SContinue n s => .SContinue n (f s)
  | .SDone n b => .SDone n b
  | .SError err => .SError err

/-- Map over the result only (upstream `second`, also `fmap`). -/
@[inline] def mapSnd (g : b → c) : Step s b → Step s c
  | .SPartial n s => .SPartial n s
  | .SContinue n s => .SContinue n s
  | .SDone n b => .SDone n (g b)
  | .SError err => .SError err

/-- Map a function over the count. -/
@[inline] def mapCount (f : Int → Int) : Step s b → Step s b
  | .SPartial n s => .SPartial (f n) s
  | .SContinue n s => .SContinue (f n) s
  | .SDone n b => .SDone (f n) b
  | .SError err => .SError err

instance : Data.Bifunctor Step where
  bimap := bimap
  mapFst := mapFst
  mapSnd := mapSnd

instance : Functor (Step s) where
  map := mapSnd

end Step

/-- Bimap discarding the count, using the supplied count instead. -/
@[inline] def bimapOverrideCount (n : Int) (f : s → s₁) (g : b → b₁) :
    Step s b → Step s₁ b₁
  | .SPartial _ s => .SPartial n (f s)
  | .SContinue _ s => .SContinue n (f s)
  | .SDone _ b => .SDone n (g b)
  | .SError err => .SError err

-- ── Functor / Bifunctor on Final ─────────────────────────────────────────────

namespace Final

/-- Map first function over the state (`FContinue`), second over the result
    (`FDone`). -/
@[inline] def bimap (f : s → t) (g : b → c) : Final s b → Final t c
  | .FContinue n s => .FContinue n (f s)
  | .FDone n b => .FDone n (g b)
  | .FError err => .FError err

/-- Map over the state only. -/
@[inline] def mapFst (f : s → t) : Final s b → Final t b
  | .FContinue n s => .FContinue n (f s)
  | .FDone n b => .FDone n b
  | .FError err => .FError err

/-- Map over the result only. -/
@[inline] def mapSnd (g : b → c) : Final s b → Final s c
  | .FContinue n s => .FContinue n s
  | .FDone n b => .FDone n (g b)
  | .FError err => .FError err

instance : Data.Bifunctor Final where
  bimap := bimap
  mapFst := mapFst
  mapSnd := mapSnd

instance : Functor (Final s) where
  map := mapSnd

end Final

/-- Bimap a `Final`, discarding the count and using the supplied count. -/
@[inline] def bimapFinalOverrideCount (n : Int) (f : s → s₁) (g : b → b₁) :
    Final s b → Final s₁ b₁
  | .FContinue _ s => .FContinue n (f s)
  | .FDone _ b => .FDone n (g b)
  | .FError err => .FError err

/-- Map a `Final` to a `Step`, overriding the count. -/
@[inline] def bimapMorphOverrideCount (n : Int) (f : s → s₁) (g : b → b₁) :
    Final s b → Step s₁ b₁
  | .FDone _ b => .SDone n (g b)
  | .FContinue _ s => .SContinue n (f s)
  | .FError err => .SError err

/-- Map a monadic function over the result `b` in `Step s b`. -/
@[inline] def mapMStep [Applicative m] (f : a → m b) : Step s a → m (Step s b)
  | .SPartial n s => pure (.SPartial n s)
  | .SContinue n s => pure (.SContinue n s)
  | .SDone n b => Step.SDone n <$> f b
  | .SError err => pure (.SError err)

/-- Map a monadic function over the result `b` in `Final s b`. -/
@[inline] def mapMFinal [Applicative m] (f : a → m b) : Final s a → m (Final s b)
  | .FDone n b => Final.FDone n <$> f b
  | .FContinue n s => pure (.FContinue n s)
  | .FError err => pure (.FError err)

-- ── The Parser type ───────────────────────────────────────────────────────────

/-- A backtracking streaming parser: consume `a`s, produce a `b`, possibly
    failing and replaying input. The state type `s` is existentially hidden (an
    implicit field), exactly as for `Fold`. Note the parameter order
    `Parser a m b`, matching upstream. -/
structure Parser (a : Type u) (m : Type u → Type v) (b : Type u) where
  /-- The hidden parser-state type. -/
  {s : Type u}
  /-- Consume one input, returning a stream command. -/
  step : s → a → m (Step s b)
  /-- The initial state (or an immediate `IDone`/`IError`). -/
  initial : m (Initial s b)
  /-- Read the result at end of input. -/
  extract : s → m (Final s b)

/-- Thrown when a parser ultimately fails. Ported as plain data (Lean has no
    GHC `Exception` class); drivers surface failure via `Except String`. -/
inductive ParseError where
  | mk : String → ParseError
  deriving Repr, DecidableEq, BEq, Inhabited

/-- Like `ParseError` but records the stream position of the error. -/
inductive ParseErrorPos where
  | mk : Int → String → ParseErrorPos
  deriving Repr, DecidableEq, BEq, Inhabited

-- ── Mapping on the output ─────────────────────────────────────────────────────

/-- `Functor`: map a function on the result `b` of a `Parser a m b`. -/
instance [Functor m] : Functor (Parser a m) where
  map f p :=
    { s := p.s
      step := fun s b => Step.mapSnd f <$> p.step s b
      initial := Initial.mapSnd f <$> p.initial
      extract := fun s => Final.mapSnd f <$> p.extract s }

/-- `rmapM f parser` maps the monadic function `f` on the output of the parser. -/
@[inline] def rmapM [Monad m] (f : b → m c) (p : Parser a m b) : Parser a m c where
  s := p.s
  step s a := p.step s a >>= mapMStep f
  initial := do
    match ← p.initial with
    | .IPartial x => pure (.IPartial x)
    | .IDone a => .IDone <$> f a
    | .IError err => pure (.IError err)
  extract := p.extract >=> mapMFinal f

-- ── Nullary parsers ───────────────────────────────────────────────────────────

/-- A parser that always yields a pure value without consuming any input. -/
@[inline] def fromPure [Monad m] (x : b) : Parser a m b where
  s := PUnit
  step _ _ := pure (.SDone 1 x)
  initial := pure (.IDone x)
  extract _ := pure (.FDone 0 x)

/-- A parser that always yields the result of an effect without consuming input. -/
@[inline] def fromEffect [Monad m] (act : m b) : Parser a m b where
  s := PUnit
  step _ _ := (Step.SDone 1 ·) <$> act
  initial := (Initial.IDone ·) <$> act
  extract _ := (Final.FDone 0 ·) <$> act

/-- A parser that always fails with an error message, consuming no input. -/
@[inline] def die [Monad m] (err : String) : Parser a m b where
  s := PUnit
  step _ _ := pure (.SError err)
  initial := pure (.IError err)
  extract _ := pure (.FError err)

/-- A parser that always fails with an effectful error message. -/
@[inline] def dieM [Monad m] (err : m String) : Parser a m b where
  s := PUnit
  step _ _ := (Step.SError ·) <$> err
  initial := (Initial.IError ·) <$> err
  extract _ := (Final.FError ·) <$> err

-- ── Sequential applicative (splitWith / split_) ───────────────────────────────

/-- Fusion state for `splitWith`: running the left parser, or the right parser
    holding the pending combining function. -/
inductive SeqParseState (sl f sr : Type u) where
  | SeqParseL : sl → SeqParseState sl f sr
  | SeqParseR : f → sr → SeqParseState sl f sr

/-- Sequential parser application. Run the left parser, then feed the remaining
    input to the right parser, combining both outputs with `func`. Fails if
    either parser fails. (Upstream's `splitWith`, the `<*>` of `Parser`.) -/
@[inline] def splitWith {m : Type u → Type v} [Monad m] {x a b c : Type u}
    (func : a → b → c) (pL : Parser x m a) (pR : Parser x m b) : Parser x m c where
  s := SeqParseState pL.s (b → c) pR.s
  initial := do
    match ← pL.initial with
    | .IPartial sl => pure (.IPartial (.SeqParseL sl))
    | .IDone bl =>
        match ← pR.initial with
        | .IPartial sr => pure (.IPartial (.SeqParseR (func bl) sr))
        | .IDone br => pure (.IDone (func bl br))
        | .IError err => pure (.IError err)
    | .IError err => pure (.IError err)
  step
    | .SeqParseL st, a => do
        match ← pL.step st a with
        | .SPartial n s => pure (.SContinue n (.SeqParseL s))
        | .SContinue n s => pure (.SContinue n (.SeqParseL s))
        | .SDone n b =>
            match ← pR.initial with
            | .IPartial sr => pure (.SContinue n (.SeqParseR (func b) sr))
            | .IDone br => pure (.SDone n (func b br))
            | .IError err => pure (.SError err)
        | .SError err => pure (.SError err)
    | .SeqParseR f st, a => (Step.bimap (.SeqParseR f) f) <$> pR.step st a
  extract
    | .SeqParseR f sR => (Final.bimap (.SeqParseR f) f) <$> pR.extract sR
    | .SeqParseL sL => do
        match ← pL.extract sL with
        | .FDone n bL =>
            match ← pR.initial with
            | .IPartial sR => (Final.bimap (.SeqParseR (func bL)) (func bL)) <$> pR.extract sR
            | .IDone bR => pure (.FDone n (func bL bR))
            | .IError err => pure (.FError err)
        | .FError err => pure (.FError err)
        | .FContinue n s => pure (.FContinue n (.SeqParseL s))

/-- Fusion state for `split_`: running the left parser or the right parser. -/
inductive SeqAState (sl sr : Type u) where
  | SeqAL : sl → SeqAState sl sr
  | SeqAR : sr → SeqAState sl sr

/-- Sequential application ignoring the left output (the `*>` of `Parser`). -/
@[inline] def split_ {m : Type u → Type v} [Monad m] {x a b : Type u}
    (pL : Parser x m a) (pR : Parser x m b) : Parser x m b where
  s := SeqAState pL.s pR.s
  initial := do
    match ← pL.initial with
    | .IPartial sl => pure (.IPartial (.SeqAL sl))
    | .IDone _ => (Initial.mapFst .SeqAR) <$> pR.initial
    | .IError err => pure (.IError err)
  step
    | .SeqAL st, a => do
        match ← pL.step st a with
        | .SPartial n s => pure (.SContinue n (.SeqAL s))
        | .SContinue n s => pure (.SContinue n (.SeqAL s))
        | .SDone n _ =>
            match ← pR.initial with
            | .IPartial s => pure (.SContinue n (.SeqAR s))
            | .IDone b => pure (.SDone n b)
            | .IError err => pure (.SError err)
        | .SError err => pure (.SError err)
    | .SeqAR st, a => (Step.mapFst .SeqAR) <$> pR.step st a
  extract
    | .SeqAR sR => (Final.mapFst .SeqAR) <$> pR.extract sR
    | .SeqAL sL => do
        match ← pL.extract sL with
        | .FDone n _ =>
            match ← pR.initial with
            | .IPartial sR => (bimapFinalOverrideCount n .SeqAR id) <$> pR.extract sR
            | .IDone bR => pure (.FDone n bR)
            | .IError err => pure (.FError err)
        | .FError err => pure (.FError err)
        | .FContinue n s => pure (.FContinue n (.SeqAL s))

/-- `Applicative`: `pure = fromPure`, `<*> = splitWith id`, `*> = split_`.
    (There is no `Monad` instance — see the module header's universe-wall note.) -/
instance [Monad m] : Applicative (Parser a m) where
  pure := fromPure
  seq f x := splitWith (fun g y => g y) f (x ())
  seqRight l r := split_ l (r ())

-- ── Sequential alternative (alt) ──────────────────────────────────────────────

/-- Fusion state for `alt`: the left parser with a backtrack count, or the
    right parser. -/
inductive AltParseState (sl sr : Type u) where
  | AltParseL : Int → sl → AltParseState sl sr
  | AltParseR : sr → AltParseState sl sr

/-- Sequential alternative. Try the left parser; if it fails, backtrack and try
    the right parser on the same input. (Upstream's `alt`, the `<|>`.) -/
@[inline] def alt {m : Type u → Type v} [Monad m] {x a : Type u}
    (pL pR : Parser x m a) : Parser x m a where
  s := AltParseState pL.s pR.s
  initial := do
    match ← pL.initial with
    | .IPartial sl => pure (.IPartial (.AltParseL 0 sl))
    | .IDone bl => pure (.IDone bl)
    | .IError _ =>
        match ← pR.initial with
        | .IPartial sr => pure (.IPartial (.AltParseR sr))
        | .IDone br => pure (.IDone br)
        | .IError err => pure (.IError err)
  step
    | .AltParseL cnt st, a => do
        match ← pL.step st a with
        | .SPartial n s => pure (.SPartial n (.AltParseL 0 s))
        | .SContinue n s => pure (.SContinue n (.AltParseL (cnt + n) s))
        | .SDone n b => pure (.SDone n b)
        | .SError _ =>
            match ← pR.initial with
            | .IPartial rR => pure (.SContinue (-cnt) (.AltParseR rR))
            | .IDone b => pure (.SDone (-cnt) b)
            | .IError err => pure (.SError err)
    | .AltParseR st, a => do
        match ← pR.step st a with
        | .SPartial n s => pure (.SPartial n (.AltParseR s))
        | .SContinue n s => pure (.SContinue n (.AltParseR s))
        | .SDone n b => pure (.SDone n b)
        | .SError err => pure (.SError err)
  extract
    | .AltParseR sR => (Final.mapFst .AltParseR) <$> pR.extract sR
    | .AltParseL cnt sL => do
        match ← pL.extract sL with
        | .FDone n b => pure (.FDone n b)
        | .FError _ =>
            match ← pR.initial with
            | .IPartial rR => pure (.FContinue (-cnt) (.AltParseR rR))
            | .IDone b => pure (.FDone (-cnt) b)
            | .IError err => pure (.FError err)
        | .FContinue n s => pure (.FContinue n (.AltParseL 0 s))

/-- `Alternative`: `failure = die "empty"`, `orElse = alt`. -/
instance [Monad m] : Alternative (Parser a m) where
  failure := die "empty"
  orElse p q := alt p (q ())

-- ── Collecting repetition (splitMany / splitSome) ────────────────────────────

/-- Fusion state for `splitMany`/`splitSome`: the inner parser state, a
    backtrack count, and the collecting fold's state. -/
inductive Fused3 (sl st : Type u) where
  | mk : sl → Int → st → Fused3 sl st

/-- The mutually-recursive collector loop of `splitMany`. Feeds each completed
    parse result into the fold and re-initializes the parser; loops while the
    parser immediately `IDone`s (hence `unsafe` — no structural measure). -/
private unsafe def collectMany {m : Type u → Type v} [Monad m] {a b c : Type u}
    (p : Parser a m b) (fld : Fold m b c) {ρ : Type u}
    (cont : Fused3 p.s fld.s → ρ) (done : c → ρ)
    (fres : Data.Fold.Step fld.s c) : m ρ := do
  match fres with
  | .Partial fs =>
      match ← p.initial with
      | .IPartial ps => pure (cont ⟨ps, 0, fs⟩)
      | .IDone pb => fld.step fs pb >>= collectMany p fld cont done
      | .IError _ => done <$> fld.final fs
  | .Done fb => pure (done fb)

/-- Run the parser zero or more times, collecting outputs with the fold. Always
    succeeds (an empty run yields the fold's default). See `Parser.many`. -/
unsafe def splitMany {m : Type u → Type v} [Monad m] {a b c : Type u}
    (p : Parser a m b) (fld : Fold m b c) : Parser a m c where
  s := Fused3 p.s fld.s
  initial := fld.initial >>= collectMany p fld Initial.IPartial Initial.IDone
  step
    | ⟨st, cnt, fs⟩, a => do
        match ← p.step st a with
        | .SPartial n s => pure (.SContinue n ⟨s, cnt + n, fs⟩)
        | .SContinue n s => pure (.SContinue n ⟨s, cnt + n, fs⟩)
        | .SDone n b => fld.step fs b >>= collectMany p fld (Step.SPartial n) (Step.SDone n)
        | .SError _ => (Step.SDone (-cnt)) <$> fld.final fs
  extract
    | ⟨s, cnt, fs⟩ =>
        if cnt == 0 then (Final.FDone 0) <$> fld.final fs
        else do
          match ← p.extract s with
          | .FError _ => (Final.FDone (-cnt)) <$> fld.final fs
          | .FDone n b =>
              match ← fld.step fs b with
              | .Partial s1 => (Final.FDone n) <$> fld.final s1
              | .Done b1 => pure (.FDone n b1)
          | .FContinue n s1 => pure (.FContinue n ⟨s1, 0, fs⟩)

/-- The collector loop of `splitSome`; the fold state is tagged `Sum.inr`
    (post-first-parse). Same `unsafe` rationale as `collectMany`. -/
private unsafe def collectSome {m : Type u → Type v} [Monad m] {a b c : Type u}
    (p : Parser a m b) (fld : Fold m b c) {ρ : Type u}
    (cont : Fused3 p.s (fld.s ⊕ fld.s) → ρ) (done : c → ρ)
    (fres : Data.Fold.Step fld.s c) : m ρ := do
  match fres with
  | .Partial fs =>
      match ← p.initial with
      | .IPartial ps => pure (cont ⟨ps, 0, .inr fs⟩)
      | .IDone pb => fld.step fs pb >>= collectSome p fld cont done
      | .IError _ => done <$> fld.final fs
  | .Done fb => pure (done fb)

/-- Run the parser one or more times, collecting outputs with the fold. Fails if
    the parser does not succeed at least once. See `Parser.some`. -/
unsafe def splitSome {m : Type u → Type v} [Monad m] {a b c : Type u}
    (p : Parser a m b) (fld : Fold m b c) : Parser a m c where
  s := Fused3 p.s (fld.s ⊕ fld.s)
  initial := do
    match ← fld.initial with
    | .Partial fs =>
        match ← p.initial with
        | .IPartial ps => pure (.IPartial ⟨ps, 0, .inl fs⟩)
        | .IDone pb => fld.step fs pb >>= collectSome p fld Initial.IPartial Initial.IDone
        | .IError err => pure (.IError err)
    | .Done _ =>
        pure (.IError "splitSome: The collecting fold terminated without consuming any elements.")
  step
    | ⟨st, cnt, .inl fs⟩, a => do
        match ← p.step st a with
        | .SPartial n s => pure (.SContinue n ⟨s, cnt + n, .inl fs⟩)
        | .SContinue n s => pure (.SContinue n ⟨s, cnt + n, .inl fs⟩)
        | .SDone n b => fld.step fs b >>= collectSome p fld (Step.SPartial n) (Step.SDone n)
        | .SError err => pure (.SError err)
    | ⟨st, cnt, .inr fs⟩, a => do
        match ← p.step st a with
        | .SPartial n s => pure (.SPartial n ⟨s, cnt + n, .inr fs⟩)
        | .SContinue n s => pure (.SContinue n ⟨s, cnt + n, .inr fs⟩)
        | .SDone n b => fld.step fs b >>= collectSome p fld (Step.SPartial n) (Step.SDone n)
        | .SError _ => (Step.SDone (-cnt)) <$> fld.final fs
  extract
    | ⟨s, _cnt, .inl fs⟩ => do
        match ← p.extract s with
        | .FError err => pure (.FError err)
        | .FDone n b =>
            match ← fld.step fs b with
            | .Partial s1 => (Final.FDone n) <$> fld.final s1
            | .Done b1 => pure (.FDone n b1)
        | .FContinue n s1 => pure (.FContinue n ⟨s1, 0, .inl fs⟩)
    | ⟨s, cnt, .inr fs⟩ => do
        match ← p.extract s with
        | .FError _ => (Final.FDone (-cnt)) <$> fld.final fs
        | .FDone n b =>
            match ← fld.step fs b with
            | .Partial s1 => (Final.FDone n) <$> fld.final s1
            | .Done b1 => pure (.FDone n b1)
        | .FContinue n s1 => pure (.FContinue n ⟨s1, 0, .inr fs⟩)

-- ── Mapping on the input ──────────────────────────────────────────────────────

/-- `lmap f parser` maps `f` on the input of the parser. -/
@[inline] def lmap (f : a → b) (p : Parser b m r) : Parser a m r where
  s := p.s
  step x a := p.step x (f a)
  initial := p.initial
  extract := p.extract

/-- `lmapM f parser` maps the monadic `f` on the input of the parser. -/
@[inline] def lmapM [Monad m] (f : a → m b) (p : Parser b m r) : Parser a m r where
  s := p.s
  step x a := f a >>= p.step x
  initial := p.initial
  extract := p.extract

/-- Include only those input elements that pass a predicate. -/
@[inline] def filter [Monad m] (f : a → Bool) (p : Parser a m b) : Parser a m b where
  s := p.s
  step x a := if f a then p.step x a else pure (.SPartial 1 x)
  initial := p.initial
  extract := p.extract

end Data.Parser
