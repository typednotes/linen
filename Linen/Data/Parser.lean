/-
  Linen.Data.Parser — streaming-parser combinators

  ## Haskell source

  Ported from `streamly-core`'s `Streamly.Internal.Data.Parser`
  (https://hackage-content.haskell.org/package/streamly-core-0.3.1/src/src/Streamly/Internal/Data/Parser.hs),
  module #26 of the `streamly` import (see
  `docs/imports/streamly/dependencies.md`).

  Combinators over the `Parser a m b` type (#25): the upgrade path from `Fold`
  (`fromFold`, `fromFoldMaybe`), element parsers (`peek`, `eof`, `satisfy`,
  `one`, `oneEq`/`oneNotEq`/`oneOf`/`noneOf`, `either`, `maybe`), length-bounded
  takes (`takeBetween`, `takeEQ`, `takeGE`), predicate takes (`takeWhile`,
  `takeWhileP`, `takeWhile1`, `dropWhile`), the repetition combinators (`many`,
  `some`), and a **list-driver** (`parseBreakList`/`parseList`) that runs a
  parser to a result.

  ## Substitutions / deviations

  - **The driver is a `List` driver, not the `Stream` driver.** Upstream drives
    parsers with `Stream.parseBreak` (in `Stream.Eliminate`), which was dropped
    at Tier 3 for the universe wall — a residual `Stream m a : Type 1` cannot
    sit inside `m : Type → Type` (documented on `Stream.Type`/`Stream.Eliminate`).
    `parseBreakList` runs a parser over an ordinary Lean `List a`, faithfully
    implementing the `Step` position/backtrack semantics (position + count,
    `SPartial` dropping the buffer, `SContinue` retaining it, `<0` backtracking),
    and returning the result plus leftover input. It needs no residual stream,
    so it sidesteps the wall while giving a runnable, testable driver.
  - **The driver is `unsafe`.** Backtracking (`SContinue`/`SPartial` with `n ≤ 0`)
    means the position is not monotone, so the drive loop has no
    structural/well-founded measure — the same sanctioned `unsafe` used by the
    stream drivers (AGENTS.md's alternative to `partial`/fuel; never `sorry`).
    `many`/`some` are `unsafe` because `Parser.splitMany`/`splitSome` are.
  - **Failure surfaced as `Except String`.** Upstream `extract` throws a
    `ParseError`; here the driver returns `Except String b`.
  - **`oneOf`/`noneOf` specialized to `List`.** Upstream takes any `Foldable`;
    Lean has no `Foldable` class, so these take a `List` with `[BEq a]`.
  - **Large secondary layer deferred.** The ~3600-line upstream module also has
    `toFold`, `postscan`, `lookAhead`, `takeP`, the exact-match family
    (`listEq`/`listEqBy`/`streamEqBy`/`subsequenceBy`), all separator/framed/
    grouped/spanning parsers (`takeEndBy*`/`takeBeginBy*`/`wordBy`/`groupBy*`/
    `takeFramedBy*`/`blockWithQuotes`/`span*`), `sequence`/`count`/`countBetween`/
    `manyP`, the interleaved/`deintercalate*`/`sepBy*`/`manyTill*` families,
    `roundRobin`, `retry*`, and the zipping parsers (`zipWithM`/`zip`/`indexed`/
    `sampleFromthen`). These build on the primary combinators here and belong to
    a later batch, matching the plan's own module-level scoping (and several are
    `Stream`-driven, hitting the same wall as the driver). `toFold` in
    particular `error`s on any backtracking parser upstream, so a faithful total
    port has no `Fold` representation — deferred rather than weakened.
-/

import Linen.Data.Parser.Type
import Linen.Data.Fold.Type
import Linen.Data.Either.Strict
import Linen.Data.Tuple.Strict

namespace Data.Parser

-- `m`'s domain and codomain universes are independent by design, but always
-- co-occur syntactically in `parseBreakList`/`parseList`, so the linter
-- can't tell they need to stay free.
set_option linter.checkUnivs false

open Data.Fold (Fold)
open Data.Either (Either')
open Data.Tuple (Tuple')

-- ── Upgrade a Fold to a Parser ────────────────────────────────────────────────

/-- Make a `Parser` from a `Fold`: sends all input to the fold, never fails. -/
@[inline] def fromFold [Monad m] (fld : Fold m a b) : Parser a m b where
  s := fld.s
  initial := do
    match ← fld.initial with
    | .Partial s1 => pure (.IPartial s1)
    | .Done b => pure (.IDone b)
  step s a := do
    match ← fld.step s a with
    | .Partial s1 => pure (.SPartial 1 s1)
    | .Done b => pure (.SDone 1 b)
  extract s := (Final.FDone 0 ·) <$> fld.final s

/-- Convert an `Option`-returning fold to an error-returning parser. `errMsg` is
    the error the parser returns when the fold yields `none`. -/
@[inline] def fromFoldMaybe [Monad m] (errMsg : String) (fld : Fold m a (Option b)) :
    Parser a m b where
  s := fld.s
  initial := do
    match ← fld.initial with
    | .Partial s1 => pure (.IPartial s1)
    | .Done (some x) => pure (.IDone x)
    | .Done none => pure (.IError errMsg)
  step s a := do
    match ← fld.step s a with
    | .Partial s1 => pure (.SPartial 1 s1)
    | .Done (some x) => pure (.SDone 1 x)
    | .Done none => pure (.SError errMsg)
  extract s := do
    match ← fld.final s with
    | some x => pure (.FDone 0 x)
    | none => pure (.FError errMsg)

-- ── Element parsers ───────────────────────────────────────────────────────────

/-- Peek the head element without consuming it. Fails at end of input. -/
@[inline] def peek [Monad m] : Parser a m a where
  s := PUnit
  initial := pure (.IPartial ⟨⟩)
  step _ a := pure (.SDone 0 a)
  extract _ := pure (.FError "peek: end of input")

/-- Succeeds iff at the end of input, fails otherwise. -/
@[inline] def eof [Monad m] : Parser a m PUnit where
  s := PUnit
  initial := pure (.IPartial ⟨⟩)
  step _ _ := pure (.SError "eof: not at end of input")
  extract _ := pure (.FDone 0 ⟨⟩)

/-- Map an `Except`-returning function on the next element. On `Except.error err`
    the parser fails; otherwise returns the `Except.ok` value. -/
@[inline] def either [Monad m] (f : a → Except String b) : Parser a m b where
  s := PUnit
  initial := pure (.IPartial ⟨⟩)
  step _ a := pure <|
    match f a with
    | .ok b => .SDone 1 b
    | .error err => .SError err
  extract _ := pure (.FError "end of input")

/-- Map an `Option`-returning function on the next element; fails on `none`. -/
@[inline] def maybe [Monad m] (f : a → Option b) : Parser a m b where
  s := PUnit
  initial := pure (.IPartial ⟨⟩)
  step _ a := pure <|
    match f a with
    | some b => .SDone 1 b
    | none => .SError "maybe: predicate failed"
  extract _ := pure (.FError "maybe: end of input")

/-- Return the next element if it passes the predicate; fails otherwise. -/
@[inline] def satisfy [Monad m] (predicate : a → Bool) : Parser a m a where
  s := PUnit
  initial := pure (.IPartial ⟨⟩)
  step _ a := pure <|
    if predicate a then .SDone 1 a else .SError "satisfy: predicate failed"
  extract _ := pure (.FError "satisfy: end of input")

/-- Consume one element from the head of the stream. Fails at end of input. -/
@[inline] def one [Monad m] : Parser a m a := satisfy (fun _ => true)

/-- Match a specific element. -/
@[inline] def oneEq [Monad m] [BEq a] (x : a) : Parser a m a := satisfy (· == x)

/-- Match anything other than the supplied element. -/
@[inline] def oneNotEq [Monad m] [BEq a] (x : a) : Parser a m a := satisfy (· != x)

/-- Match any one of the elements in the supplied list. -/
@[inline] def oneOf [Monad m] [BEq a] (xs : List a) : Parser a m a :=
  satisfy (fun x => xs.contains x)

/-- Match anything not in the supplied list. -/
@[inline] def noneOf [Monad m] [BEq a] (xs : List a) : Parser a m a :=
  satisfy (fun x => !xs.contains x)

-- ── Taking elements by count ──────────────────────────────────────────────────

/-- `takeBetween low high fld` takes a minimum of `low` and a maximum of `high`
    input elements, folding them with `fld`. Stops after `high`; fails if the
    stream ends before `low` could be taken. -/
@[inline] def takeBetween [Monad m] (low high : Int) (fld : Fold m a b) :
    Parser a m b where
  s := Tuple' Int fld.s
  initial :=
    if low ≥ 0 && high ≥ 0 && low > high then
      pure (.IError s!"takeBetween: lower bound - {low} is greater than higher bound - {high}")
    else fld.initial >>= inext (-1)
  step := fun ⟨i, s⟩ a => fld.step s a >>= snext i
  extract := fun ⟨i, s⟩ =>
    if i ≥ low && i ≤ high then (Final.FDone 0 ·) <$> fld.final s
    else pure (.FError (streamErr i))
where
  streamErr (i : Int) : String :=
    s!"takeBetween: Expecting at least {low} elements, got {i}"
  foldErr (i : Int) : String :=
    s!"takeBetween: the collecting fold terminated after consuming {i} elements, minimum {low} elements needed"
  iextract (i : Int) (s : fld.s) : m (Initial (Tuple' Int fld.s) b) :=
    if i ≥ low && i ≤ high then (Initial.IDone ·) <$> fld.final s
    else pure (.IError (foldErr i))
  inext (i : Int) : Data.Fold.Step fld.s b → m (Initial (Tuple' Int fld.s) b)
    | .Partial s =>
        let i1 := i + 1
        if i1 < high then pure (.IPartial ⟨i1, s⟩) else iextract i1 s
    | .Done b => pure (if i + 1 ≥ low then .IDone b else .IError (foldErr (i + 1)))
  snext (i : Int) : Data.Fold.Step fld.s b → m (Step (Tuple' Int fld.s) b)
    | .Partial s =>
        let i1 := i + 1
        if i1 < low then pure (.SContinue 1 ⟨i1, s⟩)
        else if i1 < high then pure (.SPartial 1 ⟨i1, s⟩)
        else (Step.SDone 1 ·) <$> fld.final s
    | .Done b => pure (if i + 1 ≥ low then .SDone 1 b else .SError (foldErr (i + 1)))

/-- Take exactly `n` input elements. Fails if the stream or fold ends first. -/
@[inline] def takeEQ [Monad m] (n : Int) (fld : Fold m a b) : Parser a m b where
  s := Tuple' Int fld.s
  initial := do
    match ← fld.initial with
    | .Partial s => if n > 0 then pure (.IPartial ⟨1, s⟩) else (Initial.IDone ·) <$> fld.final s
    | .Done b => pure <|
        if n > 0 then
          .IError s!"takeEQ: Expecting exactly {n} elements, fold terminated without consuming any elements"
        else .IDone b
  step := fun ⟨i1, r⟩ a => do
    let res ← fld.step r a
    if n > i1 then
      pure <|
        match res with
        | .Partial s => .SContinue 1 ⟨i1 + 1, s⟩
        | .Done _ => .SError s!"takeEQ: Expecting exactly {n} elements, fold terminated on {i1}"
    else
      (Step.SDone 1 ·) <$> (match res with | .Partial s => fld.final s | .Done b => pure b)
  extract := fun ⟨i, _⟩ =>
    pure (.FError s!"takeEQ: Expecting exactly {n} elements, input terminated on {i - 1}")

/-- Fusion state for `takeGE`: below the lower bound, or at/above it. -/
inductive TakeGEState (s : Type u) where
  | TakeGELT : Int → s → TakeGEState s
  | TakeGEGE : s → TakeGEState s

/-- Take at least `n` input elements, but can collect more (until the fold
    stops). Fails if the stream/fold ends before `n` elements. -/
@[inline] def takeGE [Monad m] (n : Int) (fld : Fold m a b) : Parser a m b where
  s := TakeGEState fld.s
  initial := do
    match ← fld.initial with
    | .Partial s => pure (if n > 0 then .IPartial (.TakeGELT 1 s) else .IPartial (.TakeGEGE s))
    | .Done b => pure <|
        if n > 0 then
          .IError s!"takeGE: Expecting at least {n} elements, fold terminated without consuming any elements"
        else .IDone b
  step
    | .TakeGELT i1 r, a => do
        let res ← fld.step r a
        if n > i1 then
          pure <|
            match res with
            | .Partial s => .SContinue 1 (.TakeGELT (i1 + 1) s)
            | .Done _ => .SError s!"takeGE: Expecting at least {n} elements, fold terminated on {i1}"
        else
          pure <|
            match res with
            | .Partial s => .SPartial 1 (.TakeGEGE s)
            | .Done b => .SDone 1 b
    | .TakeGEGE r, a => do
        match ← fld.step r a with
        | .Partial s => pure (.SPartial 1 (.TakeGEGE s))
        | .Done b => pure (.SDone 1 b)
  extract
    | .TakeGELT i _ => pure (.FError s!"takeGE: Expecting at least {n} elements, input terminated on {i - 1}")
    | .TakeGEGE r => (Final.FDone 0 ·) <$> fld.final r

-- ── Taking elements by predicate ──────────────────────────────────────────────

/-- Like `takeWhile` but uses a `Parser` to collect: stops when the condition
    fails or the collecting parser stops; fails when the collecting parser fails. -/
@[inline] def takeWhileP [Monad m] (predicate : a → Bool) (p : Parser a m b) :
    Parser a m b where
  s := p.s
  initial := p.initial
  extract := p.extract
  step s a :=
    if predicate a then p.step s a
    else do
      match ← p.extract s with
      | .FError err => pure (.SError err)
      | .FDone n s1 => pure (.SDone n s1)
      | .FContinue n s1 => pure (.SContinue n s1)

/-- Collect stream elements until one fails the predicate; that element is
    returned to the input. Never fails. -/
@[inline] def takeWhile [Monad m] (predicate : a → Bool) (fld : Fold m a b) :
    Parser a m b where
  s := fld.s
  initial := do
    match ← fld.initial with
    | .Partial s => pure (.IPartial s)
    | .Done b => pure (.IDone b)
  step s a :=
    if predicate a then do
      match ← fld.step s a with
      | .Partial s1 => pure (.SPartial 1 s1)
      | .Done b => pure (.SDone 1 b)
    else (Step.SDone 0 ·) <$> fld.final s
  extract s := (Final.FDone 0 ·) <$> fld.final s

/-- Like `takeWhile` but takes at least one element, otherwise fails. -/
@[inline] def takeWhile1 [Monad m] (predicate : a → Bool) (fld : Fold m a b) :
    Parser a m b where
  s := Either' fld.s fld.s
  initial := do
    match ← fld.initial with
    | .Partial s => pure (.IPartial (.Left' s))
    | .Done _ => pure (.IError "takeWhile1: fold terminated without consuming: any element")
  step
    | .Left' s, a =>
        if predicate a then process s a
        else pure (.SError "takeWhile1: predicate failed on first element")
    | .Right' s, a =>
        if predicate a then process s a
        else (Step.SDone 0 ·) <$> fld.final s
  extract
    | .Left' _ => pure (.FError "takeWhile1: end of input")
    | .Right' s => (Final.FDone 0 ·) <$> fld.final s
where
  process (s : fld.s) (a : a) : m (Step (Either' fld.s fld.s) b) := do
    match ← fld.step s a with
    | .Partial s1 => pure (.SPartial 1 (.Right' s1))
    | .Done b => pure (.SDone 1 b)

/-- Drain input while the predicate holds, discarding the results. -/
@[inline] def dropWhile [Monad m] (p : a → Bool) : Parser a m PUnit :=
  takeWhile p Data.Fold.drain

-- ── Repetition ────────────────────────────────────────────────────────────────

/-- Run the parser zero or more times, collecting outputs into a list. -/
@[inline] unsafe def many [Monad m] (p : Parser a m b) : Parser a m (List b) :=
  splitMany p Data.Fold.toList

/-- Run the parser one or more times, collecting outputs into a list. -/
@[inline] unsafe def some [Monad m] (p : Parser a m b) : Parser a m (List b) :=
  splitSome p Data.Fold.toList

-- ── The list driver ───────────────────────────────────────────────────────────

/-- Run a parser over a `List`, returning the result (or an error message) and
    the leftover unconsumed input. Faithfully follows the `Step` position and
    backtracking semantics (see `Parser.Type`). `unsafe` because backtracking
    makes the drive loop non-well-founded. -/
unsafe def parseBreakList [Monad m] (p : Parser a m b) (input : List a) :
    m (Except String b × List a) := do
  match ← p.initial with
  | .IDone b => pure (.ok b, input)
  | .IError e => pure (.error e, input)
  | .IPartial st => go st 0
where
  go (st : p.s) (pos : Nat) : m (Except String b × List a) := do
    match input[pos]? with
    | Option.none =>
        match ← p.extract st with
        | .FDone n b => pure (.ok b, input.drop (((pos : Int) + n).toNat))
        | .FContinue _ _ => pure (.error "parseBreakList: insufficient input", input.drop pos)
        | .FError e => pure (.error e, input.drop pos)
    | Option.some a =>
        match ← p.step st a with
        | .SPartial n s => go s (((pos : Int) + n).toNat)
        | .SContinue n s => go s (((pos : Int) + n).toNat)
        | .SDone n b => pure (.ok b, input.drop (((pos : Int) + n).toNat))
        | .SError e => pure (.error e, input.drop pos)

/-- Run a parser over a `List`, returning just the result or an error message. -/
@[inline] unsafe def parseList [Monad m] (p : Parser a m b) (input : List a) :
    m (Except String b) :=
  (·.1) <$> parseBreakList p input

end Data.Parser
