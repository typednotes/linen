/-
  Linen.Time.Calendar.CalendarDiffDays — calendrical (months, days) periods

  Module #1 of `docs/imports/Time/dependencies.md`'s "Genuinely new `Linen.*`
  ports" list. Ports `Data.Time.Calendar.CalendarDiffDays` from Hackage's
  `time` package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/CalendarDiffDays.hs).

  ## Design

  A `CalendarDiffDays` pairs a whole-months offset with a whole-days offset —
  the vocabulary needed to express "one month later" (a calendrical notion
  whose length in days varies) separately from "30 days later" (a fixed
  count). `Std.Time` has no counterpart: `PlainDate.addMonthsClip`/
  `addMonthsRollOver` take a bare `Month.Offset`, with no type that also
  carries a day component alongside it.

  Upstream derives `Semigroup`/`Monoid` (additive, componentwise) and no other
  structural class. `linen` has no general `Semigroup`/`Monoid` type classes
  (see `Linen.Data.PDF.Core.Name`'s module doc for the established
  substitute), so — the same treatment — the monoid operation is exposed as
  an ordinary `Append` instance (`empty`/`++`) together with the associativity
  and identity laws as `example`s, rather than as class instances of a
  `Semigroup`/`Monoid` hierarchy this codebase doesn't have.
-/
import Std.Time

namespace Time.Calendar

/-- A calendrical period: a whole-months offset and a whole-days offset,
    kept separate because a month has no fixed length in days. -/
structure CalendarDiffDays where
  /-- The whole-months component. -/
  months : Int
  /-- The whole-days component. -/
  days : Int
deriving Repr, DecidableEq, Inhabited

namespace CalendarDiffDays

-- ── Semigroup/Monoid substitute (see module doc) ──

/-- The identity period: zero months, zero days (upstream's `mempty`). -/
def empty : CalendarDiffDays := ⟨0, 0⟩

/-- Componentwise addition (upstream's `Semigroup`/`Monoid` `(<>)`). -/
def append (a b : CalendarDiffDays) : CalendarDiffDays :=
  ⟨a.months + b.months, a.days + b.days⟩

instance : Append CalendarDiffDays := ⟨append⟩

/-- Associativity of `++` on `CalendarDiffDays`. -/
example (a b c : CalendarDiffDays) : a ++ b ++ c = a ++ (b ++ c) := by
  simp only [HAppend.hAppend, Append.append, append, CalendarDiffDays.mk.injEq]
  omega

/-- `empty` is a left identity for `++`. -/
example (a : CalendarDiffDays) : empty ++ a = a := by
  simp [HAppend.hAppend, Append.append, append, empty]

/-- `empty` is a right identity for `++`. -/
example (a : CalendarDiffDays) : a ++ empty = a := by
  simp [HAppend.hAppend, Append.append, append, empty]

-- ── Named constants ──

/-- One day. -/
def calendarDay : CalendarDiffDays := ⟨0, 1⟩

/-- One week (seven days). -/
def calendarWeek : CalendarDiffDays := ⟨0, 7⟩

/-- One month. -/
def calendarMonth : CalendarDiffDays := ⟨1, 0⟩

/-- One year (twelve months). -/
def calendarYear : CalendarDiffDays := ⟨12, 0⟩

-- ── Scaling ──

/-- Scale a period by an integer factor. Note that `scale (-1)` does not
    perfectly invert a period, since month lengths vary. -/
def scale (k : Int) (d : CalendarDiffDays) : CalendarDiffDays :=
  ⟨k * d.months, k * d.days⟩

instance : HMul Int CalendarDiffDays CalendarDiffDays := ⟨scale⟩

end CalendarDiffDays
end Time.Calendar
