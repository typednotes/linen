/-
  Linen.Time.CalendarDiffTime — calendrical (months, `Duration`) periods

  Module #6 of `docs/imports/Time/dependencies.md`'s "Genuinely new
  `Linen.*` ports" list, on `Linen.Time.Calendar.CalendarDiffDays`
  (module #1). Ports `Data.Time.LocalTime.Internal.CalendarDiffTime` from
  Hackage's `time` package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/LocalTime/Internal/CalendarDiffTime.hs).

  ## Design

  The time-valued sibling of `CalendarDiffDays`: a whole-months offset
  paired with a sub-day-precision time offset, for calendrical arithmetic
  that needs finer-than-a-day resolution in its non-calendrical component
  (e.g. "one month and 90 minutes"). Upstream pairs `Integer` months with
  its own `NominalDiffTime`; per `docs/imports/Time/dependencies.md`'s
  substitution list, `NominalDiffTime` is not ported separately — this
  module is built directly on `Std.Time.Duration` instead, `Std.Time`'s own
  nanosecond-precision interval type.

  As with `CalendarDiffDays` (see its module doc), `linen` has no general
  `Semigroup`/`Monoid` type classes, so the monoid operation is exposed as
  an `Append` instance with the associativity/identity laws as `example`s.
  Upstream's `addUTCDurationClip`/`RollOver`/`diffUTCDurationClip`/
  `RollOver` (defined against `UTCTime`/`Data.Time.Calendar.Gregorian`) are
  out of scope for this module per the dependencies plan: they only compose
  already-substituted pieces (`Std.Time.PlainDate.addMonthsClip`/
  `RollOver`, `Std.Time.Timestamp`/`Duration` arithmetic), so a caller can
  already build the equivalent directly from those.
-/
import Std.Time
import Linen.Time.Calendar.CalendarDiffDays

namespace Time

/-- A calendrical period: a whole-months offset and a `Std.Time.Duration`
    offset, kept separate because a month has no fixed length. -/
structure CalendarDiffTime where
  /-- The whole-months component. -/
  months : Int
  /-- The sub-day-precision time component. -/
  time : Std.Time.Duration
deriving Repr, DecidableEq

namespace CalendarDiffTime

-- ── `Std.Time.Duration` addition lemmas (`Std.Time` proves none of these
--    itself — they are exactly the facts needed below) ──

private theorem toNanoseconds_ofNanoseconds (n : Std.Time.Nanosecond.Offset) :
    (Std.Time.Duration.ofNanoseconds n).toNanoseconds = n := by
  unfold Std.Time.Duration.ofNanoseconds Std.Time.Duration.toNanoseconds
  simp only [Std.Time.Internal.UnitVal.mul, Std.Time.Internal.Bounded.LE.byMod,
    Std.Time.Internal.UnitVal.cast, Std.Time.Internal.UnitVal.tdiv, HAdd.hAdd, Add.add,
    Std.Time.Internal.UnitVal.add]
  apply Std.Time.Internal.UnitVal.ext
  simp
  have := Int.mul_tdiv_add_tmod n.val 1000000000
  omega

private theorem ofNanoseconds_toNanoseconds (d : Std.Time.Duration) :
    Std.Time.Duration.ofNanoseconds d.toNanoseconds = d := by
  have hp := d.proof
  have hb := d.nano.property
  unfold Std.Time.Duration.ofNanoseconds Std.Time.Duration.toNanoseconds
  simp only [Std.Time.Internal.UnitVal.mul, Std.Time.Internal.Bounded.LE.byMod,
    Std.Time.Internal.UnitVal.cast, Std.Time.Internal.UnitVal.tdiv, HAdd.hAdd, Add.add,
    Std.Time.Internal.UnitVal.add]
  ext
  · apply Std.Time.Internal.UnitVal.ext
    show (d.second.val * 1000000000 + d.nano.val).tdiv 1000000000 = d.second.val
    rcases hp with ⟨h1, h2⟩
    · rw [Int.tdiv_eq_ediv_of_nonneg (by omega)]; omega
    · have h3 := Int.tdiv_eq_ediv_of_nonneg
        (a := -(d.second.val * 1000000000 + d.nano.val)) (b := 1000000000) (by omega)
      have h4 := Int.neg_tdiv (d.second.val * 1000000000 + d.nano.val) 1000000000
      omega
  · apply Subtype.ext
    show (d.second.val * 1000000000 + d.nano.val).tmod 1000000000 = d.nano.val
    rcases hp with ⟨h1, h2⟩
    · rw [Int.tmod_eq_emod_of_nonneg (by omega)]; omega
    · have h3 := Int.tmod_eq_emod_of_nonneg
        (a := -(d.second.val * 1000000000 + d.nano.val)) (b := 1000000000) (by omega)
      have h4 := Int.neg_tmod (d.second.val * 1000000000 + d.nano.val) 1000000000
      omega

private theorem nano_add_assoc (a b c : Std.Time.Nanosecond.Offset) :
    a + b + c = a + (b + c) := by
  apply Std.Time.Internal.UnitVal.ext
  show (a.val + b.val) + c.val = a.val + (b.val + c.val)
  omega

private theorem toNanoseconds_zero : Std.Time.Duration.toNanoseconds (0 : Std.Time.Duration) = 0 := by
  unfold Std.Time.Duration.toNanoseconds; rfl

private theorem duration_add_assoc (a b c : Std.Time.Duration) :
    (a.add b).add c = a.add (b.add c) := by
  unfold Std.Time.Duration.add
  rw [toNanoseconds_ofNanoseconds, toNanoseconds_ofNanoseconds, nano_add_assoc]

private theorem duration_zero_add (a : Std.Time.Duration) : Std.Time.Duration.add 0 a = a := by
  unfold Std.Time.Duration.add
  rw [toNanoseconds_zero]
  show Std.Time.Duration.ofNanoseconds ((0 : Std.Time.Nanosecond.Offset) + a.toNanoseconds) = a
  have h0 : (0 : Std.Time.Nanosecond.Offset) + a.toNanoseconds = a.toNanoseconds := by
    apply Std.Time.Internal.UnitVal.ext
    show (0 : Int) + a.toNanoseconds.val = a.toNanoseconds.val
    omega
  rw [h0, ofNanoseconds_toNanoseconds]

private theorem duration_add_zero (a : Std.Time.Duration) : Std.Time.Duration.add a 0 = a := by
  unfold Std.Time.Duration.add
  rw [toNanoseconds_zero]
  show Std.Time.Duration.ofNanoseconds (a.toNanoseconds + (0 : Std.Time.Nanosecond.Offset)) = a
  have h0 : a.toNanoseconds + (0 : Std.Time.Nanosecond.Offset) = a.toNanoseconds := by
    apply Std.Time.Internal.UnitVal.ext
    show a.toNanoseconds.val + (0 : Int) = a.toNanoseconds.val
    omega
  rw [h0, ofNanoseconds_toNanoseconds]

-- ── Semigroup/Monoid substitute (see module doc, and
--    `Linen.Time.Calendar.CalendarDiffDays`'s for the same treatment) ──

/-- The identity period: zero months, zero time (upstream's `mempty`). -/
def empty : CalendarDiffTime := ⟨0, 0⟩

/-- Componentwise addition (upstream's `Semigroup`/`Monoid` `(<>)`). -/
def append (a b : CalendarDiffTime) : CalendarDiffTime :=
  ⟨a.months + b.months, a.time + b.time⟩

instance : Append CalendarDiffTime := ⟨append⟩

/-- Associativity of `++` on `CalendarDiffTime`. -/
example (a b c : CalendarDiffTime) : a ++ b ++ c = a ++ (b ++ c) := by
  simp only [HAppend.hAppend, Append.append, append, CalendarDiffTime.mk.injEq]
  exact ⟨by omega, duration_add_assoc a.time b.time c.time⟩

/-- `empty` is a left identity for `++`. -/
example (a : CalendarDiffTime) : empty ++ a = a := by
  show CalendarDiffTime.mk (0 + a.months) (Std.Time.Duration.add 0 a.time) = a
  rw [Int.zero_add, duration_zero_add]

/-- `empty` is a right identity for `++`. -/
example (a : CalendarDiffTime) : a ++ empty = a := by
  show CalendarDiffTime.mk (a.months + 0) (Std.Time.Duration.add a.time 0) = a
  rw [Int.add_zero, duration_add_zero]

-- ── Construction ──

/-- Lift a `CalendarDiffDays` into a `CalendarDiffTime`, converting its
    whole-days component into a `Duration` of that many days. -/
def calendarTimeDays (d : Time.Calendar.CalendarDiffDays) : CalendarDiffTime :=
  ⟨d.months, Std.Time.Duration.ofSeconds (.ofInt (d.days * 86400))⟩

/-- Lift a bare `Duration` into a `CalendarDiffTime` with zero months. -/
def calendarTimeTime (dt : Std.Time.Duration) : CalendarDiffTime :=
  ⟨0, dt⟩

-- ── Scaling ──

/-- Scale a period by an integer factor. Note that `scale (-1)` does not
    perfectly invert a period, since month lengths vary. -/
def scale (k : Int) (d : CalendarDiffTime) : CalendarDiffTime :=
  ⟨k * d.months, k * d.time⟩

instance : HMul Int CalendarDiffTime CalendarDiffTime := ⟨scale⟩

end CalendarDiffTime
end Time
