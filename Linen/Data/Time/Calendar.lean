/-
  Linen.Data.Time.Calendar — proleptic Gregorian calendar days

  A small addition to `linen`'s `Time` port (`docs/imports/Time/dependencies.md`
  originally covered only `Data.Time.Clock`), added while porting
  `sqlite-simple` (`docs/imports/sqlite-simple/dependencies.md`, module #7):
  `Database.SQLite.Simple.Time.Implementation` renders/parses dates against
  Haskell's `Data.Time.Calendar.Day`, which had no counterpart in this
  codebase yet.

  ## Design

  Mirrors Haskell's `Data.Time.Calendar.Day`: a `Day` is an integer count of
  days, with `toModifiedJulianDay` giving the day number relative to the
  Modified Julian Day epoch (1858-11-17), exactly as upstream defines it, so
  that any future straight port of another `Data.Time.Calendar.*` module can
  reuse this representation unchanged.

  `fromGregorian`/`toGregorian` implement the proleptic Gregorian calendar
  conversion by delegating to `Std.Time.Date.PlainDate` (per
  `docs/imports/Time/dependencies.md`'s status note: this module originally
  hand-rolled Howard Hinnant's civil-calendar-days arithmetic itself, before
  this codebase's import process had `Std.Time` in its precedence analysis —
  `PlainDate.ofYearMonthDayClip`/`.toEpochDay`/`.ofEpochDay` already implement
  the identical algorithm, so there is no reason to keep a second copy of it
  here). Like upstream's `fromGregorian`, out-of-range month/day components
  are clipped into range rather than rejected — a real validity check is
  `fromGregorianValid` below, matching upstream's function of the same name.
-/
import Std.Time

namespace Data.Time

/-- A day, represented as an integer count of days relative to the Modified
    Julian Day epoch (1858-11-17), matching `Data.Time.Calendar.Day` upstream.
    $$\text{Day} = \mathbb{Z}$$ -/
structure Day where
  /-- Day number since the Modified Julian Day epoch (1858-11-17). -/
  toModifiedJulianDay : Int
deriving BEq, Repr, Inhabited

namespace Day

instance : Ord Day where
  compare a b := compare a.toModifiedJulianDay b.toModifiedJulianDay

instance : LT Day := ⟨fun a b => a.toModifiedJulianDay < b.toModifiedJulianDay⟩
instance : LE Day := ⟨fun a b => a.toModifiedJulianDay ≤ b.toModifiedJulianDay⟩

instance : ToString Day where
  toString d := s!"ModifiedJulianDay {d.toModifiedJulianDay}"

/-- Build a `Day` directly from its Modified Julian Day number. -/
@[inline] def ofModifiedJulianDay (n : Int) : Day := ⟨n⟩

/-- The Modified Julian Day number of the Unix epoch, 1970-01-01 — the fixed
    offset between `Day.toModifiedJulianDay` and `Std.Time.Date.PlainDate`'s
    own Unix-epoch-based `Day.Offset` convention. -/
private def unixEpochMJD : Int := 40587

-- ── Gregorian conversion (via `Std.Time.Date.PlainDate`) ──

/-- Build a `Day` from a proleptic-Gregorian `(year, month, day)`, matching
    `Data.Time.Calendar.fromGregorian`: out-of-range `month`/`day` values are
    clipped into `[1, 12]`/`[1, <days in month>]` rather than rejected —
    exactly `PlainDate.ofYearMonthDayClip`'s own clipping behaviour. -/
def fromGregorian (year : Int) (month day : Nat) : Day :=
  let y := Std.Time.Year.Offset.ofInt year
  let m : Std.Time.Month.Ordinal := Std.Time.Internal.Bounded.LE.clip (month : Int) (by decide)
  let d : Std.Time.Day.Ordinal := Std.Time.Internal.Bounded.LE.clip (day : Int) (by decide)
  let pd := Std.Time.PlainDate.ofYearMonthDayClip y m d
  ⟨pd.toEpochDay.val + unixEpochMJD⟩

/-- Decompose a `Day` back into its proleptic-Gregorian `(year, month, day)`. -/
def toGregorian (d : Day) : Int × Nat × Nat :=
  let pd := Std.Time.PlainDate.ofEpochDay (.ofInt (d.toModifiedJulianDay - unixEpochMJD))
  (pd.year.toInt, pd.month.val.toNat, pd.day.val.toNat)

/-- Whether `(year, month, day)` is a valid proleptic-Gregorian date (matching
    `fromGregorianValid`'s validity check): builds the (possibly clipped) `Day`
    and checks the round trip reproduces the original components. -/
def fromGregorianValid (year : Int) (month day : Nat) : Option Day :=
  let d := fromGregorian year month day
  if d.toGregorian == (year, month, day) then some d else none

/-- Add a number of days to a `Day`. -/
@[inline] def addDays (n : Int) (d : Day) : Day := ⟨d.toModifiedJulianDay + n⟩

/-- The signed difference in days between two `Day`s, `diffDays a b = a - b`. -/
@[inline] def diffDays (a b : Day) : Int := a.toModifiedJulianDay - b.toModifiedJulianDay

-- ── Proofs ──

theorem toGregorian_fromGregorian_epoch :
    (fromGregorian 1970 1 1).toGregorian = (1970, 1, 1) := by
  native_decide

theorem addDays_diffDays_self (d : Day) : addDays 0 d = d := by
  simp [addDays]

end Day
end Data.Time
