/-
  Linen.Time.Calendar.Quarter — an absolute count of year quarters

  Module #3 of `docs/imports/Time/dependencies.md`'s "Genuinely new `Linen.*`
  ports" list. Ports `Data.Time.Calendar.Quarter` from Hackage's `time`
  package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/Quarter.hs),
  on `Linen.Time.Calendar.Month` (module #2).

  ## Design

  Same shape as `Month` one level up: `QuarterOfYear` (upstream's four-
  constructor `Q1`..`Q4` enum) plus an absolute `Quarter` counter,
  `(year * 4) + (quarterOfYear - 1)`. `Std.Time.PlainDate.quarter` is only a
  per-date *field* (`Bounded.LE 1 4`), never an absolute counter type — the
  same gap `Month` fills one level down. `QuarterOfYear` is ported as a plain
  `Bounded.LE 1 4`-backed abbreviation rather than a bespoke four-constructor
  inductive: it is used here purely as a 1–4 ordinal (`monthOfYearQuarter`,
  the `Quarter`/`Month` conversions), so a bounded integer carries the same
  information as upstream's enum without a separate `Enum`/`Bounded`
  instance pair to re-derive. As with `Month`, upstream's `Enum`/`Ix`/`Show`/
  `Read` instances are not ported (see `Month`'s module doc for the same
  reasoning) — `addQuarters`/`diffQuarters` give the same arithmetic
  directly.
-/
import Std.Time
import Linen.Time.Calendar.Month

namespace Time.Calendar

/-- The quarter-of-year, `1..4`. -/
def QuarterOfYear := Std.Time.Internal.Bounded.LE 1 4
deriving Repr, DecidableEq

namespace QuarterOfYear

/-- The `QuarterOfYear` a given month-of-year (`1..12`) falls in. -/
def ofMonthOfYear (my : Int) : QuarterOfYear :=
  Std.Time.Internal.Bounded.LE.clip ((my - 1) / 3 + 1) (by decide)

end QuarterOfYear

/-- An absolute count of year quarters, equal to
    `(year * 4) + (quarterOfYear - 1)`. -/
structure Quarter where
  /-- The absolute quarter count. -/
  toInt : Int
deriving Repr, DecidableEq, Inhabited

namespace Quarter

instance : LE Quarter := ⟨fun a b => a.toInt ≤ b.toInt⟩
instance : LT Quarter := ⟨fun a b => a.toInt < b.toInt⟩
instance {a b : Quarter} : Decidable (a ≤ b) := inferInstanceAs (Decidable (a.toInt ≤ b.toInt))
instance {a b : Quarter} : Decidable (a < b) := inferInstanceAs (Decidable (a.toInt < b.toInt))

-- ── Construction / destruction ──

/-- Build a `Quarter` from a year and a quarter-of-year. Invalid values
    (outside `1..4`) are clipped to the correct range. -/
def yearQuarter (y : Std.Time.Year.Offset) (qy : Int) : Quarter :=
  let qy' := if qy < 1 then 1 else if qy > 4 then 4 else qy
  ⟨y.toInt * 4 + (qy' - 1)⟩

/-- Decompose a `Quarter` back into a year and a quarter-of-year (`1..4`). -/
def toYearQuarter (q : Quarter) : Std.Time.Year.Offset × Int :=
  (Std.Time.Year.Offset.ofInt (q.toInt / 4), q.toInt % 4 + 1)

/-- The year component of a `Quarter`. -/
def year (q : Quarter) : Std.Time.Year.Offset :=
  Std.Time.Year.Offset.ofInt (q.toInt / 4)

/-- The quarter-of-year (`1..4`) component of a `Quarter`. -/
def quarterOfYear (q : Quarter) : Int := q.toInt % 4 + 1

-- ── Arithmetic ──

/-- Add a signed number of quarters. -/
def addQuarters (n : Int) (q : Quarter) : Quarter := ⟨q.toInt + n⟩

/-- The signed number of quarters between two `Quarter`s. -/
def diffQuarters (a b : Quarter) : Int := a.toInt - b.toInt

-- ── `DayPeriod`-style relation to `Std.Time.PlainDate` (via `Month`) ──

/-- The first month of a quarter. -/
def firstMonth (q : Quarter) : Month :=
  Month.yearMonth q.year (3 * (q.quarterOfYear - 1) + 1)

/-- The last month of a quarter. -/
def lastMonth (q : Quarter) : Month :=
  Month.yearMonth q.year (3 * q.quarterOfYear)

/-- The first day of the quarter. -/
def periodFirstDay (q : Quarter) : Std.Time.PlainDate :=
  q.firstMonth.periodFirstDay

/-- The last day of the quarter. -/
def periodLastDay (q : Quarter) : Std.Time.PlainDate :=
  q.lastMonth.periodLastDay

/-- The `Quarter` a given `Month` falls in. -/
def monthQuarter (m : Month) : Quarter :=
  yearQuarter m.year (QuarterOfYear.ofMonthOfYear (m.toInt % 12 + 1)).val

/-- The `Quarter` a given date falls in. -/
def dayQuarter (d : Std.Time.PlainDate) : Quarter :=
  monthQuarter (Month.dayPeriod d)

end Quarter
end Time.Calendar
