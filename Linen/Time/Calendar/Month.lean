/-
  Linen.Time.Calendar.Month — an absolute count of calendar months

  Module #2 of `docs/imports/Time/dependencies.md`'s "Genuinely new `Linen.*`
  ports" list. Ports `Data.Time.Calendar.Month` from Hackage's `time` package
  (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/Month.hs).

  ## Design

  `Month` is an absolute counter — `(year * 12) + (monthOfYear - 1)` — quite
  distinct from `Std.Time.Month.Ordinal`/`.Offset`, which model "the month
  field of a date" (1–12) and "a signed month delta", never "the n-th month
  since a fixed origin". Upstream's bidirectional `YearMonth` pattern
  synonym is ported as an ordinary constructor/destructor pair
  (`yearMonth`/`toYearMonth`); its `DayPeriod` instance (`periodFirstDay`/
  `periodLastDay`/`dayPeriod`) is ported as plain functions relating a
  `Month` to `Std.Time.PlainDate` — `linen` has no `DayPeriod` type
  class (nothing else in this port needs one; a bare function per operation
  is exactly what upstream's `DayPeriod` methods amount to for a single
  instance). Upstream's `Enum`/`Ix`/`Show`/`Read` instances are not ported:
  they only exist to support Haskell's range-literal and string-literal
  syntax, which `linen` doesn't need reproduced (`addMonths`/`diffMonths`
  give the same arithmetic directly).
-/
import Std.Time

namespace Time.Calendar

/-- An absolute count of common calendar months, equal to
    `(year * 12) + (monthOfYear - 1)`. -/
structure Month where
  /-- The absolute month count. -/
  toInt : Int
deriving Repr, DecidableEq, Inhabited

namespace Month

instance : LE Month := ⟨fun a b => a.toInt ≤ b.toInt⟩
instance : LT Month := ⟨fun a b => a.toInt < b.toInt⟩
instance {a b : Month} : Decidable (a ≤ b) := inferInstanceAs (Decidable (a.toInt ≤ b.toInt))
instance {a b : Month} : Decidable (a < b) := inferInstanceAs (Decidable (a.toInt < b.toInt))

-- ── Construction / destruction ──

/-- Build a `Month` from a year and a month-of-year. Invalid months of year
    (outside `1..12`) are clipped to the correct range. -/
def yearMonth (y : Std.Time.Year.Offset) (my : Int) : Month :=
  let my' := if my < 1 then 1 else if my > 12 then 12 else my
  ⟨y.toInt * 12 + (my' - 1)⟩

/-- Decompose a `Month` back into a year and a month-of-year (`1..12`). -/
def toYearMonth (m : Month) : Std.Time.Year.Offset × Int :=
  (Std.Time.Year.Offset.ofInt (m.toInt.ediv 12), m.toInt.emod 12 + 1)

/-- The month-of-year (`1..12`) component of a `Month`, as a bounded
    `Std.Time.Month.Ordinal`. -/
def monthOfYear (m : Month) : Std.Time.Month.Ordinal :=
  Std.Time.Month.Ordinal.ofInt (m.toInt % 12 + 1) (by omega)

/-- The year component of a `Month`. -/
def year (m : Month) : Std.Time.Year.Offset :=
  Std.Time.Year.Offset.ofInt (m.toInt.ediv 12)

-- ── Arithmetic ──

/-- Add a signed number of months. -/
def addMonths (n : Int) (m : Month) : Month := ⟨m.toInt + n⟩

/-- The signed number of months between two `Month`s. -/
def diffMonths (a b : Month) : Int := a.toInt - b.toInt

-- ── `DayPeriod`-style relation to `Std.Time.PlainDate` ──

/-- The first day of the month. -/
def periodFirstDay (m : Month) : Std.Time.PlainDate :=
  Std.Time.PlainDate.ofYearMonthDayClip m.year m.monthOfYear 1

/-- The last day of the month (the day argument is clipped to the month's
    actual length). -/
def periodLastDay (m : Month) : Std.Time.PlainDate :=
  Std.Time.PlainDate.ofYearMonthDayClip m.year m.monthOfYear 31

/-- The `Month` containing a given date. -/
def dayPeriod (d : Std.Time.PlainDate) : Month :=
  yearMonth d.year d.month.val

end Month
end Time.Calendar
