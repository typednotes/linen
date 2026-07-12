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
  conversion via the well-known "days from/to civcivil" arithmetic (Howard
  Hinnant's civil-calendar-days conversion algorithm): a closed-form,
  branch-free formula over `Int`/`Nat` arithmetic with no recursion, so no
  termination argument is needed. Like upstream's `fromGregorian`, out-of-range
  month/day components are clipped into range rather than rejected — a real
  validity check is `fromGregorianValid` below, matching upstream's function of
  the same name.
-/

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

/-- The Modified Julian Day number of the Unix epoch, 1970-01-01. -/
private def unixEpochMJD : Int := 40587

-- ── Gregorian conversion (Hinnant's civil-calendar arithmetic) ──

/-- Days since 1970-01-01 for a proleptic-Gregorian date. Total: out-of-range
    `month`/`day` values are not validated here (see `fromGregorianValid` for
    a checked variant); this is the same closed-form arithmetic Hinnant's
    `days_from_civil` uses, generalized to arbitrary integers. -/
private def daysFromCivil (y : Int) (m d : Nat) : Int :=
  let y' : Int := if m ≤ 2 then y - 1 else y
  let era : Int := (if y' ≥ 0 then y' else y' - 399).fdiv 400
  let yoe : Int := y' - era * 400
  let mp : Int := ((m : Int) + 9) % 12
  let doy : Int := (153 * mp + 2).fdiv 5 + (d : Int) - 1
  let doe : Int := yoe * 365 + yoe.fdiv 4 - yoe.fdiv 100 + doy
  era * 146097 + doe - 719468

/-- Inverse of `daysFromCivil`: recovers `(year, month, day)` from a day
    count since 1970-01-01. -/
private def civilFromDays (z : Int) : Int × Nat × Nat :=
  let z := z + 719468
  let era : Int := (if z ≥ 0 then z else z - 146096).fdiv 146097
  let doe : Int := z - era * 146097
  let yoe : Int := (doe - doe.fdiv 1460 + doe.fdiv 36524 - doe.fdiv 146096).fdiv 365
  let y : Int := yoe + era * 400
  let doy : Int := doe - (365 * yoe + yoe.fdiv 4 - yoe.fdiv 100)
  let mp : Int := (5 * doy + 2).fdiv 153
  let d : Int := doy - (153 * mp + 2).fdiv 5 + 1
  let m : Int := if mp < 10 then mp + 3 else mp - 9
  (y + (if m ≤ 2 then 1 else 0), m.toNat, d.toNat)

/-- Build a `Day` from a proleptic-Gregorian `(year, month, day)`, matching
    `Data.Time.Calendar.fromGregorian`: out-of-range `month`/`day` values are
    clipped into `[1, 12]`/`[1, <days in month>]` rather than rejected. -/
def fromGregorian (year : Int) (month day : Nat) : Day :=
  let month := max 1 (min 12 month)
  let maxDay :=
    let leap := (year % 4 == 0 && year % 100 ≠ 0) || year % 400 == 0
    match month with
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    | 4 | 6 | 9 | 11 => 30
    | _ => if leap then 29 else 28
  let day := max 1 (min maxDay day)
  ⟨daysFromCivil year month day + unixEpochMJD⟩

/-- Decompose a `Day` back into its proleptic-Gregorian `(year, month, day)`. -/
def toGregorian (d : Day) : Int × Nat × Nat :=
  civilFromDays (d.toModifiedJulianDay - unixEpochMJD)

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
