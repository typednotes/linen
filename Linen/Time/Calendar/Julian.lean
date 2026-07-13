/-
  Linen.Time.Calendar.Julian — the proleptic Julian calendar

  Module #4 of `docs/imports/Time/dependencies.md`'s "Genuinely new `Linen.*`
  ports" list, on `Linen.Time.Calendar.CalendarDiffDays` (module #1). Ports
  `Data.Time.Calendar.Julian` from Hackage's `time` package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/Julian.hs),
  folding in its upstream `other-module` helper
  `Data.Time.Calendar.JulianYearDay`
  (https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Calendar/JulianYearDay.hs)
  — it exists upstream purely to support `Julian.hs`, the same "internal
  helper with one real caller" treatment this codebase already gives
  similar upstream helper modules (see e.g. `Linen.Database.SQLite.Simple`'s
  siblings), so it is not ported as a separate `linen` module.

  ## Design

  The **proleptic Julian calendar** is a genuinely different calendar system
  from the Gregorian one `Std.Time` implements throughout: its leap-year
  rule is simply `year % 4 == 0` (no Gregorian century correction), which
  shifts its month lengths' cumulative offsets from the Gregorian ones for
  any date after 4 CE. `Std.Time.PlainDate` is reused purely as the
  underlying "day count since the Unix epoch" representation (via
  `.toEpochDay`/`.ofEpochDay`) — its own YMD *interpretation* of that count
  is Gregorian and is not used here; every year/month/day tuple in this
  module is a Julian-calendar tuple, produced and consumed only by this
  module's own `toJulian`/`fromJulian`.

  Upstream's `Day` is `ModifiedJulianDay` (days since 1858-11-17); `linen`
  reuses `Std.Time`'s Unix-epoch-based `Day.Offset` instead, so every
  formula that referenced the MJD count directly is re-derived against
  `unixEpochDay + 40587` (the fixed MJD-of-1970-01-01 offset) rather than
  reusing upstream's literal constants unchanged.

  Upstream's `showJulian`/`Show`-oriented helpers are not ported (no
  `Show`/`Read` story to reproduce, per the same reasoning as `Month`'s
  module doc); the calendrical arithmetic family (`addJulianMonthsClip`/
  `RollOver`, `addJulianYearsClip`/`RollOver`,
  `addJulianDurationClip`/`RollOver`, `diffJulianDurationClip`/`RollOver`)
  is ported in full, on `CalendarDiffDays`.
-/
import Std.Time
import Linen.Time.Calendar.CalendarDiffDays

namespace Time.Calendar.Julian

/-- The fixed Modified Julian Day number of 1970-01-01 (the Unix epoch),
    used to convert `Std.Time`'s Unix-epoch day counts to/from Modified
    Julian Day numbers, which every formula below is stated in terms of
    (matching upstream, whose `Day` newtype *is* the MJD count). -/
def mjdOfUnixEpoch : Int := 40587

/-- Is `year` a leap year in the proleptic Julian calendar (`year % 4 == 0`,
    with no Gregorian-style century correction)? -/
def isLeapYear (year : Int) : Bool :=
  year % 4 == 0

/-- The length, in days, of each month of a Julian year (index `0` is
    January), depending on whether the year is a leap year. -/
def monthLengths (leap : Bool) : Array Int :=
  #[31, if leap then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

/-- The number of days in the given Julian year/month (`month` is clipped to
    `1..12`). -/
def monthLength (leap : Bool) (month : Int) : Int :=
  let month' := if month < 1 then 1 else if month > 12 then 12 else month
  (monthLengths leap).getD (month' - 1).toNat 31

/-- Convert a Julian year/month/day into a day-of-year ordinal. Invalid
    months/days are clipped to the correct range. -/
def monthAndDayToDayOfYear (leap : Bool) (month day : Int) : Int :=
  let month' := if month < 1 then 1 else if month > 12 then 12 else month
  let maxDay := monthLength leap month'
  let day' := if day < 1 then 1 else if day > maxDay then maxDay else day
  let k := if month' ≤ 2 then 0 else if leap then -1 else -2
  (367 * month' - 362) / 12 + k + day'

/-- Convert a Julian day-of-year ordinal into a month/day pair. Out-of-range
    ordinals are clipped to the correct range (`1..365`/`366`). -/
def dayOfYearToMonthAndDay (leap : Bool) (yd : Int) : Int × Int :=
  let maxYd := if leap then 366 else 365
  let yd' := if yd < 1 then 1 else if yd > maxYd then maxYd else yd
  let rec go (lens : List Int) (m : Int) (rem : Int) : Int × Int :=
    match lens with
    | [] => (m, rem)
    | n :: ns => if rem > n then go ns (m + 1) (rem - n) else (m, rem)
  go (monthLengths leap).toList 1 yd'

-- ── Julian year/day ↔ day-count ──

/-- Convert a day count (since the Unix epoch, `Std.Time`'s `Day.Offset`
    convention) into a proleptic-Julian `(year, dayOfYear)` pair. -/
def toJulianYearAndDay (unixEpochDay : Int) : Int × Int :=
  let mjd := unixEpochDay + mjdOfUnixEpoch
  let a := mjd + 678577
  let quad := a / 1461
  let d := a % 1461
  let y := min (d / 365) 3
  let yd := d - y * 365 + 1
  let year := quad * 4 + y + 1
  (year, yd)

/-- Convert a proleptic-Julian `(year, dayOfYear)` pair into a day count
    (since the Unix epoch). Invalid day-of-year numbers are clipped to the
    correct range (`1..365`/`366`). -/
def fromJulianYearAndDay (year day : Int) : Int :=
  let y := year - 1
  let maxDay := if isLeapYear year then 366 else 365
  let day' := if day < 1 then 1 else if day > maxDay then maxDay else day
  let mjd := day' + 365 * y + y / 4 - 678578
  mjd - mjdOfUnixEpoch

-- ── Julian year/month/day ↔ day-count ──

/-- Convert a day count into a proleptic-Julian `(year, month, day)`
    triple. -/
def toJulian (unixEpochDay : Int) : Int × Int × Int :=
  let (year, yd) := toJulianYearAndDay unixEpochDay
  let (month, day) := dayOfYearToMonthAndDay (isLeapYear year) yd
  (year, month, day)

/-- Convert a proleptic-Julian `(year, month, day)` triple into a day
    count. Invalid values are clipped to the correct range, month first,
    then day. -/
def fromJulian (year month day : Int) : Int :=
  fromJulianYearAndDay year (monthAndDayToDayOfYear (isLeapYear year) month day)

-- ── Calendrical arithmetic ──

private def rollOverMonths (year absMonth : Int) : Int × Int :=
  (year + (absMonth - 1) / 12, (absMonth - 1) % 12 + 1)

private def addJulianMonths (n unixEpochDay : Int) : Int × Int × Int :=
  let (y, m, d) := toJulian unixEpochDay
  let (y', m') := rollOverMonths y (m + n)
  (y', m', d)

/-- Add months, with days past the last day of the month clipped to the
    last day. For instance, 2005-01-30 + 1 month = 2005-02-28. -/
def addJulianMonthsClip (n unixEpochDay : Int) : Int :=
  let (y, m, d) := addJulianMonths n unixEpochDay
  fromJulian y m d

/-- Add months, with days past the last day of the month rolling over to
    the next month. For instance, 2005-01-30 + 1 month = 2005-03-02. -/
def addJulianMonthsRollOver (n unixEpochDay : Int) : Int :=
  let (y, m, d) := addJulianMonths n unixEpochDay
  fromJulian y m 1 + (d - 1)

/-- Add years, matching month and day, with Feb 29th clipped to Feb 28th if
    necessary. -/
def addJulianYearsClip (n unixEpochDay : Int) : Int :=
  addJulianMonthsClip (n * 12) unixEpochDay

/-- Add years, matching month and day, with Feb 29th rolled over to Mar 1st
    if necessary. -/
def addJulianYearsRollOver (n unixEpochDay : Int) : Int :=
  addJulianMonthsRollOver (n * 12) unixEpochDay

/-- Add months (clipped to the last day), then add days. -/
def addJulianDurationClip (diff : CalendarDiffDays) (unixEpochDay : Int) : Int :=
  addJulianMonthsClip diff.months unixEpochDay + diff.days

/-- Add months (rolling over to the next month), then add days. -/
def addJulianDurationRollOver (diff : CalendarDiffDays) (unixEpochDay : Int) : Int :=
  addJulianMonthsRollOver diff.months unixEpochDay + diff.days

/-- Calendrical difference between two day counts, with as many whole
    months (via the "clip" convention) as possible. -/
def diffJulianDurationClip (day2 day1 : Int) : CalendarDiffDays :=
  let (y1, m1, d1) := toJulian day1
  let (y2, m2, d2) := toJulian day2
  let ym1 := y1 * 12 + m1
  let ym2 := y2 * 12 + m2
  let ymdiff := ym2 - ym1
  let ymAllowed :=
    if day2 ≥ day1 then
      if d2 ≥ d1 then ymdiff else ymdiff - 1
    else
      if d2 ≤ d1 then ymdiff else ymdiff + 1
  let dayAllowed := addJulianDurationClip ⟨ymAllowed, 0⟩ day1
  ⟨ymAllowed, day2 - dayAllowed⟩

/-- Calendrical difference between two day counts, with as many whole
    months (via the "roll-over" convention) as possible.

    Upstream (`findpos`/`findneg`) walks outward from `ymdiff` one month at
    a time until the residual day difference's sign matches the search
    direction — an unbounded recursion with no fuel/termination argument
    upstream, and whose `else` branch (`findneg`'s `findpos (succ mdiff)`
    call) recurses in the *wrong* direction for the negative case, an
    upstream bug that can loop. Ported instead as a search over a bounded
    window of candidate month-offsets around `ymdiff`, walked in the
    correct (documented) direction for each case and returning the first
    hit — structurally terminating (a fixed-length list), and correct for
    every real difference: consecutive Julian months differ in length by
    at most a few days, so the sign of the residual can only cross zero
    within a handful of months of `ymdiff`, well inside the window below. -/
def diffJulianDurationRollOver (day2 day1 : Int) : CalendarDiffDays :=
  let (y1, m1, _) := toJulian day1
  let (y2, m2, _) := toJulian day2
  let ym1 := y1 * 12 + m1
  let ym2 := y2 * 12 + m2
  let ymdiff := ym2 - ym1
  let residual (mdiff : Int) : Int :=
    day2 - addJulianDurationRollOver ⟨mdiff, 0⟩ day1
  if day2 ≥ day1 then
    -- `findpos`: search downward from `ymdiff` for the first (i.e. largest)
    -- month-offset with a nonnegative residual.
    let candidates := (List.range 24).map (fun k : Nat => ymdiff - (k : Int))
    match candidates.find? (fun m => residual m ≥ 0) with
    | some m => ⟨m, residual m⟩
    | none => ⟨ymdiff, residual ymdiff⟩
  else
    -- `findneg`, corrected to search upward from `ymdiff` for the first
    -- (i.e. smallest) month-offset with a nonpositive residual.
    let candidates := (List.range 24).map (fun k : Nat => ymdiff + (k : Int))
    match candidates.find? (fun m => residual m ≤ 0) with
    | some m => ⟨m, residual m⟩
    | none => ⟨ymdiff, residual ymdiff⟩

end Time.Calendar.Julian
