# `time` module dependencies

Topological order of every module of the [`time`](https://hackage.haskell.org/package/time)
package (v1.15, source: https://github.com/haskell/time) imported into
`linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

**Status note — supersedes the earlier stub.** This file previously covered
only `Data.Time.Clock`, ported ad hoc as `Linen.Data.Time.Clock` while working
on `sqlite-simple`, before this codebase's import process had `Std.Time` in
its precedence analysis. Since then two more ad hoc modules were added the
same way — `Linen.Data.Time.Calendar` (`Data.Time.Calendar.Day`/Gregorian
conversion) and `Linen.Data.Time.LocalTime`
(`Data.Time.LocalTime.TimeOfDay`/`TimeZone`), both while porting
`sqlite-simple`'s `Time.Implementation` — plus `Linen.System.Time`, a small
`ffi/time.c` wrapper over C's `time()` for wall-clock reads (added because
`Linen.Data.Time.Clock.getCurrentTime` actually calls `IO.monoNanosNow`, a
*monotonic* clock, not wall-clock time — the two are not interchangeable, and
nothing under `Linen.Data.Time.*` at the time filled that gap). None of these
four existing modules reused `Std.Time`. This document is the full, rigorous
Hackage-import pass this package deserved from the start, done per the
current AGENTS.md convention.

**Reconciliation done.** `Linen.Data.Time.Calendar`/`.Clock`/`.LocalTime` are
now rebuilt on `Std.Time` (`PlainDate`, `Timestamp`/`Duration`,
`PlainTime`/`TimeZone.Offset` respectively) instead of their original ad hoc
representations, with every public name/signature preserved so callers (e.g.
`Linen.Database.SQLite.Simple.Time.Implementation`) needed only two
call-site updates — both anonymous-constructor `UTCTime`/`TimeOfDay`
literals, now `UTCTime.ofNanosSinceEpoch`/`TimeOfDay.ofHourMinuteSec`. This
also fixed `getCurrentTime`, which previously read a monotonic clock rather
than wall-clock time — it now calls `Std.Time.DateTime.Timestamp.now`.
`Linen.System.Time` (the FFI wall-clock read this bug had motivated, subsumed
outright by `Timestamp.now`) and its `ffi/time.c` shim have been deleted,
along with their `Linen.lean`/`lakefile.lean` wiring.

## Headline finding

`time`'s core job — Gregorian/ISO-week/ordinal calendar arithmetic, clocks,
durations, timezones, and `strftime`-style formatting/parsing including
locale-specific month/weekday names — is **already covered by Lean's own
`Std.Time`** (ships with the pinned `lean-toolchain`, v4.31.0). Per the
Hackage-import precedence rule (Lean stdlib > existing `linen` Haskell port >
new source), almost every `time` module is a **substitution**, not a port.
The real gap, after reading both `time`'s actual sources under
`lib/Data/Time/` and `Std.Time`'s actual sources (fetched from
`github.com/leanprover/lean4` at tag `v4.31.0`, not guessed from names), is
small: the proleptic **Julian calendar**, the **Easter-date algorithm**, two
absolute-period types (**`Month`**, **`Quarter`**) that `Std.Time` doesn't
model as such (it only has month-of-year/quarter-of-year *fields*, not a
standalone absolute counter with `DayPeriod`-style arithmetic), a **calendar
period type** (months+days / months+duration, for calendrical "add N months"
diffing), an **earth-rotation `UT1`/`UniversalTime`** conversion, and
**TAI ⇄ UTC leap-second conversion** (`Std.Time`'s TZif parser already reads
raw leap-second records out of tzdata but exposes no `AbsoluteTime`/TAI-clock
API on top of them).

## External (non-`time`) dependencies

Resolved against `time-1.15.cabal`'s `build-depends`, in Hackage-import
precedence order (Lean stdlib > existing `linen` Haskell port > new source):

- `base` → `Base` (already ported).
- `deepseq` — controls GHC's laziness, which Lean (eager by default) has no
  equivalent notion of; genuinely out of scope, the same treatment the `hip`
  entry in the top-level index gives `deepseq`.
- `template-haskell` (only used for `TH.Lift` instances, so calendar/time
  values can appear as compile-time literals in GHC) — no Lean analogue
  needed: `Std.Time.Notation`'s `datespec`/format-string macros already give
  Lean's own compile-time-literal story for this domain, and every value
  ported below is an ordinary `def`/`structure` usable directly in `#eval`/
  proofs without a separate literal-splicing mechanism.
- `Win32` (Windows-only build-dep, for `Data.Time.Clock.Internal.CTimeval`'s
  alternative code path) — dropped with the C FFI clock code below.
- The `lib/cbits/HsTime.c` C shim and `Data.Time.Clock.Internal.CTimeval`/
  `.CTimespec` — raw `gettimeofday`/`clock_gettime` FFI to read the system
  clock. `Std.Time.DateTime.Timestamp.now`/`Std.Time.Zoned.PlainDateTime.now`
  already give a portable wall-clock reading through the Lean runtime
  itself; no new FFI needed, so these are dropped as GHC-runtime-specific
  plumbing with no bearing on portable behavior, per AGENTS.md's
  "GHC-runtime-specific" exclusion category. (`Linen.System.Time`'s own
  small `ffi/time.c` shim, added independently for the same wall-clock need
  before this analysis existed, is likewise subsumed — see the status note
  above.)

## `time` modules substituted by `Std.Time` (no new port)

Each entry names the exact `Std.Time` namespace/type that covers it, checked
against `Std.Time`'s real source (`github.com/leanprover/lean4`,
`src/Std/Time/`, tag `v4.31.0`) rather than assumed from module names:

- `Data.Time.Calendar` (facade: `Day`, Gregorian `toGregorian`/`fromGregorian`,
  `addDays`/`diffDays`, `isLeapYear`) → `Std.Time.Date.PlainDate` (
  `PlainDate.ofYearMonthDay?`/`ofYearMonthDayClip`, `.toEpochDay`/`.ofEpochDay`,
  `.addDays`/`.subDays`, `.inLeapYear`) plus `Std.Time.Date.Unit.*`.
- `Data.Time.Calendar.MonthDay` (day-of-year ⇄ month/day) →
  `Std.Time.Date.ValidDate` (`ValidDate.ofOrdinal`, `.dayOfYear`).
- `Data.Time.Calendar.OrdinalDate` (`toOrdinalDate`/`fromOrdinalDate`, ISO 8601
  week-numbering year) → `PlainDate.ofYearOrdinal`/`.dayOfYear` plus
  `PlainDate.weekOfYear`/`.weekYear` (ISO week-numbering year, already
  parameterised over `firstDay : Weekday` so it covers both the ISO and the
  US-week variants `time` exposes separately).
- `Data.Time.Calendar.WeekDate` (ISO 8601 week-date, ISO/ordinal
  `Day`-periods) → `PlainDate.weekOfYear`/`.weekYear`/`.alignedWeekOfMonth`/
  `.withWeekday`, `Std.Time.Date.Unit.Week`.
- `Data.Time.Calendar.Week` (non-ISO week-numbering helpers, "week starting
  on a given weekday") → `PlainDate.weekOfYear (firstDay := ·)`/`.weekYear`/
  `.alignedWeekOfMonth`, which already take an explicit `firstDay : Weekday`
  parameter, a strict superset of `time`'s fixed-Sunday/Monday variants.
- `Data.Time.Calendar.Days` (`Day` = Modified Julian Day count, `addDays`/
  `diffDays`) → `Std.Time.Date.Unit.Day` (`Day.Offset`) and
  `PlainDate.toEpochDay`/`.ofEpochDay`.
- `Data.Time.Calendar.Gregorian` → `Std.Time.Date.PlainDate` directly (see
  `Data.Time.Calendar` above; this was always just the Gregorian-named half of
  that facade upstream).
- `Data.Time.Calendar.Types` (`Year`/`MonthOfYear`/`DayOfMonth`/`DayOfYear`
  type aliases) → `Std.Time.Date.Unit.Year.Offset`/`.Month.Ordinal`/
  `.Day.Ordinal`/`.Day.Ordinal.OfYear`.
- `Data.Time.Calendar.Private` (internal `show2`/`show4`/`clip`/`clipValid`/
  `monthLength` helpers) — no standalone port; each real caller among the
  genuinely-ported modules below (`Julian`, `Easter`) restates the one or two
  helpers it actually needs directly, the same "don't port an internal
  grab-bag module for its own sake" treatment `hoauth2`'s note on
  `microlens` already applies.
- `Data.Time.Clock`/`.Clock.Internal.DiffTime`/`.NominalDiffTime`/`.UTCTime`/
  `.SystemTime`/`.POSIXTime` (`UTCTime`, `DiffTime`, `NominalDiffTime`,
  `getCurrentTime`, POSIX-seconds conversions) → `Std.Time.Duration`
  (`Duration`, nanosecond-precision interval type) plus
  `Std.Time.DateTime.Timestamp`/`.WallTime` (`Timestamp.now`,
  `Timestamp.ofDurationSinceUnixEpoch`, `WallTime.ofDuration`) — `Std.Time`
  represents both "UTC instant" and "elapsed duration" with one coherent
  nanosecond-precision `Duration`/`Timestamp` pair instead of `time`'s
  historical four-type split (`UTCTime`/`DiffTime`/`NominalDiffTime`/
  `POSIXTime`), a strict simplification with no lost behavior. (This is the
  `Std.Time` counterpart `Linen.Data.Time.Clock`'s `UTCTime`/`NominalDiffTime`
  should be rebuilt on, per the status note above — including fixing that
  module's `getCurrentTime`, which currently reads a monotonic clock, not
  wall-clock time.)
- `Data.Time.Clock.Internal.UTCDiff` (`addUTCTime`/`diffUTCTime`) →
  `Timestamp`/`Duration`'s own `HAdd`/`HSub` instances.
- `Data.Time.Clock.System`/`.Clock.POSIX` → `Std.Time.DateTime.Timestamp`
  (already POSIX-epoch-based: `Timestamp.now`, `.ofDurationSinceUnixEpoch`,
  `.toSecondsSinceUnixEpoch`).
- `Data.Time.LocalTime`/`.LocalTime.Internal.TimeZone`/`.TimeOfDay`/
  `.LocalTime`/`.ZonedTime`/`.Foreign` (`TimeZone`, `TimeOfDay`, `LocalTime`,
  `ZonedTime`, `getCurrentTimeZone`) → `Std.Time.Zoned` (`TimeZone`,
  `TimeZone.Offset`, `Zoned.DateTime`, `Zoned.ZonedDateTime`,
  `Zoned.Database` for the IANA tzdata lookup that backs
  `getCurrentTimeZone`) plus `Std.Time.Time.PlainTime` for the time-of-day
  component. `Std.Time`'s IANA-tzdata-backed `TimeZone`/`Database` is a more
  complete substitute than `time`'s `Foreign`-based single-offset
  `getCurrentTimeZone` (it has real DST-transition rules via `ZoneRules`,
  not just "the current offset"). This is likewise the `Std.Time` counterpart
  `Linen.Data.Time.LocalTime`'s `TimeOfDay`/`TimeZone` should be rebuilt on.
- `Data.Time.Format`/`.Format.Internal`/`.Format.Format.Class`/
  `.Format.Format.Instances` (the `formatTime` pattern-letter engine) →
  `Std.Time.Format`/`Std.Time.Format.Basic` (`Text`/`Number`/`Fraction`/
  `Year`/`Modifier`, `GenericFormat`, `datespec` notation) — a
  same-shape reimplementation of the *identical* Java-`DateTimeFormatter`-
  style pattern-letter table `time`'s own `formatTime` follows (`y`, `M`,
  `d`, `H`, `m`, `s`, `S`, `z`, `Z`, `X`, etc.), already covering every
  pattern letter `time` supports.
- `Data.Time.Format.ISO8601` → `Std.Time.Formats.iso8601`/`.dateTimeWithZone`
  (in `Std.Time.Format`) plus the general `datespec` machinery for any ISO
  8601 variant not already named.
- `Data.Time.Format.Parse`/`.Format.Parse.Class`/`.Format.Parse.Instances` →
  `Std.Time.Format`'s parser side, built on `Std.Internal.Parsec.String`
  (the same parser-combinator foundation `time`'s own `Parse` module is built
  on, `Text.ParserCombinators.ReadP`, just Lean's rather than GHC's).
- `Data.Time.Format.Locale` (`TimeLocale`, `defaultTimeLocale`: locale
  month/weekday/AM-PM/era names) → **fully** covered by
  `Std.Time.Format.DateFormat.DateFormatSymbols`/`DateFormat.enUS` — verified
  by reading the actual Lean source, not assumed: `Std.Time` ships its own
  built-in `enUS` locale table with full/short/narrow month names, weekday
  names, era names, quarter names, and AM/PM markers, a strict superset of
  `defaultTimeLocale`'s month/weekday/AM-PM-only table (it additionally has
  era and quarter names, and short/narrow variants `time` doesn't). The one
  piece of `defaultTimeLocale` with no `Std.Time` equivalent —
  `knownTimeZones`, ten hardcoded RFC 822 abbreviation→offset pairs (`"EST"`,
  `"PST"`, …) — is intentionally not ported: `Std.Time.Zoned.Database`'s
  IANA-tzdata lookup is the more correct replacement for "resolve a timezone
  name to an offset" and already ships in `linen` transitively via
  `Std.Time`.
- `Data.Format` (generic zero-padded integer `show` helper, an other-module
  used by the calendar `Show` instances) → no port needed; every genuinely
  ported module below either reuses `Std.Time`'s existing padding (see
  `Duration`'s `leftPad` in `Std.Time.Duration`) or needs at most a one-line
  `Nat.repr` zero-pad, inlined at the call site rather than factored into a
  shared module for two call sites — the same "don't port an abstraction the
  task doesn't need" reasoning as `hoauth2`'s `microlens` note.

## Genuinely new `Linen.*` ports

The following have real behavior with **no** `Std.Time` equivalent — checked
by grepping `Std.Time`'s actual source tree for the relevant vocabulary
(`julian`, `easter`, `leap second`, `tai`) and finding nothing beyond the raw
leap-second *records* `Std.Time.Zoned.Database.TzIf`'s RFC 8536 TZif parser
already extracts from tzdata (no `AbsoluteTime` type or UTC⇄TAI conversion
built on top of them). Namespaced under `Linen.Time.*` per AGENTS.md's
module-hierarchy rule (not mirroring `time`'s flat `Data.Time.Calendar.*`
layout, and not `Linen.Data.Time.*` either, to avoid perpetuating the
pre-existing four modules' naming — see the status note above). Topologically
sorted:

1. **`Data.Time.Calendar.CalendarDiffDays`** → `Linen.Time.Calendar.CalendarDiffDays`
   — a calendrical period (`months`, `days`), `Semigroup`/`Monoid` under
   addition, `calendarDay`/`calendarWeek`/`calendarMonth`/`calendarYear`
   constants, and a scale-by-integer operation. No dependency on any other
   new module.
2. **`Data.Time.Calendar.Month`** → `Linen.Time.Calendar.Month` — an
   absolute count of calendar months since a fixed origin (`(year * 12) +
   (monthOfYear - 1)`), with `addMonths`/`diffMonths` and a `DayPeriod`-style
   `periodFirstDay`/`periodLastDay`/`dayPeriod` triple relating it to
   `Std.Time.Date.PlainDate`. `Std.Time` has month-of-year as a *field*
   (`PlainDate.month`, `Month.Ordinal` 1–12) but no standalone "the n-th month
   since epoch" counter type with this arithmetic; genuinely new. Depends
   only on `Std.Time.Date.PlainDate`, no other new module.
3. **`Data.Time.Calendar.Quarter`** → `Linen.Time.Calendar.Quarter` — same
   shape as `Month` one level up (`QuarterOfYear` + absolute `Quarter`
   counter, `addQuarters`/`diffQuarters`, `monthQuarter`/`dayQuarter`). Same
   gap reasoning as `Month`: `Std.Time`'s `PlainDate.quarter` is a per-date
   *field* (`Bounded.LE 1 4`), not an absolute counter type. On #2
   (`monthQuarter` is defined in terms of `Month`).
4. **`Data.Time.Calendar.Julian`** (+ its `other-module` helper
   `Data.Time.Calendar.JulianYearDay`, folded in — it exists upstream purely
   to support `Julian.hs`, the same "internal helper with one real caller"
   treatment `CipherAes`'s note gives `crypto-api`) → `Linen.Time.Calendar.Julian`
   — the **proleptic Julian calendar**: `toJulian`/`fromJulian`
   (year/month/day ⇄ `PlainDate`'s underlying day count), its own leap-year
   rule (`year % 4 == 0`, no Gregorian century correction), month lengths,
   and the `addJulianMonthsClip`/`RollOver`/`addJulianYearsClip`/`RollOver`/
   `addJulianDurationClip`/`RollOver`/`diffJulianDurationClip`/`RollOver`
   arithmetic family (on `CalendarDiffDays`, #1). This is a different
   calendar system from the Gregorian one `Std.Time` implements throughout —
   not a simplification target, a real second leap-year rule with its own
   month-length table. On #1.
5. **`Data.Time.Calendar.Easter`** → `Linen.Time.Calendar.Easter` — the
   Gregorian and Orthodox **Easter-date algorithms** (`gregorianEaster`/
   `orthodoxEaster` and their `PaschalMoon` helpers, per Reingold &
   Dershowitz's *Calendrical Calculations* ch. 8) plus `sundayAfter`. The
   Orthodox variant is defined in terms of the Julian calendar (#4); the
   Gregorian variant in terms of `Std.Time.Date.PlainDate` directly. A
   genuine algorithm with no `Std.Time` counterpart (confirmed: no `easter`
   hit anywhere in `Std.Time`'s source). On #4.
6. **`Data.Time.LocalTime.Internal.CalendarDiffTime`** →
   `Linen.Time.CalendarDiffTime` — the time-valued sibling of
   `CalendarDiffDays` (#1): `(months, Duration)` instead of `(months, days)`,
   `Semigroup`/`Monoid`, `calendarTimeDays`/`calendarTimeTime`,
   `scaleCalendarDiffTime`. Built directly on `Std.Time.Duration` rather than
   porting a separate `NominalDiffTime` (already substituted above). On #1.
7. **`Data.Time.Clock.Internal.UniversalTime`** (+ the `ut1ToLocalTime`/
   `localTimeToUT1` pair from `Data.Time.LocalTime.Internal.LocalTime`, the
   only real logic in that otherwise-substituted module — see the
   substitution list above) → `Linen.Time.UniversalTime` — `UT1`, mean solar
   time as a Modified-Julian-Date-plus-fraction rational, and its
   longitude-parameterised conversion to/from `Std.Time`'s local wall-clock
   time (`Std.Time.DateTime.WallTime`/`Std.Time.Time.PlainTime`). A small,
   fully-specified, deterministic bit of arithmetic with no GHC/FFI
   dependency and no `Std.Time` equivalent (`Std.Time` only models civil/UTC
   time, never earth-rotation-based UT1) — kept rather than dropped, since
   AGENTS.md is explicit that "must prove everything" is not license to wave
   away real, in-scope behavior. No dependency on any other new module here.
8. **`Data.Time.Clock.TAI`** (+ `Data.Time.Clock.Internal.AbsoluteTime`) →
   `Linen.Time.Clock.TAI` — `AbsoluteTime` (a TAI instant), `taiEpoch`/
   `addAbsoluteTime`/`diffAbsoluteTime`, and the day-keyed leap-second-map
   conversions `utcToTAITime`/`taiToUTCTime`/`utcDayLength` (`LeapSecondMap =
   Day -> Option Int`, deliberately caller-supplied upstream too — "no table
   is provided, as any program compiled with it would become out of date in
   six months", ported verbatim). This is a genuine gap: `Std.Time`'s RFC
   8536 TZif parser (`Std.Time.Zoned.Database.TzIf.LeapSecond`) already
   extracts the *raw* leap-second transition records baked into tzdata's
   `right/` zone files, but exposes no `AbsoluteTime` type and no
   day-granularity UTC⇄TAI conversion on top of them — this module supplies
   exactly that missing layer, and can optionally source its
   `LeapSecondMap` from `Std.Time`'s already-parsed `TzIf.LeapSecond`
   records rather than requiring the caller to hand-roll one. `time`'s own
   `taiClock` (an `IO`-returning `Maybe (DiffTime, IO AbsoluteTime)` sourced
   from a platform TAI clock syscall) is dropped: it depends on
   `getTAISystemTime`, itself part of the GHC/FFI `SystemTime` machinery
   already excluded above, and is `Nothing` on most systems in practice
   (upstream's own doc comment: "unlikely to be set correctly, without due
   care and attention"). No dependency on any other new module here (uses
   `Std.Time.Duration` directly for the `Int`-seconds correction offsets).

8 new modules total (2 upstream `other-modules` folded into their sole real
caller per above, `CTimeval`/`CTimespec`/`Data.Format`/`Calendar.Private`
dropped as internal helpers with no standalone port needed), out of 16
upstream `exposed-modules` + 20 `other-modules` (36 total Haskell modules).

## Native C library

None. `time` itself only needs native code (`lib/cbits/HsTime.c`,
`gettimeofday`/`clock_gettime`) to read the wall clock, and that need is
already met by `Std.Time`/the Lean runtime (see the "External dependencies"
section above) — no new `lakefile.lean` changes, no new FFI shim.
