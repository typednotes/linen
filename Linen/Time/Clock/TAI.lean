/-
  Linen.Time.Clock.TAI — TAI instants and leap-second-aware UTC conversion

  Module #8 of `docs/imports/Time/dependencies.md`'s "Genuinely new
  `Linen.*` ports" list. Ports `Data.Time.Clock.TAI` from Hackage's `time`
  package (v1.15,
  https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Clock/TAI.hs),
  folding in its upstream `other-module` helper
  `Data.Time.Clock.Internal.AbsoluteTime`
  (https://raw.githubusercontent.com/haskell/time/1.15/lib/Data/Time/Clock/Internal/AbsoluteTime.hs)
  — the same "internal helper with one real caller" treatment given to
  `Linen.Time.Calendar.Julian`'s `JulianYearDay` helper.

  This is a genuine gap in `Std.Time`: its RFC 8536 TZif parser
  (`Std.Time.Zoned.Database.TzIf.LeapSecond`) already extracts the *raw*
  leap-second transition records baked into tzdata's `right/` zone files,
  but exposes no `AbsoluteTime` type and no day-granularity UTC⇄TAI
  conversion on top of them — this module supplies exactly that missing
  layer.

  ## Design

  `AbsoluteTime` is TAI, an instant measured relative to the fixed
  `taiEpoch` (1858-11-17 00:00:00 TAI, i.e. Modified Julian Day `0`) by a
  `Std.Time.Duration` — the substitute for upstream's own `DiffTime` per the
  dependencies plan's substitution list.

  Upstream's `Day` (`ModifiedJulianDay Integer`) is, here, kept as a bare
  `Int` **Modified Julian Day** count rather than converted to `Std.Time`'s
  Unix-epoch-based `Day.Offset` convention (unlike
  `Linen.Time.Calendar.Julian`/`Linen.Time.Calendar.Easter`, which convert
  throughout): `taiEpoch` is fixed at MJD `0` by definition, so every
  formula below (`taiNominalDayStart`, `dayStart`) is already stated
  directly in terms of the MJD count, with no Unix-epoch offset to
  introduce.

  Upstream's `UTCTime` (a `Day` paired with a `DiffTime` *since midnight
  that day*, deliberately able to exceed `86400` seconds during a leap
  second) has no `Std.Time` substitute either: `Std.Time.PlainDateTime`'s
  `PlainTime` component is bounded to `0 ≤ t < 86400` seconds and so cannot
  represent the leap second itself, which is the entire reason this
  module's `LeapSecondMap`/`utcDayLength` exist. This is a genuine
  Std.Time gap the way the module doc above describes, not a simplification
  of upstream's own semantics — so a minimal local `UTCTime` (`day`,
  `dayTime : Std.Time.Duration`) stands in for it, matching upstream's own
  representation exactly instead of forcing a lossy `PlainDateTime`.

  `LeapSecondMap` is, per upstream's own doc comment ("no table is
  provided, as any program compiled with it would become out of date in
  six months"), deliberately left caller-supplied — ported verbatim, with
  no hardcoded leap-second table. Upstream's `taiClock` (an `IO`-returning
  `Maybe (DiffTime, IO AbsoluteTime)` sourced from a platform TAI clock
  syscall, `getTAISystemTime`) is dropped: it depends on GHC/FFI
  `SystemTime` machinery already out of scope per the dependencies plan,
  and is `Nothing` on most systems in practice (upstream's own doc comment:
  "unlikely to be set correctly, without due care and attention").
-/
import Std.Time

namespace Time.Clock.TAI

/-- A Modified Julian Day count (days since 1858-11-17), matching
    upstream's `Day`/`ModifiedJulianDay` directly — `taiEpoch` below is
    fixed at MJD `0`, so every function here is naturally stated in terms
    of this count rather than `Std.Time`'s Unix-epoch-based `Day.Offset`. -/
abbrev MJD := Int

/-- TAI: an absolute instant, measured as a `Std.Time.Duration` elapsed
    since `taiEpoch`. -/
structure AbsoluteTime where
  /-- The elapsed `Duration` since `taiEpoch` (may be negative). -/
  sinceEpoch : Std.Time.Duration
deriving Repr, DecidableEq

namespace AbsoluteTime

/-- The epoch of TAI: 1858-11-17 00:00:00 TAI (Modified Julian Day `0`). -/
def taiEpoch : AbsoluteTime := ⟨0⟩

/-- The TAI instant at the nominal (leap-second-oblivious) start of a given
    Modified Julian Day. -/
def taiNominalDayStart (day : MJD) : AbsoluteTime :=
  ⟨Std.Time.Duration.ofSeconds (.ofInt (day * 86400))⟩

/-- `addAbsoluteTime a b = a + b`. -/
def addAbsoluteTime (t : Std.Time.Duration) (a : AbsoluteTime) : AbsoluteTime :=
  ⟨a.sinceEpoch + t⟩

/-- `diffAbsoluteTime a b = a - b`. -/
def diffAbsoluteTime (a b : AbsoluteTime) : Std.Time.Duration :=
  a.sinceEpoch - b.sinceEpoch

end AbsoluteTime

open AbsoluteTime

-- ── Leap-second-aware UTC⇄TAI conversion ──

/-- A minimal substitute for upstream's `UTCTime`: a Modified Julian Day
    paired with the elapsed `Duration` since that day's nominal midnight,
    deliberately able to reach (and, during a leap second, exceed)
    `86400` seconds — see the module doc for why `Std.Time.PlainDateTime`
    cannot stand in for this. -/
structure UTCTime where
  /-- The Modified Julian Day. -/
  day : MJD
  /-- The elapsed `Duration` since that day's nominal midnight. -/
  dayTime : Std.Time.Duration
deriving Repr, DecidableEq

/-- `TAI - UTC` during a given day, as a caller-supplied table: "no table
    is provided, as any program compiled with it would become out of date
    in six months" (upstream's own doc comment). -/
abbrev LeapSecondMap := MJD → Option Int

/-- The length, in seconds (as a `Duration`), of a given UTC day — `86400`
    plus the change in the TAI–UTC offset across it (i.e. `86401` or
    `86399` on a leap-second day). `none` if either day is outside the
    map's domain. -/
def utcDayLength (lsmap : LeapSecondMap) (day : MJD) : Option Std.Time.Duration := do
  let i0 ← lsmap day
  let i1 ← lsmap (day + 1)
  some (Std.Time.Duration.ofSeconds (.ofInt (86400 + i1 - i0)))

/-- The TAI instant at the nominal start of a given UTC day (accounting for
    that day's accumulated TAI–UTC offset). `none` if the day is outside
    the map's domain. -/
def dayStart (lsmap : LeapSecondMap) (day : MJD) : Option AbsoluteTime := do
  let i ← lsmap day
  some (addAbsoluteTime (Std.Time.Duration.ofSeconds (.ofInt (day * 86400 + i))) taiEpoch)

/-- Convert a UTC time to TAI, given a `LeapSecondMap`. `none` if the
    time's day is outside the map's domain. -/
def utcToTAITime (lsmap : LeapSecondMap) (t : UTCTime) : Option AbsoluteTime := do
  let dayt ← dayStart lsmap t.day
  some (addAbsoluteTime t.dayTime dayt)

/-- Convert a TAI time to UTC, given a `LeapSecondMap`. Walks forward or
    backward from an initial day estimate until it lands on the day whose
    nominal start and length actually bracket the given instant — mirrors
    upstream's `stable` fixed-point search, but bounded to a fixed window
    of candidate days (rather than upstream's unbounded recursion) so the
    definition is structurally terminating: a UTC day differs from
    `86400` seconds by at most a couple of leap seconds, so a real input
    only ever needs to step by one or two days at most, well inside the
    window below. `none` if the search exhausts the window, or any day it
    visits is outside the map's domain. -/
def taiToUTCTime (lsmap : LeapSecondMap) (abstime : AbsoluteTime) : Option UTCTime := do
  let day0 := (diffAbsoluteTime abstime taiEpoch).toSeconds.val / 86400
  let rec stable (fuel : Nat) (day : MJD) : Option UTCTime :=
    match fuel with
    | 0 => none
    | fuel + 1 => Id.run do
      match dayStart lsmap day, utcDayLength lsmap day with
      | some dayt, some len =>
        let dtime := diffAbsoluteTime abstime dayt
        let lenSecs := len.toSeconds.val
        let dtimeSecs := dtime.toSeconds.val
        let step : Int := if lenSecs ≤ 0 then 0 else Int.ediv dtimeSecs lenSecs
        let day' := day + step
        if day' == day then some ⟨day, dtime⟩ else stable fuel day'
      | _, _ => none
  stable 8 day0

end Time.Clock.TAI
