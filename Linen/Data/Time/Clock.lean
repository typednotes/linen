/-
  Linen.Data.Time.Clock — UTC time and durations

  Provides a Haskell-compatible `Data.Time.Clock` API (`UTCTime`,
  `NominalDiffTime`, `getCurrentTime`), built on Lean's own `Std.Time` rather
  than ad hoc arithmetic — per `docs/imports/Time/dependencies.md`'s status
  note, this is a reconciliation of a module ported ad hoc while working on
  `sqlite-simple`, before this codebase's import process had `Std.Time` in
  its precedence analysis.

  ## Design

  `NominalDiffTime` wraps `Std.Time.Duration` (a nanosecond-precision signed
  interval) and `UTCTime` wraps `Std.Time.DateTime.Timestamp` (a
  nanosecond-precision UTC instant, POSIX-epoch-based) — `Std.Time`'s own
  coherent nanosecond-precision pair standing in for `time`'s historical
  four-type split (`UTCTime`/`DiffTime`/`NominalDiffTime`/`POSIXTime`), per
  the dependencies plan's substitution list. Every public function keeps its
  original name/signature from the ad hoc port, so callers (e.g.
  `Linen.Database.SQLite.Simple.Time.Implementation`) are unaffected by the
  representation change.

  ## Bug fixed by this reconciliation

  The ad hoc port's `getCurrentTime` called `IO.monoNanosNow`, a *monotonic*
  clock (arbitrary epoch, not tied to calendar time) — not real wall-clock
  time. `Std.Time.DateTime.Timestamp.now` gives a genuine wall-clock reading
  (POSIX-epoch-based), which this module now uses instead.
-/
import Std.Time

namespace Data.Time

-- ══════════════════════════════════════════════════════════════
-- NominalDiffTime
-- ══════════════════════════════════════════════════════════════

/-- A nominal time difference, backed by `Std.Time.Duration` (nanosecond
    precision). Positive values represent future offsets.
    $$\text{NominalDiffTime} \cong \mathbb{Z}\ \text{nanoseconds}$$ -/
structure NominalDiffTime where
  /-- The underlying `Std.Time.Duration`. -/
  toDuration : Std.Time.Duration
deriving Repr, DecidableEq, Inhabited

namespace NominalDiffTime

instance : BEq NominalDiffTime where
  beq a b := decide (a.toDuration = b.toDuration)

/-- Zero duration. -/
@[inline] def zero : NominalDiffTime := ⟨0⟩

/-- Create a duration from seconds (with nanosecond precision).
    $$\text{fromSeconds}(s) = s \times 10^9\;\text{ns}$$ -/
@[inline] def fromSeconds (s : Int) : NominalDiffTime :=
  ⟨Std.Time.Duration.ofSeconds (.ofInt s)⟩

/-- Create a duration from milliseconds.
    $$\text{fromMilliseconds}(ms) = ms \times 10^6\;\text{ns}$$ -/
@[inline] def fromMilliseconds (ms : Int) : NominalDiffTime :=
  ⟨Std.Time.Duration.ofMillisecond (.ofInt ms)⟩

/-- Create a duration from microseconds.
    $$\text{fromMicroseconds}(\mu s) = \mu s \times 10^3\;\text{ns}$$ -/
@[inline] def fromMicroseconds (us : Int) : NominalDiffTime :=
  ⟨Std.Time.Duration.ofNanoseconds (.ofInt (us * 1000))⟩

/-- Convert to seconds (truncating). -/
@[inline] def toSeconds (d : NominalDiffTime) : Int :=
  d.toDuration.toSeconds.val

/-- Convert to milliseconds (truncating). -/
@[inline] def toMilliseconds (d : NominalDiffTime) : Int :=
  d.toDuration.toMilliseconds.val

/-- Convert to microseconds (truncating). -/
@[inline] def toMicroseconds (d : NominalDiffTime) : Int :=
  d.toDuration.toNanoseconds.val / 1000

instance : Add NominalDiffTime where
  add a b := ⟨a.toDuration + b.toDuration⟩

instance : Sub NominalDiffTime where
  sub a b := ⟨a.toDuration - b.toDuration⟩

instance : Neg NominalDiffTime where
  neg a := ⟨a.toDuration.neg⟩

instance : Ord NominalDiffTime where
  compare a b := compare a.toDuration b.toDuration

instance : ToString NominalDiffTime where
  toString d := toString d.toDuration

-- ── Proofs ──

theorem fromSeconds_toSeconds (n : Int) : (fromSeconds n).toSeconds = n := by
  simp [fromSeconds, toSeconds, Std.Time.Duration.ofSeconds, Std.Time.Duration.toSeconds,
    Std.Time.Second.Offset.ofInt]

end NominalDiffTime

-- ══════════════════════════════════════════════════════════════
-- UTCTime
-- ══════════════════════════════════════════════════════════════

/-- A point in time, backed by `Std.Time.DateTime.Timestamp` (a
    nanosecond-precision instant since the Unix epoch, 1970-01-01 00:00:00
    UTC). $$\text{UTCTime} \cong \mathbb{N}\ \text{nanoseconds since epoch}$$ -/
structure UTCTime where
  /-- The underlying `Std.Time.DateTime.Timestamp`. -/
  toTimestamp : Std.Time.Timestamp
deriving Repr, DecidableEq, Inhabited

namespace UTCTime

instance : BEq UTCTime where
  beq a b := decide (a.toTimestamp = b.toTimestamp)

instance : Ord UTCTime where
  compare a b := compare a.toTimestamp b.toTimestamp

instance : ToString UTCTime where
  toString t :=
    let secs := t.toTimestamp.toSecondsSinceUnixEpoch.val
    s!"UTCTime({secs}s)"

/-- Build a `UTCTime` directly from a nanoseconds-since-epoch count, matching
    the ad hoc port's original representation (kept as a compatibility
    constructor for callers, e.g.
    `Linen.Database.SQLite.Simple.Time.Implementation`, that build a
    `UTCTime` from raw nanosecond arithmetic). -/
@[inline] def ofNanosSinceEpoch (n : Nat) : UTCTime :=
  ⟨Std.Time.Timestamp.ofNanosecondsSinceUnixEpoch (.ofInt (n : Int))⟩

/-- Nanoseconds since the Unix epoch. -/
@[inline] def nanosSinceEpoch (t : UTCTime) : Nat :=
  t.toTimestamp.toNanosecondsSinceUnixEpoch.val.toNat

end UTCTime

/-- Get the current UTC time — genuine wall-clock time (`Std.Time.
    DateTime.Timestamp.now`), not a monotonic reading.
    $$\text{getCurrentTime} : \text{IO}(\text{UTCTime})$$ -/
def getCurrentTime : IO UTCTime := do
  let ts ← Std.Time.Timestamp.now
  pure ⟨ts⟩

/-- Compute the difference between two times: `t1 - t2`.
    $$\text{diffUTCTime}(t_1, t_2) = t_1 - t_2$$ -/
@[inline] def diffUTCTime (t1 t2 : UTCTime) : NominalDiffTime :=
  ⟨Std.Time.Duration.ofNanoseconds
    (.ofInt ((t1.nanosSinceEpoch : Int) - (t2.nanosSinceEpoch : Int)))⟩

/-- Add a duration to a time.
    $$\text{addUTCTime}(\Delta t, t) = t + \Delta t$$ -/
@[inline] def addUTCTime (dt : NominalDiffTime) (t : UTCTime) : UTCTime :=
  UTCTime.ofNanosSinceEpoch ((t.nanosSinceEpoch : Int) + dt.toDuration.toNanoseconds.val).toNat

-- ── Proofs ──

theorem diffUTCTime_self (t : UTCTime) : diffUTCTime t t = NominalDiffTime.zero := by
  simp only [diffUTCTime, Int.sub_self]
  decide

end Data.Time
