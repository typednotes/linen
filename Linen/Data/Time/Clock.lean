/-
  Linen.Data.Time.Clock — UTC time and durations

  Wraps Lean's standard library time types to provide a Haskell-compatible
  `Data.Time.Clock` API.

  ## Design

  Uses Lean's built-in `IO.monoNanosNow` for high-resolution monotonic timing
  and nanosecond precision internally.

  ## Lean stdlib reuse

  We build on basic IO time primitives. `UTCTime` is represented as nanoseconds
  since epoch.
-/

namespace Data.Time

-- ══════════════════════════════════════════════════════════════
-- NominalDiffTime
-- ══════════════════════════════════════════════════════════════

/-- A nominal time difference in nanoseconds.
    $$\text{NominalDiffTime} = \mathbb{Z}$$
    Positive values represent future offsets. -/
structure NominalDiffTime where
  /-- Duration in nanoseconds. -/
  nanoseconds : Int
deriving BEq, Repr, Inhabited

namespace NominalDiffTime

/-- Zero duration. -/
@[inline] def zero : NominalDiffTime := ⟨0⟩

/-- Create a duration from seconds (with nanosecond precision).
    $$\text{fromSeconds}(s) = s \times 10^9\;\text{ns}$$ -/
@[inline] def fromSeconds (s : Int) : NominalDiffTime :=
  ⟨s * 1000000000⟩

/-- Create a duration from milliseconds.
    $$\text{fromMilliseconds}(ms) = ms \times 10^6\;\text{ns}$$ -/
@[inline] def fromMilliseconds (ms : Int) : NominalDiffTime :=
  ⟨ms * 1000000⟩

/-- Create a duration from microseconds.
    $$\text{fromMicroseconds}(\mu s) = \mu s \times 10^3\;\text{ns}$$ -/
@[inline] def fromMicroseconds (us : Int) : NominalDiffTime :=
  ⟨us * 1000⟩

/-- Convert to seconds (truncating). -/
@[inline] def toSeconds (d : NominalDiffTime) : Int :=
  d.nanoseconds / 1000000000

/-- Convert to milliseconds (truncating). -/
@[inline] def toMilliseconds (d : NominalDiffTime) : Int :=
  d.nanoseconds / 1000000

/-- Convert to microseconds (truncating). -/
@[inline] def toMicroseconds (d : NominalDiffTime) : Int :=
  d.nanoseconds / 1000

instance : Add NominalDiffTime where
  add a b := ⟨a.nanoseconds + b.nanoseconds⟩

instance : Sub NominalDiffTime where
  sub a b := ⟨a.nanoseconds - b.nanoseconds⟩

instance : Neg NominalDiffTime where
  neg a := ⟨-a.nanoseconds⟩

instance : Ord NominalDiffTime where
  compare a b := compare a.nanoseconds b.nanoseconds

instance : ToString NominalDiffTime where
  toString d :=
    let secs := d.nanoseconds / 1000000000
    let frac := (d.nanoseconds % 1000000000).natAbs
    if frac == 0 then s!"{secs}s"
    else s!"{secs}.{frac}s"

-- ── Proofs ──

theorem fromSeconds_toSeconds (n : Int) : (fromSeconds n).toSeconds = n := by
  simp [fromSeconds, toSeconds]

end NominalDiffTime

-- ══════════════════════════════════════════════════════════════
-- UTCTime
-- ══════════════════════════════════════════════════════════════

/-- A point in time, represented as nanoseconds since the Unix epoch (1970-01-01 00:00:00 UTC).
    $$\text{UTCTime} = \{ \text{nanos} : \mathbb{N} \}$$ -/
structure UTCTime where
  /-- Nanoseconds since Unix epoch. -/
  nanosSinceEpoch : Nat
deriving BEq, Repr, Inhabited

namespace UTCTime

instance : Ord UTCTime where
  compare a b := compare a.nanosSinceEpoch b.nanosSinceEpoch

instance : ToString UTCTime where
  toString t :=
    let secs := t.nanosSinceEpoch / 1000000000
    s!"UTCTime({secs}s)"

end UTCTime

/-- Get the current UTC time.
    $$\text{getCurrentTime} : \text{IO}(\text{UTCTime})$$ -/
def getCurrentTime : IO UTCTime := do
  let nanos ← IO.monoNanosNow
  pure ⟨nanos⟩

/-- Compute the difference between two times: `t1 - t2`.
    $$\text{diffUTCTime}(t_1, t_2) = t_1 - t_2$$ -/
@[inline] def diffUTCTime (t1 t2 : UTCTime) : NominalDiffTime :=
  ⟨(t1.nanosSinceEpoch : Int) - (t2.nanosSinceEpoch : Int)⟩

/-- Add a duration to a time.
    $$\text{addUTCTime}(\Delta t, t) = t + \Delta t$$ -/
@[inline] def addUTCTime (dt : NominalDiffTime) (t : UTCTime) : UTCTime :=
  ⟨((t.nanosSinceEpoch : Int) + dt.nanoseconds).toNat⟩

-- ── Proofs ──

theorem diffUTCTime_self (t : UTCTime) : diffUTCTime t t = NominalDiffTime.zero := by
  simp [diffUTCTime, NominalDiffTime.zero]

end Data.Time
