/-
  Linen.Network.HTTP.Date — HTTP date parsing and formatting

  Parses and formats HTTP dates per RFC 7231:
  - IMF-fixdate: `Sun, 06 Nov 1994 08:49:37 GMT`
  - RFC 850:     `Sunday, 06-Nov-94 08:49:37 GMT`
  - asctime:     `Sun Nov  6 08:49:37 1994`

  ## Design

  Hand-written parser (no attoparsec needed — HTTP dates are fixed-layout).
  Uses `String.take`/`String.drop` for substring extraction instead of
  `String.extract` with position literals.
-/

namespace Network.HTTP.Date

/-- An HTTP date with bounded fields.
    $$\text{HTTPDate} = \{ y : \mathbb{N},\; m : [1,12],\; d : [1,31],\;
    h : [0,23],\; \min : [0,59],\; s : [0,60] \}$$
    (seconds up to 60 for leap seconds) -/
structure HTTPDate where
  year   : Nat
  month  : Nat  -- 1..12
  day    : Nat  -- 1..31
  hour   : Nat  -- 0..23
  minute : Nat  -- 0..59
  second : Nat  -- 0..60 (leap second)
deriving BEq, Repr

instance : ToString HTTPDate where
  toString d :=
    let pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else s!"{n}"
    s!"{d.year}-{pad2 d.month}-{pad2 d.day} {pad2 d.hour}:{pad2 d.minute}:{pad2 d.second}"

namespace HTTPDate

private def months : Array String :=
  #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

private def weekdays : Array String :=
  #["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

private def parseMonth (s : String) : Option Nat :=
  match months.findIdx? (· == s) with
  | some i => some (i + 1)
  | none => none

private def parseNat (s : String) : Option Nat := s.trimAscii.toNat?

/-- Extract a substring by byte offsets (for ASCII-only HTTP dates). -/
private def slice (s : String) (start len : Nat) : String :=
  ((s.drop start).take len).toString

/-- Parse an HTTP date string. Supports IMF-fixdate and asctime formats.
    $$\text{parseHTTPDate} : \text{String} \to \text{Option}(\text{HTTPDate})$$ -/
def parseHTTPDate (s : String) : Option HTTPDate := do
  -- Try IMF-fixdate: "Sun, 06 Nov 1994 08:49:37 GMT"
  let chars := s.toList
  if s.length >= 29 && chars.getD 3 ' ' == ',' then
    let day ← parseNat (slice s 5 2)
    let month ← parseMonth (slice s 8 3)
    let year ← parseNat (slice s 12 4)
    let hour ← parseNat (slice s 17 2)
    let minute ← parseNat (slice s 20 2)
    let second ← parseNat (slice s 23 2)
    if month >= 1 && month <= 12 && day >= 1 && day <= 31 &&
       hour <= 23 && minute <= 59 && second <= 60 then
      some ⟨year, month, day, hour, minute, second⟩
    else none
  -- Try asctime: "Sun Nov  6 08:49:37 1994"
  else if s.length >= 24 && chars.getD 3 ' ' == ' ' then
    let month ← parseMonth (slice s 4 3)
    let day ← parseNat (slice s 8 2)
    let hour ← parseNat (slice s 11 2)
    let minute ← parseNat (slice s 14 2)
    let second ← parseNat (slice s 17 2)
    let year ← parseNat (slice s 20 4)
    if month >= 1 && month <= 12 && day >= 1 && day <= 31 &&
       hour <= 23 && minute <= 59 && second <= 60 then
      some ⟨year, month, day, hour, minute, second⟩
    else none
  else none

/-- Format an HTTP date in IMF-fixdate format (RFC 7231 preferred).
    $$\text{formatHTTPDate} : \text{HTTPDate} \to \text{String}$$
    Note: day-of-week is computed from the date. -/
def formatHTTPDate (d : HTTPDate) : String :=
  let pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else s!"{n}"
  let monthStr := months.getD (d.month - 1) "Jan"
  -- Zeller's congruence for day of week (simplified)
  let (y, m) := if d.month <= 2 then (d.year - 1, d.month + 12) else (d.year, d.month)
  let dow := (d.day + (13 * (m + 1)) / 5 + y + y / 4 - y / 100 + y / 400) % 7
  -- Zeller: 0=Sat, 1=Sun, 2=Mon, ...
  let dowIdx := match dow with
    | 0 => 6  -- Sat
    | 1 => 0  -- Sun
    | 2 => 1  -- Mon
    | 3 => 2  -- Tue
    | 4 => 3  -- Wed
    | 5 => 4  -- Thu
    | _ => 5  -- Fri
  let dayName := weekdays.getD dowIdx "Sun"
  s!"{dayName}, {pad2 d.day} {monthStr} {d.year} {pad2 d.hour}:{pad2 d.minute}:{pad2 d.second} GMT"

end HTTPDate
end Network.HTTP.Date
