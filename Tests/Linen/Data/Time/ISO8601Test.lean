/-
  Tests for `Linen.Data.Time.ISO8601`.

  Pure, so everything is checked with `#guard`. The two headline instants are
  the timestamps AWS uses in its own Signature Version 4 worked examples
  (`20130524T000000Z` from the S3 GET example, `20150830T123600Z` from the
  published signing test suite), since reproducing those byte-for-byte is
  what this module exists for.
-/
import Linen.Data.Time.ISO8601

open Data.Time Data.Time.ISO8601

namespace Tests.Data.Time.ISO8601

/-- An instant given as whole seconds since the Unix epoch. -/
private def epoch (s : Nat) : UTCTime := UTCTime.ofNanosSinceEpoch (s * 1000000000)

-- ── Padding ──

#guard pad 4 7 == "0007"
#guard pad 2 7 == "07"
#guard pad 2 70 == "70"
#guard pad 1 0 == "0"
#guard pad 0 5 == "5"
#guard pad 2 123 == "123"       -- wider than asked: not truncated

-- ── Basic format ──

#guard basicDateTime (epoch 0) == "19700101T000000Z"
#guard basicDate (epoch 0) == "19700101"
#guard basicTime (epoch 0) == "000000"

-- AWS SigV4 worked example (S3 GET), and the suite's own timestamp.
#guard basicDateTime (epoch 1369353600) == "20130524T000000Z"
#guard basicDateTime (epoch 1440938160) == "20150830T123600Z"
#guard basicDate (epoch 1440938160) == "20150830"
#guard basicTime (epoch 1440938160) == "123600"

-- Exactly sixteen characters: eight date, `T`, six time, `Z`.
#guard (basicDateTime (epoch 1440938160)).length == 16
#guard (basicDate (epoch 1440938160)).length == 8

-- ── Calendar edge cases ──

#guard basicDate (epoch 951782400) == "20000229"    -- 2000 is a leap year
#guard basicDate (epoch 1078012800) == "20040229"   -- so is 2004
#guard basicDate (epoch 978220800) == "20001231"    -- last day of a leap year
#guard basicDate (epoch 978307200) == "20010101"    -- and the next day

-- Single-digit month, day, hour, minute and second all keep their zeros —
-- the whole point of the padding.
#guard basicDateTime (epoch 1041382921) == "20030101T010201Z"

-- ── Extended format ──

#guard extendedDate (epoch 1440938160) == "2015-08-30"
#guard extendedDateTime (epoch 1440938160) == "2015-08-30T12:36:00Z"
#guard extendedDateTime (epoch 0) == "1970-01-01T00:00:00Z"

-- ── Agreement between the two formats ──

-- Basic format is extended format with the separators removed.
#guard (extendedDate (epoch 1440938160)).replace "-" "" == basicDate (epoch 1440938160)

/-! ### Signatures -/

example : Nat → Nat → String := pad
example : UTCTime → String := basicDate
example : UTCTime → String := basicTime
example : UTCTime → String := basicDateTime
example : UTCTime → String := extendedDate
example : UTCTime → String := extendedDateTime

end Tests.Data.Time.ISO8601
