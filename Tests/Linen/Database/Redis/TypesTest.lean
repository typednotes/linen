import Linen.Database.Redis.Types

/-!
  Tests for `Linen.Database.Redis.Types`.
-/

open Database.Redis.Types
open Database.Redis.Protocol (Reply)

/-- `Except Reply α` has no general `BEq` (`Reply` doesn't derive `Repr`),
    so tests compare through this helper. -/
private def isOk [BEq α] (expected : α) : Except Reply α → Bool
  | .ok a => a == expected
  | .error _ => false

def isErr [BEq α] : Except Reply α → Bool
  | .ok (_ : α) => false
  | .error _ => true

-- ── RedisArg ──

#guard encode (α := ByteArray) "hi".toUTF8 == "hi".toUTF8
#guard encode (42 : Int) == "42".toUTF8
#guard encode (-7 : Int) == "-7".toUTF8

-- ── Hand-written number parsers ──

#guard readSignedDecimal "42".toUTF8 == some 42
#guard readSignedDecimal "-42".toUTF8 == some (-42)
#guard readSignedDecimal "42abc".toUTF8 == some 42
#guard readSignedDecimal "abc".toUTF8 == none

#guard readSignedExponential "3.5".toUTF8 == some 3.5
#guard readSignedExponential "-3.5".toUTF8 == some (-3.5)
#guard readSignedExponential "1e2".toUTF8 == some 100.0
#guard readSignedExponential "1.5e2".toUTF8 == some 150.0
#guard readSignedExponential "1.5e-1".toUTF8 == some 0.15
#guard readSignedExponential "abc".toUTF8 == none

-- ── RedisResult ──

#guard isOk (α := ByteArray) "hi".toUTF8 (decode (Reply.singleLine "hi".toUTF8))
#guard isOk (α := ByteArray) "hi".toUTF8 (decode (Reply.bulk (some "hi".toUTF8)))
#guard isErr (α := ByteArray) (decode (Reply.integer 1))

#guard isOk (α := Int) 42 (decode (Reply.integer 42))
#guard isOk (α := Int) 42 (decode (Reply.bulk (some "42".toUTF8)))
#guard isErr (α := Int) (decode (Reply.bulk (some "nope".toUTF8)))

#guard isOk (α := Float) 3.5 (decode (Reply.bulk (some "3.5".toUTF8)))

#guard isOk Status.ok (decode (Reply.singleLine "OK".toUTF8))
#guard isOk Status.pong (decode (Reply.singleLine "PONG".toUTF8))
#guard isOk (Status.status "FOO".toUTF8) (decode (Reply.singleLine "FOO".toUTF8))

#guard isOk RedisType.string (decode (Reply.singleLine "string".toUTF8))
#guard isErr (α := RedisType) (decode (Reply.singleLine "bogus".toUTF8))

#guard isOk true (decode (Reply.integer 1))
#guard isOk false (decode (Reply.integer 0))
#guard isOk false (decode (Reply.bulk none))
#guard isErr (α := Bool) (decode (Reply.integer 2))

#guard isOk (α := Option Int) none (decode (Reply.bulk none))
#guard isOk (α := Option Int) (some 5) (decode (Reply.integer 5))

#guard isOk (α := List Int) [1, 2, 3]
  (decode (Reply.multiBulk (some [Reply.integer 1, Reply.integer 2, Reply.integer 3])))
#guard isErr (α := List Int) (decode (Reply.integer 1))

#guard isOk (α := Int × Int) (1, 2)
  (decode (Reply.multiBulk (some [Reply.integer 1, Reply.integer 2])))

-- `decodeKeyValuePairs` decodes a flat multi-bulk of `2n` elements as `n`
-- key/value pairs (e.g. `HGETALL`'s reply shape).
#guard isOk (α := List (ByteArray × Int)) [("a".toUTF8, 1), ("b".toUTF8, 2)]
  (decodeKeyValuePairs (Reply.multiBulk (some
    [Reply.bulk (some "a".toUTF8), Reply.integer 1,
     Reply.bulk (some "b".toUTF8), Reply.integer 2])))
#guard isErr (α := List (ByteArray × Int))
  (decodeKeyValuePairs (Reply.multiBulk (some [Reply.bulk (some "a".toUTF8)])))
