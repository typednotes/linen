import Linen.Database.Redis.Cluster.Command

/-!
  Tests for `Linen.Database.Redis.Cluster.Command`.
-/

open Database.Redis.Cluster.Command
open Database.Redis.Protocol (Reply)
open Database.Redis.Types (RedisResult)

private def isOk [BEq α] (expected : α) : Except Reply α → Bool
  | .ok a => a == expected
  | .error _ => false

-- ── `takeEvery` ──

#guard takeEvery 1 [1, 2, 3, 4, 5] == [1, 2, 3, 4, 5]
#guard takeEvery 2 [1, 2, 3, 4, 5] == [1, 3, 5]
#guard takeEvery 3 [1, 2, 3, 4, 5] == [1, 4]
#guard takeEvery 0 ([] : List Nat) == []

-- ── `parseArity` / `parseLastKeyPos` ──

#guard parseArity 2 == AritySpec.required 2
#guard parseArity (-3) == AritySpec.minimumRequired 3
#guard parseLastKeyPos 1 == LastKeyPositionSpec.lastKeyPosition 1
#guard parseLastKeyPos (-1) == LastKeyPositionSpec.unlimitedKeys 0

-- ── `parseFlag` ──

#guard isOk Flag.write (parseFlag (Reply.singleLine "write".toUTF8))
#guard isOk Flag.movableKeys (parseFlag (Reply.singleLine "movablekeys".toUTF8))
#guard isOk (Flag.other "custom".toUTF8) (parseFlag (Reply.singleLine "custom".toUTF8))

-- ── `RedisResult CommandInfo` decoding ──

/-- A `GET`-shaped `COMMAND` entry: one key at position 1, step 1. -/
def getInfoReply : Reply :=
  Reply.multiBulk (some
    [ Reply.bulk (some "get".toUTF8)
    , Reply.integer 2
    , Reply.multiBulk (some [Reply.singleLine "readonly".toUTF8, Reply.singleLine "fast".toUTF8])
    , Reply.integer 1
    , Reply.integer 1
    , Reply.integer 1
    ])

#guard isOk
  { name := "get".toUTF8
    arity := AritySpec.required 2
    flags := [Flag.readOnly, Flag.fast]
    firstKeyPosition := 1
    lastKeyPosition := LastKeyPositionSpec.lastKeyPosition 1
    stepCount := 1 : CommandInfo }
  (RedisResult.decode getInfoReply)

-- A Redis 7.0-shaped reply (trailing ACL/tips/key-specs/subcommands
-- `MultiBulk` fields) decodes the same way, ignoring the trailing fields.
def getInfoReply70 : Reply :=
  Reply.multiBulk (some
    [ Reply.bulk (some "get".toUTF8)
    , Reply.integer 2
    , Reply.multiBulk (some [Reply.singleLine "readonly".toUTF8])
    , Reply.integer 1
    , Reply.integer 1
    , Reply.integer 1
    , Reply.multiBulk (some [])
    , Reply.multiBulk (some [])
    , Reply.multiBulk (some [])
    , Reply.multiBulk (some [])
    ])

#guard isOk
  { name := "get".toUTF8
    arity := AritySpec.required 2
    flags := [Flag.readOnly]
    firstKeyPosition := 1
    lastKeyPosition := LastKeyPositionSpec.lastKeyPosition 1
    stepCount := 1 : CommandInfo }
  (RedisResult.decode getInfoReply70)

-- A malformed reply fails to decode.
#guard match (RedisResult.decode (Reply.integer 1) : Except Reply CommandInfo) with
  | .error _ => true
  | .ok _ => false

-- ── `keysForRequest` ──

def infoMap : InfoMap :=
  newInfoMap
    [ { name := "get".toUTF8, arity := AritySpec.required 2, flags := [Flag.readOnly]
      , firstKeyPosition := 1, lastKeyPosition := LastKeyPositionSpec.lastKeyPosition 1
      , stepCount := 1 }
    , { name := "mset".toUTF8, arity := AritySpec.minimumRequired 3, flags := [Flag.write]
      , firstKeyPosition := 1, lastKeyPosition := LastKeyPositionSpec.unlimitedKeys 1
      , stepCount := 2 }
    , { name := "eval".toUTF8, arity := AritySpec.minimumRequired 3
      , flags := [Flag.write, Flag.movableKeys]
      , firstKeyPosition := 0, lastKeyPosition := LastKeyPositionSpec.lastKeyPosition 0
      , stepCount := 0 }
    ]

-- Simple fixed-position case (`GET key`).
#guard keysForRequest infoMap ["GET".toUTF8, "foo".toUTF8] == some ["foo".toUTF8]

-- Case-insensitive command lookup.
#guard keysForRequest infoMap ["get".toUTF8, "foo".toUTF8] == some ["foo".toUTF8]

-- `MSET k1 v1 k2 v2`: keys at every other position from 1 to the end.
#guard keysForRequest infoMap ["MSET".toUTF8, "k1".toUTF8, "v1".toUTF8, "k2".toUTF8, "v2".toUTF8]
  == some ["k1".toUTF8, "k2".toUTF8]

-- `EVAL script numkeys key...`, a movable-keys command.
#guard keysForRequest infoMap
    ["EVAL".toUTF8, "script".toUTF8, "2".toUTF8, "k1".toUTF8, "k2".toUTF8, "extra".toUTF8]
  == some ["k1".toUTF8, "k2".toUTF8]

-- `QUIT` has no keys and isn't in the `COMMAND` output at all.
#guard keysForRequest infoMap ["QUIT".toUTF8] == some []

-- `DEBUG OBJECT key` is special-cased even though `DEBUG` isn't a real key
-- command in the `InfoMap`.
#guard keysForRequest infoMap ["DEBUG".toUTF8, "OBJECT".toUTF8, "mykey".toUTF8]
  == some ["mykey".toUTF8]

-- `XINFO STREAM key` is special-cased.
#guard keysForRequest infoMap ["XINFO".toUTF8, "STREAM".toUTF8, "mykey".toUTF8]
  == some ["mykey".toUTF8]

-- An unknown command with no metadata has no extractable keys.
#guard keysForRequest infoMap ["UNKNOWNCMD".toUTF8, "x".toUTF8] == none

-- An empty request has no extractable keys.
#guard keysForRequest infoMap [] == none

-- ── `XREAD`/`XREADGROUP` movable-keys helpers ──

#guard readXreadKeys ["STREAMS".toUTF8, "s1".toUTF8, "s2".toUTF8, "0".toUTF8, "0".toUTF8]
  == some ["s1".toUTF8, "s2".toUTF8]
#guard readXreadKeys ["COUNT".toUTF8, "5".toUTF8, "STREAMS".toUTF8, "s1".toUTF8, "0".toUTF8]
  == some ["s1".toUTF8]
#guard readXreadgroupKeys ["NOACK".toUTF8, "STREAMS".toUTF8, "s1".toUTF8, "0".toUTF8]
  == some ["s1".toUTF8]
