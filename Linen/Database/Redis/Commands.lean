/-
  Linen.Database.Redis.Commands — the Redis command surface

  ## Haskell source
  `Database.Redis.Commands` from https://hackage.haskell.org/package/hedis
  (module 11 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Commands.hs`. Each command builds a RESP request list
  and hands it to `Database.Redis.Core.sendRequest`, which sends it and
  decodes the reply via its `RedisResult` instance.

  ## Scope: the "simple" command surface only
  Upstream's `Commands.hs` is, in fact, a re-export layer: the commands with
  option records or irregular argument/reply encodings (`SET`/`setOpts`,
  `ZADD`, `SCAN`/`hscan`/`sscan`/`zscan`, `SORT`, `AUTH`/`authOpts`,
  `EXPIRE`-with-options, `BITCOUNT`/`BITOP`/`bitposOpts`, `MIGRATE`,
  `RESTORE`, `OBJECT`, `TYPE`, `PING`, `SELECT`, `EXISTS`, the Stream/Geo/
  Cluster families, …) are all *defined* in `Database.Redis.ManualCommands`
  (module 12) and merely re-exported from `Commands.hs`. This module ports
  exactly the ~125 commands that `Commands.hs` itself defines — every one a
  uniform `sendRequest [...]` one-liner. The manually-encoded commands are
  left to `Database.Redis.ManualCommands`, matching upstream's own split.

  ## Type mappings
  Upstream's reply/argument types are translated to Lean-stdlib idioms per
  AGENTS.md: `Integer`/`Int64` → `Int`, `Double` → `Float`, `ByteString` →
  `ByteArray`, `Maybe` → `Option`, tuples → `×`, `[a]` → `List`,
  `NonEmpty a` → `Data.List.NonEmpty a`.

  ## Deviations
  - `Int64` (used only by `hincrby`) maps to `Int`, matching how upstream's
    `Integer` is already mapped to `Int` throughout this port; Lean's `Int`
    is arbitrary-precision, so no information is lost and the wire encoding
    (a decimal string) is identical.
  - `hgetall` and `configGet` reply with a *flat* multi-bulk of `2n`
    elements decoded as `n` key/value pairs. Upstream relies on an
    overlapping `RedisResult [(k, v)]` instance that Lean's typeclass
    resolution cannot express (see `Database.Redis.Types`' doc-comment,
    which ports that instance as the plain function `decodeKeyValuePairs`).
    These two commands therefore return a `KeyValueReply` wrapper whose
    `RedisResult` instance decodes via `decodeKeyValuePairs`, rather than the
    bare `List (ByteArray × ByteArray)` a generic (nested-pair) instance
    would misdecode.
  - `zrevrankWithScore` sends `ZREVRANK key member` *without* a `WITHSCORE`
    token — this mirrors upstream exactly (a known upstream quirk); behaviour
    is not "corrected" here.
-/
import Linen.Data.List.NonEmpty
import Linen.Database.Redis.Core
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.Types

namespace Database.Redis.Commands

open Data.List (NonEmpty)
open Database.Redis.Core (sendRequest RedisCtx MonadRedis)
open Database.Redis.Types (RedisResult encode Status decodeKeyValuePairs)

-- ── Flat key/value reply wrapper ──

/-- A flat key/value reply (e.g. `HGETALL`, `CONFIG GET`): a multi-bulk of
    `2n` elements decoded as `n` `(key, value)` pairs, taken two at a time.
    See this module's doc-comment for why this wrapper exists rather than a
    bare `List (κ × ν)` return type. -/
structure KeyValueReply (κ ν : Type) where
  /-- The decoded key/value pairs. -/
  pairs : List (κ × ν)
deriving Repr, Inhabited

instance [RedisResult κ] [RedisResult ν] : RedisResult (KeyValueReply κ ν) where
  decode r := KeyValueReply.mk <$> decodeKeyValuePairs r

-- Every command runs in an arbitrary `RedisCtx m f` context, exactly as
-- upstream's `(RedisCtx m f) =>` constraint.
variable {m : Type → Type} {f : Type → Type} [Monad m] [MonadRedis m] [RedisCtx m f]

/-- Spread a list of key/value pairs into a flat argument list
    `[k₁, v₁, k₂, v₂, …]`. -/
private def flattenPairs (ps : List (ByteArray × ByteArray)) : List ByteArray :=
  ps.flatMap (fun (x, y) => [x, y])

-- ── Connection ──

/-- Echo the given message (`ECHO`). -/
def echo (message : ByteArray) : m (f ByteArray) :=
  sendRequest ["ECHO".toUTF8, message]

/-- Close the connection (`QUIT`). -/
def quit : m (f Status) :=
  sendRequest ["QUIT".toUTF8]

-- ── Keys ──

/-- Delete one or more keys (`DEL`); returns the number removed. -/
def del (keys : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("DEL".toUTF8 :: keys.toList)

/-- Return a serialized version of the value stored at `key` (`DUMP`). -/
def dump (key : ByteArray) : m (f ByteArray) :=
  sendRequest ["DUMP".toUTF8, key]

/-- Set a key's time to live in seconds (`EXPIRE`). -/
def expire (key : ByteArray) (seconds : Int) : m (f Bool) :=
  sendRequest ["EXPIRE".toUTF8, key, encode seconds]

/-- Set the expiration for a key as a UNIX timestamp in seconds (`EXPIREAT`). -/
def expireat (key : ByteArray) (timestamp : Int) : m (f Bool) :=
  sendRequest ["EXPIREAT".toUTF8, key, encode timestamp]

/-- Find all keys matching the given pattern (`KEYS`). -/
def keys (pattern : ByteArray) : m (f (List ByteArray)) :=
  sendRequest ["KEYS".toUTF8, pattern]

/-- Move a key to another database (`MOVE`). -/
def move (key : ByteArray) (db : Int) : m (f Bool) :=
  sendRequest ["MOVE".toUTF8, key, encode db]

/-- Remove the expiration from a key (`PERSIST`). -/
def persist (key : ByteArray) : m (f Bool) :=
  sendRequest ["PERSIST".toUTF8, key]

/-- Set a key's time to live in milliseconds (`PEXPIRE`). -/
def pexpire (key : ByteArray) (milliseconds : Int) : m (f Bool) :=
  sendRequest ["PEXPIRE".toUTF8, key, encode milliseconds]

/-- Set the expiration for a key as a UNIX timestamp in milliseconds
    (`PEXPIREAT`). -/
def pexpireat (key : ByteArray) (millisecondsTimestamp : Int) : m (f Bool) :=
  sendRequest ["PEXPIREAT".toUTF8, key, encode millisecondsTimestamp]

/-- Get the time to live for a key in milliseconds (`PTTL`). -/
def pttl (key : ByteArray) : m (f Int) :=
  sendRequest ["PTTL".toUTF8, key]

/-- Return a random key from the keyspace (`RANDOMKEY`). -/
def randomkey : m (f (Option ByteArray)) :=
  sendRequest ["RANDOMKEY".toUTF8]

/-- Rename a key (`RENAME`). -/
def rename (key : ByteArray) (newkey : ByteArray) : m (f Status) :=
  sendRequest ["RENAME".toUTF8, key, newkey]

/-- Rename a key, only if the new key does not exist (`RENAMENX`). -/
def renamenx (key : ByteArray) (newkey : ByteArray) : m (f Bool) :=
  sendRequest ["RENAMENX".toUTF8, key, newkey]

/-- Get the time to live for a key in seconds (`TTL`). -/
def ttl (key : ByteArray) : m (f Int) :=
  sendRequest ["TTL".toUTF8, key]

/-- Wait for the synchronous replication of all preceding write commands
    (`WAIT`). -/
def wait (numslaves : Int) (timeout : Int) : m (f Int) :=
  sendRequest ["WAIT".toUTF8, encode numslaves, encode timeout]

-- ── Hashes ──

/-- Delete one or more hash fields (`HDEL`). -/
def hdel (key : ByteArray) (field : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("HDEL".toUTF8 :: key :: field.toList)

/-- Determine if a hash field exists (`HEXISTS`). -/
def hexists (key : ByteArray) (field : ByteArray) : m (f Bool) :=
  sendRequest ["HEXISTS".toUTF8, key, field]

/-- Get the value of a hash field (`HGET`). -/
def hget (key : ByteArray) (field : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["HGET".toUTF8, key, field]

/-- Get all the fields and values in a hash (`HGETALL`). -/
def hgetall (key : ByteArray) : m (f (KeyValueReply ByteArray ByteArray)) :=
  sendRequest ["HGETALL".toUTF8, key]

/-- Increment the integer value of a hash field by the given number
    (`HINCRBY`). -/
def hincrby (key : ByteArray) (field : ByteArray) (increment : Int) : m (f Int) :=
  sendRequest ["HINCRBY".toUTF8, key, field, encode increment]

/-- Increment the float value of a hash field by the given amount
    (`HINCRBYFLOAT`). -/
def hincrbyfloat (key : ByteArray) (field : ByteArray) (increment : Float) : m (f Float) :=
  sendRequest ["HINCRBYFLOAT".toUTF8, key, field, encode increment]

/-- Get all the fields in a hash (`HKEYS`). -/
def hkeys (key : ByteArray) : m (f (List ByteArray)) :=
  sendRequest ["HKEYS".toUTF8, key]

/-- Get the number of fields in a hash (`HLEN`). -/
def hlen (key : ByteArray) : m (f Int) :=
  sendRequest ["HLEN".toUTF8, key]

/-- Get the values of all the given hash fields (`HMGET`). -/
def hmget (key : ByteArray) (field : NonEmpty ByteArray) : m (f (List (Option ByteArray))) :=
  sendRequest ("HMGET".toUTF8 :: key :: field.toList)

/-- Set multiple hash fields to multiple values (`HMSET`). -/
def hmset (key : ByteArray) (fieldValue : NonEmpty (ByteArray × ByteArray)) : m (f Status) :=
  sendRequest ("HMSET".toUTF8 :: key :: flattenPairs fieldValue.toList)

/-- Set the string value(s) of one or more hash fields (`HSET`). -/
def hset (key : ByteArray) (fieldValues : NonEmpty (ByteArray × ByteArray)) : m (f Int) :=
  sendRequest ("HSET".toUTF8 :: key :: flattenPairs fieldValues.toList)

/-- Set the value of a hash field, only if the field does not exist
    (`HSETNX`). -/
def hsetnx (key : ByteArray) (field : ByteArray) (value : ByteArray) : m (f Bool) :=
  sendRequest ["HSETNX".toUTF8, key, field, value]

/-- Get the length of the value of a hash field (`HSTRLEN`). -/
def hstrlen (key : ByteArray) (field : ByteArray) : m (f Int) :=
  sendRequest ["HSTRLEN".toUTF8, key, field]

/-- Get all the values in a hash (`HVALS`). -/
def hvals (key : ByteArray) : m (f (List ByteArray)) :=
  sendRequest ["HVALS".toUTF8, key]

-- ── HyperLogLogs ──

/-- Add elements to a HyperLogLog (`PFADD`). -/
def pfadd (key : ByteArray) (value : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("PFADD".toUTF8 :: key :: value.toList)

/-- Return the approximated cardinality of the HyperLogLog(s) (`PFCOUNT`). -/
def pfcount (key : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("PFCOUNT".toUTF8 :: key.toList)

/-- Merge N different HyperLogLogs into a single one (`PFMERGE`). -/
def pfmerge (destkey : ByteArray) (sourcekey : List ByteArray) : m (f ByteArray) :=
  sendRequest ("PFMERGE".toUTF8 :: destkey :: sourcekey)

-- ── Lists ──

/-- Remove and get the first element in a list, blocking if empty (`BLPOP`). -/
def blpop (keys : List ByteArray) (timeout : Int) : m (f (Option (ByteArray × ByteArray))) :=
  sendRequest ("BLPOP".toUTF8 :: keys ++ [encode timeout])

/-- As `blpop`; kept as a separate name matching upstream's `blpopFloat`. -/
def blpopFloat (keys : List ByteArray) (timeout : Int) :
    m (f (Option (ByteArray × ByteArray))) :=
  sendRequest ("BLPOP".toUTF8 :: keys ++ [encode timeout])

/-- Remove and get the last element in a list, blocking if empty (`BRPOP`). -/
def brpop (key : NonEmpty ByteArray) (timeout : Int) :
    m (f (Option (ByteArray × ByteArray))) :=
  sendRequest ("BRPOP".toUTF8 :: key.toList ++ [encode timeout])

/-- As `brpop`, with a floating-point timeout (`BRPOP`). -/
def brpopFloat (key : List ByteArray) (timeout : Float) :
    m (f (Option (ByteArray × ByteArray))) :=
  sendRequest ("BRPOP".toUTF8 :: key ++ [encode timeout])

/-- Pop from the tail of one list and push to the head of another, blocking
    if empty (`BRPOPLPUSH`). -/
def brpoplpush (source : ByteArray) (destination : ByteArray) (timeout : Int) :
    m (f (Option ByteArray)) :=
  sendRequest ["BRPOPLPUSH".toUTF8, source, destination, encode timeout]

/-- Get an element from a list by its index (`LINDEX`). -/
def lindex (key : ByteArray) (index : Int) : m (f (Option ByteArray)) :=
  sendRequest ["LINDEX".toUTF8, key, encode index]

/-- Get the length of a list (`LLEN`). -/
def llen (key : ByteArray) : m (f Int) :=
  sendRequest ["LLEN".toUTF8, key]

/-- Remove and get the first element in a list (`LPOP`). -/
def lpop (key : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["LPOP".toUTF8, key]

/-- Remove and get up to `count` elements from the head of a list (`LPOP`). -/
def lpopCount (key : ByteArray) (count : Int) : m (f (List ByteArray)) :=
  sendRequest ["LPOP".toUTF8, key, encode count]

/-- Prepend one or more values to a list (`LPUSH`). -/
def lpush (key : ByteArray) (value : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("LPUSH".toUTF8 :: key :: value.toList)

/-- Prepend a value to a list, only if the list exists (`LPUSHX`). -/
def lpushx (key : ByteArray) (value : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("LPUSHX".toUTF8 :: key :: value.toList)

/-- Get a range of elements from a list (`LRANGE`). -/
def lrange (key : ByteArray) (start stop : Int) : m (f (List ByteArray)) :=
  sendRequest ["LRANGE".toUTF8, key, encode start, encode stop]

/-- Remove elements from a list (`LREM`). -/
def lrem (key : ByteArray) (count : Int) (value : ByteArray) : m (f Int) :=
  sendRequest ["LREM".toUTF8, key, encode count, value]

/-- Set the value of an element in a list by its index (`LSET`). -/
def lset (key : ByteArray) (index : Int) (value : ByteArray) : m (f Status) :=
  sendRequest ["LSET".toUTF8, key, encode index, value]

/-- Trim a list to the specified range (`LTRIM`). -/
def ltrim (key : ByteArray) (start stop : Int) : m (f Status) :=
  sendRequest ["LTRIM".toUTF8, key, encode start, encode stop]

/-- Remove and get the last element in a list (`RPOP`). -/
def rpop (key : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["RPOP".toUTF8, key]

/-- Remove and get up to `count` elements from the tail of a list (`RPOP`). -/
def rpopCount (key : ByteArray) (count : Int) : m (f (List ByteArray)) :=
  sendRequest ["RPOP".toUTF8, key, encode count]

/-- Remove the last element of a list and prepend it to another (`RPOPLPUSH`). -/
def rpoplpush (source : ByteArray) (destination : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["RPOPLPUSH".toUTF8, source, destination]

/-- Append one or more values to a list (`RPUSH`). -/
def rpush (key : ByteArray) (value : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("RPUSH".toUTF8 :: key :: value.toList)

/-- Append a value to a list, only if the list exists (`RPUSHX`). -/
def rpushx (key : ByteArray) (value : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("RPUSHX".toUTF8 :: key :: value.toList)

-- ── Scripting ──

/-- Check existence of scripts in the script cache (`SCRIPT EXISTS`). -/
def scriptExists (script : NonEmpty ByteArray) : m (f (List Bool)) :=
  sendRequest ("SCRIPT".toUTF8 :: "EXISTS".toUTF8 :: script.toList)

/-- Remove all scripts from the script cache (`SCRIPT FLUSH`). -/
def scriptFlush : m (f Status) :=
  sendRequest ["SCRIPT".toUTF8, "FLUSH".toUTF8]

/-- Kill the script currently in execution (`SCRIPT KILL`). -/
def scriptKill : m (f Status) :=
  sendRequest ["SCRIPT".toUTF8, "KILL".toUTF8]

/-- Load a Lua script into the script cache (`SCRIPT LOAD`). -/
def scriptLoad (script : ByteArray) : m (f ByteArray) :=
  sendRequest ["SCRIPT".toUTF8, "LOAD".toUTF8, script]

-- ── Server ──

/-- Asynchronously rewrite the append-only file (`BGREWRITEAOF`). -/
def bgrewriteaof : m (f Status) :=
  sendRequest ["BGREWRITEAOF".toUTF8]

/-- Asynchronously save the dataset to disk (`BGSAVE`). -/
def bgsave : m (f Status) :=
  sendRequest ["BGSAVE".toUTF8]

/-- Asynchronously save the dataset to disk, scheduling if a rewrite is in
    progress (`BGSAVE SCHEDULE`). -/
def bgsaveSchedule : m (f Status) :=
  sendRequest ["BGSAVE".toUTF8, "SCHEDULE".toUTF8]

/-- Get the current connection name (`CLIENT GETNAME`). -/
def clientGetname : m (f (Option ByteArray)) :=
  sendRequest ["CLIENT".toUTF8, "GETNAME".toUTF8]

/-- Get the current connection ID (`CLIENT ID`). -/
def clientId : m (f Int) :=
  sendRequest ["CLIENT".toUTF8, "ID".toUTF8]

/-- Get the list of client connections (`CLIENT LIST`). -/
def clientList : m (f (List ByteArray)) :=
  sendRequest ["CLIENT".toUTF8, "LIST".toUTF8]

/-- Stop processing commands from clients for some time (`CLIENT PAUSE`). -/
def clientPause (timeout : Int) : m (f Status) :=
  sendRequest ["CLIENT".toUTF8, "PAUSE".toUTF8, encode timeout]

/-- Set the current connection name (`CLIENT SETNAME`). -/
def clientSetname (connectionName : ByteArray) : m (f Status) :=
  sendRequest ["CLIENT".toUTF8, "SETNAME".toUTF8, connectionName]

/-- Get the total number of Redis commands (`COMMAND COUNT`). -/
def commandCount : m (f Int) :=
  sendRequest ["COMMAND".toUTF8, "COUNT".toUTF8]

/-- Get array of specific Redis command details (`COMMAND INFO`). -/
def commandInfo (commandName : List ByteArray) : m (f (List ByteArray)) :=
  sendRequest ("COMMAND".toUTF8 :: "INFO".toUTF8 :: commandName)

/-- Get the value of one or more configuration parameters (`CONFIG GET`). -/
def configGet (parameter : NonEmpty ByteArray) : m (f (KeyValueReply ByteArray ByteArray)) :=
  sendRequest ("CONFIG".toUTF8 :: "GET".toUTF8 :: parameter.toList)

/-- Reset the stats returned by `INFO` (`CONFIG RESETSTAT`). -/
def configResetstat : m (f Status) :=
  sendRequest ["CONFIG".toUTF8, "RESETSTAT".toUTF8]

/-- Rewrite the configuration file with the in-memory configuration
    (`CONFIG REWRITE`). -/
def configRewrite : m (f Status) :=
  sendRequest ["CONFIG".toUTF8, "REWRITE".toUTF8]

/-- Set a configuration parameter to the given value (`CONFIG SET`). -/
def configSet (parameter : ByteArray) (value : ByteArray) : m (f Status) :=
  sendRequest ["CONFIG".toUTF8, "SET".toUTF8, parameter, value]

/-- Return the number of keys in the selected database (`DBSIZE`). -/
def dbsize : m (f Int) :=
  sendRequest ["DBSIZE".toUTF8]

/-- Get debugging information about a key (`DEBUG OBJECT`). -/
def debugObject (key : ByteArray) : m (f ByteArray) :=
  sendRequest ["DEBUG".toUTF8, "OBJECT".toUTF8, key]

/-- Remove all keys from all databases (`FLUSHALL`). -/
def flushall : m (f Status) :=
  sendRequest ["FLUSHALL".toUTF8]

/-- Remove all keys from the current database (`FLUSHDB`). -/
def flushdb : m (f Status) :=
  sendRequest ["FLUSHDB".toUTF8]

/-- Get the UNIX timestamp of the last successful save to disk (`LASTSAVE`). -/
def lastsave : m (f Int) :=
  sendRequest ["LASTSAVE".toUTF8]

/-- Synchronously save the dataset to disk (`SAVE`). -/
def save : m (f Status) :=
  sendRequest ["SAVE".toUTF8]

/-- Make the server a replica of another instance (`SLAVEOF`). -/
def slaveof (host : ByteArray) (port : ByteArray) : m (f Status) :=
  sendRequest ["SLAVEOF".toUTF8, host, port]

/-- Return the current server time as `(seconds, microseconds)` (`TIME`). -/
def time : m (f (Int × Int)) :=
  sendRequest ["TIME".toUTF8]

-- ── Sets ──

/-- Add one or more members to a set (`SADD`). -/
def sadd (key : ByteArray) (member : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("SADD".toUTF8 :: key :: member.toList)

/-- Get the number of members in a set (`SCARD`). -/
def scard (key : ByteArray) : m (f Int) :=
  sendRequest ["SCARD".toUTF8, key]

/-- Subtract multiple sets (`SDIFF`). -/
def sdiff (key : NonEmpty ByteArray) : m (f (List ByteArray)) :=
  sendRequest ("SDIFF".toUTF8 :: key.toList)

/-- Subtract multiple sets and store the result in a key (`SDIFFSTORE`). -/
def sdiffstore (destination : ByteArray) (key : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("SDIFFSTORE".toUTF8 :: destination :: key.toList)

/-- Intersect multiple sets (`SINTER`). -/
def sinter (key : NonEmpty ByteArray) : m (f (List ByteArray)) :=
  sendRequest ("SINTER".toUTF8 :: key.toList)

/-- Intersect multiple sets and store the result in a key (`SINTERSTORE`). -/
def sinterstore (destination : ByteArray) (key : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("SINTERSTORE".toUTF8 :: destination :: key.toList)

/-- Determine if a value is a member of a set (`SISMEMBER`). -/
def sismember (key : ByteArray) (member : ByteArray) : m (f Bool) :=
  sendRequest ["SISMEMBER".toUTF8, key, member]

/-- Get all the members in a set (`SMEMBERS`). -/
def smembers (key : ByteArray) : m (f (List ByteArray)) :=
  sendRequest ["SMEMBERS".toUTF8, key]

/-- Move a member from one set to another (`SMOVE`). -/
def smove (source : ByteArray) (destination : ByteArray) (member : ByteArray) : m (f Bool) :=
  sendRequest ["SMOVE".toUTF8, source, destination, member]

/-- Remove one or more members from a set (`SREM`). -/
def srem (key : ByteArray) (member : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("SREM".toUTF8 :: key :: member.toList)

/-- Add multiple sets (`SUNION`). -/
def sunion (key : NonEmpty ByteArray) : m (f (List ByteArray)) :=
  sendRequest ("SUNION".toUTF8 :: key.toList)

/-- Add multiple sets and store the result in a key (`SUNIONSTORE`). -/
def sunionstore (destination : ByteArray) (key : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("SUNIONSTORE".toUTF8 :: destination :: key.toList)

-- ── Sorted Sets ──

/-- Get the number of members in a sorted set (`ZCARD`). -/
def zcard (key : ByteArray) : m (f Int) :=
  sendRequest ["ZCARD".toUTF8, key]

/-- Count members in a sorted set with scores within the given range
    (`ZCOUNT`). -/
def zcount (key : ByteArray) (min max : Float) : m (f Int) :=
  sendRequest ["ZCOUNT".toUTF8, key, encode min, encode max]

/-- Increment the score of a member in a sorted set (`ZINCRBY`). -/
def zincrby (key : ByteArray) (increment : Int) (member : ByteArray) : m (f Float) :=
  sendRequest ["ZINCRBY".toUTF8, key, encode increment, member]

/-- Count members in a sorted set within the given lexicographical range
    (`ZLEXCOUNT`). -/
def zlexcount (key : ByteArray) (min max : ByteArray) : m (f Int) :=
  sendRequest ["ZLEXCOUNT".toUTF8, key, min, max]

/-- Determine the index of a member in a sorted set (`ZRANK`). -/
def zrank (key : ByteArray) (member : ByteArray) : m (f (Option Int)) :=
  sendRequest ["ZRANK".toUTF8, key, member]

/-- Determine the index and score of a member in a sorted set
    (`ZRANK … WITHSCORE`). -/
def zrankWithScore (key : ByteArray) (member : ByteArray) : m (f (Option (Int × Float))) :=
  sendRequest ["ZRANK".toUTF8, key, member, "WITHSCORE".toUTF8]

/-- Remove one or more members from a sorted set (`ZREM`). -/
def zrem (key : ByteArray) (member : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("ZREM".toUTF8 :: key :: member.toList)

/-- Remove members in a sorted set within the given lexicographical range
    (`ZREMRANGEBYLEX`). -/
def zremrangebylex (key : ByteArray) (min max : ByteArray) : m (f Int) :=
  sendRequest ["ZREMRANGEBYLEX".toUTF8, key, min, max]

/-- Remove members in a sorted set within the given index range
    (`ZREMRANGEBYRANK`). -/
def zremrangebyrank (key : ByteArray) (start stop : Int) : m (f Int) :=
  sendRequest ["ZREMRANGEBYRANK".toUTF8, key, encode start, encode stop]

/-- Remove members in a sorted set within the given scores
    (`ZREMRANGEBYSCORE`). -/
def zremrangebyscore (key : ByteArray) (min max : Float) : m (f Int) :=
  sendRequest ["ZREMRANGEBYSCORE".toUTF8, key, encode min, encode max]

/-- Determine the index of a member in a sorted set, high to low
    (`ZREVRANK`). -/
def zrevrank (key : ByteArray) (member : ByteArray) : m (f (Option Int)) :=
  sendRequest ["ZREVRANK".toUTF8, key, member]

/-- As `zrevrank`, decoding an index/score pair. Sends no `WITHSCORE` token,
    exactly as upstream (see this module's doc-comment). -/
def zrevrankWithScore (key : ByteArray) (member : ByteArray) : m (f (Option (Int × Float))) :=
  sendRequest ["ZREVRANK".toUTF8, key, member]

/-- Get the score associated with a member in a sorted set (`ZSCORE`). -/
def zscore (key : ByteArray) (member : ByteArray) : m (f (Option Float)) :=
  sendRequest ["ZSCORE".toUTF8, key, member]

-- ── Strings ──

/-- Append a value to a key (`APPEND`). -/
def append (key : ByteArray) (value : ByteArray) : m (f Int) :=
  sendRequest ["APPEND".toUTF8, key, value]

/-- Find the first bit set or clear in a string (`BITPOS`). -/
def bitpos (key : ByteArray) (bit start end_ : Int) : m (f Int) :=
  sendRequest ["BITPOS".toUTF8, key, encode bit, encode start, encode end_]

/-- Decrement the integer value of a key by one (`DECR`). -/
def decr (key : ByteArray) : m (f Int) :=
  sendRequest ["DECR".toUTF8, key]

/-- Decrement the integer value of a key by the given number (`DECRBY`). -/
def decrby (key : ByteArray) (decrement : Int) : m (f Int) :=
  sendRequest ["DECRBY".toUTF8, key, encode decrement]

/-- Get the value of a key (`GET`). -/
def get (key : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["GET".toUTF8, key]

/-- Return the bit value at offset in the string stored at key (`GETBIT`). -/
def getbit (key : ByteArray) (offset : Int) : m (f Int) :=
  sendRequest ["GETBIT".toUTF8, key, encode offset]

/-- Get a substring of the string stored at a key (`GETRANGE`). -/
def getrange (key : ByteArray) (start end_ : Int) : m (f ByteArray) :=
  sendRequest ["GETRANGE".toUTF8, key, encode start, encode end_]

/-- Set the string value of a key and return its old value (`GETSET`). -/
def getset (key : ByteArray) (value : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["GETSET".toUTF8, key, value]

/-- Increment the integer value of a key by one (`INCR`). -/
def incr (key : ByteArray) : m (f Int) :=
  sendRequest ["INCR".toUTF8, key]

/-- Increment the integer value of a key by the given amount (`INCRBY`). -/
def incrby (key : ByteArray) (increment : Int) : m (f Int) :=
  sendRequest ["INCRBY".toUTF8, key, encode increment]

/-- Increment the float value of a key by the given amount (`INCRBYFLOAT`). -/
def incrbyfloat (key : ByteArray) (increment : Float) : m (f Float) :=
  sendRequest ["INCRBYFLOAT".toUTF8, key, encode increment]

/-- Get the values of all the given keys (`MGET`). -/
def mget (key : NonEmpty ByteArray) : m (f (List (Option ByteArray))) :=
  sendRequest ("MGET".toUTF8 :: key.toList)

/-- Set multiple keys to multiple values (`MSET`). -/
def mset (keyValue : NonEmpty (ByteArray × ByteArray)) : m (f Status) :=
  sendRequest ("MSET".toUTF8 :: flattenPairs keyValue.toList)

/-- Set multiple keys to multiple values, only if none exist (`MSETNX`). -/
def msetnx (keyValue : NonEmpty (ByteArray × ByteArray)) : m (f Bool) :=
  sendRequest ("MSETNX".toUTF8 :: flattenPairs keyValue.toList)

/-- Set the value and expiration in milliseconds of a key (`PSETEX`). -/
def psetex (key : ByteArray) (milliseconds : Int) (value : ByteArray) : m (f Status) :=
  sendRequest ["PSETEX".toUTF8, key, encode milliseconds, value]

/-- Set or clear the bit at offset in the string value stored at key
    (`SETBIT`). -/
def setbit (key : ByteArray) (offset : Int) (value : ByteArray) : m (f Int) :=
  sendRequest ["SETBIT".toUTF8, key, encode offset, value]

/-- Set the value and expiration of a key (`SETEX`). -/
def setex (key : ByteArray) (seconds : Int) (value : ByteArray) : m (f Status) :=
  sendRequest ["SETEX".toUTF8, key, encode seconds, value]

/-- Set the value of a key, only if the key does not exist (`SETNX`). -/
def setnx (key : ByteArray) (value : ByteArray) : m (f Bool) :=
  sendRequest ["SETNX".toUTF8, key, value]

/-- Overwrite part of a string at key starting at the given offset
    (`SETRANGE`). -/
def setrange (key : ByteArray) (offset : Int) (value : ByteArray) : m (f Int) :=
  sendRequest ["SETRANGE".toUTF8, key, encode offset, value]

/-- Get the length of the value stored in a key (`STRLEN`). -/
def strlen (key : ByteArray) : m (f Int) :=
  sendRequest ["STRLEN".toUTF8, key]

end Database.Redis.Commands
