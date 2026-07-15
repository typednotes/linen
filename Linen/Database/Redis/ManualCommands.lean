/-
  Linen.Database.Redis.ManualCommands — commands with irregular encodings

  ## Haskell source
  `Database.Redis.ManualCommands` from https://hackage.haskell.org/package/hedis
  (module 12 of the `hedis` import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/ManualCommands.hs`. This is upstream's home for every
  command whose argument encoding or reply shape is irregular enough that it
  is hand-written rather than produced by the uniform `sendRequest [...]`
  pattern of `Database.Redis.Commands` (module 11); `Commands.hs` merely
  re-exports everything defined here.

  ## Type mappings
  As in `Database.Redis.Commands`: `Integer`/`Int64` → `Int`, `Double` →
  `Float`, `ByteString` → `ByteArray`, `Maybe` → `Option`, tuples → `×`,
  `[a]` → `List`, `NonEmpty a` → `Data.List.NonEmpty a`. Upstream's option
  records become Lean `structure`s with `Option`/`Bool` fields plus a
  `xDefault : XOpts` value matching upstream's `defaultXOpts`; upstream's
  flag-enum `data` types become Lean `inductive`s with `RedisArg` instances,
  preserving upstream's exact wire tokens and argument order.

  ## Flat key/value replies
  Several commands here reply with a *flat* multi-bulk of `2n` elements that
  upstream decodes to `n` pairs via the overlapping `RedisResult [(k,v)]`
  instance Lean cannot express (see `Database.Redis.Types` and
  `Database.Redis.Commands`). They reuse `Database.Redis.Commands.KeyValueReply`
  (`WITHSCORES` ranges, `HSCAN`/`ZSCAN`) exactly as `HGETALL`/`CONFIG GET` do.

  ## Deviations
  - `RedisResult` decoders that upstream builds by combining per-Redis-version
    equations with `Semigroup`'s `<>` (`XInfoConsumersResponse`,
    `XInfoGroupsResponse`, `XInfoStreamResponse`) are ported as an explicit
    "try each shape in order, first success wins" fallback (`decodeFirst`),
    which is exactly what `Either`'s `Semigroup` does.
  - The textual replies of `CLUSTER INFO`/`CLUSTER NODES` are parsed by small
    hand-written `ByteArray` splitters (`bsLines`/`bsSplit`/`bsWords`/…) built
    by structural recursion on `List UInt8`, substituting for upstream's
    `Data.ByteString.Char8` helpers (`lines`/`split`/`words`/`readInteger`),
    the same way `Database.Redis.Types` substitutes `bytestring-lexing`.
  - `inf` is `Float`-typed (`1/0`) rather than upstream's `RealFloat a => a`;
    Lean has no need for the polymorphic form here.
-/
import Linen.Data.List.NonEmpty
import Linen.Database.Redis.Cluster.Command
import Linen.Database.Redis.Commands
import Linen.Database.Redis.Core
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.Types

namespace Database.Redis.ManualCommands

open Data.List (NonEmpty)
open Database.Redis.Core (sendRequest RedisCtx MonadRedis)
open Database.Redis.Types
  (RedisResult RedisArg encode decode Status RedisType decodeKeyValuePairs readSignedDecimal)
open Database.Redis.Commands (KeyValueReply)
open Database.Redis.Protocol (Reply)
open Database.Redis.Cluster.Command (CommandInfo)

-- ── Small argument-building helpers ──

/-- `["<name>"]` if `b`, else `[]` (upstream's `["FLAG" | b]`). -/
private def flag (b : Bool) (name : ByteArray) : List ByteArray :=
  if b then [name] else []

/-- `["<name>", encode x]` if `o = some x`, else `[]`
    (upstream's `maybe [] (\x -> [name, encode x]) o`). -/
private def optEnc [RedisArg α] (name : ByteArray) (o : Option α) : List ByteArray :=
  match o with
  | some x => [name, encode x]
  | none => []

/-- `["<name>", x]` if `o = some x`, else `[]` (bare-bytes variant of `optEnc`). -/
private def optRaw (name : ByteArray) (o : Option ByteArray) : List ByteArray :=
  match o with
  | some x => [name, x]
  | none => []

/-- Spread `[(a₁,b₁),…]` into `[a₁,b₁,…]`. -/
private def flattenPairs (ps : List (ByteArray × ByteArray)) : List ByteArray :=
  ps.flatMap (fun (x, y) => [x, y])

/-- Try each decoder in order, returning the first `Except.ok`; if all fail,
    return the last failure (mirrors upstream's `d₁ <> d₂ <> …` on
    `Either`). -/
private def decodeFirst {α : Type} (r : Reply) :
    List (Reply → Except Reply α) → Except Reply α
  | [] => Except.error r
  | [d] => d r
  | d :: ds =>
    match d r with
    | Except.ok x => Except.ok x
    | Except.error _ => decodeFirst r ds

-- Every command runs in an arbitrary `RedisCtx m f` context (upstream's
-- `(RedisCtx m f) =>` constraint).
variable {m : Type → Type} {f : Type → Type} [Monad m] [MonadRedis m] [RedisCtx m f]

-- ── Objects, LINSERT, TYPE ──

/-- Number of references of the value at `key` (`OBJECT REFCOUNT`). -/
def objectRefcount (key : ByteArray) : m (f Int) :=
  sendRequest ["OBJECT".toUTF8, "refcount".toUTF8, key]

/-- Idle time of the value at `key` (`OBJECT IDLETIME`). -/
def objectIdletime (key : ByteArray) : m (f Int) :=
  sendRequest ["OBJECT".toUTF8, "idletime".toUTF8, key]

/-- Internal encoding of the value at `key` (`OBJECT ENCODING`). -/
def objectEncoding (key : ByteArray) : m (f ByteArray) :=
  sendRequest ["OBJECT".toUTF8, "encoding".toUTF8, key]

/-- Insert `value` before `pivot` in a list (`LINSERT … BEFORE`). -/
def linsertBefore (key pivot value : ByteArray) : m (f Int) :=
  sendRequest ["LINSERT".toUTF8, key, "BEFORE".toUTF8, pivot, value]

/-- Insert `value` after `pivot` in a list (`LINSERT … AFTER`). -/
def linsertAfter (key pivot value : ByteArray) : m (f Int) :=
  sendRequest ["LINSERT".toUTF8, key, "AFTER".toUTF8, pivot, value]

/-- Determine the type stored at `key` (`TYPE`). -/
def getType (key : ByteArray) : m (f RedisType) :=
  sendRequest ["TYPE".toUTF8, key]

-- ── Slowlog ──

/-- A single entry from the slowlog. -/
structure Slowlog where
  /-- Unique progressive identifier for the entry. -/
  id : Int
  /-- Unix timestamp at which the command was processed. -/
  timestamp : Int
  /-- Execution time, in microseconds. -/
  micros : Int
  /-- The command and its arguments. -/
  cmd : List ByteArray
  /-- Client IP and port (only reported since Redis 4.0). -/
  clientIpAndPort : Option ByteArray
  /-- Client name (only reported since Redis 4.0). -/
  clientName : Option ByteArray
deriving BEq, Inhabited

instance : RedisResult Slowlog where
  decode
    | .multiBulk (some [a, b, c, d]) =>
      (do pure ⟨← decode a, ← decode b, ← decode c, ← decode d, none, none⟩)
    | .multiBulk (some [a, b, c, d, ip, cname]) =>
      (do pure ⟨← decode a, ← decode b, ← decode c, ← decode d, some (← decode ip),
        some (← decode cname)⟩)
    | r => Except.error r

/-- Read entries from the slowlog (`SLOWLOG GET`). -/
def slowlogGet (n : Int) : m (f (List Slowlog)) :=
  sendRequest ["SLOWLOG".toUTF8, "GET".toUTF8, encode n]

/-- Number of entries in the slowlog (`SLOWLOG LEN`). -/
def slowlogLen : m (f Int) :=
  sendRequest ["SLOWLOG".toUTF8, "LEN".toUTF8]

/-- Clear the slowlog (`SLOWLOG RESET`). -/
def slowlogReset : m (f Status) :=
  sendRequest ["SLOWLOG".toUTF8, "RESET".toUTF8]

-- ── Sorted-set range queries ──

/-- Range of members by index (`ZRANGE`). -/
def zrange (key : ByteArray) (start stop : Int) : m (f (List ByteArray)) :=
  sendRequest ["ZRANGE".toUTF8, key, encode start, encode stop]

/-- Range of members by index, with scores (`ZRANGE … WITHSCORES`). -/
def zrangeWithscores (key : ByteArray) (start stop : Int) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZRANGE".toUTF8, key, encode start, encode stop, "WITHSCORES".toUTF8]

/-- Range of members by index, high to low (`ZREVRANGE`). -/
def zrevrange (key : ByteArray) (start stop : Int) : m (f (List ByteArray)) :=
  sendRequest ["ZREVRANGE".toUTF8, key, encode start, encode stop]

/-- Range of members by index, high to low, with scores
    (`ZREVRANGE … WITHSCORES`). -/
def zrevrangeWithscores (key : ByteArray) (start stop : Int) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZREVRANGE".toUTF8, key, encode start, encode stop, "WITHSCORES".toUTF8]

/-- Range of members by score (`ZRANGEBYSCORE`). -/
def zrangebyscore (key : ByteArray) (min max : Float) : m (f (List ByteArray)) :=
  sendRequest ["ZRANGEBYSCORE".toUTF8, key, encode min, encode max]

/-- Range of members by score, with scores (`ZRANGEBYSCORE … WITHSCORES`). -/
def zrangebyscoreWithscores (key : ByteArray) (min max : Float) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZRANGEBYSCORE".toUTF8, key, encode min, encode max, "WITHSCORES".toUTF8]

/-- Range of members by score, limited (`ZRANGEBYSCORE … LIMIT`). -/
def zrangebyscoreLimit (key : ByteArray) (min max : Float) (offset count : Int) :
    m (f (List ByteArray)) :=
  sendRequest ["ZRANGEBYSCORE".toUTF8, key, encode min, encode max,
    "LIMIT".toUTF8, encode offset, encode count]

/-- Range of members by score, with scores, limited
    (`ZRANGEBYSCORE … WITHSCORES LIMIT`). -/
def zrangebyscoreWithscoresLimit (key : ByteArray) (min max : Float) (offset count : Int) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZRANGEBYSCORE".toUTF8, key, encode min, encode max,
    "WITHSCORES".toUTF8, "LIMIT".toUTF8, encode offset, encode count]

/-- Range of members by score, high to low (`ZREVRANGEBYSCORE`). -/
def zrevrangebyscore (key : ByteArray) (min max : Float) : m (f (List ByteArray)) :=
  sendRequest ["ZREVRANGEBYSCORE".toUTF8, key, encode min, encode max]

/-- Range of members by score, high to low, with scores
    (`ZREVRANGEBYSCORE … WITHSCORES`). -/
def zrevrangebyscoreWithscores (key : ByteArray) (min max : Float) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZREVRANGEBYSCORE".toUTF8, key, encode min, encode max, "WITHSCORES".toUTF8]

/-- Range of members by score, high to low, limited (`ZREVRANGEBYSCORE … LIMIT`). -/
def zrevrangebyscoreLimit (key : ByteArray) (min max : Float) (offset count : Int) :
    m (f (List ByteArray)) :=
  sendRequest ["ZREVRANGEBYSCORE".toUTF8, key, encode min, encode max,
    "LIMIT".toUTF8, encode offset, encode count]

/-- Range of members by score, high to low, with scores, limited
    (`ZREVRANGEBYSCORE … WITHSCORES LIMIT`). -/
def zrevrangebyscoreWithscoresLimit (key : ByteArray) (min max : Float) (offset count : Int) :
    m (f (KeyValueReply ByteArray Float)) :=
  sendRequest ["ZREVRANGEBYSCORE".toUTF8, key, encode min, encode max,
    "WITHSCORES".toUTF8, "LIMIT".toUTF8, encode offset, encode count]

-- ── SORT ──

/-- Sort order for `SORT`. -/
inductive SortOrder where
  | asc
  | desc
deriving BEq, Inhabited

/-- Options for the `SORT` command. -/
structure SortOpts where
  /-- `BY` pattern (omitted when `none`). -/
  sortBy : Option ByteArray
  /-- `LIMIT` offset/count. -/
  sortLimit : Int × Int
  /-- `GET` patterns. -/
  sortGet : List ByteArray
  /-- Sort order. -/
  sortOrder : SortOrder
  /-- Sort lexicographically rather than numerically (`ALPHA`). -/
  sortAlpha : Bool
deriving Inhabited

/-- Redis default `SortOpts` — omit `BY`/`GET`, whole collection, ascending,
    numeric. -/
def sortDefault : SortOpts :=
  ⟨none, (0, -1), [], SortOrder.asc, false⟩

/-- Build a `SORT`/`SORT … STORE` request (upstream's `sortInternal`). -/
private def sortInternal {α : Type} [RedisResult α]
    (key : ByteArray) (destination : Option ByteArray) (opts : SortOpts) : m (f α) :=
  let byArg := match opts.sortBy with | some p => ["BY".toUTF8, p] | none => []
  let (off, cnt) := opts.sortLimit
  let limitArg := ["LIMIT".toUTF8, encode off, encode cnt]
  let getArg := opts.sortGet.flatMap (fun p => ["GET".toUTF8, p])
  let orderArg := match opts.sortOrder with | .asc => ["ASC".toUTF8] | .desc => ["DESC".toUTF8]
  let storeArg := match destination with | some d => ["STORE".toUTF8, d] | none => []
  sendRequest (["SORT".toUTF8, key] ++ byArg ++ limitArg ++ getArg ++ orderArg
    ++ flag opts.sortAlpha "ALPHA".toUTF8 ++ storeArg)

/-- Sort the elements in a list, set or sorted set (`SORT`). -/
def sort (key : ByteArray) (opts : SortOpts) : m (f (List ByteArray)) :=
  sortInternal key none opts

/-- Sort and store the result under `dest` (`SORT … STORE`). -/
def sortStore (key dest : ByteArray) (opts : SortOpts) : m (f Int) :=
  sortInternal key (some dest) opts

-- ── ZUNIONSTORE / ZINTERSTORE ──

/-- Aggregation function for `ZUNIONSTORE`/`ZINTERSTORE`. -/
inductive Aggregate where
  | sum
  | min
  | max
deriving BEq, Inhabited

/-- Build a `ZUNIONSTORE`/`ZINTERSTORE` request (upstream's `zstoreInternal`). -/
private def zstoreInternal (cmd dest : ByteArray) (keys : List ByteArray)
    (weights : List Float) (aggregate : Aggregate) : m (f Int) :=
  let agg := match aggregate with
    | .sum => "SUM".toUTF8 | .min => "MIN".toUTF8 | .max => "MAX".toUTF8
  let weightsArg := if weights.isEmpty then [] else "WEIGHTS".toUTF8 :: weights.map encode
  sendRequest ([cmd, dest, encode (Int.ofNat keys.length)] ++ keys
    ++ weightsArg ++ ["AGGREGATE".toUTF8, agg])

/-- Union multiple sorted sets, storing the result (`ZUNIONSTORE`). -/
def zunionstore (dest : ByteArray) (keys : List ByteArray) (aggregate : Aggregate) : m (f Int) :=
  zstoreInternal "ZUNIONSTORE".toUTF8 dest keys [] aggregate

/-- Union multiple weighted sorted sets, storing the result
    (`ZUNIONSTORE … WEIGHTS`). -/
def zunionstoreWeights (dest : ByteArray) (kws : List (ByteArray × Float))
    (aggregate : Aggregate) : m (f Int) :=
  let (keys, weights) := kws.unzip
  zstoreInternal "ZUNIONSTORE".toUTF8 dest keys weights aggregate

/-- Intersect multiple sorted sets, storing the result (`ZINTERSTORE`). -/
def zinterstore (dest : ByteArray) (keys : NonEmpty ByteArray) (aggregate : Aggregate) :
    m (f Int) :=
  zstoreInternal "ZINTERSTORE".toUTF8 dest keys.toList [] aggregate

/-- Intersect multiple weighted sorted sets, storing the result
    (`ZINTERSTORE … WEIGHTS`). -/
def zinterstoreWeights (dest : ByteArray) (kws : NonEmpty (ByteArray × Float))
    (aggregate : Aggregate) : m (f Int) :=
  let (keys, weights) := kws.toList.unzip
  zstoreInternal "ZINTERSTORE".toUTF8 dest keys weights aggregate

-- ── ZRANGEBYLEX ──

/-- A lexicographical range endpoint for `ZRANGEBYLEX` (`[incl`, `(excl`,
    `-`, `+`). -/
inductive RangeLex (α : Type) where
  | incl (a : α)
  | excl (a : α)
  | minr
  | maxr

instance [RedisArg α] : RedisArg (RangeLex α) where
  encode
    | .incl bs => "[".toUTF8 ++ encode bs
    | .excl bs => "(".toUTF8 ++ encode bs
    | .minr => "-".toUTF8
    | .maxr => "+".toUTF8

/-- Range of members by lexicographical range (`ZRANGEBYLEX`). -/
def zrangebylex (key : ByteArray) (min max : RangeLex ByteArray) : m (f (List ByteArray)) :=
  sendRequest ["ZRANGEBYLEX".toUTF8, key, encode min, encode max]

/-- Range of members by lexicographical range, limited (`ZRANGEBYLEX … LIMIT`). -/
def zrangebylexLimit (key : ByteArray) (min max : RangeLex ByteArray) (offset count : Int) :
    m (f (List ByteArray)) :=
  sendRequest ["ZRANGEBYLEX".toUTF8, key, encode min, encode max,
    "LIMIT".toUTF8, encode offset, encode count]

-- ── Scripting ──

/-- Execute a Lua script server side (`EVAL`). -/
def eval {α : Type} [RedisResult α] (script : ByteArray) (keys args : List ByteArray) :
    m (f α) :=
  sendRequest (["EVAL".toUTF8, script, encode (Int.ofNat keys.length)] ++ keys ++ args)

/-- As `eval`, sending the script's SHA1 hash (`EVALSHA`). -/
def evalsha {α : Type} [RedisResult α] (script : ByteArray) (keys args : List ByteArray) :
    m (f α) :=
  sendRequest (["EVALSHA".toUTF8, script, encode (Int.ofNat keys.length)] ++ keys ++ args)

/-- Debug mode for `SCRIPT DEBUG`. -/
inductive DebugMode where
  | yes
  | sync
  | no
deriving BEq, Inhabited

instance : RedisArg DebugMode where
  encode
    | .yes => "YES".toUTF8
    | .sync => "SYNC".toUTF8
    | .no => "NO".toUTF8

/-- Set the debug mode for executed scripts (`SCRIPT DEBUG`). Sends the
    subcommand as the single token `"SCRIPT DEBUG"`, exactly as upstream. -/
def scriptDebug (mode : DebugMode) : m (f Bool) :=
  sendRequest ["SCRIPT DEBUG".toUTF8, encode mode]

-- ── Bit operations ──

/-- Count set bits in a string (`BITCOUNT`). -/
def bitcount (key : ByteArray) : m (f Int) :=
  sendRequest ["BITCOUNT".toUTF8, key]

/-- Count set bits in a byte range of a string (`BITCOUNT … start end`). -/
def bitcountRange (key : ByteArray) (start end_ : Int) : m (f Int) :=
  sendRequest ["BITCOUNT".toUTF8, key, encode start, encode end_]

/-- Perform a bitwise operation between strings (`BITOP`). -/
private def bitop (op : ByteArray) (ks : List ByteArray) : m (f Int) :=
  sendRequest ("BITOP".toUTF8 :: op :: ks)

/-- Bitwise AND of `srcs` into `dst` (`BITOP AND`). -/
def bitopAnd (dst : ByteArray) (srcs : List ByteArray) : m (f Int) :=
  bitop "AND".toUTF8 (dst :: srcs)

/-- Bitwise OR of `srcs` into `dst` (`BITOP OR`). -/
def bitopOr (dst : ByteArray) (srcs : List ByteArray) : m (f Int) :=
  bitop "OR".toUTF8 (dst :: srcs)

/-- Bitwise XOR of `srcs` into `dst` (`BITOP XOR`). -/
def bitopXor (dst : ByteArray) (srcs : List ByteArray) : m (f Int) :=
  bitop "XOR".toUTF8 (dst :: srcs)

/-- Bitwise NOT of `src` into `dst` (`BITOP NOT`). -/
def bitopNot (dst src : ByteArray) : m (f Int) :=
  bitop "NOT".toUTF8 [dst, src]

-- ── MIGRATE ──

/-- Transfer a single key to another Redis instance (`MIGRATE`). -/
def migrate (host port key : ByteArray) (destinationDb timeout : Int) : m (f Status) :=
  sendRequest ["MIGRATE".toUTF8, host, port, key, encode destinationDb, encode timeout]

/-- Authentication for `MIGRATE` (`AUTH pass` or `AUTH2 user pass`). -/
inductive MigrateAuth where
  | auth (pass : ByteArray)
  | auth2 (user pass : ByteArray)
deriving BEq, Inhabited

/-- Options for the `MIGRATE` command. -/
structure MigrateOpts where
  /-- Keep the key on the local instance (`COPY`). -/
  migrateCopy : Bool
  /-- Replace an existing key on the remote instance (`REPLACE`). -/
  migrateReplace : Bool
  /-- Optional authentication. -/
  migrateAuth : Option MigrateAuth
deriving Inhabited

/-- Redis default `MigrateOpts` — no copy/replace/auth. -/
def migrateDefault : MigrateOpts :=
  ⟨false, false, none⟩

/-- Transfer multiple keys to another Redis instance (`MIGRATE … KEYS`). -/
def migrateMultiple (host port : ByteArray) (destinationDb timeout : Int)
    (opts : MigrateOpts) (keys : List ByteArray) : m (f Status) :=
  let auth_ := match opts.migrateAuth with
    | none => []
    | some (.auth pass) => ["AUTH".toUTF8, pass]
    | some (.auth2 user pass) => ["AUTH2".toUTF8, user, pass]
  sendRequest (["MIGRATE".toUTF8, host, port, ByteArray.empty, encode destinationDb, encode timeout]
    ++ auth_ ++ flag opts.migrateCopy "COPY".toUTF8 ++ flag opts.migrateReplace "REPLACE".toUTF8
    ++ keys)

-- ── RESTORE ──

/-- Create a key from a serialized value (`RESTORE`). -/
def restore (key : ByteArray) (timeToLive : Int) (serializedValue : ByteArray) : m (f Status) :=
  sendRequest ["RESTORE".toUTF8, key, encode timeToLive, serializedValue]

/-- Options for the `RESTORE` command. -/
structure RestoreOpts where
  /-- Replace an existing key (`REPLACE`). -/
  restoreReplace : Bool
  /-- Treat `timeToLive` as an absolute Unix time (`ABSTTL`). -/
  restoreAbsTTL : Bool
  /-- Set the key's idle time (`IDLE`). -/
  restoreIdle : Option Int
  /-- Set the key's access frequency (`FREQ`). -/
  restoreFreq : Option Int
deriving Inhabited

/-- Default `RestoreOpts` — no options set. -/
def restoreOptsDefault : RestoreOpts :=
  ⟨false, false, none, none⟩

/-- Create a key from a serialized value with options (`RESTORE`). -/
def restoreOpts (key : ByteArray) (timeToLive : Int) (serializedValue : ByteArray)
    (opts : RestoreOpts) : m (f Status) :=
  sendRequest (["RESTORE".toUTF8, key, encode timeToLive, serializedValue]
    ++ flag opts.restoreReplace "REPLACE".toUTF8 ++ flag opts.restoreAbsTTL "ABSTTL".toUTF8
    ++ optEnc "IDLE".toUTF8 opts.restoreIdle ++ optEnc "FREQ".toUTF8 opts.restoreFreq)

/-- Create a key from a serialized value, replacing any existing key
    (`RESTORE … REPLACE`). -/
def restoreReplace (key : ByteArray) (timeToLive : Int) (serializedValue : ByteArray) :
    m (f Status) :=
  sendRequest ["RESTORE".toUTF8, key, encode timeToLive, serializedValue, "REPLACE".toUTF8]

-- ── SET ──

/-- Set-on-condition flag for `SET`/`ZADD`/`GEOADD` (`NX`/`XX`). -/
inductive Condition where
  | nx
  | xx
deriving BEq, Inhabited

instance : RedisArg Condition where
  encode
    | .nx => "NX".toUTF8
    | .xx => "XX".toUTF8

/-- Set the string value of a key (`SET`). -/
def set (key value : ByteArray) : m (f Status) :=
  sendRequest ["SET".toUTF8, key, value]

/-- Options for the `SET` command. -/
structure SetOpts where
  /-- Expire time in seconds (`EX`). -/
  setSeconds : Option Int
  /-- Expire time in milliseconds (`PX`). -/
  setMilliseconds : Option Int
  /-- Unix expire time in seconds (`EXAT`). -/
  setUnixSeconds : Option Int
  /-- Unix expire time in milliseconds (`PXAT`). -/
  setUnixMilliseconds : Option Int
  /-- Set-on-condition (`NX`/`XX`). -/
  setCondition : Option Condition
  /-- Retain the key's existing TTL (`KEEPTTL`). -/
  setKeepTTL : Bool
deriving Inhabited

/-- Default `SetOpts` — no options set. -/
def setDefault : SetOpts :=
  ⟨none, none, none, none, none, false⟩

/-- Build `SET`'s option arguments (upstream's `internalSetOptsToArgs`). -/
private def internalSetOptsToArgs (o : SetOpts) : List ByteArray :=
  optEnc "EX".toUTF8 o.setSeconds ++ optEnc "PX".toUTF8 o.setMilliseconds
  ++ optEnc "EXAT".toUTF8 o.setUnixSeconds ++ optEnc "PXAT".toUTF8 o.setUnixMilliseconds
  ++ flag o.setKeepTTL "KEEPTTL".toUTF8
  ++ (match o.setCondition with | some c => [encode c] | none => [])

/-- Set the string value of a key, with options (`SET`). -/
def setOpts (key value : ByteArray) (opts : SetOpts) : m (f Status) :=
  sendRequest (["SET".toUTF8, key, value] ++ internalSetOptsToArgs opts)

/-- Set a key and return its old value (`SET … GET`). -/
def setGet (key value : ByteArray) : m (f ByteArray) :=
  sendRequest ["SET".toUTF8, key, value, "GET".toUTF8]

/-- Set a key with options and return its old value (`SET … GET`). -/
def setGetOpts (key value : ByteArray) (opts : SetOpts) : m (f ByteArray) :=
  sendRequest (["SET".toUTF8, key, value, "GET".toUTF8] ++ internalSetOptsToArgs opts)

-- ── CLIENT REPLY ──

/-- Reply mode for `CLIENT REPLY`. -/
inductive ReplyMode where
  | on
  | off
  | skip
deriving BEq, Inhabited

instance : RedisArg ReplyMode where
  encode
    | .on => "ON".toUTF8
    | .off => "OFF".toUTF8
    | .skip => "SKIP".toUTF8

/-- Instruct the server whether to reply to commands (`CLIENT REPLY`). Sends
    the subcommand as the single token `"CLIENT REPLY"`, exactly as upstream. -/
def clientReply (mode : ReplyMode) : m (f Bool) :=
  sendRequest ["CLIENT REPLY".toUTF8, encode mode]

-- ── Random set members / pop ──

/-- Get a random member from a set (`SRANDMEMBER`). -/
def srandmember (key : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["SRANDMEMBER".toUTF8, key]

/-- Get `count` random members from a set (`SRANDMEMBER … count`). -/
def srandmemberN (key : ByteArray) (count : Int) : m (f (List ByteArray)) :=
  sendRequest ["SRANDMEMBER".toUTF8, key, encode count]

/-- Remove and return a random member from a set (`SPOP`). -/
def spop (key : ByteArray) : m (f (Option ByteArray)) :=
  sendRequest ["SPOP".toUTF8, key]

/-- Remove and return `count` random members from a set (`SPOP … count`). -/
def spopN (key : ByteArray) (count : Int) : m (f (List ByteArray)) :=
  sendRequest ["SPOP".toUTF8, key, encode count]

-- ── INFO / EXISTS ──

/-- Get information and statistics about the server (`INFO`). -/
def info : m (f ByteArray) :=
  sendRequest ["INFO".toUTF8]

/-- Get information about a specific section (`INFO section`). -/
def infoSection (sectionName : ByteArray) : m (f ByteArray) :=
  sendRequest ["INFO".toUTF8, sectionName]

/-- Determine if a key exists (`EXISTS`). -/
def «exists» (key : ByteArray) : m (f Bool) :=
  sendRequest ["EXISTS".toUTF8, key]

-- ── SCAN family ──

/-- A SCAN cursor. -/
structure Cursor where
  /-- The raw cursor bytes. -/
  value : ByteArray
deriving BEq, Inhabited

instance : RedisArg Cursor where
  encode c := c.value

instance : RedisResult Cursor where
  decode
    | .bulk (some s) => Except.ok ⟨s⟩
    | r => Except.error r

/-- The initial cursor (`"0"`). -/
def cursor0 : Cursor :=
  ⟨"0".toUTF8⟩

/-- Options for the `SCAN` family. -/
structure ScanOpts where
  /-- `MATCH` pattern. -/
  scanMatch : Option ByteArray
  /-- `COUNT` hint. -/
  scanCount : Option Int
deriving Inhabited

/-- Default `ScanOpts` — no `MATCH`/`COUNT`. -/
def scanOptsDefault : ScanOpts :=
  ⟨none, none⟩

/-- Append `MATCH`/`COUNT` arguments to a scan command (upstream's
    `addScanOpts`). -/
private def addScanOpts (cmd : List ByteArray) (o : ScanOpts) : List ByteArray :=
  cmd ++ optRaw "MATCH".toUTF8 o.scanMatch ++ optEnc "COUNT".toUTF8 o.scanCount

/-- Incrementally iterate the keyspace, with options (`SCAN`). -/
def scanOpts (cursor : Cursor) (opts : ScanOpts) (mtype : Option ByteArray) :
    m (f (Cursor × List ByteArray)) :=
  sendRequest (addScanOpts ["SCAN".toUTF8, encode cursor] opts
    ++ (match mtype with | some t => ["TYPE".toUTF8, t] | none => []))

/-- Incrementally iterate the keyspace (`SCAN`). -/
def scan (cursor : Cursor) : m (f (Cursor × List ByteArray)) :=
  scanOpts cursor scanOptsDefault none

/-- Incrementally iterate set elements, with options (`SSCAN`). -/
def sscanOpts (key : ByteArray) (cursor : Cursor) (opts : ScanOpts) :
    m (f (Cursor × List ByteArray)) :=
  sendRequest (addScanOpts ["SSCAN".toUTF8, key, encode cursor] opts)

/-- Incrementally iterate set elements (`SSCAN`). -/
def sscan (key : ByteArray) (cursor : Cursor) : m (f (Cursor × List ByteArray)) :=
  sscanOpts key cursor scanOptsDefault

/-- Incrementally iterate hash fields/values, with options (`HSCAN`). -/
def hscanOpts (key : ByteArray) (cursor : Cursor) (opts : ScanOpts) :
    m (f (Cursor × KeyValueReply ByteArray ByteArray)) :=
  sendRequest (addScanOpts ["HSCAN".toUTF8, key, encode cursor] opts)

/-- Incrementally iterate hash fields/values (`HSCAN`). -/
def hscan (key : ByteArray) (cursor : Cursor) :
    m (f (Cursor × KeyValueReply ByteArray ByteArray)) :=
  hscanOpts key cursor scanOptsDefault

/-- Incrementally iterate sorted-set members/scores, with options (`ZSCAN`). -/
def zscanOpts (key : ByteArray) (cursor : Cursor) (opts : ScanOpts) :
    m (f (Cursor × KeyValueReply ByteArray Float)) :=
  sendRequest (addScanOpts ["ZSCAN".toUTF8, key, encode cursor] opts)

/-- Incrementally iterate sorted-set members/scores (`ZSCAN`). -/
def zscan (key : ByteArray) (cursor : Cursor) :
    m (f (Cursor × KeyValueReply ByteArray Float)) :=
  zscanOpts key cursor scanOptsDefault

-- ── ZADD ──

/-- Update-on-condition flag for `ZADD` (`GT`/`LT`). -/
inductive SizeCondition where
  | gt
  | lt
deriving BEq, Inhabited

instance : RedisArg SizeCondition where
  encode
    | .gt => "GT".toUTF8
    | .lt => "LT".toUTF8

/-- Options for the `ZADD` command. -/
structure ZaddOpts where
  /-- Add-on-condition (`NX`/`XX`). -/
  zaddCondition : Option Condition
  /-- Update-on-condition (`GT`/`LT`). -/
  zaddSizeCondition : Option SizeCondition
  /-- Report the number of changed elements rather than added ones (`CH`). -/
  zaddChange : Bool
  /-- Act like `ZINCRBY` (`INCR`). -/
  zaddIncrement : Bool
deriving Inhabited

/-- Redis default `ZaddOpts` — no options set. -/
def zaddDefault : ZaddOpts :=
  ⟨none, none, false, false⟩

/-- Add members to a sorted set, with options (`ZADD`). -/
def zaddOpts (key : ByteArray) (scoreMembers : List (Float × ByteArray)) (opts : ZaddOpts) :
    m (f Int) :=
  let condition := match opts.zaddCondition with | some c => [encode c] | none => []
  let sizeCondition := match opts.zaddSizeCondition with | some c => [encode c] | none => []
  let scores := scoreMembers.flatMap (fun (x, y) => [encode x, y])
  sendRequest (["ZADD".toUTF8, key] ++ condition ++ sizeCondition
    ++ flag opts.zaddChange "CH".toUTF8 ++ flag opts.zaddIncrement "INCR".toUTF8 ++ scores)

/-- Add members to a sorted set (`ZADD`). -/
def zadd (key : ByteArray) (scoreMembers : List (Float × ByteArray)) : m (f Int) :=
  zaddOpts key scoreMembers zaddDefault

-- ── Streams: trimming ──

/-- Trimming strategy for `XADD`/`XTRIM`. -/
inductive TrimStrategy where
  /-- `MAXLEN` — evict while length exceeds the threshold. -/
  | maxlen (threshold : Int)
  /-- `MINID` — evict entries with IDs below the threshold. -/
  | minId (threshold : ByteArray)
deriving BEq, Inhabited

/-- Type of trimming: exact or approximate (with optional `LIMIT`). -/
inductive TrimType where
  /-- Exact trimming (`=`). -/
  | exact
  /-- Approximate trimming (`~`), with an optional eviction `LIMIT`. -/
  | approx (limit : Option Int)
deriving BEq, Inhabited

/-- Trimming options (strategy plus type). -/
structure TrimOpts where
  /-- The trimming strategy. -/
  trimStrategy : TrimStrategy
  /-- The trimming type. -/
  trimType : TrimType
deriving Inhabited

/-- Build the low-level trim arguments (upstream's `internalTrimArgToList`). -/
private def internalTrimArgToList (o : TrimOpts) : List ByteArray :=
  let (approxArg, limitArg) := match o.trimType with
    | .exact => ("=".toUTF8, [])
    | .approx limit => ("~".toUTF8,
        match limit with | some l => ["LIMIT".toUTF8, encode l] | none => [])
  let trimArg := match o.trimStrategy with
    | .maxlen mx => ["MAXLEN".toUTF8, approxArg, encode mx]
    | .minId i => ["MINID".toUTF8, approxArg, i]
  trimArg ++ limitArg

/-- Assemble a `TrimOpts` (upstream's `trimOpts`). -/
def trimOpts (strategy : TrimStrategy) (type : TrimType) : TrimOpts :=
  ⟨strategy, type⟩

-- ── Streams: records ──

/-- A stream entry: its ID and field/value pairs. -/
structure StreamsRecord where
  /-- The entry ID. -/
  recordId : ByteArray
  /-- The entry's field/value pairs. -/
  keyValues : List (ByteArray × ByteArray)
deriving BEq, Inhabited

/-- Chunk a flat list into consecutive pairs, dropping a trailing odd element. -/
private def chunkPairs : List ByteArray → List (ByteArray × ByteArray)
  | x :: y :: rest => (x, y) :: chunkPairs rest
  | _ => []

instance : RedisResult StreamsRecord where
  decode
    | .multiBulk (some [.bulk (some recordId), .multiBulk (some rawKeyValues)]) =>
      (do
        let kvList ← rawKeyValues.mapM (decode (α := ByteArray))
        pure ⟨recordId, chunkPairs kvList⟩)
    | a => Except.error a

-- ── XADD ──

/-- Options for the `XADD` command. -/
structure XAddOpts where
  /-- Trim the stream right after adding (`MAXLEN`/`MINID`). -/
  xAddTrimOpts : Option TrimOpts
  /-- Don't create the stream if it doesn't exist (`NOMKSTREAM`). -/
  xAddNoMkStream : Bool
deriving Inhabited

/-- Default `XAddOpts` — no trim, create the stream. -/
def defaultXAddOpts : XAddOpts :=
  ⟨none, false⟩

/-- Add an entry to a stream, with options (`XADD`). -/
def xaddOpts (key entryId : ByteArray) (fieldValues : List (ByteArray × ByteArray))
    (opts : XAddOpts) : m (f ByteArray) :=
  let trimArgs := match opts.xAddTrimOpts with | some t => internalTrimArgToList t | none => []
  sendRequest (["XADD".toUTF8, key] ++ flag opts.xAddNoMkStream "NOMKSTREAM".toUTF8
    ++ trimArgs ++ [entryId] ++ flattenPairs fieldValues)

/-- Add an entry to a stream (`XADD`). -/
def xadd (key entryId : ByteArray) (fieldValues : List (ByteArray × ByteArray)) :
    m (f ByteArray) :=
  xaddOpts key entryId fieldValues defaultXAddOpts

-- ── XAUTOCLAIM ──

/-- Options for the `XAUTOCLAIM` family. -/
structure XAutoclaimOpts where
  /-- Upper limit on entries to claim (`COUNT`, default 100). -/
  xAutoclaimCount : Option Int
deriving Inhabited

/-- Default `XAutoclaimOpts` — no explicit count. -/
def defaultXAutoclaimOpts : XAutoclaimOpts :=
  ⟨none⟩

/-- Result of the `XAUTOCLAIM` family: the next cursor ID, the claimed
    messages, and the IDs of already-deleted messages. -/
structure XAutoclaimResult (resultFormat : Type) where
  /-- ID to pass as the start of the next `xautoclaim` call. -/
  xAutoclaimResultId : ByteArray
  /-- Successfully claimed messages. -/
  xAutoclaimClaimedMessages : List resultFormat
  /-- IDs of messages in the PEL that were already deleted from the stream. -/
  xAutoclaimDeletedMessages : List ByteArray

instance [RedisResult α] : RedisResult (XAutoclaimResult α) where
  decode
    | .multiBulk (some [.bulk (some rid), claimedMsg, deletedMsg]) =>
      (do pure ⟨rid, ← decode claimedMsg, ← decode deletedMsg⟩)
    | .multiBulk (some [.bulk (some rid), .multiBulk (some [])]) =>
      Except.ok ⟨rid, [], []⟩
    | a => Except.error a

/-- `XAUTOCLAIM` result carrying full message data. -/
abbrev XAutoclaimStreamsResult := XAutoclaimResult StreamsRecord

/-- `XAUTOCLAIM … JUSTID` result carrying only message IDs. -/
abbrev XAutoclaimJustIdsResult := XAutoclaimResult ByteArray

/-- Claim pending stream entries, with options (`XAUTOCLAIM`). -/
def xautoclaimOpts (key group consumer : ByteArray) (minIdleTime : Int) (start : ByteArray)
    (opts : XAutoclaimOpts) : m (f XAutoclaimStreamsResult) :=
  sendRequest (["XAUTOCLAIM".toUTF8, key, group, consumer, encode minIdleTime, start]
    ++ optEnc "COUNT".toUTF8 opts.xAutoclaimCount)

/-- Claim pending stream entries (`XAUTOCLAIM`). -/
def xautoclaim (key group consumer : ByteArray) (minIdleTime : Int) (start : ByteArray) :
    m (f XAutoclaimStreamsResult) :=
  xautoclaimOpts key group consumer minIdleTime start defaultXAutoclaimOpts

/-- Claim pending stream entries, returning only IDs, with options
    (`XAUTOCLAIM … JUSTID`). -/
def xautoclaimJustIdsOpts (key group consumer : ByteArray) (minIdleTime : Int)
    (start : ByteArray) (opts : XAutoclaimOpts) : m (f XAutoclaimJustIdsResult) :=
  sendRequest (["XAUTOCLAIM".toUTF8, key, group, consumer, encode minIdleTime, start]
    ++ optEnc "COUNT".toUTF8 opts.xAutoclaimCount ++ ["JUSTID".toUTF8])

/-- Claim pending stream entries, returning only IDs (`XAUTOCLAIM … JUSTID`). -/
def xautoclaimJustIds (key group consumer : ByteArray) (minIdleTime : Int) (start : ByteArray) :
    m (f XAutoclaimJustIdsResult) :=
  xautoclaimJustIdsOpts key group consumer minIdleTime start defaultXAutoclaimOpts

-- ── XREAD ──

/-- Options for `XREAD`. -/
structure XReadOpts where
  /-- Block for up to this many milliseconds waiting for entries (`BLOCK`). -/
  block : Option Int
  /-- Maximum number of entries per stream (`COUNT`). -/
  recordCount : Option Int
deriving Inhabited

/-- Default `XReadOpts` — no blocking, no count. -/
def defaultXreadOpts : XReadOpts :=
  ⟨none, none⟩

/-- A per-stream `XREAD`/`XREADGROUP` response. -/
structure XReadResponse where
  /-- The stream name. -/
  stream : ByteArray
  /-- The returned entries. -/
  records : List StreamsRecord
deriving BEq, Inhabited

instance : RedisResult XReadResponse where
  decode
    | .multiBulk (some [.bulk (some stream), .multiBulk (some rawRecords)]) =>
      (do pure ⟨stream, ← rawRecords.mapM decode⟩)
    | a => Except.error a

/-- Build `XREAD`'s arguments (upstream's `internalXreadArgs`). -/
private def internalXreadArgs (streamsAndIds : List (ByteArray × ByteArray))
    (o : XReadOpts) : List ByteArray :=
  optEnc "BLOCK".toUTF8 o.block ++ optEnc "COUNT".toUTF8 o.recordCount
  ++ ["STREAMS".toUTF8] ++ streamsAndIds.map (·.1) ++ streamsAndIds.map (·.2)

/-- Read from one or more streams, with options (`XREAD`). -/
def xreadOpts (streamsAndIds : List (ByteArray × ByteArray)) (opts : XReadOpts) :
    m (f (Option (List XReadResponse))) :=
  sendRequest ("XREAD".toUTF8 :: internalXreadArgs streamsAndIds opts)

/-- Read from one or more streams (`XREAD`). -/
def xread (streamsAndIds : List (ByteArray × ByteArray)) :
    m (f (Option (List XReadResponse))) :=
  xreadOpts streamsAndIds defaultXreadOpts

-- ── XREADGROUP ──

/-- Options for `XREADGROUP`. -/
structure XReadGroupOpts where
  /-- Block for up to this many milliseconds (`BLOCK`). -/
  xReadGroupBlock : Option Int
  /-- Maximum number of entries per stream (`COUNT`). -/
  xReadGroupCount : Option Int
  /-- Do not add read entries to the PEL (`NOACK`). -/
  xReadGroupNoAck : Bool
deriving Inhabited

/-- Default `XReadGroupOpts` — no blocking/count, acknowledging. -/
def defaultXReadGroupOpts : XReadGroupOpts :=
  ⟨none, none, false⟩

/-- Read from streams as part of a consumer group, with options
    (`XREADGROUP`). -/
def xreadGroupOpts (groupName consumerName : ByteArray)
    (streamsAndIds : List (ByteArray × ByteArray)) (opts : XReadGroupOpts) :
    m (f (Option (List XReadResponse))) :=
  let internal := optEnc "COUNT".toUTF8 opts.xReadGroupCount
    ++ optEnc "BLOCK".toUTF8 opts.xReadGroupBlock
    ++ flag opts.xReadGroupNoAck "NOACK".toUTF8 ++ ["STREAMS".toUTF8]
    ++ streamsAndIds.map (·.1) ++ streamsAndIds.map (·.2)
  sendRequest (["XREADGROUP".toUTF8, "GROUP".toUTF8, groupName, consumerName] ++ internal)

/-- Read from streams as part of a consumer group (`XREADGROUP`). -/
def xreadGroup (groupName consumerName : ByteArray)
    (streamsAndIds : List (ByteArray × ByteArray)) : m (f (Option (List XReadResponse))) :=
  xreadGroupOpts groupName consumerName streamsAndIds defaultXReadGroupOpts

-- ── XGROUP ──

/-- Options for `XGROUP CREATE`. -/
structure XGroupCreateOpts where
  /-- Create the stream if it does not exist (`MKSTREAM`). -/
  xGroupCreateMkStream : Bool
  /-- Enable lag tracking from an arbitrary ID (`ENTRIESREAD`). -/
  xGroupCreateEntriesRead : Option ByteArray
deriving Inhabited

/-- Default `XGroupCreateOpts` — no `MKSTREAM`, no `ENTRIESREAD`. -/
def defaultXGroupCreateOpts : XGroupCreateOpts :=
  ⟨false, none⟩

/-- Create a consumer group, with options (`XGROUP CREATE`). -/
def xgroupCreateOpts (stream groupName startId : ByteArray) (opts : XGroupCreateOpts) :
    m (f Status) :=
  sendRequest (["XGROUP".toUTF8, "CREATE".toUTF8, stream, groupName, startId]
    ++ flag opts.xGroupCreateMkStream "MKSTREAM".toUTF8
    ++ optRaw "ENTRIESREAD".toUTF8 opts.xGroupCreateEntriesRead)

/-- Create a consumer group (`XGROUP CREATE`). -/
def xgroupCreate (stream groupName startId : ByteArray) : m (f Status) :=
  xgroupCreateOpts stream groupName startId defaultXGroupCreateOpts

/-- Create a consumer in a group (`XGROUP CREATECONSUMER`). -/
def xgroupCreateConsumer (key group consumer : ByteArray) : m (f Bool) :=
  sendRequest ["XGROUP".toUTF8, "CREATECONSUMER".toUTF8, key, group, consumer]

/-- Options for `XGROUP SETID`. -/
structure XGroupSetIdOpts where
  /-- Enable lag tracking from an arbitrary ID (`ENTRIESREAD`). -/
  xGroupSetIdEntriesRead : Option ByteArray
deriving Inhabited

/-- Default `XGroupSetIdOpts` — no `ENTRIESREAD`. -/
def defaultXGroupSetIdOpts : XGroupSetIdOpts :=
  ⟨none⟩

/-- Set the last-delivered ID for a group, with options (`XGROUP SETID`). -/
def xgroupSetIdOpts (stream group messageId : ByteArray) (opts : XGroupSetIdOpts) :
    m (f Status) :=
  sendRequest (["XGROUP".toUTF8, "SETID".toUTF8, stream, group, messageId]
    ++ optRaw "ENTRIESREAD".toUTF8 opts.xGroupSetIdEntriesRead)

/-- Set the last-delivered ID for a group (`XGROUP SETID`). -/
def xgroupSetId (stream group messageId : ByteArray) : m (f Status) :=
  xgroupSetIdOpts stream group messageId defaultXGroupSetIdOpts

/-- Delete a consumer from a group (`XGROUP DELCONSUMER`). -/
def xgroupDelConsumer (stream group consumer : ByteArray) : m (f Int) :=
  sendRequest ["XGROUP".toUTF8, "DELCONSUMER".toUTF8, stream, group, consumer]

/-- Destroy a consumer group (`XGROUP DESTROY`). -/
def xgroupDestroy (stream group : ByteArray) : m (f Bool) :=
  sendRequest ["XGROUP".toUTF8, "DESTROY".toUTF8, stream, group]

-- ── XACK / XRANGE / XLEN ──

/-- Acknowledge processed messages (`XACK`). -/
def xack (stream groupName : ByteArray) (messageIds : List ByteArray) : m (f Int) :=
  sendRequest (["XACK".toUTF8, stream, groupName] ++ messageIds)

/-- Range of stream entries (`XRANGE`). -/
def xrange (stream start end_ : ByteArray) (count : Option Int) : m (f (List StreamsRecord)) :=
  sendRequest (["XRANGE".toUTF8, stream, start, end_] ++ optEnc "COUNT".toUTF8 count)

/-- Range of stream entries in reverse (`XREVRANGE`). -/
def xrevRange (stream end_ start : ByteArray) (count : Option Int) :
    m (f (List StreamsRecord)) :=
  sendRequest (["XREVRANGE".toUTF8, stream, end_, start] ++ optEnc "COUNT".toUTF8 count)

/-- Number of entries in a stream (`XLEN`). -/
def xlen (stream : ByteArray) : m (f Int) :=
  sendRequest ["XLEN".toUTF8, stream]

-- ── XPENDING ──

/-- Summary response of `XPENDING`. -/
structure XPendingSummaryResponse where
  /-- Total number of pending messages. -/
  numPendingMessages : Int
  /-- Smallest pending message ID. -/
  smallestPendingMessageId : ByteArray
  /-- Largest pending message ID. -/
  largestPendingMessageId : ByteArray
  /-- Per-consumer pending-message counts. -/
  numPendingMessagesByconsumer : List (ByteArray × Int)
deriving BEq, Inhabited

/-- Chunk a flat list of replies into consecutive pairs. -/
private def chunkReplies : List Reply → List (Reply × Reply)
  | x :: y :: rest => (x, y) :: chunkReplies rest
  | _ => []

/-- Decode a list of `(consumer, count)` reply pairs. -/
private def decodeGroupCounts : List (Reply × Reply) → Except Reply (List (ByteArray × Int))
  | [] => Except.ok []
  | (x, y) :: rest => do
    let dx ← decode x
    let dy ← decode y
    let r ← decodeGroupCounts rest
    pure ((dx, dy) :: r)

instance : RedisResult XPendingSummaryResponse where
  decode
    | .multiBulk (some [.integer n, .bulk (some smallest), .bulk (some largest),
        .multiBulk (some [.multiBulk (some rawGroupsAndCounts)])]) =>
      (do pure ⟨n, smallest, largest, ← decodeGroupCounts (chunkReplies rawGroupsAndCounts)⟩)
    | a => Except.error a

/-- Get summary information about pending messages (`XPENDING`). -/
def xpendingSummary (stream group : ByteArray) : m (f XPendingSummaryResponse) :=
  sendRequest ["XPENDING".toUTF8, stream, group]

/-- A detail record returned by `XPENDING` with a range. -/
structure XPendingDetailRecord where
  /-- The pending message ID. -/
  messageId : ByteArray
  /-- The consumer that owns the message. -/
  consumer : ByteArray
  /-- Milliseconds since the message was last delivered. -/
  millisSinceLastDelivered : Int
  /-- Number of times the message was delivered. -/
  numTimesDelivered : Int
deriving BEq, Inhabited

instance : RedisResult XPendingDetailRecord where
  decode
    | .multiBulk (some [.bulk (some messageId), .bulk (some consumer), .integer m1, .integer m2]) =>
      Except.ok ⟨messageId, consumer, m1, m2⟩
    | a => Except.error a

/-- Options for the detailed form of `XPENDING`. -/
structure XPendingDetailOpts where
  /-- Only messages owned by this consumer. -/
  xPendingDetailConsumer : Option ByteArray
  /-- Only messages idle for at least this many milliseconds (`IDLE`). -/
  xPendingDetailIdle : Option Int
deriving Inhabited

/-- Default `XPendingDetailOpts` — no filters. -/
def defaultXPendingDetailOpts : XPendingDetailOpts :=
  ⟨none, none⟩

/-- Get detailed information about pending messages (`XPENDING` with a range). -/
def xpendingDetail (stream group startId endId : ByteArray) (count : Int)
    (opts : XPendingDetailOpts) : m (f (List XPendingDetailRecord)) :=
  let consumerArg := match opts.xPendingDetailConsumer with | some c => [c] | none => []
  sendRequest (["XPENDING".toUTF8, stream, group] ++ optEnc "IDLE".toUTF8 opts.xPendingDetailIdle
    ++ [startId, endId, encode count] ++ consumerArg)

-- ── XCLAIM ──

/-- Options for `XCLAIM`. -/
structure XClaimOpts where
  /-- Set the idle time of claimed messages (`IDLE`). -/
  xclaimIdle : Option Int
  /-- Set the last-delivery time of claimed messages (`TIME`). -/
  xclaimTime : Option Int
  /-- Set the retry counter of claimed messages (`RETRYCOUNT`). -/
  xclaimRetryCount : Option Int
  /-- Create the PEL entry even if the message is not already pending (`FORCE`). -/
  xclaimForce : Bool
deriving Inhabited

/-- Default `XClaimOpts` — no options set. -/
def defaultXClaimOpts : XClaimOpts :=
  ⟨none, none, none, false⟩

/-- Build an `XCLAIM` request (upstream's `xclaimRequest`). -/
private def xclaimRequest (stream group consumer : ByteArray) (minIdleTime : Int)
    (opts : XClaimOpts) (messageIds : List ByteArray) : List ByteArray :=
  ["XCLAIM".toUTF8, stream, group, consumer, encode minIdleTime] ++ messageIds
  ++ optEnc "IDLE".toUTF8 opts.xclaimIdle ++ optEnc "TIME".toUTF8 opts.xclaimTime
  ++ optEnc "RETRYCOUNT".toUTF8 opts.xclaimRetryCount ++ flag opts.xclaimForce "FORCE".toUTF8

/-- Claim pending messages (`XCLAIM`). -/
def xclaim (stream group consumer : ByteArray) (minIdleTime : Int) (opts : XClaimOpts)
    (messageIds : List ByteArray) : m (f (List StreamsRecord)) :=
  sendRequest (xclaimRequest stream group consumer minIdleTime opts messageIds)

/-- Claim pending messages, returning only IDs (`XCLAIM … JUSTID`). -/
def xclaimJustIds (stream group consumer : ByteArray) (minIdleTime : Int) (opts : XClaimOpts)
    (messageIds : List ByteArray) : m (f (List ByteArray)) :=
  sendRequest (xclaimRequest stream group consumer minIdleTime opts messageIds ++ ["JUSTID".toUTF8])

-- ── XDEL / XTRIM ──

/-- Delete entries from a stream (`XDEL`). -/
def xdel (stream : ByteArray) (messageIds : NonEmpty ByteArray) : m (f Int) :=
  sendRequest ("XDEL".toUTF8 :: stream :: messageIds.toList)

/-- Trim a stream to a bound (`XTRIM`). -/
def xtrim (stream : ByteArray) (opts : TrimOpts) : m (f Int) :=
  sendRequest ("XTRIM".toUTF8 :: stream :: internalTrimArgToList opts)

/-- Positive infinity, for `inf`/`-inf` Redis argument values. -/
def inf : Float :=
  (1.0 : Float) / 0.0

-- ── Geo: units, order, coordinates, locations ──

/-- Distance unit for geo commands. -/
inductive GeoUnit where
  | meters
  | kilometers
  | feet
  | miles
deriving BEq, Inhabited

instance : RedisArg GeoUnit where
  encode
    | .meters => "m".toUTF8
    | .kilometers => "km".toUTF8
    | .feet => "ft".toUTF8
    | .miles => "mi".toUTF8

/-- Result ordering for geo searches. -/
inductive GeoOrder where
  | asc
  | desc
deriving BEq, Inhabited

instance : RedisArg GeoOrder where
  encode
    | .asc => "ASC".toUTF8
    | .desc => "DESC".toUTF8

/-- A longitude/latitude coordinate pair. -/
structure GeoCoordinates where
  /-- Longitude. -/
  geoLongitude : Float
  /-- Latitude. -/
  geoLatitude : Float
deriving BEq, Inhabited

instance : RedisResult GeoCoordinates where
  decode
    | .multiBulk (some [lon, lat]) => (do pure ⟨← decode lon, ← decode lat⟩)
    | r => Except.error r

/-- A geo-search result member with its optional distance/hash/coordinates. -/
structure GeoLocation where
  /-- The member name. -/
  geoLocationMember : ByteArray
  /-- Distance from the query point (`WITHDIST`). -/
  geoLocationDist : Option Float
  /-- Geohash integer (`WITHHASH`). -/
  geoLocationHash : Option Int
  /-- Coordinates (`WITHCOORD`). -/
  geoLocationCoordinates : Option GeoCoordinates
deriving BEq, Inhabited

/-- Decode the optional detail replies that follow a geo member. -/
private def decodeGeoLocationDetails (md : Option Float) (mh : Option Int)
    (mc : Option GeoCoordinates) :
    List Reply → Except Reply (Option Float × Option Int × Option GeoCoordinates)
  | [] => Except.ok (md, mh, mc)
  | x :: xs =>
    match x with
    | .multiBulk _ => do
      let coord ← decode x
      decodeGeoLocationDetails md mh (some coord) xs
    | .integer _ => do
      let hv ← decode x
      decodeGeoLocationDetails md (some hv) mc xs
    | _ => do
      let d ← decode x
      decodeGeoLocationDetails (some d) mh mc xs

instance : RedisResult GeoLocation where
  decode
    | r@(.bulk (some _)) => (do pure ⟨← decode r, none, none, none⟩)
    | r@(.singleLine _) => (do pure ⟨← decode r, none, none, none⟩)
    | .multiBulk (some (memberReply :: details)) =>
      (do
        let member ← decode memberReply
        let (md, mh, mc) ← decodeGeoLocationDetails none none none details
        pure ⟨member, md, mh, mc⟩)
    | r => Except.error r

-- ── Geo: search specifications and options ──

/-- The origin of a `GEOSEARCH` (`FROMMEMBER`/`FROMLONLAT`). -/
inductive GeoSearchFrom where
  | fromMember (member : ByteArray)
  | fromLonLat (lon lat : Float)

/-- The shape of a `GEOSEARCH` (`BYRADIUS`/`BYBOX`). -/
inductive GeoSearchBy where
  | byRadius (radius : Float) (unit : GeoUnit)
  | byBox (width height : Float) (unit : GeoUnit)

/-- Options for `GEOSEARCH`. -/
structure GeoSearchOpts where
  /-- Return coordinates (`WITHCOORD`). -/
  geoSearchWithCoord : Bool
  /-- Return distances (`WITHDIST`). -/
  geoSearchWithDist : Bool
  /-- Return geohashes (`WITHHASH`). -/
  geoSearchWithHash : Bool
  /-- Limit the number of results (`COUNT`). -/
  geoSearchCount : Option Int
  /-- Return any `COUNT` results, not the closest (`ANY`). -/
  geoSearchCountAny : Bool
  /-- Result ordering. -/
  geoSearchOrder : Option GeoOrder
deriving Inhabited

/-- Default `GeoSearchOpts` — no flags/count/order. -/
def defaultGeoSearchOpts : GeoSearchOpts :=
  ⟨false, false, false, none, false, none⟩

/-- Options for `GEOSEARCHSTORE`. -/
structure GeoSearchStoreOpts where
  /-- Limit the number of results (`COUNT`). -/
  geoSearchStoreCount : Option Int
  /-- Return any `COUNT` results, not the closest (`ANY`). -/
  geoSearchStoreCountAny : Bool
  /-- Result ordering. -/
  geoSearchStoreOrder : Option GeoOrder
  /-- Store the distance rather than the geohash (`STOREDIST`). -/
  geoSearchStoreStoredist : Bool
deriving Inhabited

/-- Default `GeoSearchStoreOpts` — no count/order/storedist. -/
def defaultGeoSearchStoreOpts : GeoSearchStoreOpts :=
  ⟨none, false, none, false⟩

/-- Options for `GEOADD`. -/
structure GeoAddOpts where
  /-- Add-on-condition (`NX`/`XX`). -/
  geoAddCondition : Option Condition
  /-- Report the number of changed elements rather than added ones (`CH`). -/
  geoAddChange : Bool
deriving Inhabited

/-- Redis default `GeoAddOpts` — no condition/change. -/
def defaultGeoAddOpts : GeoAddOpts :=
  ⟨none, false⟩

private def geoSearchFromArgs : GeoSearchFrom → List ByteArray
  | .fromMember member => ["FROMMEMBER".toUTF8, member]
  | .fromLonLat lon lat => ["FROMLONLAT".toUTF8, encode lon, encode lat]

private def geoSearchByArgs : GeoSearchBy → List ByteArray
  | .byRadius radius unit => ["BYRADIUS".toUTF8, encode radius, encode unit]
  | .byBox width height unit => ["BYBOX".toUTF8, encode width, encode height, encode unit]

private def geoSearchOptsArgs (o : GeoSearchOpts) : List ByteArray :=
  let orderArg := match o.geoSearchOrder with | some ord => [encode ord] | none => []
  let countArg := match o.geoSearchCount with
    | some c => ["COUNT".toUTF8, encode c] ++ flag o.geoSearchCountAny "ANY".toUTF8
    | none => []
  orderArg ++ countArg ++ flag o.geoSearchWithCoord "WITHCOORD".toUTF8
  ++ flag o.geoSearchWithDist "WITHDIST".toUTF8 ++ flag o.geoSearchWithHash "WITHHASH".toUTF8

private def geoSearchStoreOptsArgs (o : GeoSearchStoreOpts) : List ByteArray :=
  let orderArg := match o.geoSearchStoreOrder with | some ord => [encode ord] | none => []
  let countArg := match o.geoSearchStoreCount with
    | some c => ["COUNT".toUTF8, encode c] ++ flag o.geoSearchStoreCountAny "ANY".toUTF8
    | none => []
  orderArg ++ countArg ++ flag o.geoSearchStoreStoredist "STOREDIST".toUTF8

-- ── Geo: commands ──

/-- Add members to a geospatial index, with options (`GEOADD`). -/
def geoaddOpts (key : ByteArray) (values : List (Float × Float × ByteArray))
    (opts : GeoAddOpts) : m (f Int) :=
  let conditionArg := match opts.geoAddCondition with | some c => [encode c] | none => []
  let valueArgs := values.flatMap (fun (lon, lat, member) => [encode lon, encode lat, member])
  sendRequest (["GEOADD".toUTF8, key] ++ conditionArg ++ flag opts.geoAddChange "CH".toUTF8
    ++ valueArgs)

/-- Add members to a geospatial index (`GEOADD`). -/
def geoadd (key : ByteArray) (values : List (Float × Float × ByteArray)) : m (f Int) :=
  geoaddOpts key values defaultGeoAddOpts

/-- Distance between two members of a geospatial index (`GEODIST`). -/
def geodist (key member1 member2 : ByteArray) (munit : Option GeoUnit) :
    m (f (Option Float)) :=
  sendRequest (["GEODIST".toUTF8, key, member1, member2]
    ++ (match munit with | some u => [encode u] | none => []))

/-- Longitude/latitude of members of a geospatial index (`GEOPOS`). -/
def geopos (key : ByteArray) (members : List ByteArray) :
    m (f (List (Option GeoCoordinates))) :=
  sendRequest (["GEOPOS".toUTF8, key] ++ members)

/-- Query a geospatial index for members within an area (`GEOSEARCH`). -/
def geoSearch (key : ByteArray) (fromSpec : GeoSearchFrom) (bySpec : GeoSearchBy)
    (opts : GeoSearchOpts) : m (f (List GeoLocation)) :=
  sendRequest (["GEOSEARCH".toUTF8, key] ++ geoSearchFromArgs fromSpec ++ geoSearchByArgs bySpec
    ++ geoSearchOptsArgs opts)

/-- Query a geospatial index and store the result (`GEOSEARCHSTORE`). -/
def geoSearchStore (destination source : ByteArray) (fromSpec : GeoSearchFrom)
    (bySpec : GeoSearchBy) (opts : GeoSearchStoreOpts) : m (f Int) :=
  sendRequest (["GEOSEARCHSTORE".toUTF8, destination, source] ++ geoSearchFromArgs fromSpec
    ++ geoSearchByArgs bySpec ++ geoSearchStoreOptsArgs opts)

-- ── XINFO CONSUMERS ──

/-- Information about one consumer in a group (`XINFO CONSUMERS`). -/
structure XInfoConsumersResponse where
  /-- Consumer name. -/
  xinfoConsumerName : ByteArray
  /-- Number of pending messages for the consumer. -/
  xinfoConsumerNumPendingMessages : Int
  /-- Milliseconds since the consumer's last attempted interaction. -/
  xinfoConsumerIdleTime : Int
  /-- Milliseconds since the last successful interaction (`none` before
      Redis 7.0). -/
  xinfoConsumerInactive : Option Int
deriving BEq, Inhabited

private def decodeXInfoConsumers6 (r : Reply) : Except Reply XInfoConsumersResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .bulk (some name), .bulk (some k1), .integer pending,
      .bulk (some k2), .integer idle]) =>
    if k0 == "name".toUTF8 && k1 == "pending".toUTF8 && k2 == "idle".toUTF8 then
      Except.ok ⟨name, pending, idle, none⟩
    else Except.error r
  | _ => Except.error r

private def decodeXInfoConsumers7 (r : Reply) : Except Reply XInfoConsumersResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .bulk (some name), .bulk (some k1), .integer pending,
      .bulk (some k2), .integer idle, .bulk (some k3), .integer inactive]) =>
    if k0 == "name".toUTF8 && k1 == "pending".toUTF8 && k2 == "idle".toUTF8
        && k3 == "inactive".toUTF8 then
      Except.ok ⟨name, pending, idle, some inactive⟩
    else Except.error r
  | _ => Except.error r

instance : RedisResult XInfoConsumersResponse where
  decode r := decodeFirst r [decodeXInfoConsumers6, decodeXInfoConsumers7]

/-- Information about the consumers of a group (`XINFO CONSUMERS`). -/
def xinfoConsumers (stream group : ByteArray) : m (f (List XInfoConsumersResponse)) :=
  sendRequest ["XINFO".toUTF8, "CONSUMERS".toUTF8, stream, group]

-- ── XINFO GROUPS ──

/-- Information about one consumer group (`XINFO GROUPS`). -/
structure XInfoGroupsResponse where
  /-- Group name. -/
  xinfoGroupsGroupName : ByteArray
  /-- Number of consumers in the group. -/
  xinfoGroupsNumConsumers : Int
  /-- Length of the group's pending entries list. -/
  xinfoGroupsNumPendingMessages : Int
  /-- ID of the last entry delivered to the group. -/
  xinfoGroupsLastDeliveredMessageId : ByteArray
  /-- Read counter of the last delivered entry (`none` before Redis 7.0). -/
  xinfoGroupsEntriesRead : Option Int
  /-- Number of undelivered entries (`none` before Redis 7.0 or when
      undeterminable). -/
  xinfoGroupsLag : Option Int
deriving BEq, Inhabited

private def decodeXInfoGroups6 (r : Reply) : Except Reply XInfoGroupsResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .bulk (some name), .bulk (some k1), .integer consumers,
      .bulk (some k2), .integer pending, .bulk (some k3), .bulk (some lastId)]) =>
    if k0 == "name".toUTF8 && k1 == "consumers".toUTF8 && k2 == "pending".toUTF8
        && k3 == "last-delivered-id".toUTF8 then
      Except.ok ⟨name, consumers, pending, lastId, none, none⟩
    else Except.error r
  | _ => Except.error r

private def decodeXInfoGroups7 (r : Reply) : Except Reply XInfoGroupsResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .bulk (some name), .bulk (some k1), .integer consumers,
      .bulk (some k2), .integer pending, .bulk (some k3), .bulk (some lastId),
      .bulk (some k4), .integer entriesRead, .bulk (some k5), .integer lag]) =>
    if k0 == "name".toUTF8 && k1 == "consumers".toUTF8 && k2 == "pending".toUTF8
        && k3 == "last-delivered-id".toUTF8 && k4 == "entries-read".toUTF8
        && k5 == "lag".toUTF8 then
      Except.ok ⟨name, consumers, pending, lastId, some entriesRead, some lag⟩
    else Except.error r
  | _ => Except.error r

instance : RedisResult XInfoGroupsResponse where
  decode r := decodeFirst r [decodeXInfoGroups6, decodeXInfoGroups7]

/-- Information about the consumer groups of a stream (`XINFO GROUPS`). -/
def xinfoGroups (stream : ByteArray) : m (f (List XInfoGroupsResponse)) :=
  sendRequest ["XINFO".toUTF8, "GROUPS".toUTF8, stream]

-- ── XINFO STREAM ──

/-- Information about a stream (`XINFO STREAM`), with a separate constructor
    for an empty stream (no first/last entry), mirroring upstream's two record
    constructors. The `Since Redis 7.0` fields are `none` on earlier versions. -/
inductive XInfoStreamResponse where
  /-- A non-empty stream. -/
  | stream
      (xinfoStreamLength xinfoStreamRadixTreeKeys xinfoStreamRadixTreeNodes : Int)
      (xinfoMaxDeletedEntryId : Option ByteArray)
      (xinfoEntriesAdded : Option Int)
      (xinfoRecordedFirstEntryId : Option ByteArray)
      (xinfoStreamNumGroups : Int)
      (xinfoStreamLastEntryId : ByteArray)
      (xinfoStreamFirstEntry xinfoStreamLastEntry : StreamsRecord)
  /-- An empty stream (no first/last entry). -/
  | empty
      (xinfoStreamLength xinfoStreamRadixTreeKeys xinfoStreamRadixTreeNodes : Int)
      (xinfoMaxDeletedEntryId : Option ByteArray)
      (xinfoEntriesAdded : Option Int)
      (xinfoRecordedFirstEntryId : Option ByteArray)
      (xinfoStreamNumGroups : Int)
      (xinfoStreamLastEntryId : ByteArray)
deriving BEq, Inhabited

private def decodeXInfoStream5e (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .integer groups,
      .bulk (some k4), .bulk (some lastId), .bulk (some k5), .bulk none,
      .bulk (some k6), .bulk none]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "groups".toUTF8 && k4 == "last-generated-id".toUTF8
        && k5 == "first-entry".toUTF8 && k6 == "last-entry".toUTF8 then
      Except.ok (.empty len rtk rtn none none none groups lastId)
    else Except.error r
  | _ => Except.error r

private def decodeXInfoStream5f (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .integer groups,
      .bulk (some k4), .bulk (some lastId), .bulk (some k5), rawFirst,
      .bulk (some k6), rawLast]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "groups".toUTF8 && k4 == "last-generated-id".toUTF8
        && k5 == "first-entry".toUTF8 && k6 == "last-entry".toUTF8 then
      (do pure (.stream len rtk rtn none none none groups lastId (← decode rawFirst) (← decode rawLast)))
    else Except.error r
  | _ => Except.error r

private def decodeXInfoStream6e (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .bulk (some lastId),
      .bulk (some k4), .integer groups, .bulk (some k5), .bulk none,
      .bulk (some k6), .bulk none]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "last-generated-id".toUTF8 && k4 == "groups".toUTF8
        && k5 == "first-entry".toUTF8 && k6 == "last-entry".toUTF8 then
      Except.ok (.empty len rtk rtn none none none groups lastId)
    else Except.error r
  | _ => Except.error r

private def decodeXInfoStream6f (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .bulk (some lastId),
      .bulk (some k4), .integer groups, .bulk (some k5), rawFirst,
      .bulk (some k6), rawLast]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "last-generated-id".toUTF8 && k4 == "groups".toUTF8
        && k5 == "first-entry".toUTF8 && k6 == "last-entry".toUTF8 then
      (do pure (.stream len rtk rtn none none none groups lastId (← decode rawFirst) (← decode rawLast)))
    else Except.error r
  | _ => Except.error r

private def decodeXInfoStream7e (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .bulk (some lastId),
      .bulk (some k4), .bulk (some maxDel), .bulk (some k5), .integer added,
      .bulk (some k6), .bulk (some recFirst), .bulk (some k7), .integer groups,
      .bulk (some k8), .bulk none, .bulk (some k9), .bulk none]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "last-generated-id".toUTF8 && k4 == "max-deleted-entry-id".toUTF8
        && k5 == "entries-added".toUTF8 && k6 == "recorded-first-entry-id".toUTF8
        && k7 == "groups".toUTF8 && k8 == "first-entry".toUTF8 && k9 == "last-entry".toUTF8 then
      Except.ok (.empty len rtk rtn (some maxDel) (some added) (some recFirst) groups lastId)
    else Except.error r
  | _ => Except.error r

private def decodeXInfoStream7f (r : Reply) : Except Reply XInfoStreamResponse :=
  match r with
  | .multiBulk (some [.bulk (some k0), .integer len, .bulk (some k1), .integer rtk,
      .bulk (some k2), .integer rtn, .bulk (some k3), .bulk (some lastId),
      .bulk (some k4), .bulk (some maxDel), .bulk (some k5), .integer added,
      .bulk (some k6), .bulk (some recFirst), .bulk (some k7), .integer groups,
      .bulk (some k8), rawFirst, .bulk (some k9), rawLast]) =>
    if k0 == "length".toUTF8 && k1 == "radix-tree-keys".toUTF8 && k2 == "radix-tree-nodes".toUTF8
        && k3 == "last-generated-id".toUTF8 && k4 == "max-deleted-entry-id".toUTF8
        && k5 == "entries-added".toUTF8 && k6 == "recorded-first-entry-id".toUTF8
        && k7 == "groups".toUTF8 && k8 == "first-entry".toUTF8 && k9 == "last-entry".toUTF8 then
      (do pure (.stream len rtk rtn (some maxDel) (some added) (some recFirst) groups lastId
        (← decode rawFirst) (← decode rawLast)))
    else Except.error r
  | _ => Except.error r

instance : RedisResult XInfoStreamResponse where
  decode r := decodeFirst r [decodeXInfoStream5e, decodeXInfoStream5f, decodeXInfoStream6e,
    decodeXInfoStream6f, decodeXInfoStream7e, decodeXInfoStream7f]

/-- Get information about a stream (`XINFO STREAM`). -/
def xinfoStream (stream : ByteArray) : m (f XInfoStreamResponse) :=
  sendRequest ["XINFO".toUTF8, "STREAM".toUTF8, stream]

-- ── AUTH / SELECT / PING ──

/-- Options for the `AUTH` command. -/
structure AuthOpts where
  /-- Username (`none` uses the default user; requires Redis 6.0+). -/
  authOptsUsername : Option ByteArray
deriving Inhabited

/-- Default `AuthOpts` — no username. -/
def defaultAuthOpts : AuthOpts :=
  ⟨none⟩

/-- Authenticate to the server, with options (`AUTH`). -/
def authOpts (password : ByteArray) (opts : AuthOpts) : m (f Status) :=
  sendRequest (["AUTH".toUTF8]
    ++ (match opts.authOptsUsername with | some u => [u] | none => []) ++ [password])

/-- Authenticate to the server (`AUTH`). -/
def auth (password : ByteArray) : m (f Status) :=
  authOpts password defaultAuthOpts

/-- Select the database for the current connection (`SELECT`). -/
def select (ix : Int) : m (f Status) :=
  sendRequest ["SELECT".toUTF8, encode ix]

/-- Ping the server (`PING`). -/
def ping : m (f Status) :=
  sendRequest ["PING".toUTF8]

-- ── EXPIRE with options ──

/-- Condition flag for the `EXPIRE`/`EXPIREAT`/`PEXPIREAT`-with-options
    commands (`NX`/`XX` on the timestamp, or `GT`/`LT` on the value). -/
inductive ExpireOpts where
  | time (c : Condition)
  | value (c : SizeCondition)

instance : RedisArg ExpireOpts where
  encode
    | .time c => encode c
    | .value c => encode c

/-- Set a key's expiration as a millisecond Unix timestamp, with a condition
    (`PEXPIREAT`). -/
def pexpireatOpts (key : ByteArray) (millisecondsTimestamp : Int) (opts : ExpireOpts) :
    m (f Bool) :=
  sendRequest ["PEXPIREAT".toUTF8, key, encode millisecondsTimestamp, encode opts]

/-- Set a key's TTL in seconds, with a condition (`EXPIRE`). -/
def expireOpts (key : ByteArray) (seconds : Int) (opts : ExpireOpts) : m (f Bool) :=
  sendRequest ["EXPIRE".toUTF8, key, encode seconds, encode opts]

/-- Set a key's expiration as a Unix timestamp in seconds, with a condition
    (`EXPIREAT`). -/
def expireatOpts (key : ByteArray) (timestamp : Int) (opts : ExpireOpts) : m (f Bool) :=
  sendRequest ["EXPIREAT".toUTF8, key, encode timestamp, encode opts]

-- ── FLUSH with options ──

/-- Flush mode for `FLUSHDB`/`FLUSHALL` (`SYNC`/`ASYNC`). -/
inductive FlushOpts where
  | sync
  | async

instance : RedisArg FlushOpts where
  encode
    | .sync => "SYNC".toUTF8
    | .async => "ASYNC".toUTF8

/-- Remove all keys from the current database, with a mode (`FLUSHDB`). -/
def flushdbOpts (opts : FlushOpts) : m (f Status) :=
  sendRequest ["FLUSHDB".toUTF8, encode opts]

/-- Remove all keys from all databases, with a mode (`FLUSHALL`). -/
def flushallOpts (opts : FlushOpts) : m (f Status) :=
  sendRequest ["FLUSHALL".toUTF8, encode opts]

-- ── BITPOS with options ──

/-- Range unit for `BITPOS` (`BYTE`/`BIT`). -/
inductive BitposType where
  | byte
  | bit

instance : RedisArg BitposType where
  encode
    | .byte => "BYTE".toUTF8
    | .bit => "BIT".toUTF8

/-- Range specification for `BITPOS`. -/
inductive BitposOpts where
  /-- Only a start index. -/
  | start (s : Int)
  /-- A start and end index, with an optional range unit. -/
  | startEnd (start end_ : Int) (bits : Option BitposType)

/-- Find the first bit set to `bit`, with a range (`BITPOS`). -/
def bitposOpts (key_ : ByteArray) (bit : Int) (opts : BitposOpts) : m (f Int) :=
  let rest := match opts with
    | .start s => [encode s]
    | .startEnd start end_ bits =>
      [encode start, encode end_] ++ (match bits with | some b => [encode b] | none => [])
  sendRequest ("BITPOS".toUTF8 :: key_ :: encode bit :: rest)

-- ── ByteArray text helpers (the `Data.ByteString.Char8` substitution) ──

/-- Convert a byte list to a `ByteArray`. -/
private def toBA (l : List UInt8) : ByteArray :=
  ⟨l.toArray⟩

/-- Longest prefix of `l` whose bytes satisfy `p` (upstream's
    `Char8.takeWhile`). -/
private def bsTakeWhile (p : UInt8 → Bool) : List UInt8 → List UInt8
  | [] => []
  | b :: rest => if p b then b :: bsTakeWhile p rest else []

/-- Split `l` on every occurrence of `sep`, keeping empty segments
    (upstream's `Char8.split`). -/
private def bsSplitAux (sep : UInt8) : List UInt8 → List UInt8 → List (List UInt8)
  | [], acc => [acc.reverse]
  | b :: rest, acc =>
    if b == sep then acc.reverse :: bsSplitAux sep rest [] else bsSplitAux sep rest (b :: acc)

/-- Split `l` on every occurrence of `sep` (upstream's `Char8.split`). -/
private def bsSplit (sep : UInt8) (l : List UInt8) : List (List UInt8) :=
  bsSplitAux sep l []

/-- Split into lines on `'\n'`, with no trailing empty line for a final
    newline (upstream's `Char8.lines`). -/
private def bsLinesAux : List UInt8 → List UInt8 → List (List UInt8)
  | [], [] => []
  | [], acc => [acc.reverse]
  | b :: rest, acc =>
    if b == '\n'.toUInt8 then acc.reverse :: bsLinesAux rest [] else bsLinesAux rest (b :: acc)

/-- Split into lines (upstream's `Char8.lines`). -/
private def bsLines (l : List UInt8) : List (List UInt8) :=
  bsLinesAux l []

/-- True if `b` is an ASCII whitespace byte. -/
private def isSpaceByte (b : UInt8) : Bool :=
  b == 32 || b == 9 || b == 10 || b == 13 || b == 11 || b == 12

/-- Split into whitespace-separated words, dropping empty tokens (upstream's
    `Char8.words`). -/
private def bsWordsAux : List UInt8 → List UInt8 → List (List UInt8)
  | [], [] => []
  | [], acc => [acc.reverse]
  | b :: rest, acc =>
    if isSpaceByte b then
      (if acc.isEmpty then bsWordsAux rest [] else acc.reverse :: bsWordsAux rest [])
    else bsWordsAux rest (b :: acc)

/-- Split into words (upstream's `Char8.words`). -/
private def bsWords (l : List UInt8) : List (List UInt8) :=
  bsWordsAux l []

/-- Read a (possibly signed) integer prefix of `ba`, as upstream's
    `Char8.readInteger` (ignores trailing bytes). -/
private def readIntBytes (ba : ByteArray) : Option Int :=
  readSignedDecimal ba

/-- True if `needle` is a prefix of `l`. -/
private def bsIsPrefix : List UInt8 → List UInt8 → Bool
  | [], _ => true
  | _ :: _, [] => false
  | a :: as, b :: bs => a == b && bsIsPrefix as bs

/-- Find the first occurrence of `needle`, returning the bytes before it and
    the bytes from it onward; `(hay, [])` if absent (upstream's
    `BS.breakSubstring`). -/
private def bsBreakSubstringAux (needle : List UInt8) :
    List UInt8 → List UInt8 → (List UInt8 × List UInt8)
  | before, [] => (before.reverse, [])
  | before, b :: rest =>
    if bsIsPrefix needle (b :: rest) then (before.reverse, b :: rest)
    else bsBreakSubstringAux needle (b :: before) rest

/-- Break `hay` at the first occurrence of `needle` (upstream's
    `BS.breakSubstring`). -/
private def bsBreakSubstring (needle hay : List UInt8) : (List UInt8 × List UInt8) :=
  bsBreakSubstringAux needle [] hay

-- ── CLUSTER INFO ──

/-- Cluster state reported by `CLUSTER INFO`. -/
inductive ClusterInfoResponseState where
  | ok
  | down
deriving BEq, Inhabited

/-- The parsed reply of `CLUSTER INFO`. Field names drop upstream's redundant
    `clusterInfoResponse` prefix (Lean namespaces them under the structure).
    Fields absent before Redis 7 are `Option`. -/
structure ClusterInfoResponse where
  /-- `cluster_state`. -/
  state : ClusterInfoResponseState
  /-- `cluster_slots_assigned`. -/
  slotsAssigned : Int
  /-- `cluster_slots_ok`. -/
  slotsOK : Int
  /-- `cluster_slots_pfail`. -/
  slotsPfail : Int
  /-- `cluster_slots_fail`. -/
  slotsFail : Int
  /-- `cluster_known_nodes`. -/
  knownNodes : Int
  /-- `cluster_size`. -/
  size : Int
  /-- `cluster_current_epoch`. -/
  currentEpoch : Int
  /-- `cluster_my_epoch`. -/
  myEpoch : Int
  /-- `cluster_stats_messages_sent`. -/
  statsMessagesSent : Int
  /-- `cluster_stats_messages_received`. -/
  statsMessagesReceived : Int
  /-- `total_cluster_links_buffer_limit_exceeded`. -/
  totalLinksBufferLimitExceeded : Int
  /-- `cluster_stats_messages_ping_sent`. -/
  statsMessagesPingSent : Option Int
  /-- `cluster_stats_messages_ping_received`. -/
  statsMessagesPingReceived : Option Int
  /-- `cluster_stats_messages_pong_sent`. -/
  statsMessagesPongSent : Option Int
  /-- `cluster_stats_messages_pong_received`. -/
  statsMessagesPongReceived : Option Int
  /-- `cluster_stats_messages_meet_sent`. -/
  statsMessagesMeetSent : Option Int
  /-- `cluster_stats_messages_meet_received`. -/
  statsMessagesMeetReceived : Option Int
  /-- `cluster_stats_messages_fail_sent`. -/
  statsMessagesFailSent : Option Int
  /-- `cluster_stats_messages_fail_received`. -/
  statsMessagesFailReceived : Option Int
  /-- `cluster_stats_messages_publish_sent`. -/
  statsMessagesPublishSent : Option Int
  /-- `cluster_stats_messages_publish_received`. -/
  statsMessagesPublishReceived : Option Int
  /-- `cluster_stats_messages_auth_req_sent`. -/
  statsMessagesAuthReqSent : Option Int
  /-- `cluster_stats_messages_auth_req_received`. -/
  statsMessagesAuthReqReceived : Option Int
  /-- `cluster_stats_messages_auth_ack_sent`. -/
  statsMessagesAuthAckSent : Option Int
  /-- `cluster_stats_messages_auth_ack_received`. -/
  statsMessagesAuthAckReceived : Option Int
  /-- `cluster_stats_messages_update_sent`. -/
  statsMessagesUpdateSent : Option Int
  /-- `cluster_stats_messages_update_received`. -/
  statsMessagesUpdateReceived : Option Int
  /-- `cluster_stats_messages_mfstart_sent`. -/
  statsMessagesMfstartSent : Option Int
  /-- `cluster_stats_messages_mfstart_received`. -/
  statsMessagesMfstartReceived : Option Int
  /-- `cluster_stats_messages_module_sent`. -/
  statsMessagesModuleSent : Option Int
  /-- `cluster_stats_messages_module_received`. -/
  statsMessagesModuleReceived : Option Int
  /-- `cluster_stats_messages_publishshard_sent`. -/
  statsMessagesPublishshardSent : Option Int
  /-- `cluster_stats_messages_publishshard_received`. -/
  statsMessagesPublishshardReceived : Option Int
deriving BEq, Inhabited

/-- The all-defaults `ClusterInfoResponse` (upstream's `defClusterInfoResponse`). -/
def defClusterInfoResponse : ClusterInfoResponse :=
  { state := .down, slotsAssigned := 0, slotsOK := 0, slotsPfail := 0, slotsFail := 0,
    knownNodes := 0, size := 0, currentEpoch := 0, myEpoch := 0, statsMessagesSent := 0,
    statsMessagesReceived := 0, totalLinksBufferLimitExceeded := 0,
    statsMessagesPingSent := none, statsMessagesPingReceived := none,
    statsMessagesPongSent := none, statsMessagesPongReceived := none,
    statsMessagesMeetSent := none, statsMessagesMeetReceived := none,
    statsMessagesFailSent := none, statsMessagesFailReceived := none,
    statsMessagesPublishSent := none, statsMessagesPublishReceived := none,
    statsMessagesAuthReqSent := none, statsMessagesAuthReqReceived := none,
    statsMessagesAuthAckSent := none, statsMessagesAuthAckReceived := none,
    statsMessagesUpdateSent := none, statsMessagesUpdateReceived := none,
    statsMessagesMfstartSent := none, statsMessagesMfstartReceived := none,
    statsMessagesModuleSent := none, statsMessagesModuleReceived := none,
    statsMessagesPublishshardSent := none, statsMessagesPublishshardReceived := none }

private def parseClusterState (ba : ByteArray) : Option ClusterInfoResponseState :=
  if ba == "ok".toUTF8 then some .ok
  else if ba == "fail".toUTF8 then some .down
  else none

/-- Fold `CLUSTER INFO`'s `key:value` lines into a `ClusterInfoResponse`
    (upstream's `parseClusterInfoResponse`). Unknown keys are ignored; a
    mandatory field failing to parse aborts with `none`. -/
private def parseClusterInfoResponse :
    List (List ByteArray) → ClusterInfoResponse → Option ClusterInfoResponse
  | [], resp => some resp
  | entry :: fs, resp =>
    match entry with
    | [k, v] =>
      if k == "cluster_state".toUTF8 then
        (parseClusterState v).bind (fun s => parseClusterInfoResponse fs { resp with state := s })
      else if k == "cluster_slots_assigned".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with slotsAssigned := n })
      else if k == "cluster_slots_ok".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with slotsOK := n })
      else if k == "cluster_slots_pfail".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with slotsPfail := n })
      else if k == "cluster_slots_fail".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with slotsFail := n })
      else if k == "cluster_known_nodes".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with knownNodes := n })
      else if k == "cluster_size".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with size := n })
      else if k == "cluster_current_epoch".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with currentEpoch := n })
      else if k == "cluster_my_epoch".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with myEpoch := n })
      else if k == "cluster_stats_messages_sent".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with statsMessagesSent := n })
      else if k == "cluster_stats_messages_received".toUTF8 then
        (readIntBytes v).bind (fun n => parseClusterInfoResponse fs { resp with statsMessagesReceived := n })
      else if k == "total_cluster_links_buffer_limit_exceeded".toUTF8 then
        parseClusterInfoResponse fs { resp with totalLinksBufferLimitExceeded := (readIntBytes v).getD 0 }
      else if k == "cluster_stats_messages_ping_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPingSent := readIntBytes v }
      else if k == "cluster_stats_messages_ping_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPingReceived := readIntBytes v }
      else if k == "cluster_stats_messages_pong_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPongSent := readIntBytes v }
      else if k == "cluster_stats_messages_pong_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPongReceived := readIntBytes v }
      else if k == "cluster_stats_messages_meet_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesMeetSent := readIntBytes v }
      else if k == "cluster_stats_messages_meet_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesMeetReceived := readIntBytes v }
      else if k == "cluster_stats_messages_fail_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesFailSent := readIntBytes v }
      else if k == "cluster_stats_messages_fail_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesFailReceived := readIntBytes v }
      else if k == "cluster_stats_messages_publish_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPublishSent := readIntBytes v }
      else if k == "cluster_stats_messages_publish_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPublishReceived := readIntBytes v }
      else if k == "cluster_stats_messages_auth_req_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesAuthReqSent := readIntBytes v }
      else if k == "cluster_stats_messages_auth_req_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesAuthReqReceived := readIntBytes v }
      else if k == "cluster_stats_messages_auth_ack_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesAuthAckSent := readIntBytes v }
      else if k == "cluster_stats_messages_auth_ack_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesAuthAckReceived := readIntBytes v }
      else if k == "cluster_stats_messages_update_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesUpdateSent := readIntBytes v }
      else if k == "cluster_stats_messages_update_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesUpdateReceived := readIntBytes v }
      else if k == "cluster_stats_messages_mfstart_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesMfstartSent := readIntBytes v }
      else if k == "cluster_stats_messages_mfstart_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesMfstartReceived := readIntBytes v }
      else if k == "cluster_stats_messages_module_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesModuleSent := readIntBytes v }
      else if k == "cluster_stats_messages_module_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesModuleReceived := readIntBytes v }
      else if k == "cluster_stats_messages_publishshard_sent".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPublishshardSent := readIntBytes v }
      else if k == "cluster_stats_messages_publishshard_received".toUTF8 then
        parseClusterInfoResponse fs { resp with statsMessagesPublishshardReceived := readIntBytes v }
      else parseClusterInfoResponse fs resp
    | _ => parseClusterInfoResponse fs resp

instance : RedisResult ClusterInfoResponse where
  decode
    | r@(.bulk (some bulkData)) =>
      let fields := (bsLines bulkData.toList).map
        (fun line => (bsSplit ':'.toUInt8 (bsTakeWhile (· != '\r'.toUInt8) line)).map toBA)
      match parseClusterInfoResponse fields defClusterInfoResponse with
      | some resp => Except.ok resp
      | none => Except.error r
    | r => Except.error r

/-- Provide info about the cluster (`CLUSTER INFO`). -/
def clusterInfo : m (f ClusterInfoResponse) :=
  sendRequest ["CLUSTER".toUTF8, "INFO".toUTF8]

-- ── CLUSTER NODES ──

/-- A slot specification in a `CLUSTER NODES` entry. -/
inductive ClusterNodesResponseSlotSpec where
  | singleSlot (slot : Int)
  | slotRange (start end_ : Int)
  | slotImporting (slot : Int) (node : ByteArray)
  | slotMigrating (slot : Int) (node : ByteArray)
deriving BEq, Inhabited

/-- One node entry from `CLUSTER NODES`. -/
structure ClusterNodesResponseEntry where
  /-- Node ID. -/
  nodeId : ByteArray
  /-- Node IP/hostname. -/
  nodeIp : ByteArray
  /-- Node port. -/
  nodePort : Int
  /-- Node flags. -/
  nodeFlags : List ByteArray
  /-- Master node ID (`none` for a master). -/
  masterId : Option ByteArray
  /-- Last ping sent (ms). -/
  pingSent : Int
  /-- Last pong received (ms). -/
  pongReceived : Int
  /-- Config epoch. -/
  configEpoch : Int
  /-- Link state. -/
  linkState : ByteArray
  /-- Assigned slots. -/
  slots : List ClusterNodesResponseSlotSpec
deriving BEq, Inhabited

/-- The parsed reply of `CLUSTER NODES`. -/
structure ClusterNodesResponse where
  /-- The node entries. -/
  entries : List ClusterNodesResponseEntry
deriving BEq, Inhabited

private def readSlotRange (slotSpec : List UInt8) : Option ClusterNodesResponseSlotSpec :=
  match bsBreakSubstring "-".toUTF8.toList slotSpec with
  | (_, []) => none
  | (leftPart, rightPart) =>
    match readIntBytes (toBA leftPart), readIntBytes (toBA (rightPart.drop 1)) with
    | some a, some b => some (.slotRange a b)
    | _, _ => none

private def readSlotImportMigrate (slotSpec : List UInt8) : Option ClusterNodesResponseSlotSpec :=
  match bsBreakSubstring "->-".toUTF8.toList slotSpec with
  | (_, []) =>
    match bsBreakSubstring "-<-".toUTF8.toList slotSpec with
    | (_, []) => none
    | (leftPart, rightPart) =>
      (readIntBytes (toBA (leftPart.drop 1))).map (fun slot =>
        .slotImporting slot (toBA (rightPart.take (rightPart.length - 1))))
  | (leftPart, rightPart) =>
    (readIntBytes (toBA (leftPart.drop 1))).map (fun slot =>
      .slotMigrating slot (toBA (rightPart.take (rightPart.length - 1))))

private def readNodeSlot (slotSpec : List UInt8) : Option ClusterNodesResponseSlotSpec :=
  if slotSpec.contains '['.toUInt8 then readSlotImportMigrate slotSpec
  else if slotSpec.contains '-'.toUInt8 then readSlotRange slotSpec
  else (readIntBytes (toBA slotSpec)).map .singleSlot

private def parseNodeInfo (line : List UInt8) : Option ClusterNodesResponseEntry :=
  match bsWords line with
  | nodeId :: hostNamePort :: flags :: masterNodeId :: pingSent :: pongRecv :: epoch
      :: linkState :: slots =>
    match bsSplit ':'.toUInt8 hostNamePort with
    | [hostName, port] =>
      match readIntBytes (toBA port), readIntBytes (toBA pingSent),
          readIntBytes (toBA pongRecv), readIntBytes (toBA epoch) with
      | some p, some ps, some pr, some ep =>
        some
          { nodeId := toBA nodeId, nodeIp := toBA hostName, nodePort := p,
            nodeFlags := (bsSplit ','.toUInt8 flags).map toBA,
            masterId := if toBA masterNodeId == "-".toUTF8 then none else some (toBA masterNodeId),
            pingSent := ps, pongReceived := pr, configEpoch := ep, linkState := toBA linkState,
            slots := (slots.map readNodeSlot).filterMap id }
      | _, _, _, _ => none
    | _ => none
  | _ => none

instance : RedisResult ClusterNodesResponse where
  decode
    | r@(.bulk (some bulkData)) =>
      match (bsLines bulkData.toList).mapM parseNodeInfo with
      | some infos => Except.ok ⟨infos⟩
      | none => Except.error r
    | r => Except.error r

/-- List the cluster's nodes (`CLUSTER NODES`). -/
def clusterNodes : m (f ClusterNodesResponse) :=
  sendRequest ["CLUSTER".toUTF8, "NODES".toUTF8]

-- ── CLUSTER SLOTS ──

/-- A node in a `CLUSTER SLOTS` entry. -/
structure ClusterSlotsNode where
  /-- Node IP. -/
  nodeIP : ByteArray
  /-- Node port. -/
  nodePort : Int
  /-- Node ID. -/
  nodeID : ByteArray
deriving BEq, Inhabited

instance : RedisResult ClusterSlotsNode where
  decode
    | .multiBulk (some (.bulk (some ip) :: .integer port :: .bulk (some nid) :: _)) =>
      Except.ok ⟨ip, port, nid⟩
    | a => Except.error a

/-- One slot-range entry from `CLUSTER SLOTS`. -/
structure ClusterSlotsResponseEntry where
  /-- First slot in the range. -/
  startSlot : Int
  /-- Last slot in the range. -/
  endSlot : Int
  /-- Master node serving the range. -/
  master : ClusterSlotsNode
  /-- Replica nodes for the range. -/
  replicas : List ClusterSlotsNode
deriving BEq, Inhabited

instance : RedisResult ClusterSlotsResponseEntry where
  decode
    | .multiBulk (some (.integer startSlot :: .integer endSlot :: masterData :: replicas)) =>
      (do pure ⟨startSlot, endSlot, ← decode masterData, ← replicas.mapM decode⟩)
    | a => Except.error a

/-- The parsed reply of `CLUSTER SLOTS`. -/
structure ClusterSlotsResponse where
  /-- The slot-range entries. -/
  entries : List ClusterSlotsResponseEntry
deriving BEq, Inhabited

instance : RedisResult ClusterSlotsResponse where
  decode
    | .multiBulk (some bulkData) => (do pure ⟨← bulkData.mapM decode⟩)
    | a => Except.error a

/-- Get the mapping of cluster slots to nodes (`CLUSTER SLOTS`). -/
def clusterSlots : m (f ClusterSlotsResponse) :=
  sendRequest ["CLUSTER".toUTF8, "SLOTS".toUTF8]

-- ── CLUSTER SETSLOT / GETKEYSINSLOT / COMMAND ──

/-- Mark a slot as importing from a node (`CLUSTER SETSLOT … IMPORTING`). -/
def clusterSetSlotImporting (slot : Int) (sourceNodeId : ByteArray) : m (f Status) :=
  sendRequest ["CLUSTER".toUTF8, "SETSLOT".toUTF8, encode slot, "IMPORTING".toUTF8, sourceNodeId]

/-- Mark a slot as migrating to a node (`CLUSTER SETSLOT … MIGRATING`). -/
def clusterSetSlotMigrating (slot : Int) (destinationNodeId : ByteArray) : m (f Status) :=
  sendRequest ["CLUSTER".toUTF8, "SETSLOT".toUTF8, encode slot, "MIGRATING".toUTF8, destinationNodeId]

/-- Clear any importing/migrating state on a slot (`CLUSTER SETSLOT STABLE`). -/
def clusterSetSlotStable (slot : Int) : m (f Status) :=
  sendRequest ["CLUSTER".toUTF8, "SETSLOT".toUTF8, "STABLE".toUTF8, encode slot]

/-- Bind a slot to a node (`CLUSTER SETSLOT … NODE`). -/
def clusterSetSlotNode (slot : Int) (node : ByteArray) : m (f Status) :=
  sendRequest ["CLUSTER".toUTF8, "SETSLOT".toUTF8, encode slot, "NODE".toUTF8, node]

/-- List keys in a slot (`CLUSTER GETKEYSINSLOT`). -/
def clusterGetKeysInSlot (slot count : Int) : m (f (List ByteArray)) :=
  sendRequest ["CLUSTER".toUTF8, "GETKEYSINSLOT".toUTF8, encode slot, encode count]

/-- Get details of all Redis commands (`COMMAND`). -/
def command : m (f (List CommandInfo)) :=
  sendRequest ["COMMAND".toUTF8]

end Database.Redis.ManualCommands
