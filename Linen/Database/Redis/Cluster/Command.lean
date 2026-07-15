/-
  Linen.Database.Redis.Cluster.Command — `CLUSTER COMMAND`/`COMMAND` reply
  parsing and key-position extraction

  ## Haskell source
  `Database.Redis.Cluster.Command` from
  https://hackage.haskell.org/package/hedis (module 6 of the `hedis`
  import, see `docs/imports/hedis/dependencies.md`),
  `src/Database/Redis/Cluster/Command.hs`.

  ## `unordered-containers` substitution
  Upstream's `InfoMap` is a `Data.HashMap.Strict.HashMap String CommandInfo`
  keyed by the lower-cased command name. Per
  `docs/imports/hedis/dependencies.md`'s external-dependency note,
  `unordered-containers` maps directly onto stdlib `Std.HashMap`. Rather
  than round-tripping through `String` (which would need a UTF-8 decode of
  what is really just an ASCII command name), the key is kept as the
  lower-cased `ByteArray` directly — `Std.HashMap` works over any
  `BEq`/`Hashable` key, both of which `ByteArray` already has.

  ## Deviations
  - Upstream's three `decode` equations for `CommandInfo` (the bare 6-field
    reply, the Redis 6.0 reply with one trailing ACL-categories field, and
    the Redis 7.0 reply with three further trailing fields) are collapsed
    into one: after the first six fields, any number of further
    `MultiBulk` fields (ACL categories/tips/key-specs/subcommands) is
    accepted and ignored. This is a forward-compatible generalisation of
    upstream's fixed 0/1/4-trailing-field cases, not a behavioural
    narrowing.
  - `takeEvery`'s Haskell source recurses as
    `takeEvery n (x:xs) = x : takeEvery n (drop (n-1) xs)`; called with
    `n = 0` this would loop forever in Haskell too (`drop (-1) xs = xs`,
    unchanged) — it is simply never called that way upstream (the
    `stepCount == 0` case is special-cased away before `takeEvery` is
    reached). The Lean port is total for every `n` (including `0`, where it
    degenerates to returning the whole remaining list) via a well-founded
    recursion on `l.length`, which strictly decreases on every step
    regardless of how much `drop` removes.
-/
import Linen.Database.Redis.Protocol
import Linen.Database.Redis.Types
import Std.Data.HashMap

namespace Database.Redis.Cluster.Command

open Database.Redis.Protocol (Reply)
open Database.Redis.Types (RedisResult readSignedDecimal)

-- ── Types ──

/-- A `COMMAND`-reply flag describing a command's behaviour. -/
inductive Flag where
  | write
  | readOnly
  | denyOOM
  | admin
  | pubsub
  | noScript
  | random
  | sortForScript
  | loading
  | stale
  | skipMonitor
  | asking
  | fast
  | movableKeys
  /-- Any flag name not recognised above. -/
  | other (s : ByteArray)
  deriving BEq, Inhabited

/-- A command's declared arity: either exactly `Required n` arguments, or at
    least `MinimumRequired n` (Redis reports the latter as a negative
    number whose absolute value is the minimum). -/
inductive AritySpec where
  | required (n : Int)
  | minimumRequired (n : Int)
  deriving BEq, Inhabited

/-- The position of a command's last key argument: either a fixed
    `LastKeyPosition`, or `UnlimitedKeys n` meaning "the last key is `n`
    arguments before the end" (Redis reports the latter as a negative
    number). -/
inductive LastKeyPositionSpec where
  | lastKeyPosition (n : Int)
  | unlimitedKeys (n : Int)
  deriving BEq, Inhabited

/-- Routing metadata for one command, as reported by `COMMAND`/
    `CLUSTER COMMAND`. -/
structure CommandInfo where
  name : ByteArray
  arity : AritySpec
  flags : List Flag
  firstKeyPosition : Int
  lastKeyPosition : LastKeyPositionSpec
  stepCount : Int
  deriving BEq, Inhabited

-- ── Decoding a `CommandInfo` from a `COMMAND` reply entry ──

/-- Decode a single `COMMAND`-reply flag (a `SingleLine` reply). -/
def parseFlag : Reply → Except Reply Flag
  | .singleLine flag =>
    Except.ok <|
      if flag == "write".toUTF8 then Flag.write
      else if flag == "readonly".toUTF8 then Flag.readOnly
      else if flag == "denyoom".toUTF8 then Flag.denyOOM
      else if flag == "admin".toUTF8 then Flag.admin
      else if flag == "pubsub".toUTF8 then Flag.pubsub
      else if flag == "noscript".toUTF8 then Flag.noScript
      else if flag == "random".toUTF8 then Flag.random
      else if flag == "sort_for_script".toUTF8 then Flag.sortForScript
      else if flag == "loading".toUTF8 then Flag.loading
      else if flag == "stale".toUTF8 then Flag.stale
      else if flag == "skip_monitor".toUTF8 then Flag.skipMonitor
      else if flag == "asking".toUTF8 then Flag.asking
      else if flag == "fast".toUTF8 then Flag.fast
      else if flag == "movablekeys".toUTF8 then Flag.movableKeys
      else Flag.other flag
  | r => Except.error r

/-- A non-negative arity is exact; a negative one reports (as its absolute
    value) the minimum number of arguments. -/
def parseArity (i : Int) : AritySpec :=
  if i ≥ 0 then AritySpec.required i else AritySpec.minimumRequired (-i)

/-- A non-negative last-key position is fixed; a negative one reports (via
    `-i - 1`) how many arguments before the end the last key is. -/
def parseLastKeyPos (i : Int) : LastKeyPositionSpec :=
  if i < 0 then LastKeyPositionSpec.unlimitedKeys (-i - 1)
  else LastKeyPositionSpec.lastKeyPosition i

/-- Is `r` a (possibly-null) `MultiBulk` reply? Used to accept and ignore
    any trailing ACL-categories/tips/key-specs/subcommands fields, per the
    module doc-comment's "Deviations" note. -/
private def isMultiBulk : Reply → Bool
  | .multiBulk _ => true
  | _ => false

instance : RedisResult CommandInfo where
  decode r :=
    match r with
    | .multiBulk (some (nameR :: arityR :: flagsR :: firstR :: lastR :: stepR :: rest)) =>
      match nameR, arityR, flagsR, firstR, lastR, stepR with
      | .bulk (some commandName), .integer aritySpec, .multiBulk (some replyFlags),
        .integer firstKeyPos, .integer lastKeyPos, .integer replyStepCount =>
        if rest.isEmpty ∨ rest.all isMultiBulk then
          match replyFlags.mapM parseFlag with
          | Except.error e => Except.error e
          | Except.ok parsedFlags =>
            Except.ok {
              name := commandName
              arity := parseArity aritySpec
              flags := parsedFlags
              firstKeyPosition := firstKeyPos
              lastKeyPosition := parseLastKeyPos lastKeyPos
              stepCount := replyStepCount
            }
        else
          Except.error r
      | _, _, _, _, _, _ => Except.error r
    | _ => Except.error r

-- ── `InfoMap`: command name → `CommandInfo` ──

/-- Lower-case an ASCII byte (leaves non-letters unchanged). -/
private def lowerByte (b : UInt8) : UInt8 :=
  if 'A'.toUInt8 ≤ b ∧ b ≤ 'Z'.toUInt8 then b + 32 else b

/-- Lower-case an ASCII command name. -/
def lowerCaseName (name : ByteArray) : ByteArray :=
  ByteArray.mk (name.toList.map lowerByte).toArray

/-- A map from lower-cased command name to its routing metadata. -/
abbrev InfoMap := Std.HashMap ByteArray CommandInfo

/-- Build an `InfoMap` from a `COMMAND` reply's decoded `CommandInfo`s. -/
def newInfoMap (infos : List CommandInfo) : InfoMap :=
  infos.foldl (fun m c => m.insert (lowerCaseName c.name) c) {}

-- ── Extracting a request's keys ──

/-- Take every `step`th element of `l`, starting with the first (matching
    upstream's `takeEvery`, e.g. `takeEvery 2 [1,2,3,4,5] = [1,3,5]`). See
    the module doc-comment's "Deviations" note for the `step = 0` case. -/
def takeEvery (step : Nat) (l : List α) : List α :=
  match l with
  | [] => []
  | x :: xs => x :: takeEvery step (xs.drop (step - 1))
termination_by l.length
decreasing_by simp [List.length_drop]; omega

/-- Parse a `readMaybe`-style *exact* (no trailing garbage) decimal
    integer, as used for `EVAL`/`EVALSHA`/`ZUNIONSTORE`/`ZINTERSTORE`'s
    leading numkeys argument. -/
private def readExactInt (bytes : ByteArray) : Option Int :=
  match bytes.toList with
  | [] => none
  | b0 :: rest0 =>
    let (neg, digitsList) := if b0 == '-'.toUInt8 then (true, rest0) else (false, bytes.toList)
    if digitsList.isEmpty ∨ ¬ digitsList.all (fun b => '0'.toUInt8 ≤ b ∧ b ≤ '9'.toUInt8) then
      none
    else
      let n := digitsList.foldl (fun acc b => acc * 10 + (b - '0'.toUInt8).toNat) 0
      some (if neg then -(Int.ofNat n) else Int.ofNat n)

/-- `EVAL`/`EVALSHA`/`ZUNIONSTORE`/`ZINTERSTORE`'s movable-keys rule: the
    first argument is a decimal `numkeys`, followed by exactly that many
    keys. -/
def readNumKeys : List ByteArray → Option (List ByteArray)
  | [] => none
  | rawNumKeys :: rest =>
    match readExactInt rawNumKeys with
    | some n => some (rest.take n.toNat)
    | none => none

/-- `XREAD`'s movable-keys rule: skip `COUNT n`/`BLOCK n` options, then
    `STREAMS key... id...` — the keys are the first half of what follows
    `STREAMS`. -/
def readXreadKeys : List ByteArray → Option (List ByteArray)
  | [] => none
  | b :: rest =>
    if b == "COUNT".toUTF8 ∨ b == "BLOCK".toUTF8 then
      match rest with
      | _ :: rest' => readXreadKeys rest'
      | [] => none
    else if b == "STREAMS".toUTF8 then
      some (rest.take (rest.length / 2))
    else
      none

/-- `XREADGROUP`'s movable-keys rule: like `readXreadKeys`, with an
    additional `NOACK` option to skip. -/
def readXreadgroupKeys : List ByteArray → Option (List ByteArray)
  | [] => none
  | b :: rest =>
    if b == "COUNT".toUTF8 ∨ b == "BLOCK".toUTF8 then
      match rest with
      | _ :: rest' => readXreadgroupKeys rest'
      | [] => none
    else if b == "NOACK".toUTF8 then
      readXreadgroupKeys rest
    else if b == "STREAMS".toUTF8 then
      some (rest.take (rest.length / 2))
    else
      none

/-- Movable-keys commands' keys aren't at fixed positions; each has its own
    ad-hoc rule (matching upstream's `parseMovable`). -/
def parseMovable : List ByteArray → Option (List ByteArray)
  | [] => none
  | [cmd] => if cmd == "XREAD".toUTF8 then readXreadKeys [] else none
  | cmd :: rest1 =>
    match rest1 with
    | key :: rest0 =>
      if cmd == "SORT".toUTF8 then
        some [key]
      else if cmd == "EVAL".toUTF8 ∨ cmd == "EVALSHA".toUTF8 ∨
              cmd == "ZUNIONSTORE".toUTF8 ∨ cmd == "ZINTERSTORE".toUTF8 then
        readNumKeys rest0
      else if cmd == "XREAD".toUTF8 then
        readXreadKeys rest1
      else
        match rest1 with
        | c1 :: _ :: _ :: rest' =>
          if cmd == "XREADGROUP".toUTF8 ∧ c1 == "GROUP".toUTF8 then
            readXreadgroupKeys rest'
          else
            none
        | _ => none
    | [] => none

/-- `MovableKeys`-flagged commands? -/
def isMovable (info : CommandInfo) : Bool :=
  info.flags.contains Flag.movableKeys

/-- Extract the keys touched by a request, given its command's routing
    metadata. -/
def keysForRequest' (info : CommandInfo) (request : List ByteArray) : Option (List ByteArray) :=
  if isMovable info then
    parseMovable request
  else if info.stepCount == 0 then
    some []
  else
    let possibleKeys :=
      match info.lastKeyPosition with
      | .lastKeyPosition endPos =>
        (request.drop info.firstKeyPosition.toNat).take (1 + endPos - info.firstKeyPosition).toNat
      | .unlimitedKeys endPos =>
        (request.take (request.length - endPos.toNat)).drop info.firstKeyPosition.toNat
    some (takeEvery info.stepCount.toNat possibleKeys)

/-- Fall back to `InfoMap`-driven key extraction for a request whose
    command isn't one of the special-cased shapes in `keysForRequest`. -/
private def keysForRequestGeneral (info : InfoMap) (request : List ByteArray) :
    Option (List ByteArray) :=
  match request with
  | [] => none
  | command :: _ =>
    match info.get? (lowerCaseName command) with
    | none => none
    | some cmdInfo => keysForRequest' cmdInfo request

/-- Extract the keys touched by a request, special-casing the handful of
    commands whose `COMMAND` metadata doesn't (fully) describe their key
    positions, then falling back to `InfoMap`-driven extraction. -/
def keysForRequest (info : InfoMap) (request : List ByteArray) : Option (List ByteArray) :=
  match request with
  | [] => none
  | [cmd] =>
    if cmd == "QUIT".toUTF8 then some [] else keysForRequestGeneral info request
  | [cmd, sub, key] =>
    if cmd == "DEBUG".toUTF8 ∧ sub == "OBJECT".toUTF8 then some [key]
    else if cmd == "OBJECT".toUTF8 ∧
            (sub == "refcount".toUTF8 ∨ sub == "encoding".toUTF8 ∨ sub == "idletime".toUTF8) then
      some [key]
    else if cmd == "XINFO".toUTF8 then some [key]
    else keysForRequestGeneral info request
  | cmd :: _ :: key :: _ =>
    if cmd == "XINFO".toUTF8 then some [key] else keysForRequestGeneral info request
  | _ => keysForRequestGeneral info request

end Database.Redis.Cluster.Command
