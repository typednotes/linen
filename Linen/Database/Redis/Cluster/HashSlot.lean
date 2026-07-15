/-
  Linen.Database.Redis.Cluster.HashSlot — CRC16-based key→slot hashing

  ## Haskell source
  `Database.Redis.Cluster.HashSlot` from
  https://hackage.haskell.org/package/hedis (module 1 of the `hedis` import,
  see `docs/imports/hedis/dependencies.md`).

  Redis Cluster splits the keyspace into `numHashSlots` (16384) slots. A
  key's slot is `CRC16(key) mod 16384`, where the hashed substring is the
  portion of the key between `{` and `}` if the key contains a "hash tag"
  (allowing an application to force several keys onto the same slot), or
  the whole key otherwise.

  ## Design
  `crc16` is upstream's own note: "Taken from crc16 package" — a CRC-16/XMODEM
  checksum (polynomial `0x1021`, no reflection, initial value `0`), computed
  byte-by-byte (via `ByteArray.foldl`) and, within each byte, bit-by-bit (a
  fixed 8-step countdown). `findSubKey`'s brace search is written over
  `ByteArray.toList` so both helpers are plain structural recursion on a
  `List UInt8` — no `partial`, no fuel, no well-founded proof obligation.
-/

namespace Database.Redis.Cluster.HashSlot

/-- A Redis Cluster hash slot, `0 ≤ slot < 16384`. -/
structure HashSlot where
  toUInt16 : UInt16
  deriving BEq, DecidableEq, Repr, Inhabited

/-- The fixed number of hash slots in a Redis Cluster: $$16384 = 2^{14}$$. -/
def numHashSlots : Nat := 16384

/-- One step of the CRC-16/XMODEM update for a single bit of the current byte. -/
private def crc16Bit (crc : UInt16) : UInt16 :=
  if crc &&& 0x8000 != 0 then
    (crc <<< 1) ^^^ 0x1021
  else
    crc <<< 1

/-- Fold 8 bit-steps of the CRC update into `crc`. Structural recursion on the
    fixed countdown `n`, not a fuel parameter dodging termination — the loop
    always runs exactly 8 times, one for each bit of the current byte. -/
private def crc16UpdateBits (n : Nat) (crc : UInt16) : UInt16 :=
  match n with
  | 0 => crc
  | n + 1 => crc16UpdateBits n (crc16Bit crc)

/-- Mix one byte into a running CRC-16/XMODEM checksum. -/
private def crc16UpdateByte (crc : UInt16) (b : UInt8) : UInt16 :=
  crc16UpdateBits 8 (crc ^^^ ((b.toUInt16) <<< 8))

/-- CRC-16/XMODEM checksum of a `ByteArray`, matching upstream `crc16`. -/
def crc16 (bytes : ByteArray) : UInt16 :=
  bytes.foldl crc16UpdateByte 0

/-- Find the first `}` in `bs`, splitting it into `(before, after)` — the
    bytes strictly before the brace and the bytes strictly after it.
    Structural recursion on the list. -/
private def splitAtClose : List UInt8 → Option (List UInt8 × List UInt8)
  | [] => none
  | b :: rest =>
    if b == '}'.toUInt8 then
      some ([], rest)
    else
      match splitAtClose rest with
      | none => none
      | some (before, after) => some (b :: before, after)

/-- Find the first `{` in `bs`, returning the bytes after it (or `none` if
    there is no `{`). Structural recursion on the list. -/
private def splitAtOpen : List UInt8 → Option (List UInt8)
  | [] => none
  | b :: rest =>
    if b == '{'.toUInt8 then
      some rest
    else
      splitAtOpen rest

/-- Find the sub-key to hash: the substring strictly between the first `{`
    and the following `}` (Redis Cluster's "hash tag" convention), provided
    that substring is non-empty. Falls back to the whole key otherwise. -/
def findSubKey (key : ByteArray) : ByteArray :=
  match splitAtOpen key.toList with
  | none => key
  | some afterOpen =>
    match splitAtClose afterOpen with
    | some (sub@(_ :: _), _) => ByteArray.mk sub.toArray
    | _ => key

/-- Compute the hash slot for a key, applying the hash-tag substring rule
    and reducing the CRC-16 checksum modulo `numHashSlots`.
    $$\text{keyToSlot}(k) = \mathrm{CRC16}(\text{findSubKey}(k)) \bmod 16384$$ -/
def keyToSlot (key : ByteArray) : HashSlot :=
  ⟨(crc16 (findSubKey key)) % 16384⟩

end Database.Redis.Cluster.HashSlot
