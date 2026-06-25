/-
  Linen.Data.Bits — Bitwise operations typeclass

  Provides `Bits` and `FiniteBits` typeclasses mirroring Haskell's `Data.Bits`,
  with instances for Lean's fixed-width unsigned integer types (`UInt8`,
  `UInt16`, `UInt32`, `UInt64`).

  ## Design

  Lean's standard library already provides bitwise operations on `UInt*` types
  (`&&&`, `|||`, `^^^`, `<<<`, `>>>`, complement). This module adds the uniform
  typeclass interface Lean core lacks, plus `testBit`, `popCount`, `bit`,
  `zeroBits`, and (for `FiniteBits`) leading/trailing-zero counts — the latter
  carrying proofs that they are bounded by the bit width.
-/

namespace Data

/-- Typeclass for types supporting bitwise operations.

    $$\text{Bits}(\alpha)$$ requires at minimum: bitwise AND, OR, XOR,
    complement, shifts, bit testing, and a notion of zero bits.

    Corresponds to Haskell's `Data.Bits.Bits`. -/
class Bits (α : Type u) where
  /-- Bitwise AND. $$a \mathbin{\&} b$$ -/
  and : α → α → α
  /-- Bitwise OR. $$a \mathbin{|} b$$ -/
  or : α → α → α
  /-- Bitwise XOR. $$a \oplus b$$ -/
  xor : α → α → α
  /-- Bitwise complement. $$\sim a$$ -/
  complement : α → α
  /-- Left shift by $n$ bits. $$a \ll n$$ -/
  shiftL : α → Nat → α
  /-- Right shift by $n$ bits. $$a \gg n$$ -/
  shiftR : α → Nat → α
  /-- Test bit at position $n$ (zero-indexed from LSB).
      $$\text{testBit}(a, n) = ((a \gg n) \mathbin{\&} 1) \neq 0$$ -/
  testBit : α → Nat → Bool
  /-- The value with only bit $n$ set. $$\text{bit}(n) = 1 \ll n$$ -/
  bit : Nat → α
  /-- Count the number of set bits (population count).
      $$\text{popCount}(a) = |\{i \mid \text{testBit}(a, i)\}|$$ -/
  popCount : α → Nat
  /-- The all-zeros value. $$\text{zeroBits} = 0$$ -/
  zeroBits : α
  /-- Bit size if fixed-width, `none` for arbitrary-width.
      $$\text{bitSizeMaybe} \in \{\text{none}\} \cup \{\text{some}(n) \mid n \in \mathbb{N}\}$$ -/
  bitSizeMaybe : Option Nat

/-- Typeclass for fixed-width bit types, extending `Bits`.

    $$\text{FiniteBits}(\alpha)$$ adds `finiteBitSize`, `countLeadingZeros`,
    and `countTrailingZeros`. -/
class FiniteBits (α : Type u) extends Bits α where
  /-- The fixed bit width. $$\text{finiteBitSize} \in \mathbb{N}$$ -/
  finiteBitSize : Nat
  /-- Count of set bits (population count), bounded by `finiteBitSize`.
      $$\text{popCount}(a) \leq \text{finiteBitSize}$$ -/
  popCountBounded : α → {n : Nat // n ≤ finiteBitSize}
  /-- Count of leading zeros from MSB, bounded by `finiteBitSize`. -/
  countLeadingZeros : α → {n : Nat // n ≤ finiteBitSize}
  /-- Count of trailing zeros from LSB, bounded by `finiteBitSize`. -/
  countTrailingZeros : α → {n : Nat // n ≤ finiteBitSize}

-- ── Helpers using UInt64 as common representation ──

/-- Pop count for the low `n` bits of a UInt64 value (recursive for provability). -/
private def popCountN (x : UInt64) : Nat → Nat
  | 0 => 0
  | n + 1 => popCountN x n + if ((x >>> n.toUInt64) &&& 1) != 0 then 1 else 0

private theorem popCountN_le (x : UInt64) (n : Nat) : popCountN x n ≤ n := by
  induction n with
  | zero => simp [popCountN]
  | succ k ih => unfold popCountN; split <;> omega

/-- Pop count for all 64 bits of a UInt64 value. -/
private def popCountU64 (x : UInt64) : Nat := popCountN x 64

/-- Count leading zeros for a value with given bit size. -/
private def clzU64 (x : UInt64) (size : Nat) : Nat :=
  let rec go : Nat → Nat
    | 0 => size
    | i + 1 =>
      if ((x >>> i.toUInt64) &&& 1) != 0 then size - (i + 1)
      else go i
  go size

private theorem clzU64_go_le (x : UInt64) (size : Nat) :
    ∀ n, clzU64.go x size n ≤ size := by
  intro n; induction n with
  | zero => simp [clzU64.go]
  | succ k ih => unfold clzU64.go; split <;> omega

private theorem clzU64_le (x : UInt64) (size : Nat) : clzU64 x size ≤ size :=
  clzU64_go_le x size size

/-- Count trailing zeros for a value with given bit size (recursive for provability). -/
private def ctzRec (x : UInt64) (size i : Nat) : Nat :=
  if i >= size then size
  else if ((x >>> i.toUInt64) &&& 1) != 0 then i
  else ctzRec x size (i + 1)
termination_by size - i

private theorem ctzRec_le (x : UInt64) (size i : Nat) : ctzRec x size i ≤ size := by
  unfold ctzRec
  split
  · omega
  · split
    · omega
    · exact ctzRec_le x size (i + 1)
termination_by size - i

-- ── UInt8 instance ──────────────────────────────

instance : Bits UInt8 where
  and a b := a &&& b
  or a b := a ||| b
  xor a b := a ^^^ b
  complement a := UInt8.complement a
  shiftL a n := a <<< n.toUInt8
  shiftR a n := a >>> n.toUInt8
  testBit a n := ((a >>> n.toUInt8) &&& 1) != 0
  bit n := 1 <<< n.toUInt8
  popCount a := popCountN a.toUInt64 8
  zeroBits := 0
  bitSizeMaybe := some 8

instance : FiniteBits UInt8 where
  finiteBitSize := 8
  popCountBounded a := ⟨popCountN a.toUInt64 8, popCountN_le a.toUInt64 8⟩
  countLeadingZeros a := ⟨clzU64 a.toUInt64 8, clzU64_le a.toUInt64 8⟩
  countTrailingZeros a := ⟨ctzRec a.toUInt64 8 0, ctzRec_le a.toUInt64 8 0⟩

-- ── UInt16 instance ─────────────────────────────

instance : Bits UInt16 where
  and a b := a &&& b
  or a b := a ||| b
  xor a b := a ^^^ b
  complement a := UInt16.complement a
  shiftL a n := a <<< n.toUInt16
  shiftR a n := a >>> n.toUInt16
  testBit a n := ((a >>> n.toUInt16) &&& 1) != 0
  bit n := 1 <<< n.toUInt16
  popCount a := popCountN a.toUInt64 16
  zeroBits := 0
  bitSizeMaybe := some 16

instance : FiniteBits UInt16 where
  finiteBitSize := 16
  popCountBounded a := ⟨popCountN a.toUInt64 16, popCountN_le a.toUInt64 16⟩
  countLeadingZeros a := ⟨clzU64 a.toUInt64 16, clzU64_le a.toUInt64 16⟩
  countTrailingZeros a := ⟨ctzRec a.toUInt64 16 0, ctzRec_le a.toUInt64 16 0⟩

-- ── UInt32 instance ─────────────────────────────

instance : Bits UInt32 where
  and a b := a &&& b
  or a b := a ||| b
  xor a b := a ^^^ b
  complement a := UInt32.complement a
  shiftL a n := a <<< n.toUInt32
  shiftR a n := a >>> n.toUInt32
  testBit a n := ((a >>> n.toUInt32) &&& 1) != 0
  bit n := 1 <<< n.toUInt32
  popCount a := popCountN a.toUInt64 32
  zeroBits := 0
  bitSizeMaybe := some 32

instance : FiniteBits UInt32 where
  finiteBitSize := 32
  popCountBounded a := ⟨popCountN a.toUInt64 32, popCountN_le a.toUInt64 32⟩
  countLeadingZeros a := ⟨clzU64 a.toUInt64 32, clzU64_le a.toUInt64 32⟩
  countTrailingZeros a := ⟨ctzRec a.toUInt64 32 0, ctzRec_le a.toUInt64 32 0⟩

-- ── UInt64 instance ─────────────────────────────

instance : Bits UInt64 where
  and a b := a &&& b
  or a b := a ||| b
  xor a b := a ^^^ b
  complement a := UInt64.complement a
  shiftL a n := a <<< n.toUInt64
  shiftR a n := a >>> n.toUInt64
  testBit a n := ((a >>> n.toUInt64) &&& 1) != 0
  bit n := 1 <<< n.toUInt64
  popCount := popCountU64
  zeroBits := 0
  bitSizeMaybe := some 64

instance : FiniteBits UInt64 where
  finiteBitSize := 64
  popCountBounded a := ⟨popCountU64 a, popCountN_le a 64⟩
  countLeadingZeros a := ⟨clzU64 a 64, clzU64_le a 64⟩
  countTrailingZeros a := ⟨ctzRec a 64 0, ctzRec_le a 64 0⟩

-- ── Derived operations ──────────────────────────

namespace Bits

/-- Set a specific bit to 1.
    $$\text{setBit}(a, n) = a \mathbin{|} \text{bit}(n)$$ -/
@[inline] def setBit [Bits α] (a : α) (n : Nat) : α :=
  Bits.or a (Bits.bit n)

/-- Clear a specific bit to 0.
    $$\text{clearBit}(a, n) = a \mathbin{\&} \sim\text{bit}(n)$$ -/
@[inline] def clearBit [Bits α] (a : α) (n : Nat) : α :=
  Bits.and a (Bits.complement (Bits.bit n))

/-- Toggle a specific bit.
    $$\text{complementBit}(a, n) = a \oplus \text{bit}(n)$$ -/
@[inline] def complementBit [Bits α] (a : α) (n : Nat) : α :=
  Bits.xor a (Bits.bit n)

end Bits

end Data
