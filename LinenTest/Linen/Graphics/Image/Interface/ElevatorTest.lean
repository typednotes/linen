/-
  Tests for `Linen.Graphics.Image.Interface.Elevator` — the `clamp01`
  helper and every `Elevator` instance's conversions, in both directions,
  including round trips and boundary values.
-/
import Linen.Graphics.Image.Interface.Elevator

open Graphics.Image.Interface.Elevator

-- ── clamp01 / clamp01F32 ──

#guard clamp01 (-1.0) == 0.0
#guard clamp01 0.5 == 0.5
#guard clamp01 2.0 == 1.0
#guard clamp01F32 (-1.0) == 0.0
#guard clamp01F32 0.5 == 0.5
#guard clamp01F32 2.0 == 1.0

-- ── `Elevator UInt8` ──

-- Identity and boundary values.
#guard toWord8 (0 : UInt8) == 0
#guard toWord8 (255 : UInt8) == 255

-- Widening scales by `maxTarget / 255`.
#guard toWord16 (0 : UInt8) == 0
#guard toWord16 (255 : UInt8) == 65535
#guard toWord16 (128 : UInt8) == 32896
#guard toWord32 (255 : UInt8) == 4294967295
#guard toWord32 (128 : UInt8) == 2155905152
#guard toWord64 (255 : UInt8) == 18446744073709551615

-- Widening to a float squashes to `[0, 1]` by dividing by 255.
#guard toFloat (0 : UInt8) == 0.0
#guard toFloat (255 : UInt8) == 1.0
#guard toFloat (128 : UInt8) == (128.0 / 255.0 : Float)
#guard toFloat32 (128 : UInt8) == (128.0 / 255.0 : Float32)

-- Round trip through `fromFloat`.
#guard (Elevator.fromFloat (Elevator.toFloat (255 : UInt8)) : UInt8) == 255
#guard (Elevator.fromFloat (Elevator.toFloat (0 : UInt8)) : UInt8) == 0

-- ── `Elevator UInt16` ──

#guard toWord16 (65535 : UInt16) == 65535

-- Narrowing to `UInt8` divides by `65535 / 255 = 257`.
#guard toWord8 (0 : UInt16) == 0
#guard toWord8 (65535 : UInt16) == 255
#guard toWord8 (32768 : UInt16) == 127

-- Widening to `UInt32`/`UInt64` scales up.
#guard toWord32 (65535 : UInt16) == 4294967295
#guard toWord64 (65535 : UInt16) == 18446744073709551615

#guard toFloat (65535 : UInt16) == 1.0
#guard toFloat (0 : UInt16) == 0.0

-- ── `Elevator UInt32` ──

#guard toWord32 (4294967295 : UInt32) == 4294967295
#guard toWord8 (4294967295 : UInt32) == 255
#guard toWord16 (4294967295 : UInt32) == 65535
#guard toWord64 (4294967295 : UInt32) == 18446744073709551615
#guard toFloat (4294967295 : UInt32) == 1.0
#guard toFloat (0 : UInt32) == 0.0

-- ── `Elevator UInt64` ──

#guard toWord64 (18446744073709551615 : UInt64) == 18446744073709551615
#guard toWord8 (18446744073709551615 : UInt64) == 255
#guard toWord16 (18446744073709551615 : UInt64) == 65535
#guard toWord32 (18446744073709551615 : UInt64) == 4294967295
#guard toFloat (18446744073709551615 : UInt64) == 1.0
#guard toFloat (0 : UInt64) == 0.0

-- `fromFloat` targeting `UInt64` scales by `18446744073709551615`; `0.5`
-- lands exactly on `9223372036854775808` (`Float`'s 53-bit mantissa cannot
-- represent the odd `maxBound` exactly, so it rounds to the nearest even
-- power-of-two-adjacent value — see the module doc-comment's discussion of
-- the analogous `Int` quirk, which is the same underlying phenomenon).
#guard (Elevator.fromFloat (0.5 : Float) : UInt64) == 9223372036854775808
#guard (Elevator.fromFloat (0.0 : Float) : UInt64) == 0

-- ── `Elevator Int` ──
-- Negative values are clamped to `0` before scaling.

#guard toWord8 ((-5) : Int) == 0
#guard toWord8 (9223372036854775807 : Int) == 255
#guard toWord16 (9223372036854775807 : Int) == 65535
#guard toWord32 (9223372036854775807 : Int) == 4294967295

-- `toWord64` is upstream's `fromIntegral . max 0`: a direct cast, not a
-- rescaling, since `Int`'s conceptual maximum already fits `UInt64`'s range.
#guard toWord64 (9223372036854775807 : Int) == 9223372036854775807
#guard toWord64 ((-5) : Int) == 0

#guard toFloat (0 : Int) == 0.0
#guard toFloat (9223372036854775807 : Int) == 1.0

#guard (Elevator.fromFloat (0.0 : Float) : Int) == 0
#guard (Elevator.fromFloat ((-0.1) : Float) : Int) == 0
-- Same floating-point-precision quirk as the `UInt64` case above.
#guard (Elevator.fromFloat (1.0 : Float) : Int) == 9223372036854775808
#guard (Elevator.fromFloat (0.5 : Float) : Int) == 4611686018427387904

-- ── `Elevator Float32` ──

#guard toFloat32 ((0.5 : Float32)) == 0.5
#guard toFloat ((0.5 : Float32)) == 0.5
#guard (Elevator.fromFloat (0.25 : Float) : Float32) == 0.25

#guard toWord8 ((0.0 : Float32)) == 0
#guard toWord8 ((0.5 : Float32)) == 128
#guard toWord8 ((1.0 : Float32)) == 255
-- Values outside `[0, 1]` are clamped by `clamp01F32` before scaling.
#guard toWord8 ((2.0 : Float32)) == 255
#guard toWord8 (((-1.0) : Float32)) == 0
#guard toWord16 ((0.5 : Float32)) == 32768
#guard toWord32 ((0.5 : Float32)) == 2147483648
#guard toWord64 ((0.5 : Float32)) == 9223372036854775808

-- ── `Elevator Float` ──

#guard toFloat ((0.5 : Float)) == 0.5
#guard toFloat32 ((0.5 : Float)) == 0.5
#guard (Elevator.fromFloat (0.5 : Float) : Float) == 0.5

#guard toWord8 ((0.5 : Float)) == 128
#guard toWord16 ((0.5 : Float)) == 32768
#guard toWord32 ((0.5 : Float)) == 2147483648
#guard toWord64 ((0.5 : Float)) == 9223372036854775808
-- Values outside `[0, 1]` are clamped by `clamp01` before scaling.
#guard toWord8 ((2.0 : Float)) == 255
#guard toWord8 (((-1.0) : Float)) == 0
