/-
  Linen.Graphics.Image.Interface.Elevator — precision-changing conversions
  between pixel component types

  ## Haskell equivalent
  `Graphics.Image.Interface.Elevator` from
  https://hackage.haskell.org/package/hip (module #2 of the `hip` import
  plan, see `docs/imports/hip/dependencies.md`).

  ## Design

  Upstream's `Elevator` class lets a pixel *component* value change precision
  — e.g. an 8-bit channel widened to 16-bit, or a `Double` channel in
  `[0, 1]` narrowed to a `Word8` in `[0, 255]` — while rescaling the value so
  it stays in the target type's natural range. It is built from four private
  helpers (`dropDown`, `raiseUp`, `squashTo1`, `stretch`) plus the exported
  `clamp01`, and one instance per component type hip supports.

  ## Type mapping

  Per `Linen.Codec.Picture.Types` (`Pixel8 := UInt8`, `Pixel16 := UInt16`,
  `Pixel32 := UInt32`, `PixelF := Float32`), this codebase's pixel components
  already target Lean's fixed-width unsigned integer types and `Float32` for
  single precision. This port follows the same mapping and adds Lean's
  arbitrary-precision `Int` and 64-bit `Float` for upstream's `Int` and
  `Double`:

  | upstream       | Lean       |
  |----------------|------------|
  | `Word8`        | `UInt8`    |
  | `Word16`       | `UInt16`   |
  | `Word32`       | `UInt32`   |
  | `Word64`       | `UInt64`   |
  | `Int`          | `Int`      |
  | `Float`        | `Float32`  |
  | `Double`       | `Float`    |

  Upstream's six methods `toWord8`/`toWord16`/`toWord32`/`toWord64`/
  `toFloat`/`toDouble`/`fromDouble` are renamed only where the Lean type they
  target differs in name from the Haskell one: `toFloat` → `toFloat32`
  (targets `Float32`, matching upstream's single-precision `Float`) and
  `toDouble`/`fromDouble` → `toFloat`/`fromFloat` (targeting Lean's 64-bit
  `Float`, the counterpart of upstream's `Double`).

  ## Deferred instances (out of scope for this module)

  Upstream additionally instantiates `Elevator` for `Word` (GHC's
  platform-native machine word), `Int8`/`Int16`/`Int32`/`Int64`, and
  `Complex e`. None of these are ported here:

  * `Word` is GHC's platform-native unsigned machine word — on every
    platform hip actually ships for it is bit-for-bit identical to `Word64`,
    so it is GHC/platform-specific plumbing with no independent Lean
    counterpart, in the same spirit as the `Storable`/`Typeable`/`NFData`
    instances the `hip` scope note in `docs/imports/hip/dependencies.md`
    already drops throughout this import.
  * `Int8`/`Int16`/`Int32`/`Int64` are fixed-width *signed* component types.
    No pixel type ported so far in this codebase (`Linen.Codec.Picture.Types`
    included) uses a signed fixed-width component, so there is no consumer
    for these instances yet; they can be added faithfully (upstream's own
    `max 0` clamp-then-`dropDown`/`raiseUp`/`squashTo1` pattern, reusing the
    same helpers below) if and when a future module needs them.
  * `Elevator (Complex e)` discards the imaginary part and elevates the real
    part alone. `Linen.Data.Complex` already exists in this codebase, but
    this instance is postponed to module #9
    (`Graphics.Image.ColorSpace.Complex`, per `docs/imports/hip/
    dependencies.md`) where complex-valued pixels are actually introduced —
    keeping this module's dependency footprint to plain numeric component
    types only, matching its place in the topological plan ("no dependency
    on any other hip module").

  Lean's `Int` is arbitrary-precision, unlike Haskell's fixed-width (in
  practice 64-bit) machine `Int`. The `Elevator Int` instance below treats
  `Int`'s conceptual upper bound as `Int64`'s maximum, `9223372036854775807`
  — the same assumption upstream's own `Int` instance makes about its
  platform's native `Int` — and clamps negative values to `0` first, exactly
  as upstream's `max 0` does for every signed instance.

  ## Scaling formulas

  All four helpers are ported as literal transcriptions of upstream's
  `dropDown`/`raiseUp`/`squashTo1`/`stretch`, specialised from upstream's
  `(Integral a, Bounded a, Integral b, Bounded b)` constraints to concrete
  `UInt64` arithmetic (`UInt64` covers the full range of every unsigned
  component type ported here, so every conversion is done by widening to
  `UInt64`, computing, then narrowing back down):

  * `dropDown maxA maxB e = e / (maxA / maxB)` — lower precision by dividing.
  * `raiseUp maxA maxB e = e * (maxB / maxA)` — raise precision by
    multiplying.
  * `squashTo1 maxA e = e.toFloat / maxA.toFloat` — widen an integral value
    to a float in `[0, 1]` by dividing by the source type's maximum.
  * `stretch maxB x = round (maxB.toFloat * clamp01 x)` — narrow a float,
    clamped to `[0, 1]`, to an integral value by scaling up by the target
    type's maximum and rounding to the nearest integer.
  * `clamp01 x = min (max 0 x) 1` — clamp a float to `[0, 1]`, exported
    exactly as upstream's `clamp01`.
-/

namespace Graphics.Image.Interface.Elevator

-- ── Clamping ──

/-- Upstream's `clamp01 :: (Ord a, Floating a) => a -> a`, clamping a value
to the closed unit interval `[0, 1]`. -/
def clamp01 (x : Float) : Float :=
  min (max 0 x) 1

/-- `Float32` counterpart of `clamp01` (upstream's `clamp01` is itself
polymorphic over any `Floating` type; both Lean float precisions need their
own instantiation since Lean's `Float`/`Float32` are distinct types). -/
def clamp01F32 (x : Float32) : Float32 :=
  min (max 0 x) 1

-- ── Generic scaling helpers ──
-- `maxA`/`maxB` are the maximum representable value, as a `UInt64`, of the
-- source/target integral component type.

/-- Upstream's `dropDown`: lower the precision of an integral component by
dividing its value by `maxA / maxB`. -/
def dropDown (maxA maxB e : UInt64) : UInt64 :=
  e / (maxA / maxB)

/-- Upstream's `raiseUp`: increase the precision of an integral component by
multiplying its value by `maxB / maxA`. -/
def raiseUp (maxA maxB e : UInt64) : UInt64 :=
  e * (maxB / maxA)

/-- Upstream's `squashTo1`: convert an integral component to a fractional
value in `[0, 1]` by dividing by the source type's maximum representable
value `maxA`. -/
def squashTo1 (maxA e : UInt64) : Float :=
  e.toFloat / maxA.toFloat

/-- `Float32` counterpart of `squashTo1`. -/
def squashTo1F32 (maxA e : UInt64) : Float32 :=
  e.toFloat32 / maxA.toFloat32

/-- Upstream's `stretch`: convert a floating value, clamped to `[0, 1]` by
`clamp01`, to an integral value by scaling it up by the target type's maximum
representable value `maxB` and rounding to the nearest integer. -/
def stretch (maxB : UInt64) (x : Float) : UInt64 :=
  (maxB.toFloat * clamp01 x).round.toUInt64

/-- `Float32` counterpart of `stretch`. -/
def stretchF32 (maxB : UInt64) (x : Float32) : UInt64 :=
  (maxB.toFloat32 * clamp01F32 x).round.toUInt64

-- ── The `Elevator` class ──

/-- A class with a set of convenient functions that allow for changing
precision of channels within pixels, while scaling the values to keep them
in an appropriate range. Upstream's `Elevator`.

```
#eval (Elevator.toWord8 (0.0 : Float), Elevator.toWord8 (0.5 : Float), Elevator.toWord8 (1.0 : Float))
-- (0, 128, 255)
```
-/
class Elevator (e : Type) where
  /-- Values are scaled to the `[0, 255]` range. -/
  toWord8 : e → UInt8
  /-- Values are scaled to the `[0, 65535]` range. -/
  toWord16 : e → UInt16
  /-- Values are scaled to the `[0, 4294967295]` range. -/
  toWord32 : e → UInt32
  /-- Values are scaled to the `[0, 18446744073709551615]` range. -/
  toWord64 : e → UInt64
  /-- Values are scaled to the `[0.0, 1.0]` range (single precision). -/
  toFloat32 : e → Float32
  /-- Values are scaled to the `[0.0, 1.0]` range (double precision). -/
  toFloat : e → Float
  /-- Values are scaled from the `[0.0, 1.0]` range (double precision). -/
  fromFloat : Float → e

export Elevator (toWord8 toWord16 toWord32 toWord64 toFloat32 toFloat fromFloat)

-- ── Maximum representable values, as `UInt64`, of each source type ──

private def maxU8 : UInt64 := 255
private def maxU16 : UInt64 := 65535
private def maxU32 : UInt64 := 4294967295
private def maxU64 : UInt64 := 18446744073709551615
/-- Lean's `Int` is arbitrary-precision; this port treats its conceptual
upper bound as `Int64`'s maximum, matching upstream's own assumption about
its platform's native (in practice 64-bit) `Int` — see the module
doc-comment. -/
private def maxInt : UInt64 := 9223372036854775807

-- ── Instances ──

/-- Values between `[0, 255]`. -/
instance : Elevator UInt8 where
  toWord8 := id
  toWord16 e := (raiseUp maxU8 maxU16 e.toUInt64).toUInt16
  toWord32 e := (raiseUp maxU8 maxU32 e.toUInt64).toUInt32
  toWord64 e := raiseUp maxU8 maxU64 e.toUInt64
  toFloat32 e := squashTo1F32 maxU8 e.toUInt64
  toFloat e := squashTo1 maxU8 e.toUInt64
  fromFloat x := (stretch maxU8 x).toUInt8

/-- Values between `[0, 65535]`. -/
instance : Elevator UInt16 where
  toWord8 e := (dropDown maxU16 maxU8 e.toUInt64).toUInt8
  toWord16 := id
  toWord32 e := (raiseUp maxU16 maxU32 e.toUInt64).toUInt32
  toWord64 e := raiseUp maxU16 maxU64 e.toUInt64
  toFloat32 e := squashTo1F32 maxU16 e.toUInt64
  toFloat e := squashTo1 maxU16 e.toUInt64
  fromFloat x := (stretch maxU16 x).toUInt16

/-- Values between `[0, 4294967295]`. -/
instance : Elevator UInt32 where
  toWord8 e := (dropDown maxU32 maxU8 e.toUInt64).toUInt8
  toWord16 e := (dropDown maxU32 maxU16 e.toUInt64).toUInt16
  toWord32 := id
  toWord64 e := raiseUp maxU32 maxU64 e.toUInt64
  toFloat32 e := squashTo1F32 maxU32 e.toUInt64
  toFloat e := squashTo1 maxU32 e.toUInt64
  fromFloat x := (stretch maxU32 x).toUInt32

/-- Values between `[0, 18446744073709551615]`. -/
instance : Elevator UInt64 where
  toWord8 e := (dropDown maxU64 maxU8 e).toUInt8
  toWord16 e := (dropDown maxU64 maxU16 e).toUInt16
  toWord32 e := (dropDown maxU64 maxU32 e).toUInt32
  toWord64 := id
  toFloat32 e := squashTo1F32 maxU64 e
  toFloat e := squashTo1 maxU64 e
  fromFloat := stretch maxU64

/-- Values between `[0, 9223372036854775807]` — see the module doc-comment
for why Lean's arbitrary-precision `Int` is treated as bounded by `Int64`'s
maximum here, matching upstream's `Int` instance. Negative values are
clamped to `0` first, exactly as upstream's `max 0` does for every signed
instance. -/
instance : Elevator Int where
  toWord8 e := (dropDown maxInt maxU8 (max 0 e).toNat.toUInt64).toUInt8
  toWord16 e := (dropDown maxInt maxU16 (max 0 e).toNat.toUInt64).toUInt16
  toWord32 e := (dropDown maxInt maxU32 (max 0 e).toNat.toUInt64).toUInt32
  -- Upstream's `toWord64 = fromIntegral . max 0`: a direct cast with no
  -- rescaling, since `Int`'s maximum (`maxInt`, treated as `Int64`'s
  -- maximum) is already within `Word64`'s range.
  toWord64 e := (max 0 e).toNat.toUInt64
  toFloat32 e := squashTo1F32 maxInt (max 0 e).toNat.toUInt64
  toFloat e := squashTo1 maxInt (max 0 e).toNat.toUInt64
  fromFloat x := Int.ofNat (stretch maxInt x).toNat

/-- Values between `[0.0, 1.0]` (single precision). -/
instance : Elevator Float32 where
  toWord8 e := (stretchF32 maxU8 e).toUInt8
  toWord16 e := (stretchF32 maxU16 e).toUInt16
  toWord32 e := (stretchF32 maxU32 e).toUInt32
  toWord64 e := stretchF32 maxU64 e
  toFloat32 := id
  toFloat e := e.toFloat
  fromFloat x := x.toFloat32

/-- Values between `[0.0, 1.0]` (double precision). -/
instance : Elevator Float where
  toWord8 e := (stretch maxU8 e).toUInt8
  toWord16 e := (stretch maxU16 e).toUInt16
  toWord32 e := (stretch maxU32 e).toUInt32
  toWord64 e := stretch maxU64 e
  toFloat32 e := e.toFloat32
  toFloat := id
  fromFloat := id

end Graphics.Image.Interface.Elevator
