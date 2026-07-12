/-
  Tests for `Linen.Graphics.Image.ColorSpace.Binary` — the `Bit` component
  type, its bitwise/arithmetic/`Elevator` instances, `toNum`/`fromNum`, and
  the `PixelX Bit` binary-pixel helpers (`on`/`off`/`isOn`/`isOff`/
  `fromBool`) plus the generic bitwise operators on `PixelX e`.

  Fixture/example names are prefixed `csBinary` to avoid clashing with any
  other test file's identifiers in the shared `Tests` namespace.
-/
import Linen.Graphics.Image.ColorSpace.Binary

open Graphics.Image.ColorSpace.Binary
open Graphics.Image.ColorSpace.X (X PixelX)
open Graphics.Image.Interface.Elevator (toWord8 toWord16 toWord32 toWord64 toFloat32 toFloat
  fromFloat)

-- ── `Bit` — construction and bitwise operators ──

#guard zero == (⟨false⟩ : Bit)
#guard one == (⟨true⟩ : Bit)
#guard bool2bit true == one
#guard bool2bit false == zero
#guard bit2bool one == true
#guard bit2bool zero == false

#guard (one &&& one) == one
#guard (one &&& zero) == zero
#guard (zero &&& zero) == zero
#guard (one ||| zero) == one
#guard (zero ||| zero) == zero
#guard (one ^^^ one) == zero
#guard (one ^^^ zero) == one
#guard (~~~one) == zero
#guard (~~~zero) == one

-- ── `Num`-style arithmetic on `Bit` ──

#guard (one + zero) == one
#guard (zero + zero) == zero
#guard (one - zero) == one
#guard (zero - one) == zero
#guard (one - one) == zero
#guard (one * one) == one
#guard (one * zero) == zero
#guard (0 : Bit) == zero
#guard (1 : Bit) == one
#guard (7 : Bit) == one

-- ── `Elevator Bit` ──

#guard toWord8 zero == (0 : UInt8)
#guard toWord8 one == (255 : UInt8)
#guard toWord16 one == (65535 : UInt16)
#guard toWord32 one == (4294967295 : UInt32)
#guard toWord64 one == (18446744073709551615 : UInt64)
#guard toFloat32 zero == (0 : Float32)
#guard toFloat32 one == (1 : Float32)
#guard toFloat zero == (0 : Float)
#guard toFloat one == (1 : Float)
#guard fromFloat (0 : Float) == zero
#guard fromFloat (1 : Float) == one
#guard fromFloat (-3.5 : Float) == one

-- ── `toNum`/`fromNum` ──

#guard toNum (a := Int) zero == 0
#guard toNum (a := Int) one == 1
#guard fromNum (a := Int) 0 == zero
#guard fromNum (a := Int) 5 == one
#guard fromNum (a := Int) (-3) == one

-- ── `PixelX Bit` binary pixels ──

#guard on == (⟨one⟩ : PixelX Bit)
#guard off == (⟨zero⟩ : PixelX Bit)
#guard isOn (fromBool true) == true
#guard isOn (fromBool false) == false
#guard isOff (fromBool false) == true
#guard isOff (fromBool true) == false
#guard fromBool true == on
#guard fromBool false == off

-- ── Generic bitwise operators on `PixelX e` ──

/-- A `PixelX Bit` fixture distinct from `on`/`off`, used to exercise the
generic `AndOp`/`OrOp`/`XorOp`/`Complement (PixelX e)` instances. -/
def csBinaryOn : PixelX Bit := on

def csBinaryOff : PixelX Bit := off

#guard (csBinaryOn &&& csBinaryOn) == csBinaryOn
#guard (csBinaryOn &&& csBinaryOff) == csBinaryOff
#guard (csBinaryOn ||| csBinaryOff) == csBinaryOn
#guard (csBinaryOff ||| csBinaryOff) == csBinaryOff
#guard (csBinaryOn ^^^ csBinaryOn) == csBinaryOff
#guard (csBinaryOn ^^^ csBinaryOff) == csBinaryOn
#guard (~~~csBinaryOn) == csBinaryOff
#guard (~~~csBinaryOff) == csBinaryOn
