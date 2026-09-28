/-
  Tests for `Linen.Graphics.Image.ColorSpace.Complex` — the deferred
  `Elevator (Complex e)` instance, and the `(+:)`/`realPart`/`imagPart`/
  `conjugate`/`magnitude`/`phase`/`mkPolar`/`cis`/`polar` pixel-level
  operations (ported as `mkComplexPx`/`realPartPx`/`imagPartPx`/
  `conjugatePx`/`magnitudePx`/`phasePx`/`mkPolarPx`/`cisPx`/`polarPx`, plus
  their `Float32` counterparts).

  Fixture/example names are prefixed `csComplex` to avoid clashing with any
  other test file's identifiers in the shared `Tests` namespace (in
  particular `Tests.Linen.Graphics.Image.ColorSpace.YTest`/`RGBTest`, whose
  `PixelY`/`PixelRGB` fixtures this file also builds on top of).
-/
import Linen.Graphics.Image.ColorSpace.Complex
import Linen.Graphics.Image.ColorSpace.Y
import Linen.Graphics.Image.ColorSpace.RGB
import Linen.Data.Complex

open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.Y (Y PixelY)
open Graphics.Image.ColorSpace.RGB (RGB PixelRGB)
open Graphics.Image.ColorSpace.Complex
open Data (Complex)

-- ── `Elevator (Complex e)` — discards the imaginary part ──

def csComplexU8 : Complex UInt8 := ⟨(255 : UInt8), (10 : UInt8)⟩

#guard Elevator.toWord8 csComplexU8 == 255
#guard Elevator.toFloat csComplexU8 == 1.0
#guard (Elevator.fromFloat (1.0 : Float) : Complex UInt8) == ⟨255, 0⟩
#guard (Elevator.fromFloat (0.0 : Float) : Complex UInt8) == ⟨0, 0⟩

-- Widening/narrowing still discards the imaginary part, just like the real
-- component's own `Elevator` instance.
#guard Elevator.toWord16 csComplexU8 == 65535

-- ── `mkComplexPx`/`realPartPx`/`imagPartPx` — generic over any `Elevator e` ──

def csComplexRe : PixelRGB Int := ⟨1, 2, 3⟩
def csComplexIm : PixelRGB Int := ⟨4, 5, 6⟩
def csComplexRGB : PixelRGB (Complex Int) := ⟨⟨1, 4⟩, ⟨2, 5⟩, ⟨3, 6⟩⟩

#guard (mkComplexPx (cs := RGB) csComplexRe csComplexIm : PixelRGB (Complex Int)) == csComplexRGB
#guard (realPartPx (cs := RGB) csComplexRGB : PixelRGB Int) == csComplexRe
#guard (imagPartPx (cs := RGB) csComplexRGB : PixelRGB Int) == csComplexIm

-- Single-channel colour space works exactly the same way.
def csComplexYRe : PixelY Int := ⟨7⟩
def csComplexYIm : PixelY Int := ⟨9⟩
def csComplexYPx : PixelY (Complex Int) := ⟨⟨7, 9⟩⟩

#guard (mkComplexPx (cs := Y) csComplexYRe csComplexYIm : PixelY (Complex Int)) == csComplexYPx
#guard (realPartPx (cs := Y) csComplexYPx : PixelY Int) == csComplexYRe
#guard (imagPartPx (cs := Y) csComplexYPx : PixelY Int) == csComplexYIm

-- ── `conjugatePx` ──

#guard (conjugatePx (cs := RGB) csComplexRGB : PixelRGB (Complex Int)) ==
  ⟨⟨1, -4⟩, ⟨2, -5⟩, ⟨3, -6⟩⟩
#guard (conjugatePx (cs := Y) csComplexYPx : PixelY (Complex Int)) == ⟨⟨7, -9⟩⟩

-- ── Scalar polar-form helpers ──

#guard magnitudeOf (⟨3.0, 4.0⟩ : Complex Float) == 5.0
#guard phaseOf (⟨1.0, 0.0⟩ : Complex Float) == 0.0
-- The magnitude-zero case: phase is defined to be `0`, per upstream.
#guard phaseOf (⟨0.0, 0.0⟩ : Complex Float) == 0.0
#guard mkPolarOf 5.0 0.0 == (⟨5.0, 0.0⟩ : Complex Float)
#guard cisOf 0.0 == (⟨1.0, 0.0⟩ : Complex Float)

#guard magnitudeOfF32 (⟨3.0, 4.0⟩ : Complex Float32) == 5.0
#guard phaseOfF32 (⟨1.0, 0.0⟩ : Complex Float32) == 0.0
#guard mkPolarOfF32 5.0 0.0 == (⟨5.0, 0.0⟩ : Complex Float32)
#guard cisOfF32 0.0 == (⟨1.0, 0.0⟩ : Complex Float32)

-- ── Polar form, lifted to pixels (double precision) ──

def csComplexPolarPx : PixelY (Complex Float) := ⟨⟨3.0, 4.0⟩⟩
def csComplexUnitPx : PixelY (Complex Float) := ⟨⟨1.0, 0.0⟩⟩

#guard (magnitudePx (cs := Y) csComplexPolarPx : PixelY Float) == ⟨5.0⟩
#guard (phasePx (cs := Y) csComplexUnitPx : PixelY Float) == ⟨0.0⟩
#guard (polarPx (cs := Y) csComplexUnitPx : PixelY Float × PixelY Float) == (⟨1.0⟩, ⟨0.0⟩)
#guard (mkPolarPx (cs := Y) (⟨5.0⟩ : PixelY Float) (⟨0.0⟩ : PixelY Float) :
  PixelY (Complex Float)) == ⟨⟨5.0, 0.0⟩⟩
#guard (cisPx (cs := Y) (⟨0.0⟩ : PixelY Float) : PixelY (Complex Float)) == ⟨⟨1.0, 0.0⟩⟩

-- ── Polar form, lifted to pixels (single precision) ──

def csComplexPolarPxF32 : PixelY (Complex Float32) := ⟨⟨3.0, 4.0⟩⟩
def csComplexUnitPxF32 : PixelY (Complex Float32) := ⟨⟨1.0, 0.0⟩⟩

#guard (magnitudePxF32 (cs := Y) csComplexPolarPxF32 : PixelY Float32) == ⟨5.0⟩
#guard (phasePxF32 (cs := Y) csComplexUnitPxF32 : PixelY Float32) == ⟨0.0⟩
#guard (polarPxF32 (cs := Y) csComplexUnitPxF32 : PixelY Float32 × PixelY Float32) ==
  (⟨1.0⟩, ⟨0.0⟩)
#guard (mkPolarPxF32 (cs := Y) (⟨5.0⟩ : PixelY Float32) (⟨0.0⟩ : PixelY Float32) :
  PixelY (Complex Float32)) == ⟨⟨5.0, 0.0⟩⟩
#guard (cisPxF32 (cs := Y) (⟨0.0⟩ : PixelY Float32) : PixelY (Complex Float32)) ==
  ⟨⟨1.0, 0.0⟩⟩
