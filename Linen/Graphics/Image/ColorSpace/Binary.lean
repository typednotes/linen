/-
  Linen.Graphics.Image.ColorSpace.Binary — bit-valued binary pixels, built on
  `X` (module #10), for thresholding/morphology

  ## Haskell equivalent
  `Graphics.Image.ColorSpace.Binary` from https://hackage.haskell.org/package/hip
  (module #11 of the `hip` import plan, see `docs/imports/hip/dependencies.md`,
  which lists it as depending only on `X` (#10)).

  Upstream's export list is exactly `Bit(..), on, off, isOn, isOff, fromBool,
  zero, one, bit2bool, bool2bit, toNum, fromNum` — every one of those is
  ported below, plus the `Bits`-class instances upstream gives to `Bit` and
  to `Pixel X Bit` (see the "bitwise operators" section below for how those
  are ported).

  ## Representation decision: `Bit` as a `Bool` wrapper

  Upstream's `Bit` is `newtype Bit = Bit Word8`, an *unconstrained* `Word8`
  that every function in this module takes great care to keep at exactly `0`
  or `1` (`zero`/`one`, `bool2bit`, `fromInteger`, `fromDouble`, …) — the
  `Word8` carrier is upstream's way of getting `Storable`/`Data.Vector.Unbox`
  for free, not a meaningful design choice about *value range*. This port
  drops `Storable`/`Unbox` entirely (per the package-wide scope note in
  `dependencies.md`: GHC FFI/vector-backing machinery with no Lean
  counterpart), which removes the only reason to keep a wider carrier type
  around. With that reason gone, `Bit` is represented directly as a `Bool`
  wrapper (`structure Bit where b : Bool`): every upstream case split on
  `Bit 0`/`Bit _` becomes a case split on `false`/`true`, `Bit`'s `Bits`
  instance's four core operators (`.&.`/`.|.`/`xor`/`complement`) become
  `Bool`'s native `&&`/`||`/`Bool.xor`/`!`, and the type itself statically
  rules out the "some other `Word8` value snuck in" case upstream's
  constructor could never actually produce anyway. This matches the
  `AGENTS.md` Hackage-import guidance to prefer the representation that
  keeps the port faithful without carrying over GHC-specific plumbing
  (`Word8`-for-`Storable`) that has no work left to do once that plumbing is
  dropped.

  ## Bitwise operators: `Data.Bits` → Lean's four core operator classes

  Haskell's `Bits` class has many methods; Lean's standard library does not
  have one combined counterpart, but it does have four separate one-method
  classes for exactly the operators upstream's `Bit`/`Pixel X Bit` instances
  actually give non-default, non-numeric-generic bodies to: `AndOp`
  (`.&.` → `&&&`), `OrOp` (`.|.` → `|||`), `XorOp` (`xor` → `^^^`), and
  `Complement` (`complement` → `~~~`). This is the same "split a Haskell
  class into the several narrower Lean classes that cover exactly what's
  used" pattern already applied to `Num` throughout this port (e.g.
  `Linen.Graphics.Image.Interface.Elevator`'s own doc-comment, and
  `Linen.Graphics.Image.ColorSpace.X`'s `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat`
  split). `shift`/`rotate`/`testBit`/`bit`/`zeroBits`/`bitSizeMaybe`/
  `bitSize`/`isSigned`/`popCount` have no Lean stdlib counterpart at all (no
  generic shift/rotate/bit-count class exists), and — since `Bit`/`Pixel X
  Bit` are always exactly one bit wide by construction — they carry no
  information a caller couldn't already get more directly from the `Bool`
  representation itself (`shift b 0 = b; shift _ _ = Bit 0` is answered
  by `if n == 0 then b else off`, `testBit`/`popCount`/`bit` are already
  answered by `isOn`/`bool2bit`/`b.b`, `zeroBits` by `zero`/`off`). They are
  dropped here as GHC-`Data.Bits`-specific API surface with no current call
  site, the same treatment `Elevator`'s own doc-comment gives to the
  `Elevator` methods it drops for lack of a Lean stdlib counterpart.

  `AndOp`/`OrOp`/`XorOp`/`Complement (PixelX e)` are ported generically over
  any `[AndOp e]`/`[OrOp e]`/`[XorOp e]`/`[Complement e]`, via
  `Graphics.Image.Interface.liftPx`/`liftPx2` — mirroring exactly how
  `X.lean` itself generalises `Add`/`Sub`/`Mul`/`Div`/`Neg`/`OfNat` on
  `PixelX e` over `[Add e]`/etc., rather than upstream's `Bit`-specific
  `Bits (Pixel X Bit)` instance. This is strictly more general (it still
  gives `Bits (Pixel X Bit)`-equivalent instances once `[AndOp Bit]` etc. are
  in scope below) and keeps `PixelX`'s operator instances in one consistent
  style across both modules.

  ## `Ord Bit` — deferred

  Upstream derives `Ord` for `Bit` alongside `Eq`. As with `X.lean`'s own
  deferral of `Ord (Pixel X e)` (see that module's doc-comment): no
  `ColorSpace.*` module ported so far needs a total order on a pixel
  component, so this port does not add one speculatively; it can be added
  faithfully (`false < true`) if and when a later module (e.g. `Processing.
  Binary`'s morphology, module #19) actually needs it.

  ## `Show Bit` — dropped in favour of derived `Repr`

  Upstream's custom `Show Bit` (`show (Bit 0) = "0"; show _ = "1"`) is
  superseded by the derived `Repr`, per the package-wide `Show`→`Repr`
  simplification already used throughout `Y`/`X`/etc.

  ## `Elevator Bit`

  Ported directly against module #2's `Elevator` class: a `false` bit maps
  to every target type's `0`, a `true` bit to every target type's maximum
  representable value (`toWord8`/`toWord16`/`toWord32`/`toWord64`) or `1`
  (`toFloat32`/`toFloat`), matching upstream's `toWord8 (Bit 0) = 0; toWord8
  _ = maxBound` pattern exactly. `fromFloat` matches upstream's `fromDouble 0
  = Bit 0; fromDouble _ = Bit 1`: any nonzero float (positive, negative, or
  fractional) becomes `one`.

  ## `toNum`/`fromNum`

  Upstream's `toNum :: Num a => Bit -> a` and `fromNum :: (Eq a, Num a) => a
  -> Bit` are generic over any `Num a`; ported here against the narrower
  `[OfNat a 0] [OfNat a 1]` (for `toNum`) and `[BEq a] [OfNat a 0]` (for
  `fromNum`) — exactly the fragment of `Num`/`Eq` each function's body
  actually needs, again following the `Num`-splitting convention used
  throughout this port.

  ## `Storable`/`Unbox` instances — dropped

  Per the package-wide scope note in `dependencies.md`: GHC FFI/vector-
  backing machinery with no Lean counterpart.
-/

import Linen.Graphics.Image.ColorSpace.X
import Linen.Graphics.Image.Interface.Elevator

open Graphics.Image.Interface (liftPx liftPx2)
open Graphics.Image.Interface.Elevator (Elevator)
open Graphics.Image.ColorSpace.X (X PixelX)

namespace Graphics.Image.ColorSpace.Binary

-- ── `Bit` — a component type restricted to exactly two values ──

/-- A pixel component restricted to exactly two values, `zero`/`one`
(`off`/`on` once wrapped as a pixel). Upstream's `newtype Bit = Bit Word8`
(constrained by convention to `0`/`1`) — see the module doc-comment for why
this port represents it directly as a `Bool` wrapper instead. -/
structure Bit where
  /-- The underlying boolean value: `false` is upstream's `Bit 0`, `true` is
  upstream's `Bit 1`. -/
  b : Bool
deriving BEq, Repr, Inhabited

/-- Upstream's `zero`, i.e. `Bit 0`. -/
def zero : Bit := ⟨false⟩

/-- Upstream's `one`, i.e. `Bit 1`. -/
def one : Bit := ⟨true⟩

/-- Convert a `Bool` to a `Bit`. Upstream's `bool2bit`. -/
def bool2bit (x : Bool) : Bit := ⟨x⟩

/-- Convert a `Bit` to a `Bool`. Upstream's `bit2bool`. -/
def bit2bool (b : Bit) : Bool := b.b

-- ── Bitwise operators on `Bit` ──
-- See the module doc-comment for why only these four (of upstream's full
-- `Bits` class) are ported.

instance : AndOp Bit where
  and a b := ⟨a.b && b.b⟩

instance : OrOp Bit where
  or a b := ⟨a.b || b.b⟩

instance : XorOp Bit where
  xor a b := ⟨Bool.xor a.b b.b⟩

instance : Complement Bit where
  complement a := ⟨!a.b⟩

-- ── `Num`-style arithmetic on `Bit` ──
-- Upstream's `Num Bit` instance: `(+) = (.|.)`, `(*) = (.&.)`, a custom
-- `(-)`, and `fromInteger 0 = Bit 0 / fromInteger _ = Bit 1`. `negate` is
-- not overridden upstream (it would fall back to the default `Num` method),
-- so no `Neg Bit` instance is ported here — see the module doc-comment's
-- convention of only porting what upstream gives an explicit body.

instance : Add Bit where
  add a b := a ||| b

instance : Sub Bit where
  sub a b := ⟨a.b && !b.b⟩

instance : Mul Bit where
  mul a b := a &&& b

instance {n : Nat} : OfNat Bit n where
  ofNat := ⟨n != 0⟩

-- ── `Elevator Bit` ──

instance : Elevator Bit where
  toWord8 b := if b.b then 255 else 0
  toWord16 b := if b.b then 65535 else 0
  toWord32 b := if b.b then 4294967295 else 0
  toWord64 b := if b.b then 18446744073709551615 else 0
  toFloat32 b := if b.b then 1 else 0
  toFloat b := if b.b then 1 else 0
  fromFloat x := ⟨x != 0⟩

-- ── Generic numeric conversions ──

/-- Convert a `Bit` to any type with `0`/`1` literals: `zero ↦ 0`, `one ↦ 1`.
Upstream's `toNum :: Num a => Bit -> a`, narrowed to the fragment of `Num`
this actually needs — see the module doc-comment. -/
def toNum {a : Type} [OfNat a 0] [OfNat a 1] (x : Bit) : a :=
  if x.b then (1 : a) else (0 : a)

/-- Convert any type with a `0` literal and decidable equality to a `Bit`:
`0 ↦ zero`, anything else `↦ one`. Upstream's `fromNum :: (Eq a, Num a) => a
-> Bit`, narrowed likewise. -/
def fromNum {a : Type} [BEq a] [OfNat a 0] (x : a) : Bit :=
  ⟨x != (0 : a)⟩

-- ── `Pixel X Bit` — a binary pixel ──

/-- Represents value `true`/`1` in binary: a foreground pixel of an object.
Upstream's `on`. -/
def on : PixelX Bit := ⟨one⟩

/-- Represents value `false`/`0` in binary: a background pixel. Upstream's
`off`. -/
def off : PixelX Bit := ⟨zero⟩

/-- Convert a `Bool` to a binary pixel.

```
#guard isOn (fromBool true)
```

Upstream's `fromBool`. -/
def fromBool (x : Bool) : PixelX Bit := ⟨bool2bit x⟩

/-- Test if a binary pixel's value is `on`. Upstream's `isOn`. -/
def isOn (p : PixelX Bit) : Bool := bit2bool p.x

/-- Test if a binary pixel's value is `off`. Upstream's `isOff`. -/
def isOff (p : PixelX Bit) : Bool := !(isOn p)

-- ── Bitwise operators on `PixelX e` ──
-- Ported generically over any `[AndOp e]`/`[OrOp e]`/`[XorOp e]`/
-- `[Complement e]`, giving `Bits (Pixel X Bit)`-equivalent instances once
-- specialised to `e := Bit` above — see the module doc-comment.

instance [Elevator e] [AndOp e] : AndOp (PixelX e) where
  and := liftPx2 (cs := X) (e := e) (· &&& ·)

instance [Elevator e] [OrOp e] : OrOp (PixelX e) where
  or := liftPx2 (cs := X) (e := e) (· ||| ·)

instance [Elevator e] [XorOp e] : XorOp (PixelX e) where
  xor := liftPx2 (cs := X) (e := e) (· ^^^ ·)

instance [Elevator e] [Complement e] : Complement (PixelX e) where
  complement := liftPx (cs := X) (e := e) (~~~ ·)

end Graphics.Image.ColorSpace.Binary
