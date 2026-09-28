/-
  Tests for `Linen.Numeric.Lens`.
-/
import Linen.Numeric.Lens

open Control.Lens Numeric.Lens

namespace Tests.Linen.Numeric.Lens

-- `integral`: a `Prism' Int Nat`, embedding `Nat` into `Int`; matches iff
-- the `Int` is non-negative.
#guard withPrism integral (fun bt _ => bt 3) = (3 : Int)
#guard withPrism integral (fun _ seta => seta 5) = Sum.inr 5
#guard withPrism integral (fun _ seta => seta (-1)) = Sum.inl (-1)

-- `base`/`binary`/`octal`/`decimal`/`hex`: show/read a `Nat` in the given
-- base, round-tripping through `String`.
#guard withPrism decimal (fun bt _ => bt 42) = "42"
#guard withPrism decimal (fun _ seta => seta "42") = Sum.inr 42
#guard withPrism decimal (fun _ seta => seta "4x2") = Sum.inl "4x2"

#guard withPrism hex (fun bt _ => bt 0xcafe) = "cafe"
#guard withPrism hex (fun _ seta => seta "cafe") = Sum.inr 0xcafe

#guard withPrism binary (fun bt _ => bt 5) = "101"
#guard withPrism binary (fun _ seta => seta "101") = Sum.inr 5

#guard withPrism octal (fun bt _ => bt 8) = "10"
#guard withPrism octal (fun _ seta => seta "10") = Sum.inr 8

#guard withPrism (base 36 (by decide)) (fun bt _ => bt 1767707668033969) = "helloworld"
#guard withPrism (base 36 (by decide)) (fun _ seta => seta "helloworld") = Sum.inr 1767707668033969

-- an empty string, or one with no digits at all, never matches.
#guard withPrism decimal (fun _ seta => seta "") = Sum.inl ""

-- `adding`/`subtracting`/`negated`: `Iso' Int Int`.
#guard withIso (adding (3 : Int)) (fun sa _ => sa 4) = 7
#guard withIso (adding (3 : Int)) (fun _ bt => bt 7) = 4
#guard withIso (subtracting (3 : Int)) (fun sa _ => sa 7) = 4
#guard withIso (negated (A := Int)) (fun sa _ => sa 5) = -5

-- `multiplying`/`dividing`/`exponentiating`: `Iso' Float Float`. `Float`
-- equality isn't `Decidable` (no `DecidableEq Float`), so these compare via
-- `Float`'s `BEq` (`==`, returning a plain `Bool`) rather than `=`.
#guard withIso (multiplying (2.0 : Float)) (fun sa _ => sa 5.0) == 10.0
#guard withIso (dividing (2.0 : Float)) (fun sa _ => sa 10.0) == 5.0
#guard withIso (exponentiating (2.0 : Float)) (fun sa _ => sa 3.0) == 9.0

end Tests.Linen.Numeric.Lens
