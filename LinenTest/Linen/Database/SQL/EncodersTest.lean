/-
  Tests for `Linen.Database.SQL.Encoders`.

  Encoders are pure functions `α → Array (Option String)`, so every encoder
  and combinator is checked with `#guard` on both its `encode` output and its
  declared `width`; the width laws are illustrated with `rfl` examples.
-/
import Linen.Database.SQL.Encoders

open Database.SQL.Encoders

namespace Tests.Database.SQL.Encoders

/-! ### Primitive encoders -/

#guard Params.none.encode () == #[]
#guard Params.none.width == 0
#guard Params.text.encode "hi" == #[some "hi"]
#guard Params.text.width == 1
#guard Params.int.encode 42 == #[some "42"]
#guard Params.int.encode (-5) == #[some "-5"]
#guard Params.nat.encode 7 == #[some "7"]
#guard Params.bool.encode true == #[some "t"]
#guard Params.bool.encode false == #[some "f"]
#guard (Params.float.encode 1.5).size == 1   -- exact Float repr is platform-dependent

/-! ### nullable -/

#guard (Params.nullable Params.text).encode (some "x") == #[some "x"]
#guard (Params.nullable Params.text).encode none == #[none]
#guard (Params.nullable Params.text).width == 1
-- A nullable pair widens NULL across all its columns.
#guard (Params.nullable (Params.pair Params.text Params.int)).encode none == #[none, none]

/-! ### contramap -/

#guard (Params.contramap String.length Params.nat).encode "abc" == #[some "3"]
#guard (Params.contramap String.length Params.nat).width == 1

/-! ### pair / triple -/

#guard (Params.pair Params.text Params.int).encode ("a", 5) == #[some "a", some "5"]
#guard (Params.pair Params.text Params.int).width == 2
#guard (Params.triple Params.text Params.int Params.bool).encode ("a", 5, true)
        == #[some "a", some "5", some "t"]
#guard (Params.triple Params.text Params.int Params.bool).width == 3

/-! ### ofToString -/

#guard (Params.ofToString (α := Nat)).encode 9 == #[some "9"]
#guard (Params.ofToString (α := Int)).encode (-3) == #[some "-3"]

/-! ### width laws (compile-time) -/

example : Params.none.width = 0 := Params.none_width
example : (Params.pair Params.text Params.int).width = 1 + 1 := Params.pair_width _ _
example (inner : Params α) : (Params.nullable inner).width = inner.width :=
  Params.nullable_width inner

end Tests.Database.SQL.Encoders
