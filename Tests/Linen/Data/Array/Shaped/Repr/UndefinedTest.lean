/-
  Tests for `Linen.Data.Array.Shaped.Repr.Undefined` — the `Undefined`
  array representation.

  Only `extent` is exercised with a `#guard`: reading an element is
  intentionally `panic!`-on-read (matching upstream's `error`), so there is
  no total value to illustrate that with `#guard`.
-/
import Linen.Data.Array.Shaped.Repr.Undefined
import Linen.Data.Array.Shaped.Index

open Data.Array.Shaped

namespace Tests.Data.Array.Shaped.Repr.Undefined

private def arr : Undefined DIM2 Nat := ⟨ix2 2 3⟩

#guard Source.extent arr == ix2 2 3

end Tests.Data.Array.Shaped.Repr.Undefined
