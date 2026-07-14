/-
  Linen.Data.List.Lens — `_head`, `_tail`, `_init`, `_last`, specialized to
  Lean stdlib `List`

  Port of Hackage's `lens-5.3.6`'s `Data.List.Lens` (fetched and read via
  Hackage's rendered source). Upstream's real content is a thin,
  `List`-specialized re-export:

  ```
  _head :: Traversal' [a] a
  _head = _Cons . _1

  _tail :: Traversal' [a] [a]
  _tail = _Cons . _2

  _init :: Traversal' [a] [a]
  _init = _Snoc . _1

  _last :: Traversal' [a] a
  _last = _Snoc . _2
  ```

  — i.e. exactly `Control.Lens.Cons`'s own `_head`/`_tail`/`_init`/`_last`,
  restated at the concrete type `[a]` for discoverability in `Data.List`'s
  own namespace. `Linen.Control.Lens.Cons` already gives `Cons (List A) B A
  (List B)` and `Snoc (List A) B A (List B)` instances
  (`instConsList`/`instSnocList`), so `Linen.Control.Lens.Cons`'s own
  `_head`/`_tail`/`_init`/`_last` already specialize correctly to `List`
  when called at that type — no new instance is needed here, only (mirroring
  upstream's own module) the same specialization restated under `Linen.
  Data.List.Lens`'s name for parity with every other per-container module in
  this batch. -/

import Linen.Control.Lens.Cons

namespace Data.List.Lens

/-- `_head :: Traversal' [a] a` — `Control.Lens._head` specialized to
    `List`. -/
@[inline] def _head {A : Type u} : Control.Lens.Traversal' (List A) A :=
  Control.Lens._head

/-- `_tail :: Traversal' [a] [a]` — `Control.Lens._tail` specialized to
    `List`. -/
@[inline] def _tail {A : Type u} : Control.Lens.Traversal' (List A) (List A) :=
  Control.Lens._tail

/-- `_init :: Traversal' [a] [a]` — `Control.Lens._init` specialized to
    `List`. -/
@[inline] def _init {A : Type u} : Control.Lens.Traversal' (List A) (List A) :=
  Control.Lens._init

/-- `_last :: Traversal' [a] a` — `Control.Lens._last` specialized to
    `List`. -/
@[inline] def _last {A : Type u} : Control.Lens.Traversal' (List A) A :=
  Control.Lens._last

end Data.List.Lens
