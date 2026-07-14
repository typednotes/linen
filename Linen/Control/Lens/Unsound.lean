/-
  Linen.Control.Lens.Unsound — `lensProduct`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Unsound` (fetched and read
  via Hackage's rendered Haddock and source). Upstream's own module-level
  warning, carried over verbatim:

  > One commonly asked question is: can we combine two lenses, `Lens' a b`
  > and `Lens' a c` into `Lens' a (b, c)`. This is fair thing to ask, but
  > such operation is unsound in general. See `lensProduct`.

  and `lensProduct`'s own doc comment, likewise carried over verbatim:

  > A lens product. There is no law-abiding way to do this in general.
  > Result is only a valid `Lens` if the input lenses project disjoint parts
  > of the structure `s`. Otherwise "you get what you put in" law
  >
  > `view l (set l v s) ≡ v`
  >
  > is violated by
  >
  > ```
  > let badLens :: Lens' (Int, Char) (Int, Int); badLens = lensProduct _1 _1
  > view badLens (set badLens (1,2) (3,'x'))
  > ```
  > giving `(2,2)`, but we should get `(1,2)`.

  **Scope note (`prismSum`, `adjoin`).** Upstream's other two combinators
  are out of scope for this port: `prismSum :: APrism s t a b -> APrism s t
  c d -> Prism s t (Either a c) (Either b d)` needs `Prism`/`APrism`
  (`Control.Lens.Prism`, not yet ported at the public-API level — see
  `Linen.Control.Lens.Type`'s own scope note); `adjoin :: Traversal' s a ->
  Traversal' s a -> Traversal' s a` is implemented upstream via `partsOf`,
  `both`, and `each` (`Control.Lens.Traversal`/`Control.Lens.Each`, also not
  yet ported), none of which this batch's scope includes. Both are left for
  whichever later batch ports those modules.

  **Scope note (`ALens'`/`view`/`set` vs. a direct `Lens'`).** Upstream
  types `lensProduct :: ALens' s a -> ALens' s b -> Lens' s (a, b)`,
  reusing the `ALens'`/`(^#)`/`(#~)` replaying machinery so that a lens
  already instantiated at one functor can be safely reused twice inside a
  single build. `linen` has ported no `ALens`/`cloneLens` (see
  `Linen.Control.Lens.Lens`'s scope note for why — `ALens` exists only to
  support exactly this kind of "run a `Lens` more than once" replay, and
  nothing in `linen`'s ported scope needs it elsewhere), so `lensProduct` is
  typed here directly at `Lens' s a -> Lens' s b -> Lens' s (a, b)` instead,
  built from `view`/`set` rather than `(^#)`/`(#~)` — the same semantics,
  since a plain `Lens'` can already be run through `view`/`set` as many
  times as needed with no replaying required. -/

import Linen.Control.Lens.Getter
import Linen.Control.Lens.Setter
import Linen.Control.Lens.Lens

namespace Control.Lens

-- ── lensProduct ─────────────────────────────────

/-- `lensProduct :: ALens' s a -> ALens' s b -> Lens' s (a, b)`: combine two
    lenses focused on (supposedly) disjoint parts of the same structure `s`
    into a single lens focused on both parts at once, reading/writing them
    together as a pair —
    `lensProduct l1 l2 f s = (\(a,b) -> s & l1 .~ a & l2 .~ b) <$> f (view
    l1 s, view l2 s)`.

    **This does not satisfy the lens laws in general** — see the module
    doc-comment. It is only lawful when `l1` and `l2` project disjoint parts
    of `s`; otherwise "you get what you put in" fails, e.g. `lensProduct _1
    _1` on `(Int, Char)` (both lenses aimed at the *same* field). Are you
    looking for `Control.Lens.Lens.alongside`? -/
@[inline] def lensProduct {S A B : Type u} (l1 : Lens' S A) (l2 : Lens' S B) : Lens' S (A × B) :=
  fun {F} [Functor F] (f : A × B → F (A × B)) (s : S) =>
    Functor.map (fun (a, b) => s |> set l1 a |> set l2 b) (f (view l1 s, view l2 s))

end Control.Lens
