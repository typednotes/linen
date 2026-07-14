/-
  Linen.Control.Lens.Cons — `Cons`, `Snoc`, `cons`, `uncons`, `_head`,
  `_tail`, `snoc`, `unsnoc`, `_init`, `_last`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Cons` (fetched and read via
  Hackage's rendered Haddock and source). Upstream's real classes:

  ```
  class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _Cons :: Prism s t (a, s) (b, t)

  class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _Snoc :: Prism s t (s, a) (t, b)
  ```

  A `Cons`/`Snoc` instance says "an `s` is isomorphic to either an empty
  container, or (respectively) an element prepended to / appended to a
  smaller one" — the `Prism` that `List.cons`/`::` and snoc-append are built
  from. `S`/`B` are each class's real inputs, `A`/`T` `outParam`s computed
  from them, exactly as in `Linen.Control.Lens.Each`/`Linen.Control.Lens.
  Tuple`'s `Field1`.

  **Composing `_Cons . _1`/`_Cons . _2`/`_Snoc . _1`/`_Snoc . _2` by hand.**
  Upstream defines `_head = _Cons . _1`, `_tail = _Cons . _2`, `_init =
  _Snoc . _1`, `_last = _Snoc . _2`, relying on `(.)` to compose a
  profunctor-polymorphic `Prism` with a concrete `Lens` (`Field1`'s `_1`/
  `_2`) into a concrete `Traversal`. `linen`'s `Prism` is
  profunctor-polymorphic (`∀ p f, (Choice p, Applicative f) => …`,
  `Linen.Control.Lens.Prism`), but `linen`'s `Traversal`/`Lens` are
  deliberately *not* (`Linen.Control.Lens.Type`'s own design choice), and
  `linen` has ported no generic "run a `Prism`, then feed the result through
  a `Lens`" composition combinator — the same gap `Linen.Control.Lens.Prism`
  's own `outside` already works around by using `withPrism` to pull out the
  concrete "build"/"match" functions a `Prism` packages, then builds the
  result by hand. `_head`/`_tail`/`_init`/`_last` below do exactly the same:
  `withPrism _Cons`/`withPrism _Snoc` recovers `bt`/`seta`, and the
  `Traversal` is assembled directly from those, with the same observable
  behaviour as the upstream composition. -/

import Linen.Control.Lens.Prism
import Linen.Control.Lens.Review

namespace Control.Lens

-- ── Cons ────────────────────────────────────────

/-- `class Cons s t a b | … where _Cons :: Prism s t (a,s) (b,t)`: an `s` is
    either empty, or an `a` prepended to a smaller `s`. -/
class Cons (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _Cons : Prism S T (A × S) (B × T)

export Cons (_Cons)

/-- `instance Cons [a] [b] a b where _Cons = prism (uncurry (:)) $ \aas ->
    case aas of (a:as) -> Right (a,as); [] -> Left []`. -/
instance instConsList {A B : Type u} : Cons (List A) B A (List B) where
  _Cons := prism (fun p => p.1 :: p.2) (fun s => match s with
    | a :: as => .inr (a, as)
    | [] => .inl [])

-- ── Snoc ────────────────────────────────────────

/-- `class Snoc s t a b | … where _Snoc :: Prism s t (s,a) (t,b)`: an `s` is
    either empty, or an `a` appended to a smaller `s`. -/
class Snoc (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _Snoc : Prism S T (S × A) (T × B)

export Snoc (_Snoc)

/-- `instance Snoc [a] [b] a b where _Snoc = prism (\(as,a) -> as ++ [a]) $
    \aas -> if null aas then Left [] else Right (init aas, last aas)`.

    Uses `List.getLast?`/`List.dropLast` in place of upstream's partial
    `Prelude.init`/`Prelude.last`, matching `Linen.Control.Lens.Internal.
    List`'s own precedent of a total substitution with identical observable
    behaviour on the (here, already-checked-nonempty) input. -/
instance instSnocList {A B : Type u} : Snoc (List A) B A (List B) where
  _Snoc := prism (fun p => p.1 ++ [p.2]) (fun s => match s.getLast? with
    | none => .inl []
    | some a => .inr (s.dropLast, a))

/-- Lean's native `Array` snoc-appends naturally (`Array.push`/`Array.pop`/
    `Array.back?`), unlike `Cons` (prepending to an `Array` is `O(n)` and has
    no dedicated core primitive) — hence `Array` gets a `Snoc` instance here
    but no `Cons` instance (see the module's scope note). -/
instance instSnocArray {A B : Type u} : Snoc (Array A) B A (Array B) where
  _Snoc := prism (fun p => p.1.push p.2) (fun s => match s.back? with
    | none => .inl #[]
    | some a => .inr (s.pop, a))

-- ── cons / uncons ─────────────────────────────────

/-- `cons :: Cons s s a a => a -> s -> s`: prepend an element —
    `cons = curry (simply review _Cons)`.

    **Scope note (`(<|)`).** Upstream's infix alias reuses the tokens `<|`,
    which Lean's own `Init.Prelude` already binds globally to backward
    function application (`f <| x = f x`, the same role Haskell's `($)`
    plays) — a from-scratch `infixr " <| "` here would silently overload
    that token everywhere `Linen` is imported, making *every* existing use
    of `<|` as pipe-application ambiguous. Skipped; call `cons` directly. -/
@[inline] def cons {S A : Type u} [Cons S A A S] (a : A) (s : S) : S :=
  review (_Cons (S := S) (B := A)) (a, s)

/-- `uncons :: Cons s s a a => s -> Maybe (a, s)`: split off the first
    element, if any — `uncons = simply preview _Cons`, implemented directly
    via `withPrism` (see the module's composition doc comment). -/
@[inline] def uncons {S A : Type u} [Cons S A A S] (s : S) : Option (A × S) :=
  withPrism (_Cons (S := S) (B := A)) (fun _ seta => (seta s).elim (fun _ => none) some)

-- ── _head / _tail ────────────────────────────────

/-- `_head :: Cons s s a a => Traversal' s a`: focus on the first element, if
    any — `_head = _Cons . _1` (see the module's composition doc comment for
    why this is built directly via `withPrism` rather than generic optic
    composition). -/
@[inline] def _head {S A : Type u} [Cons S A A S] : Traversal' S A :=
  fun {F} [Applicative F] afa s =>
    withPrism (_Cons (S := S) (B := A)) (fun bt seta =>
      match seta s with
      | .inl t => pure t
      | .inr (a, rest) => (fun a' => bt (a', rest)) <$> afa a)

/-- `_tail :: Cons s s a a => Traversal' s s`: focus on everything after the
    first element, if any — `_tail = _Cons . _2`. -/
@[inline] def _tail {S A : Type u} [Cons S A A S] : Traversal' S S :=
  fun {F} [Applicative F] afs s =>
    withPrism (_Cons (S := S) (B := A)) (fun bt seta =>
      match seta s with
      | .inl t => pure t
      | .inr (a, rest) => (fun rest' => bt (a, rest')) <$> afs rest)

-- ── snoc / unsnoc ─────────────────────────────────

/-- `snoc :: Snoc s s a a => s -> a -> s`: append an element —
    `snoc = curry (simply review _Snoc)`.

    **Scope note (`(|>)`).** Same clash as `cons`'s `(<|)` note: Lean's
    `Init.Prelude` already binds `|>` globally to forward pipe application
    (`x |> f = f x`). Skipped; call `snoc` directly. -/
@[inline] def snoc {S A : Type u} [Snoc S A A S] (s : S) (a : A) : S :=
  review (_Snoc (S := S) (B := A)) (s, a)

/-- `unsnoc :: Snoc s s a a => s -> Maybe (s, a)`: split off the last
    element, if any — `unsnoc = simply preview _Snoc`. -/
@[inline] def unsnoc {S A : Type u} [Snoc S A A S] (s : S) : Option (S × A) :=
  withPrism (_Snoc (S := S) (B := A)) (fun _ seta => (seta s).elim (fun _ => none) some)

-- ── _init / _last ────────────────────────────────

/-- `_init :: Snoc s s a a => Traversal' s s`: focus on everything before the
    last element, if any — `_init = _Snoc . _1`. -/
@[inline] def _init {S A : Type u} [Snoc S A A S] : Traversal' S S :=
  fun {F} [Applicative F] afs s =>
    withPrism (_Snoc (S := S) (B := A)) (fun bt seta =>
      match seta s with
      | .inl t => pure t
      | .inr (rest, a) => (fun rest' => bt (rest', a)) <$> afs rest)

/-- `_last :: Snoc s s a a => Traversal' s a`: focus on the last element, if
    any — `_last = _Snoc . _2`. -/
@[inline] def _last {S A : Type u} [Snoc S A A S] : Traversal' S A :=
  fun {F} [Applicative F] afa s =>
    withPrism (_Snoc (S := S) (B := A)) (fun bt seta =>
      match seta s with
      | .inl t => pure t
      | .inr (rest, a) => (fun a' => bt (rest, a')) <$> afa a)

end Control.Lens
