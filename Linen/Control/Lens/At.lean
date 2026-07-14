/-
  Linen.Control.Lens.At — `Ixed`, `At`, `ix`, `at`, `ixAt`, `sans`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.At` (fetched and read
  straight from the real source, `src/Control/Lens/At.hs` at tag `v5.3.6`,
  not recalled from memory). Upstream declares:

  ```
  type family Index (s :: Type) :: Type
  type family IxValue (m :: Type) :: Type

  class Ixed m where
    ix :: Index m -> Traversal' m (IxValue m)

  class Ixed m => At m where
    at :: Index m -> Lens' m (Maybe (IxValue m))

  sans :: At m => Index m -> m -> m
  sans k m = m & at k .~ Nothing
  ```

  `ix` focuses (with `Traversal'`, so possibly missing) the element at a
  given index of a container; `at`, for a `Map`-like container, additionally
  turns "missing" into a first-class `Maybe`/`Option` so a single `Lens` can
  read, insert, update, *and* delete. `Index`/`IxValue` are type families —
  functions from a container type to (respectively) its index type and its
  element type, deterministic in the container type alone. `linen` has no
  type families; following the `outParam` idiom this codebase already
  settled on for the exact same kind of "determined by the first parameter"
  relationship (`Linen.Control.Lens.Tuple`'s `Field1`..`Field9`, whose own
  doc comment explains the idiom), `Index`/`IxValue` become two `outParam`
  type-class parameters `I`/`V` on `Ixed`/`At` themselves, rather than
  associated types. Upstream's method is literally named `at`, a reserved
  tactic-location token in Lean (`rw … at h`); it is declared here as
  `«at»`, escaped with guillemets exactly as this codebase already escapes
  other Haskell names that collide with a Lean keyword (e.g.
  `Linen.Data.ByteString`'s `«break»`).

  **Scope note (batch scope — classes and combinators only, no per-container
  instances).** This batch is restricted to `Ixed`, `At`, `ix`, `at`,
  `ixAt`, and `sans` themselves; the many per-container instances upstream
  defines directly in this same file (`Map`, `IntMap`, `HashMap`, `Set`,
  `IntSet`, `HashSet`, `Seq`, `NonEmpty`, `Tree`, `Array`/`UArray`, the
  `Vector` family, `Text`, `ByteString`, and 2–9-tuples via `each`) are
  deliberately deferred to a later, per-container batch — several of the
  corresponding `linen` container types (e.g. `Data.Map`, `Data.Set`,
  `Data.IntMap`) already exist, but wiring them up here risks clashing with
  other in-flight work on exactly those modules. Only three of upstream's
  own instances are ported below, chosen because each is genuinely a single
  self-contained case needing no container beyond what this module already
  imports (`Option`, `List`, and Lean's own function type), matching
  upstream's own `instance Eq e => Ixed (e -> a)`, `instance Ixed (Maybe
  a)`, `instance Ixed [a]`, and `instance At (Maybe a)`:

  * `Ixed (E → A) E A` — upstream's `instance Eq e => Ixed (e -> a)`.
  * `Ixed (List A) Nat A` — upstream's `instance Ixed [a]`, with `Index`
    narrowed from `Int` (guarded against negative indices) to `Nat` (which
    cannot be negative in the first place).
  * `Ixed (Option A) Unit A` and `At (Option A) Unit A` — upstream's
    `instance Ixed (Maybe a)` / `instance At (Maybe a)`.

  **Scope note (`Contains`/`icontains`, `iix`/`iat`).** Upstream's `Contains`
  class (`contains :: Index m -> Lens' m Bool`, for membership-testing
  containers like `IntSet`/`Set`/`HashSet`) is a third, independent class in
  this same file; it is not `ix`/`at`/`sans`, and every instance of it needs
  exactly the same not-yet-in-scope container types as the deferred `Ixed`/
  `At` instances above, so it is skipped here too, for the later batch to
  pick up alongside them. `iix`/`iat`, upstream's indexed variants of `ix`/
  `at` (`IndexedTraversal'`/`IndexedLens'`-valued, built via `indexed`), are
  likewise skipped — this batch's scope is exactly `ix`/`at`/`sans` (plus
  `ixAt`, upstream's own un-indexed helper connecting the two classes). -/

import Linen.Control.Lens.Lens
import Linen.Control.Lens.Traversal

open Data.Functor

namespace Control.Lens

-- ── Ixed / ix ───────────────────────────────────

/-- `class Ixed m where ix :: Index m -> Traversal' m (IxValue m)`: a
    `Traversal` onto the element (if any) at index `i` of `m` — `M` is the
    class's real input; `I` (the index type) and `V` (the element type) are
    `outParam`s computed from it, modeling upstream's `Index`/`IxValue`
    type families. -/
class Ixed (M : Type u) (I : outParam (Type v)) (V : outParam (Type u)) where
  ix : I → Traversal' M V

export Ixed (ix)

-- ── At / at ─────────────────────────────────────

/-- `class Ixed m => At m where at :: Index m -> Lens' m (Maybe (IxValue
    m))`: a `Lens` onto `Option V`, the presence-or-absence of the element
    at index `i` of `m` — reading it reports whether `i` is present (and
    with what value), writing `some v`/`none` through it inserts/deletes. -/
class At (M : Type u) (I : outParam (Type v)) (V : outParam (Type u))
    extends Ixed M I V where
  «at» : I → Lens' M (Option V)

export At («at»)

-- ── ixAt ────────────────────────────────────────

/-- `ixAt :: At m => Index m -> Traversal' m (IxValue m)`: `ix` is always
    derivable from `at` alone — `ixAt i = at i . traverse` (upstream's
    default implementation of `ix` for any `At` instance): first find the
    `Option V` at index `i`, then (via `traversed`'s `Traversable Option`
    instance) traverse into it only if it is actually present. -/
@[inline] def ixAt {M I V : Type u} [At M I V] (i : I) : Traversal' M V :=
  fun {F} [Applicative F] (f : V → F V) (m : M) => «at» i (traversed f) m

-- ── sans ────────────────────────────────────────

/-- `sans :: At m => Index m -> m -> m`: `sans k m = m & at k .~ Nothing`
    — delete the value (if any) at index `k` of a `Map`-like container. -/
@[inline] def sans {M I V : Type u} [At M I V] (i : I) (m : M) : M :=
  set («at» i) none m

-- ── Instances (upstream's own — functions, `List`, `Option`) ──

/-- `instance Eq e => Ixed (e -> a) where ix e p f = p (f e) <&> \a e' -> if
    e == e' then a else f e'`: a function is "indexed" by its argument;
    writing through `ix e` updates the value at `e` alone, leaving every
    other input untouched. -/
instance instIxedFun {E A : Type u} [DecidableEq E] : Ixed (E → A) E A where
  ix e := fun {F} [Applicative F] (f : A → F A) (g : E → A) =>
    (fun a e' => if e' = e then a else g e') <$> f (g e)

/-- Structural helper for `instIxedList`: writes through the focused element
    at position `i` of a list, leaving every other element untouched, or
    leaves the whole list untouched (as `pure`) if `i` runs past its end —
    matching upstream's `go` local to `instance Ixed [a]`. -/
@[inline] def ixListGo {F : Type u → Type u} [Applicative F] {A : Type u} (f : A → F A) :
    List A → Nat → F (List A)
  | [], _ => pure []
  | a :: as, 0 => (· :: as) <$> f a
  | a :: as, i + 1 => (a :: ·) <$> ixListGo f as i

/-- `instance Ixed [a] where ix k f xs0 = … go xs0 k`: a `List`'s index is
    its position; unlike upstream's `Int` (guarded against `k < 0`), `Nat`
    cannot be negative in the first place, so no such guard is needed here. -/
instance instIxedList {A : Type u} : Ixed (List A) Nat A where
  ix i := fun {F} [Applicative F] (f : A → F A) (xs : List A) => ixListGo f xs i

/-- `instance Ixed (Maybe a) where ix ~() f (Just a) = Just <$> f a; ix ~()
    _ Nothing = pure Nothing`: an `Option`'s only possible "index" is `()`
    (there is nothing to index by), and its element (if any) is its
    contents. -/
instance instIxedOption {A : Type u} : Ixed (Option A) Unit A where
  ix _ := fun {F} [Applicative F] (f : A → F A) (m : Option A) =>
    match m with
    | some a => some <$> f a
    | none => pure none

/-- `instance At (Maybe a) where at ~() f = f`: reading/writing the "value
    at index `()`" of an `Option` is *exactly* reading/writing the `Option`
    itself — `at ()` is the identity lens. -/
instance instAtOption {A : Type u} : At (Option A) Unit A where
  «at» _ := fun {F} [Functor F] (f : Option A → F (Option A)) (m : Option A) => f m

end Control.Lens
