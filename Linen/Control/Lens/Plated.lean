/-
  Linen.Control.Lens.Plated — `Plated`, `children`, `transform`,
  `transformOf`, `transformM`, `transformMOf`, `rewrite`, `rewriteOf`,
  `universe`, `universeOf`, `cosmos`, `cosmosOf`, `holes`, `holesOf`,
  `contexts`, `contextsOf`, `para`, `paraOf`, `composOpFold`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Plated` (fetched and read via
  Hackage's rendered Haddock and source; only the core, load-bearing subset
  is ported — see the scope notes below for what is skipped and why).
  `Plated a` marks a type as "self-similar": a value of type `a` may contain
  further values of type `a` as immediate children, reachable through a
  single `Traversal' a a` (upstream's `plate`).

  **Deviation (`plate`'s default).** Upstream's default is `plate = uniplate`,
  itself built on a `Data a` (SYB-generic) constraint. `linen` has ported no
  generic-programming machinery to derive that default from, so the default
  here is `ignored` — "no self-similar substructure at all" — matching
  upstream's own fallback behaviour for a type with no natural recursive
  shape (e.g. `Int`, `Bool`), and requiring every genuinely self-similar type
  to write its own `plate` by hand (pattern-matching each constructor and
  applying the traversal function to its recursive fields via `<$>`/`<*>`/
  `pure`), exactly as upstream's own hand-written `Plated` instances already
  look before any generic derivation kicks in.

  **Termination (real proof, not fuel).** Every combinator below that
  recurses through an arbitrary caller-supplied `plate`/`Traversal'` —
  `transformOf`, `transformMOf`, `rewriteOf`, `universeOf`, `cosmosOf`,
  `contextsOf`, `paraOf` — faces the same fundamental issue upstream's own
  unbounded Haskell recursion never has to answer for: `plate` is an
  *opaque* `Traversal'` (a rank-2 function argument), not a constructor
  pattern-match Lean's termination checker can see through. A first version
  of this module dodged that with an explicit `fuel : Nat` bound — exactly
  the pattern `AGENTS.md` forbids. This version replaces it with a genuine
  decrease witness: every one of these combinators now takes an explicit
  hypothesis `hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a` (or, for
  the `Plated`-class-based combinators with no explicit `l`, the class
  itself now carries this proof as the field `plate_decreasing`) — a real,
  caller-discharged obligation that the children `l`/`plate` actually visits
  are always strictly smaller than their parent, not a numeric budget that
  silently truncates.

  **Why `WellFounded.fix` directly, not `termination_by`/`decreasing_by`.**
  The natural way to *use* `hDec` is to extract the concrete list of
  children via `toListOf l a : List A` (already computable — no recursion
  needed for the extraction itself), recurse over that list's elements
  (each paired with its own membership proof via `List.attach`), and
  rebuild. Lean's ordinary `termination_by`/`decreasing_by` equation-compiler
  machinery, however, does not expose a function's own *fixed* (non-varying)
  parameters — such as `hDec` itself — inside the tactic context once the
  recursive call sits inside a closure passed to `List.map` (confirmed
  experimentally: a `decreasing_by` goal generated for a call inside
  `(xs.attach).map fun ⟨c, hc⟩ => f hDec c` simply does not list `hDec` among
  its hypotheses, even though it is referenced in the very term being
  elaborated — an elaborator limitation, not a mathematical one). Every
  combinator that needs to recurse into a *list* of children (as opposed to
  a single scalar argument) is therefore built on one shared primitive,
  `foldChildrenOf`, written directly against `WellFounded.fix`, where the
  induction hypothesis `ih` is supplied by hand and `hDec` is an ordinary,
  fully-accessible local variable throughout. (The one combinator that
  *does* recurse on a single scalar argument, `rewriteFix`, has no such
  problem and uses the ordinary `termination_by`/`decreasing_by` macros
  directly — confirmed working the same way, with the caller's hypothesis
  visible in context.)

  **Why every one of these combinators takes an explicit `[SizeOf A]`
  instance.** `sizeOf` is resolved by *typeclass search*, and for a fully
  abstract, unconstrained `A : Type u` type parameter with no `[SizeOf A]`
  argument in scope, the only instance Lean can find is the universal
  fallback `instSizeOfDefault` (`sizeOf _ := 0` for any type). A definition
  elaborated against that fallback has `sizeOf` baked in as the constant-`0`
  function *for every instantiation*, not re-resolved per call site — so an
  `hDec` hypothesis stated without an explicit `[SizeOf A]` parameter would
  be phrased in terms of `instSizeOfDefault`'s `0 < 0` (never satisfiable),
  not a real type's own genuine structural size, making the whole obligation
  meaningless. Every combinator below therefore takes `[SizeOf A]`
  explicitly, so that at each call site the caller's own concrete type's
  real (usually auto-derived) `SizeOf` instance is the one `sizeOf`
  resolves to, both in `hDec`'s statement and in the well-founded recursion
  built from it.

  (Separately: on this toolchain, calling `sizeOf`/`SizeOf.sizeOf` so as to
  *evaluate* it to a printed/compared `Nat` value for a self-referential
  inductive currently fails at run time with "Failed to find LCNF signature"
  — a toolchain-level code-generation gap in auto-derived `SizeOf`
  instances, confirmed to reproduce even for a two-constructor scratch type
  with no relation to this module. This does not affect anything here:
  every use of `sizeOf` below is confined to the *type* of a `Prop`-valued
  hypothesis (`hDec`/`plate_decreasing`), which is erased at run time by
  proof irrelevance and never actually evaluated — confirmed working
  end-to-end via `#guard` in this module's own tests.)

  **Honest scope note (`rewriteFuel`, renamed `rewriteFix`).** The inner
  fixed-point search every `rewriteOf` node runs — "keep applying rule `f`
  to a node as long as it fires" — is *not* structurally decreasing in
  `toListOf l a`'s children the way the rest of this module's recursion is:
  applying `f` rewrites a node in place, and nothing about `Plated`/`f`'s
  general shape guarantees the rewritten node is smaller by any fixed
  measure (a well-behaved rule shrinks its target in practice, but that is
  a property of the *specific* `f`, not something `rewriteOf`'s type can
  see). Upstream's own unbounded `go x = maybe x go (f x)` has exactly the
  same gap — a pathological `f` (e.g. one that always fires) loops forever
  in Haskell too. Per `AGENTS.md`'s own carve-out for exactly this situation
  ("simplifications are for cases upstream itself doesn't fully specify" —
  non-termination of a user-supplied rewrite rule is precisely that), the
  honest fix is to make the caller supply their *own* well-founded measure
  witnessing that their specific `f` terminates: `rewriteFix` takes an
  explicit `measure : A → Nat` and `hDec : ∀ a a', f a = some a' → measure
  a' < measure a`. This is a real, checked, total function — not fuel, and
  not `partial` — it simply moves the (unavoidable, upstream-shared)
  responsibility for proving *this specific rule* terminates to whoever
  supplies the rule, exactly where the actual termination fact lives.

  **Scope note (`rewriteMFuel`/`rewriteMOf`/`rewriteM`, dropped).** The
  monadic sibling of the fixed-point search above has no equally honest
  fix available without additional machinery this port does not have.
  `rewriteFix`'s witness works because, at the point a recursive call is
  made, the *actual* value `a'` produced by `f a` is already in hand (a
  concrete pattern-match on `Option A`), so `hDec a a' h` directly discharges
  the obligation. For `f : A → M (Option A))` with an arbitrary `Monad M`,
  there is no way to state an equally usable hypothesis: something like
  `f a = pure (some a')` is not what is actually known after running `f a`
  inside a `do`-block for a general `M` (running an action and observing its
  result is not the same as that action being *equal to* `pure` of that
  result, for effectful `M` such as `IO`/`StateT`), and a genuinely general
  treatment needs a postcondition-style predicate transformer (e.g.
  something in the shape of `SatisfiesM`) that `linen` has not ported
  anywhere. Since this combinator has no other call site in this batch's
  scope, it is dropped here rather than kept fuelled or falsified with an
  unusable hypothesis — the same treatment this codebase gives any
  combinator with "no honest terminating total-function formulation" (see
  e.g. this module's own `plate`-default note, or `Control.Lens.Traversal`'s
  `Backwards` scope note, for other examples of a documented, reasoned
  skip).

  **Scope note (`rewriteOn`/`rewriteOnOf`/`rewriteMOn`/`rewriteMOnOf`,
  `transformOn`/`transformOnOf`/`transformMOn`/`transformMOnOf`,
  `universeOn`/`universeOnOf`, `cosmosOn`/`cosmosOnOf`,
  `contextsOn`/`contextsOnOf`, `holesOn`/`holesOnOf`, `paraOf`'s `On`
  cousins).** Every `*On`/`*OnOf` combinator is its plain counterpart
  pre-composed with a `Setter`/`Fold` that first locates a sub-region to
  operate on (`fooOn l = fooOn' l id`-style) — genuinely convenient upstream,
  but each one adds nothing beyond what composing the plain combinator with
  that same `Setter`/`Fold` already gives a caller directly (`transformOnOf
  l f = over l (transform f)`, spelled out). Skipped as pure convenience
  wrappers with no new capability.

  **Scope note (`(...)`, the infix "compose through a plate" operator,
  `deep`, `parts`).** `(...)` and `deep` both need the profunctor-generalized
  `Optical`/`Conjoined`-polymorphic optic shapes `Linen.Control.Lens.Getter`'s
  own `to`/`like` doc comment already explains `linen`'s concrete optics
  cannot host (no `p`-parameter on `Traversal'`/`Fold`, so a combinator
  needing to work across an arbitrary intervening profunctor has no faithful
  home). `parts` needs the reified-traversal `ATraversal`/`overA`-style
  "replay this traversal as a `Lens' s [a]`" machinery `Linen.Control.Lens.
  Traversal`'s own scope note already flags as unavailable (no safe way to
  replay an already-applied optic at a different functor). All three
  skipped for the same underlying reasons those notes already give.

  **Scope note (`gplate`/`gplate1`/`GPlated`/`GPlated1`).** Generic-derivation
  machinery for `plate` via `GHC.Generics`/`Generic1`; `linen` has ported no
  generic-programming infrastructure anywhere (see the `plate` default note
  above), so there is nothing for these to derive from. Skipped. -/

import Linen.Control.Lens.Type
import Linen.Control.Lens.Traversal
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Internal.Context
import Linen.Control.Monad.State

open Control.Lens.Internal Control.Monad.State

namespace Control.Lens

-- ── Plated ──────────────────────────────────────

/-- `Plated a`: a type whose values may contain further values of the same
    type `a` as immediate children, reachable through the single `Traversal'
    a a` method `plate`. See the module's deviation note on why the default
    is `ignored` (no substructure) rather than upstream's `Data`-generic
    `uniplate`, and the module's termination note on why every instance must
    also supply `plate_decreasing`, a genuine proof that `plate` only ever
    visits strictly smaller children. -/
class Plated (A : Type u) [SizeOf A] where
  /-- `plate :: Traversal' a a`: visit every immediate self-similar child. -/
  plate : Traversal' A A := ignored
  /-- A real termination witness: every child `plate` visits is strictly
      `sizeOf`-smaller than its parent. Every `Plated` instance must supply
      this itself (no default is given — see the module's termination note
      for why a default here can only ever be phrased about `plate`'s
      *own* default, `ignored`, not about whatever `plate` an instance
      actually chooses to override it with). -/
  plate_decreasing : ∀ a, ∀ c ∈ toListOf plate a, sizeOf c < sizeOf a

export Plated (plate plate_decreasing)

-- ── children ────────────────────────────────────

/-- `children :: Plated a => a -> [a]`: the immediate self-similar children
    of a value — `children = toListOf plate`. -/
@[inline] def children {A : Type u} [SizeOf A] [Plated A] (a : A) : List A :=
  toListOf plate a

-- ── foldChildrenOf ───────────────────────────────

/-- The shared well-founded recursion every combinator below that recurses
    through an arbitrary caller-supplied `cs : A → List A` (in practice
    always `toListOf l` for some `Traversal' A A`) is built from: compute a
    result for every child of `a` — each one strictly `sizeOf`-smaller than
    `a`, per `hDec` — then combine `a` with its children's already-computed
    results via `step`. See the module's termination note for why this is
    written directly against `WellFounded.fix` (with the induction
    hypothesis `ih` supplied by hand) rather than via `termination_by`/
    `decreasing_by`: the latter's auto-generated tactic goals do not expose
    `hDec` once the recursive call sits inside the `List.attach.map`
    closure this needs. -/
def foldChildrenOf {A : Type u} [SizeOf A] {R : Type u} (cs : A → List A)
    (hDec : ∀ a, ∀ c ∈ cs a, sizeOf c < sizeOf a) (step : A → List R → R) : A → R :=
  WellFounded.fix (measure (sizeOf (α := A))).wf
    (fun a ih => step a ((cs a).attach.map fun ⟨c, hc⟩ => ih c (hDec a c hc)))

-- ── transform / transformOf ─────────────────────

/-- Rebuild `a` with its immediate children replaced by `ds`, in the same
    order `toListOf l a` lists them — the same "run `l` once more at
    `F := State Nat`, threading a position counter" trick `holesOf` (below)
    uses to substitute a single position, generalized to substitute every
    position at once. Falls back to the original child `x` if `ds` runs out
    before every position is visited; this never happens in practice, since
    every call site below supplies a `ds` of exactly `(toListOf l a).length`
    elements, by construction. Restricted to `Type` (rather than
    universe-polymorphic `Type u`) for the same reason `holesOf`/
    `mapAccumLOf`/`scanl1Of` are: it threads a `Control.Monad.State.State`. -/
def substituteChildrenOf {A : Type} (l : Traversal' A A) (a : A) (ds : List A) : A :=
  evalState
    (l (F := State Nat)
      (fun x => do
        let j ← get
        modify (· + 1)
        pure (ds.getD j x))
      a)
    0

/-- `transformOf :: ASetter a b a b -> (b -> b) -> a -> b`: rewrite every
    node in a self-similar structure, bottom-up (children rewritten before
    their parent). See the module's termination note for `hDec`, and
    `substituteChildrenOf`'s note for why this is `Type`-restricted. -/
def transformOf {A : Type} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (f : A → A) (a : A) : A :=
  foldChildrenOf (toListOf l) hDec (fun a ds => f (substituteChildrenOf l a ds)) a

/-- `transform :: Plated a => (a -> a) -> a -> a`: `transformOf plate`,
    threading the instance's own `plate_decreasing` witness. -/
@[inline] def transform {A : Type} [SizeOf A] [Plated A] (f : A → A) (a : A) : A :=
  transformOf plate plate_decreasing f a

-- ── transformM / transformMOf ────────────────────

/-- `transformMOf :: Monad m => LensLike (WrappedMonad m) a b a b -> (b -> m
    b) -> a -> m b`: monadic `transformOf`, threading the effect through
    every child (left to right, via `List.mapM`) before applying `f` to the
    already-transformed node. Shares `transformOf`'s termination note. -/
def transformMOf {A : Type} [SizeOf A] {M : Type → Type} [Monad M] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (f : A → M A) (a : A) : M A :=
  foldChildrenOf (toListOf l) hDec
    (fun a (mds : List (M A)) => do
      let ds ← mds.mapM id
      f (substituteChildrenOf l a ds))
    a

/-- `transformM :: (Monad m, Plated a) => (a -> m a) -> a -> m a`:
    `transformMOf plate`. -/
@[inline] def transformM {A : Type} [SizeOf A] {M : Type → Type} [Monad M] [Plated A]
    (f : A → M A) (a : A) : M A :=
  transformMOf plate plate_decreasing f a

-- ── rewrite / rewriteOf ──────────────────────────

set_option linter.unusedVariables false in
/-- The inner fixed-point search every `rewriteOf` node runs: keep applying
    the rule `f` to a node as long as it fires, stopping when `f` returns
    `none` — upstream's `go x = maybe x go (f x)`. See the module's honest
    scope note on why this needs the *caller* to supply their own
    termination witness for their specific `f` (`measure`/`hDec`), rather
    than any bound this module could derive itself. -/
def rewriteFix {A : Type u} (f : A → Option A) (measure : A → Nat)
    (hDec : ∀ a a', f a = some a' → measure a' < measure a) (a : A) : A :=
  match hf : f a with
  | none => a
  | some a' => rewriteFix f measure hDec a'
termination_by measure a
decreasing_by exact hDec a a' hf

/-- `rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b`: repeatedly
    apply a rewrite rule everywhere in a self-similar structure, bottom-up,
    until no more rewrites apply anywhere — upstream's `go = transformOf l
    (\\x -> maybe x go (f x))`. The structural descent through `l` is
    justified by `hDec`; the per-node fixed-point search is justified by the
    caller's own `measure`/`hDecF` for `f`, per `rewriteFix`'s note.

    The eta-expansion `fun a => rewriteFix f measure hDecF a` (rather than
    passing the partially-applied `rewriteFix f measure hDecF` directly) is
    load-bearing, not stylistic: passing that partial application straight
    into `transformOf` (which closes over it inside `foldChildrenOf`'s
    `WellFounded.fix`) reproducibly crashes Lean 4.31.0's LCNF compiler
    backend (`explicitBoxing.tryCorrectLetDeclType`, "unknown join point")
    whenever the resulting definition is later evaluated (e.g. by `#guard`).
    Eta-expanding one more layer avoids handing the compiler that specific
    closure-of-a-closure shape and sidesteps the crash; both forms are
    definitionally and observably identical. -/
@[inline] def rewriteOf {A : Type} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (f : A → Option A) (measure : A → Nat)
    (hDecF : ∀ a a', f a = some a' → measure a' < measure a) (a : A) : A :=
  transformOf l hDec (fun a => rewriteFix f measure hDecF a) a

/-- `rewrite :: Plated a => (a -> Maybe a) -> a -> a`: `rewriteOf plate`,
    threading the instance's own `plate_decreasing` witness alongside the
    caller's `measure`/`hDecF` for `f`. -/
@[inline] def rewrite {A : Type} [SizeOf A] [Plated A] (f : A → Option A) (measure : A → Nat)
    (hDecF : ∀ a a', f a = some a' → measure a' < measure a) (a : A) : A :=
  rewriteOf plate plate_decreasing f measure hDecF a

-- ── universe / universeOf ────────────────────────

/-- `universeOf :: Getting (Endo [a]) a a -> a -> [a]`: every transitive
    descendant of a value, including the value itself, in pre-order (self,
    then every descendant of each child in turn). See the module's
    termination note for `hDec`, in place of the previous `fuel` bound. -/
def universeOf {A : Type u} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (a : A) : List A :=
  foldChildrenOf (toListOf l) hDec (fun a rs => a :: rs.flatten) a

/-- `universe :: Plated a => a -> [a]`: `universeOf plate`.

    Named `«universe»` (Lean escaped-identifier syntax) since `universe` is a
    reserved keyword (Lean's own `universe ...` declaration command). -/
@[inline] def «universe» {A : Type u} [SizeOf A] [Plated A] (a : A) : List A :=
  universeOf plate plate_decreasing a

-- ── cosmos / cosmosOf ────────────────────────────

/-- `cosmosOf :: (Applicative f, Contravariant f) => LensLike' f a a ->
    LensLike' f a a`: `universeOf`, packaged as a `Fold` so it composes with
    other optics — `cosmosOf d = folding (universeOf d)`. -/
@[inline] def cosmosOf {A : Type u} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) : Fold A A :=
  folding (universeOf l hDec)

/-- `cosmos :: Plated a => Fold a a`: `cosmosOf plate`. -/
@[inline] def cosmos {A : Type u} [SizeOf A] [Plated A] : Fold A A :=
  cosmosOf plate plate_decreasing

-- ── holes / holesOf ──────────────────────────────

/-- `holesOf :: ATraversal' a a -> a -> [Pretext (->) a a a]`: one editable
    "one-hole context" per immediate child — pairing that child's current
    value with a function that rebuilds the whole structure given a
    replacement for just that child, leaving every other child as it was.

    **Deviation from upstream's `Pretext`.** As with `Getter.to`/`Fold.
    filtered`'s identical deviation note, `linen`'s concrete optics are plain
    `LensLike`-shaped functions rather than `Profunctor`-parameterized ones,
    so this lands directly at `Linen.Control.Lens.Internal.Context`'s
    concrete `Context a a a` carrier (the shape `Pretext (->) a a a`
    degenerates to) rather than the fully profunctor-polymorphic `Pretext`.

    **Implementation note.** Built by running `l` once to count the
    children (`toListOf`), then once more per child at `F := State Nat`,
    threading a running position counter that substitutes the replacement
    only at the matching position and leaves every other visited child as
    its original value — an $O(n^2)$ but observably faithful stand-in for
    upstream's $O(n)$ `Bazaar`-zipper implementation (the same trade-off this
    codebase's `Internal.List.stripSuffix` note already documents
    elsewhere). Restricted to `Type` (rather than universe-polymorphic
    `Type u`) since it threads a `Control.Monad.State.State`, matching
    `Linen.Control.Lens.Traversal`'s identical restriction on
    `mapAccumLOf`/`scanl1Of` for the same reason. This combinator needs no
    termination proof of its own: it recurses once, structurally, over a
    single already-computed list (`(List.range cs.length).zip cs`), never
    through `l` a second time in a way that visits children of children. -/
def holesOf {A : Type} (l : Traversal' A A) (a : A) : List (Context A A A) :=
  let cs := toListOf l a
  ((List.range cs.length).zip cs).map fun (i, c) =>
    Context.mk
      (fun b =>
        evalState
          (l (F := State Nat)
            (fun x => do
              let j ← get
              modify (· + 1)
              pure (if j == i then b else x))
            a)
          0)
      c

/-- `holes :: Plated a => a -> [Pretext (->) a a a]`: `holesOf plate`. -/
@[inline] def holes {A : Type} [SizeOf A] [Plated A] (a : A) : List (Context A A A) :=
  holesOf plate a

-- ── contexts / contextsOf ────────────────────────

/-- `contextsOf :: ATraversal' a a -> a -> [Context a a a]`: one editable
    context per transitive subterm (including the value itself), recursively
    descending into every hole `holesOf` finds and composing rebuild
    functions on the way back up. `holesOf l a` and `foldChildrenOf`'s
    recursively-computed sub-contexts are built from the very same ordered
    child list (`toListOf l a`), so zipping them together pairs each hole
    with the recursive contexts of exactly its own child. Shares the
    module's termination note on `hDec`, and `holesOf`'s note on landing at
    the concrete `Context` carrier and the `Type`-only universe
    restriction. -/
def contextsOf {A : Type} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (a : A) : List (Context A A A) :=
  foldChildrenOf (toListOf l) hDec
    (fun a subs =>
      Context.mk id a ::
        ((holesOf l a).zip subs).flatMap fun (ctx, sub) =>
          sub.map fun s => Context.mk (ctx.peek ∘ s.peek) s.pos)
    a

/-- `contexts :: Plated a => a -> [Context a a a]`: `contextsOf plate`. -/
@[inline] def contexts {A : Type} [SizeOf A] [Plated A] (a : A) : List (Context A A A) :=
  contextsOf plate plate_decreasing a

-- ── para / paraOf ────────────────────────────────

/-- `paraOf :: Getting (Endo [a]) a a -> (a -> [r] -> r) -> a -> r`: a
    paramorphism over a self-similar structure — `f` sees both the current
    node and the already-computed results of every immediate child, in
    order. See the module's termination note for `hDec`, in place of the
    previous `fuel` bound. -/
def paraOf {A R : Type u} [SizeOf A] (l : Traversal' A A)
    (hDec : ∀ a, ∀ c ∈ toListOf l a, sizeOf c < sizeOf a) (f : A → List R → R) (a : A) : R :=
  foldChildrenOf (toListOf l) hDec (fun a rs => f a rs) a

/-- `para :: Plated a => (a -> [r] -> r) -> a -> r`: `paraOf plate`. -/
@[inline] def para {A R : Type u} [SizeOf A] [Plated A] (f : A → List R → R) (a : A) : R :=
  paraOf plate plate_decreasing f a

-- ── composOpFold ─────────────────────────────────

/-- `composOpFold :: Plated a => b -> (b -> b -> b) -> (a -> b) -> a -> b`:
    right-fold over the immediate children of a value, mapping each through
    `f` and combining with `c` — one level deep only, no recursion (so no
    termination proof needed). -/
@[inline] def composOpFold {A B : Type u} [SizeOf A] [Plated A] (z : B) (c : B → B → B)
    (f : A → B) (a : A) : B :=
  foldrOf plate (fun a' acc => c (f a') acc) z a

end Control.Lens
