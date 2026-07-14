# `profunctors` module dependencies

Topological order of every module of the
[`profunctors`](https://hackage.haskell.org/package/profunctors) package
(v5.6.3, source: https://hackage.haskell.org/package/profunctors) to import
into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import
convention. **A prerequisite of [`lens`](../lens/dependencies.md)**: modern
`lens` (≥ 4) encodes every optic (`Iso`, `Lens`, `Prism`, `Traversal`, …) as a
constrained natural transformation between profunctors (`type Lens s t a b =
forall p. Strong p => Optic p s t a b`, etc.), so `Data.Profunctor.*`'s
classes are load-bearing for the port, not incidental.

**Status: done.** All 16 modules ported; `lake build Linen Tests` passes.
This file was written ahead of `lens` itself, per AGENTS.md's recursive rule
("before porting a dependency that itself pulls in further Hackage
libraries, first write its own topologically-ordered dependency list").

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

## Headline finding

None of `Profunctor`/`Strong`/`Choice`/`Closed`/`Traversing`/`Mapping`/
`Sieve`/`Rep`/`Cayley` etc. exist anywhere in the Lean stdlib or already in
`linen` — `linen` has `Bifunctor` (`Linen/Data/Bifunctor.lean`) and
`Contravariant` (`Linen/Data/Functor.lean`), but nothing shaped like a
two-argument, contravariant-in-the-first/covariant-in-the-second profunctor
class. This is a genuine, foundational new port, not a substitution.

## External (non-`profunctors`) dependencies

Resolved against `profunctors-5.6.3.cabal`'s `build-depends`, in
Hackage-import precedence order (Lean stdlib > existing `linen` Haskell port
> new source):

- `base` → `Base` (already ported).
- `transformers` → Lean's own `ExceptT`/`ReaderT`/`StateT`.
- `base-orphans` — back-compat orphan instances for older GHC/`base`
  releases; no Lean analogue needed (Lean has one active stdlib version per
  toolchain, not the multi-`base`-version compatibility matrix this package
  patches over).
- `bifunctors` → `Linen.Data.Bifunctor` (already ported; only the core
  `Bifunctor` class is needed here, for `Data.Profunctor.Cayley`'s and the
  `Data.Profunctor.Types` product/sum profunctor instances).
- `contravariant` → `Linen.Data.Functor.Contravariant` (already ported).
- `comonad` and `distributive` — full upstream `comonad`/`distributive` are
  each multi-module packages (comonad transformers `Env`/`Store`/`Traced`;
  `Distributive` instances for a long list of container types).
  `profunctors` itself only ever uses the bare `Comonad` class (`extract`/
  `extend`, for `Data.Profunctor.Cayley`) and the bare `Distributive` class
  (`distribute`/`collect`, for `Data.Profunctor.Rep`/`.Closed`). Neither is
  in `linen` yet, and porting either package's full module tree for one
  class each would repeat the over-import `duckdb-ffi`'s scope note and
  `hip`'s `Chart`/`Histogram` exclusion both already reject — so the two
  classes are ported directly inside this package's own module tree instead
  of as separate Hackage imports: `Comonad` alongside
  `Linen.Control.Profunctor.Cayley`, `Distributive` alongside
  `Linen.Control.Profunctor.Rep`. If a later import needs the fuller
  `comonad`/`distributive` surface (comonad transformers, more
  `Distributive` instances), promote these to their own
  `docs/imports/comonad`/`docs/imports/distributive` entries then; nothing
  here blocks that.
- `tagged` — `Data.Tagged` is a one-field phantom-typed wrapper
  (`newtype Tagged s b = Tagged b`) used by `instance Choice Tagged`. Ported
  directly as a two-line `structure Tagged (s : Type u) (β : Type v) where
  unTagged : β` inside `Linen.Control.Profunctor.Types` rather than as a
  separate package — the same "narrow, fold it in" treatment `hoauth2`'s
  `dependencies.md` gives `microlens`.

## Topologically sorted `profunctors` modules

Namespace: ported under `Control.Profunctor` (Lean stdlib places typeclass
hierarchies like this under `Control.*`, e.g. `Control.Functor`-shaped
things; upstream's `Data.Profunctor` naming is a Haskell-historical accident
— `Profunctor` is a typeclass over `Type → Type → Type`, the same kind of
thing as `Functor`/`Category`, which Lean's own `Init.Prelude` and this
project's `Linen/Control/*` both place under `Control.*`). Upstream module →
this port's module:

1. `Data.Profunctor.Unsafe` → `Linen.Control.Profunctor.Unsafe` — the base
   `Profunctor` class (`dimap`, plus `lmap`/`rmap` defaults) and its `(->)`
   instance.
2. `Data.Profunctor.Types` → `Linen.Control.Profunctor.Types` — `Star`,
   `Costar`, `WrappedArrow`, `Forget`, and (per the `tagged` note above)
   `Tagged`, all with their `Profunctor` instances. Depends on #1.
3. `Data.Profunctor.Strong` → `Linen.Control.Profunctor.Strong` — `Strong`
   class (`first'`/`second'`); backs `Control.Lens.Lens`. Depends on #1, #2.
4. `Data.Profunctor.Choice` → `Linen.Control.Profunctor.Choice` — `Choice`
   class (`left'`/`right'`); backs `Control.Lens.Prism`. Depends on #1, #2.
5. `Data.Profunctor.Sieve` → `Linen.Control.Profunctor.Sieve` — `Sieve`
   class (`sieve : p a b -> a -> f b`), relates a profunctor to a covariant
   functor it "runs into" (`Star`'s case). Depends on #1, #2.
6. `Data.Profunctor.Rep` → `Linen.Control.Profunctor.Rep` — `Representable`
   class (`Distributive`-backed factoring of `Star`); ported alongside the
   folded-in `Distributive` class per the note above. Depends on #1, #2, #5.
7. `Data.Profunctor.Closed` → `Linen.Control.Profunctor.Closed` — `Closed`
   class (`closed : p a b -> p (x -> a) (x -> b)`); backs
   `Control.Lens.Internal.Zoom`'s function-space handling. Depends on #1, #2.
8. `Data.Profunctor.Traversing` → `Linen.Control.Profunctor.Traversing` —
   `Traversing` class (`traverse'`, `wander`); the direct profunctor
   counterpart of `Control.Lens.Traversal`'s van Laarhoven encoding. Depends
   on #3, #4.
9. `Data.Profunctor.Mapping` → `Linen.Control.Profunctor.Mapping` —
   `Mapping` class (`map'`), the `Setter`-side dual of `Traversing`. Depends
   on #7, #8.
10. `Data.Profunctor.Composition` → `Linen.Control.Profunctor.Composition` —
    `Procompose`/`Rift`, profunctor composition. Depends on #1, #2.
11. `Data.Profunctor.Adjunction` → `Linen.Control.Profunctor.Adjunction` —
    `Adjunction` class relating a profunctor to an adjoint functor pair.
    Depends on #6, #10.
12. `Data.Profunctor.Cayley` → `Linen.Control.Profunctor.Cayley` —
    `Cayley`/applicative-functor-indexed profunctor product, plus the
    folded-in `Comonad` class per the note above. Depends on #1, #2.
13. `Data.Profunctor.Monad` → `Linen.Control.Profunctor.Monad` —
    `ProfunctorFunctor`/`ProfunctorMonad`/`ProfunctorComonad` classes over
    profunctor transformers. Depends on #1, #2.
14. `Data.Profunctor.Ran` → `Linen.Control.Profunctor.Ran` — `Ran`/`Rift`,
    profunctor right Kan extension. Depends on #1, #2, #10.
15. `Data.Profunctor.Yoneda` → `Linen.Control.Profunctor.Yoneda` —
    `Yoneda`/`Coyoneda` for profunctors (distinct from, but analogous to,
    `Data.Functor.Yoneda` used inside `lens`'s own `Internal.Bazaar`/
    `Magma`). Depends on #1, #2.
16. `Data.Profunctor` → `Control.Profunctor.Basic` (a thin re-export facade;
    named to avoid colliding with the `Control.Profunctor` namespace root
    itself) → re-exports #1–#9 (the classes `lens` actually consumes).
    Depends on all of the above.

**Scope note.** `lens` itself (checked against its own `build-depends`
version range `profunctors >= 5.5.2 && < 6`) only actually calls into
`Unsafe`/`Types`/`Strong`/`Choice`/`Closed`/`Traversing`/`Mapping`/`Sieve`/
`Rep` (modules #1–#9 above); `Composition`/`Adjunction`/`Cayley`/`Monad`/
`Ran`/`Yoneda` (#10–#15) are category-theoretic extras with no call site
anywhere in `lens`'s own source. They are still listed above (this file
covers the whole package, per AGENTS.md's convention of listing every
module), but when actually porting, #10–#15 can be deferred past whatever
`lens` needs first without blocking it.
