# `indexed-traversable` module dependencies

Topological order of every module of the
[`indexed-traversable`](https://hackage.haskell.org/package/indexed-traversable)
package (v0.1.5, source: https://hackage.haskell.org/package/indexed-traversable)
to import into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import
convention. **A prerequisite of [`lens`](../lens/dependencies.md)**:
`Control.Lens.Indexed`/`.Internal.Indexed` and nearly every per-container
instance module (`Data.Map.Lens`, `Data.Sequence.Lens`, `Data.Vector.Lens`, …)
are built directly on `FunctorWithIndex`/`FoldableWithIndex`/
`TraversableWithIndex`.

**Status: done.** All 4 modules ported; `lake build Linen Tests` passes.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

## Headline finding

Lean's own `Foldable`/`Traversable`/`Functor` type classes (stdlib and
`Linen.Data.Traversable`) have no indexed variants — there is no stdlib
`mapWithIndex`-as-a-typeclass-method notion to substitute with. This is a
small (4-module) but genuinely new port; there is no `linen` or stdlib
equivalent to point to instead.

## External (non-`indexed-traversable`) dependencies

- `base` → `Base` (already ported).
- `containers` → `Containers` (already ported; needed for the `Map`/`Seq`
  `WithIndex` instances).
- `transformers` → Lean's own `ExceptT`/`ReaderT`/`StateT`.
- `array` — only used by this package's (flag-gated, pre-`base-4.18`)
  `Data.Foldable1.WithIndex` compatibility shim for arrays predating
  `Foldable1` landing in `base`; `linen` targets one current Lean toolchain,
  so this compatibility path is dropped as a GHC/`base`-version-matrix
  concern with no Lean analogue (the same treatment `base-orphans` gets in
  the `profunctors` entry above).
- `foldable1-classes-compat` (conditional dependency, `base-ge-4-18` flag
  false) — same GHC/`base`-version-compatibility shim, dropped for the same
  reason.

## Topologically sorted `indexed-traversable` modules

Namespace: ported under `Data.*.WithIndex`, mirroring upstream exactly —
these are direct indexed generalizations of the stdlib-shaped
`Functor`/`Foldable`/`Traversable` classes `linen` already places under
`Data.*` (`Linen.Data.Traversable`, `Linen.Data.Foldable`), so the same
namespace convention applies. Upstream module → this port's module:

1. `Data.Functor.WithIndex` → `Linen.Data.Functor.WithIndex` — `FunctorWithIndex i f` class
   (`mapWithIndex : (i -> a -> b) -> f a -> f b`) plus instances for `[]`,
   `Option`/`Maybe`, `Prod`, `Std.HashMap`, `Linen.Data.Map`,
   `Linen.Data.IntMap`.
2. `Data.Foldable.WithIndex` → `Linen.Data.Foldable.WithIndex` —
   `FoldableWithIndex i f` class (`foldrWithIndex`/`ifoldMap`) plus the same
   instance set. Depends on #1 for the shared index type per instance (no
   direct import edge, but same instance surface).
3. `Data.Foldable1.WithIndex` → `Linen.Data.Foldable1.WithIndex` —
   `Foldable1WithIndex`, the non-empty-witnessing variant
   (`ifoldMap1 :: Semigroup m => (i -> a -> m) -> f a -> m`), for containers
   statically known to be non-empty. Depends on #2.
4. `Data.Traversable.WithIndex` → `Linen.Data.Traversable.WithIndex` —
   `TraversableWithIndex i t` class (`itraverse`), the one
   `Control.Lens.Indexed`/`Control.Lens.Traversal` actually build indexed
   optics on top of. Depends on #1, #2.

**Scope note.** `lens` itself only ever needs `FunctorWithIndex`/
`FoldableWithIndex`/`TraversableWithIndex` (#1, #2, #4); `Foldable1WithIndex`
(#3) has no call site in `lens`'s own source (checked against `lens`'s
`build-depends`, which pins `indexed-traversable >= 0.1 && < 0.2`, a range
that predates `indexed-traversable-instances`' `Foldable1`-adjacent additions
being load-bearing anywhere in `lens`). #3 can be deferred without blocking
`lens`.

## `indexed-traversable-instances` folded in, not a separate entry

Upstream `lens` also depends on `indexed-traversable-instances` (adds
`WithIndex` instances for `Data.Sequence.Seq`, `Data.Tree.Tree`,
`Data.HashMap`, etc. — split out from the base package only to avoid
`indexed-traversable` itself depending on `containers`/`unordered-containers`
for GHC-boot-library reasons that don't apply to Lean). Its instances are
folded directly into modules #1/#2/#4 above rather than kept as a separate
package/entry, the same "one wrapper, not two Hackage entries for what's
really one concern" treatment the top-level index gives `direct-sqlite`
folded into `sqlite-simple`. `linen` has no `Data.Tree`/`Data.Sequence` port
yet (see the `lens` `dependencies.md`'s scope note on `Data.Tree.Lens`/
`Data.Sequence.Lens`), so only the `Std.HashMap`/`Linen.Data.Map`/
`Linen.Data.IntMap` instances apply for now; the rest can be added once (if)
those containers are ported.
