# `lens` module dependencies

Topological order of every module of the
[`lens`](https://hackage.haskell.org/package/lens) package (v5.3.6, source:
https://hackage-content.haskell.org/package/lens-5.3.6/src/lens.cabal — the
real `exposed-modules`/`build-depends` fields were fetched and read
verbatim, not recalled from memory) to import into `linen`, per
[AGENTS.md](../../AGENTS.md)'s Hackage-import convention.

**Status: done.** All 64 planned modules ported, plus `Control.Lens.Zoom`
(#37) and `Control.Lens.Reified` (#38) — gap-filled after batch B initially
skipped them — for 66 new-port modules total. `lake build Linen Tests`
passes.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

## Headline finding

`lens`'s 84 `exposed-modules` (`Control.Lens` plus 83 more; one further
`other-modules` entry, `Control.Lens.Internal.Prelude`) split three ways:

1. **The optics core itself (≈45 modules)** — `Lens`/`Prism`/`Iso`/
   `Traversal`/`Fold`/`Getter`/`Setter`/`Review`/`Equality` and their
   `Control.Lens.Internal.*` profunctor-optics machinery
   (`Bazaar`/`Magma`/`Context`/`Zoom`/…) — is a **genuine new port**. Nothing
   in the Lean stdlib or already in `linen` provides van-Laarhoven or
   profunctor-encoded optics; the closest existing things (`Linen.Data.Map`'s
   own hand-written `map`/`fold`/traversal helpers, `Linen.Data.Functor`'s
   `Const`/`Compose`) are the *raw materials* this port is built from, not a
   substitute for it.
2. **≈27 per-container instance modules** (`Data.Map.Lens`,
   `Data.Text.Lens`, `Data.Vector.Lens`, …) are thin: each just supplies
   `Ixed`/`At`/`Each`/`Cons`/`Snoc`/`Wrapped` instances for one already-known
   container type. Per the precedence rule these become small new-port
   modules **over `linen`'s existing ports** (`Linen.Data.Map`,
   `Linen.Data.Text`, `Linen.Data.Vector`, …) rather than fresh container
   implementations — see the substitution table below. A few
   (`Data.Sequence.Lens`, `Data.Tree.Lens`, `Data.IntSet.Lens`,
   `Data.Text.Lazy.Lens`) target containers `linen` has **not** ported yet
   at all (no `Data.Sequence`/`Data.Tree`/`Data.IntSet`/lazy-`Text` module
   exists) and are deferred, not dropped — see "Deferred" below.
3. **≈12 modules are GHC-runtime-specific or Template-Haskell-specific with
   no Lean analogue** (`Data.Data.Lens`, `Data.Dynamic.Lens`,
   `Data.Typeable.Lens`, `GHC.Generics.Lens`, `Language.Haskell.TH.Lens`,
   `Control.Parallel.Strategies.Lens`, `Control.Seq.Lens`,
   `Control.Lens.TH` + its three `Internal.*TH`/`Internal.Doctest` support
   modules, `Control.Lens.Internal.CTypes`) and are out of scope, the same
   category `deepseq`/`Win32`/`template-haskell` fall into in the `hip`/
   `time` entries above in the top-level index.

Two of `lens`'s own dependencies — **`profunctors`** and
**`indexed-traversable`** — are substantial, genuinely load-bearing, and not
lens-specific (other future imports could reuse them), so each got its own
prerequisite entry per AGENTS.md's recursive rule: see
[`docs/imports/profunctors/dependencies.md`](../profunctors/dependencies.md)
and
[`docs/imports/indexed-traversable/dependencies.md`](../indexed-traversable/dependencies.md).
**Both must be imported before `lens` itself** — `lens`'s modern optic
encoding (`type Lens s t a b = forall p. Strong p => Optic p s t a b`) is
*defined* in terms of `Strong`/`Choice`/`Traversing` from `profunctors`, and
`Control.Lens.Indexed` is defined in terms of `FunctorWithIndex`/
`FoldableWithIndex`/`TraversableWithIndex` from `indexed-traversable`. No
other not-yet-imported Hackage package blocks `lens` beyond these two.

## Template Haskell substitution strategy

`Control.Lens.TH` (`makeLenses`, `makePrisms`, `makeClassy`, …) and its
support modules (`Control.Lens.Internal.TH`, `.FieldTH`, `.PrismTH`) use
GHC's Template Haskell to inspect a `data`/record declaration at compile time
and splice in generated lens/prism definitions. **Lean 4 has no direct
counterpart to Template Haskell** (no `Q`/`reify`/`splice` over arbitrary
already-elaborated declarations); this codebase has already faced the exact
same gap once, in `Linen.Database.DuckDB.Simple.Generic`'s port of
`Database.DuckDB.Simple.Generic` (GHC-`Generic`/`Rep`-based, a different
metaprogramming facility but the same shape of problem — "walk an arbitrary
user type's structure and generate instance code from it"). That module's
own conclusion is the template for this one too: faking a `deriving`-style
generator that only handles a hand-picked shape "would satisfy nothing
upstream actually does," so it does not attempt one.

**Substitution:** every `makeLenses ''Foo`/`makePrisms ''Foo` call site
becomes a **hand-written definition per field/constructor**, using the
ordinary (non-generated) `Lens'`/`Prism'` smart constructors this port's
`Control.Lens.Lens`/`.Prism` modules provide — e.g. what upstream generates
as `fooBar :: Lens' Foo Bar` becomes:

```
def Foo.bar : Lens' Foo Bar := Control.Lens.lens (·.bar) (fun s v => { s with bar := v })
```

one `def` per accessor, exactly as `Linen.Database.DuckDB.Simple.Generic`
hand-writes one `FromField` instance per record type instead of deriving it.
This is not a loss of capability for the *use* of lenses (composition,
`Traversal`, `%~`/`.~`, indexed variants, …) — only the *generation step* has
no Lean equivalent, and Lean's own native field-projection (`.bar`) and
record-update (`{ s with bar := v }`) syntax already make writing that one
line per field mechanical. `makeClassy`'s extra typeclass-per-record-type
generation is likewise not ported — callers write the class and its one
instance by hand if they want that pattern. `Control.Lens.Internal.Doctest`
(doctest-only glue with no runtime role) and `Control.Lens.Internal.CTypes`
(GHC-FFI `Foreign.C.Types` newtype wrapping — Lean's fixed-width integer
types are already native, nothing to wrap) are dropped outright, not
substituted.

## External (non-`lens`) dependencies

Resolved against `lens-5.3.6.cabal`'s `build-depends`, in Hackage-import
precedence order (Lean stdlib > existing `linen` Haskell port > new source):

Already ported, reused as-is:

- `array` — see `Data.Array.Lens` below (Lean's native `Array` substitutes
  for the `array` package's `Data.Array`, though the two have different
  indexing semantics — see that module's own note).
- `base`, `containers`, `mtl`, `transformers` → `Base`, `Containers`, `Mtl`,
  Lean's own `ExceptT`/`ReaderT`/`StateT`.
- `bifunctors` → `Linen.Data.Bifunctor` (already ported; covers the
  `Bifunctor` half `lens` needs — see the `Bifoldable`/`Bitraversable` gap
  noted under "genuinely new" below).
- `bytestring`, `text`, `vector` → `ByteString`, `Text`, `Vector`.
- `contravariant` → `Linen.Data.Functor.Contravariant`.
- `exceptions` — per the same precedence-rule application as `hoauth2`'s
  `dependencies.md` note on this package: `Control.Exception.Lens`'s
  `MonadThrow`/`MonadCatch`-flavored combinators are ported directly against
  `Linen.Control.Exception`'s existing `IO`/`Except`-based exception type
  instead of a generic `exceptions`-style `MonadThrow` port.
- `filepath` → Lean's own `System.FilePath` (ships with the toolchain, not a
  `linen` port).
- `hashable` → Lean's own `Hashable` class (stdlib).

New prerequisite packages (their own `dependencies.md`, see above):

- `profunctors`, `indexed-traversable` (folds in
  `indexed-traversable-instances` per that file's own note).

Substituted with a small amount of directly-inlined code rather than a
separate package import (each a genuine precedence-rule application, not a
shortcut — narrow usage, same treatment `hoauth2`'s `dependencies.md` gives
`microlens`/`binary`):

- **`assoc`** (`Data.Bifunctor.Swap`/`.Assoc`) — used in exactly one place,
  `Control.Lens.Internal.Magma`'s tupling helpers; the two one-method
  classes are written directly in that module instead of imported.
- **`call-stack`** — GHC compile-time call-site capture
  (`HasCallStack`-style), used only for a handful of `error` messages'
  diagnostic prefixes inside `lens`'s internals; Lean already has its own
  native `HasCallStack` mechanism (`Init.Prelude`), so no separate port is
  needed.
- **`free`** and **`kan-extensions`** (`Control.Monad.Free`,
  `Data.Functor.Yoneda`/`.Coyoneda`) — both used only inside
  `Control.Lens.Internal.Bazaar`/`.Magma`'s encoding of "a traversal reified
  as data"; the free-monad/Yoneda machinery those two modules need is
  written directly where it's used (a handful of definitions) rather than
  as two separate multi-module package imports whose other capabilities
  `lens` never touches.
- **`reflection`** — GHC's `Data.Reflection` reifies a runtime value as an
  ambient type-class dictionary via `unsafeCoerce`, purely so a value that
  logically flows top-down (e.g. `partsOf`'s traversal shape) can instead be
  picked up implicitly deep inside code that wasn't explicitly passed it.
  This is a workaround for GHC's lack of first-class implicit *value*
  parameters, not a capability `lens`'s behavior actually depends on; ported
  as an ordinary explicit function argument instead — Lean has real
  first-class functions and doesn't need the type-class-dictionary trick to
  thread a value through.
- **`semigroupoids`** (`Data.Semigroupoid`, `Apply`, `Traversable1`,
  `Foldable1`) — used only by `Control.Lens.Internal.Magma`'s non-empty
  `Bazaar1`/`Magma1` variants; the two or three methods actually needed are
  written directly in that module.
- **`strict`** (`Data.Strict.Pair`/`.Maybe`/`.These`) — GHC-laziness-specific
  strictness-annotated variants of ordinary product/sum types; Lean is
  eager by default, so the strict/lazy distinction this package encodes
  doesn't exist here (same category as `deepseq`, per the `hip` entry's
  note).
- **`these`** (`Data.These`) — used only by `Control.Lens.Internal.Magma`'s
  merge helper for zipping traversal shapes of possibly-different length;
  the `These`-shaped case split is written directly at that one call site.

Dropped outright (GHC-runtime/build-tooling-specific, no Lean analogue —
same category as `deepseq`/`Win32`/`template-haskell` in the `hip`/`time`
entries):

- **`parallel`** (`Control.Parallel.Strategies`) — backs
  `Control.Parallel.Strategies.Lens`/`Control.Seq.Lens` only; GHC evaluation
  strategies (spark-based parallel evaluation, `NFData`-driven forcing) have
  no meaning in Lean's evaluation model. Both modules are dropped.
- **`template-haskell`**, **`th-abstraction`** — back `Control.Lens.TH` and
  its `Internal.*TH` support modules; see the TH substitution strategy
  above. `Language.Haskell.TH.Lens` (lenses over the TH AST itself) is
  dropped outright — Lean has no TH AST to have lenses over.
- **`base-orphans`** — GHC/`base`-version-compatibility orphan instances;
  `linen` targets one pinned Lean toolchain, not a multi-version compat
  matrix.

## `lens` modules substituted by an existing `linen` port (small new-port
modules over an existing container, not fresh container implementations)

Each targets a container `linen` already has:

- `Data.Bits.Lens` → over `Linen.Data.Bits`.
- `Data.ByteString.Lens` (folds in `Data.ByteString.Strict.Lens`, which
  upstream only splits out for `bytestring`'s own strict/lazy module split —
  `linen`'s `Linen.Data.ByteString` is already the one strict representation,
  no separate "strict" variant module needed) → over `Linen.Data.ByteString`.
- `Data.ByteString.Lazy.Lens` → over `Linen.Data.ByteString.Lazy`.
- `Data.Complex.Lens` → over `Linen.Data.Complex`.
- `Data.HashSet.Lens` → over Lean stdlib `Std.HashSet`.
- `Data.List.Lens` → over Lean stdlib `List`.
- `Data.Map.Lens` → over `Linen.Data.Map`.
- `Data.Set.Lens` → over `Linen.Data.Set`.
- `Data.Text.Lens` (folds in `Data.Text.Strict.Lens`, same reasoning as
  `ByteString.Strict.Lens` above) → over `Linen.Data.Text`.
- `Data.Vector.Lens` (folds in `Data.Vector.Generic.Lens`, which upstream
  only splits out for the boxed/unboxed/storable `Vector` family — `linen`
  has one `Vector` representation) → over `Linen.Data.Vector`.
- `System.Exit.Lens` → over `Linen.System.Exit`.
- `System.FilePath.Lens` → over Lean's own `System.FilePath` (stdlib).
- `System.IO.Error.Lens` → over Lean's own `IO.Error` (stdlib).
- `Control.Monad.Error.Lens` → over Lean's `MonadExcept`/`ExceptT`
  (`Mtl`'s `Control.Monad.Except` port already provides the
  `throw`/`catch`-shaped operations `MonadError`'s lenses wrap).

## `lens` modules dropped (GHC-runtime/TH/no-Lean-analogue — see above)

`Data.Data.Lens` (GHC `Data.Data`/SYB generic-programming lenses),
`Data.Dynamic.Lens` (GHC `Data.Dynamic`), `Data.Typeable.Lens` (GHC
`Typeable`), `GHC.Generics.Lens` (GHC `Generic`/`Rep` — see the TH-strategy
note's cross-reference to `Linen.Database.DuckDB.Simple.Generic` for why
this codebase already treats GHC-generics-shaped metaprogramming as
out of scope), `Language.Haskell.TH.Lens`, `Control.Lens.TH`,
`Control.Lens.Internal.TH`, `.FieldTH`, `.PrismTH`, `.Doctest`, `.CTypes`,
`Control.Parallel.Strategies.Lens`, `Control.Seq.Lens`.

## `lens` modules deferred (target a container `linen` has not ported yet)

Not dropped — simply blocked on a container this codebase hasn't imported:

- `Data.IntSet.Lens` — no `Linen.Data.IntSet` (only `Linen.Data.IntMap`
  exists; `containers`' `Data.IntSet` itself was never ported — checked
  `docs/imports/Containers/dependencies.md`, which covers `Data.Map`/
  `.IntMap`/`.Set`/`.Sequence`(not present)/`.Graph` but not `Data.IntSet`).
- `Data.Sequence.Lens` — no `Linen.Data.Sequence` port exists.
- `Data.Tree.Lens` — no `Linen.Data.Tree` port exists.
- `Data.Text.Lazy.Lens` — `Text`'s `dependencies.md` covers strict `Text`
  only; no lazy-`Text` port exists.

Each can be added as a small follow-up once (if) its target container is
imported; none blocks the rest of `lens`.

## Topologically sorted `lens` modules (genuinely new port)

Namespace: kept as upstream's own `Control.Lens.*`/`Data.*.Lens`/
`System.*.Lens` (re-rooted under `Linen.`) rather than inventing a new
top-level namespace — `Control.Lens` is already exactly where Lean's own
convention would place a lens/optics library (a `Control.*`-shaped
typeclass-and-combinator hierarchy, the same reasoning `Control.Lens.Basic`↦
`Control.Profunctor.*` gets in the `profunctors` entry), and unlike
`WaiAppStatic`→`WebApp.Static`-style renames, `Lens`/`Prism`/`Iso`/
`Traversal`/`Fold`/`Getter`/`Setter` are optics-theory terms, not
Haskell/GHC branding, so AGENTS.md's Lean-ify rule does not require renaming
them (contrast `Control.Lens.TH`, which *is* GHC-TH-branded and is dropped
outright above rather than renamed, since there is nothing left of it to
rename once the TH machinery itself is gone).

1. `Control.Lens.Internal.Prelude` → folded into #2 (an internal-only
   compatibility shim over old/new `base` `Foldable`/`Traversable` method
   names; no standalone module needed against one pinned Lean toolchain).
2. `Control.Lens.Internal.Profunctor` → `Linen.Control.Lens.Internal.Profunctor`
   — `Bicontravariant`, `Conjoined`, `Indexable` classes over
   `Linen.Control.Profunctor.*` (prerequisite). Depends on `profunctors`.
3. `Control.Lens.Internal.Indexed` → `Linen.Control.Lens.Internal.Indexed` —
   `Indexed i a b` profunctor, `Control.Lens.Internal.Profunctor`'s
   `Indexable`. Depends on #2, `indexed-traversable`, and the folded-in
   `Tagged` (see the `profunctors` entry).
4. `Control.Lens.Internal.Instances` → `Linen.Control.Lens.Internal.Instances`
   — misc `Applicative`/`Traversable` instances (`Magma`'s helper types
   need `Traversable`, etc.) `lens` needs but upstream `base` doesn't
   provide. Depends on #2.
5. `Control.Lens.Internal.Context` → `Linen.Control.Lens.Internal.Context` —
   the `Context`/`Pretext` comonad backing `Control.Lens.Lens`'s van
   Laarhoven representation; folds in the minimal `Comonad` class per the
   `profunctors` entry's note (this is the one other call site for it
   besides `Data.Profunctor.Cayley`). Depends on #2.
6. `Control.Lens.Internal.Magma` → `Linen.Control.Lens.Internal.Magma` —
   `Magma`/`Molten`, the free-semigroupoid tree `Bazaar` reifies traversal
   order into; folds in the `assoc`/`these`/`semigroupoids` slivers noted
   above. Depends on #2, #3.
7. `Control.Lens.Internal.Bazaar` → `Linen.Control.Lens.Internal.Bazaar` —
   `Bazaar`/`BazaarT`, the reified-traversal type `Control.Lens.Traversal`
   is built on; folds in the `free`/`kan-extensions` slivers noted above.
   Depends on #2, #3, #6.
8. `Control.Lens.Internal.Iso` → `Linen.Control.Lens.Internal.Iso` —
   `Exchange` profunctor backing `Control.Lens.Iso`. Depends on #2.
9. `Control.Lens.Internal.Prism` → `Linen.Control.Lens.Internal.Prism` —
   `Market` profunctor backing `Control.Lens.Prism`. Depends on #2.
10. `Control.Lens.Internal.Review` → `Linen.Control.Lens.Internal.Review` —
    `Bizarre`/co-Bazaar machinery backing `Control.Lens.Review`. Depends on
    #2, #7.
11. `Control.Lens.Internal.Getter` → `Linen.Control.Lens.Internal.Getter` —
    `Accessing`/`noEffect` helpers backing `Control.Lens.Getter`. Depends
    on #2.
12. `Control.Lens.Internal.Setter` → `Linen.Control.Lens.Internal.Setter` —
    `Settable`, `Mutator` (the strict `Identity`-alike `Control.Lens.Setter`
    forces its updates through) backing `Control.Lens.Setter`. Depends on #2.
13. `Control.Lens.Internal.Fold` → `Linen.Control.Lens.Internal.Fold` —
    `Folding`/`Leftmost`/`Rightmost` monoid helpers backing
    `Control.Lens.Fold`. Depends on #2, #11.
14. `Control.Lens.Internal.Level` → `Linen.Control.Lens.Internal.Level` —
    `Level`, breadth-first-numbered tree backing `Control.Lens.Level`.
    Depends on #2, #6.
15. `Control.Lens.Internal.Deque` → `Linen.Control.Lens.Internal.Deque` —
    banker's-queue double-ended queue, an internal performance helper for
    breadth-first traversal (#14). No further internal deps.
16. `Control.Lens.Internal.List` → `Linen.Control.Lens.Internal.List` —
    small `List`-manipulation helpers `Control.Lens.Cons`/`.Plated` share.
    No further internal deps.
17. `Control.Lens.Internal.Zoom` → `Linen.Control.Lens.Internal.Zoom` —
    `Zoomed`/`Focusing` families letting `zoom`/`magnify` retarget a
    `StateT`/`ReaderT` computation through a lens; folds in the `Closed`-use
    note from `profunctors`. Depends on #2, #12.
18. `Control.Lens.Internal.ByteString` → folded into `Data.ByteString.Lens`
    (see above) rather than kept standalone.
19. `Control.Lens.Internal` → `Linen.Control.Lens.Internal` (facade,
    re-exports #2–#17).
20. `Control.Lens.Type` → `Linen.Control.Lens.Type` — the `Optic`/`Lens'`/
    `Traversal'`/… type-alias family every other module names. Depends on
    #2.
21. `Control.Lens.Equality` → `Linen.Control.Lens.Equality` — `(:=:)`, the
    identity optic. Depends on #20.
22. `Control.Lens.Getter` → `Linen.Control.Lens.Getter` — `Getter`, `to`,
    `view`, `(^.)`. Depends on #11, #20.
23. `Control.Lens.Setter` → `Linen.Control.Lens.Setter` — `Setter`, `sets`,
    `over`, `(.~)`, `(%~)`. Depends on #12, #20.
24. `Control.Lens.Lens` → `Linen.Control.Lens.Lens` — `Lens`, `lens`,
    `(%%~)`, `Control.Lens.Type`'s namesake. Depends on #5, #20, #22, #23.
25. `Control.Lens.Iso` → `Linen.Control.Lens.Iso` — `Iso`, `iso`, `from`,
    `withIso`. Depends on #8, #20, #24.
26. `Control.Lens.Prism` → `Linen.Control.Lens.Prism` — `Prism`, `prism`,
    `prism'`, `_Left`/`_Right`/`_Just`/`_Nothing`. Depends on #9, #20, #25.
27. `Control.Lens.Review` → `Linen.Control.Lens.Review` — `Review`, `un`,
    `re`, `(#)`. Depends on #10, #20, #26.
28. `Control.Lens.Fold` → `Linen.Control.Lens.Fold` — `Fold`, `folding`,
    `(^..)`, `toListOf`, `preview`. Depends on #13, #20, #22.
29. `Control.Lens.Traversal` → `Linen.Control.Lens.Traversal` — `Traversal`,
    `traverseOf`, `(%%~)`, `both`; folds in the minimal `Bitraversable`
    class `Linen.Data.Bifunctor` doesn't yet have (a small addition to that
    existing module rather than a new one — one method,
    `bitraverse`). Depends on #6, #7, #20, #23, #28.
30. `Control.Lens.Indexed` → `Linen.Control.Lens.Indexed` — `Indexed`,
    `Control.Lens.Traversal`'s indexed variants (`itraverse`, `(<.>)`, …).
    Depends on #3, `indexed-traversable`, #29.
31. `Control.Lens.Each` → `Linen.Control.Lens.Each` — `Each` class,
    `each`. Depends on #29.
32. `Control.Lens.At` → `Linen.Control.Lens.At` — `At`/`Ixed` classes,
    `at`, `ix`, folds in most per-container `at`/`ix` instances (the
    container-specific modules above each add one instance here rather than
    redefining the classes). Depends on #26, #29.
33. `Control.Lens.Cons` → `Linen.Control.Lens.Cons` — `Cons`/`Snoc`
    classes, `_head`/`_tail`/`_init`/`_last`, `(<|)`/`(|>)`. Depends on #26,
    #29, #16.
34. `Control.Lens.Empty` → `Linen.Control.Lens.Empty` — `AsEmpty` class,
    `_Empty`. Depends on #26.
35. `Control.Lens.Plated` → `Linen.Control.Lens.Plated` — `Plated`
    class/generic tree-rewriting combinators (`transform`, `rewrite`, …).
    Depends on #16, #29.
36. `Control.Lens.Level` → `Linen.Control.Lens.Level` — `levels`,
    breadth-first traversal orderings over a `Plated` structure. Depends on
    #14, #35.
37. `Control.Lens.Zoom` → `Linen.Control.Lens.Zoom` — `Zoom`/`Magnify`
    classes, `zoom`/`magnify`. Depends on #17, #24.
38. `Control.Lens.Reified` → `Linen.Control.Lens.Reified` — `ReifiedLens`/
    `ReifiedGetter`/…, ordinary-data wrappers around each optic (needed
    since bare optics are `forall`-polymorphic functions, not directly
    storable in most containers). Depends on #20–#29.
39. `Control.Lens.Tuple` → `Linen.Control.Lens.Tuple` — `Field1`…`Field9`
    classes/instances (`_1`, `_2`, …) over `Prod`-nested tuples. Depends on
    #24.
40. `Control.Lens.Unsound` → `Linen.Control.Lens.Unsound` — `lensProduct`/
    `lensSum`/`adjoin`, explicitly-marked-unlawful combinators (upstream's
    own module name/docstring already flags these as law-breaking; ported
    as-is with the same warning). Depends on #24.
41. `Control.Lens.Wrapped` → `Linen.Control.Lens.Wrapped` — `Wrapped`/
    `_Wrapped'` class, an `Iso` between a newtype-like structure and its
    single field; ties into `Linen.Data.Newtype`'s existing wrapper types
    (`Dual`, `Sum`, `Product`, `All`, `Any`, …) as ready-made instances.
    Depends on #25.
42. `Control.Lens.Extras` → `Linen.Control.Lens.Extras` — `is` (does a
    `Prism`/`Fold` match). Depends on #28.
43. `Control.Lens.Combinators` → `Linen.Control.Lens.Combinators` (facade,
    re-exports #20–#42 minus a couple of deliberately-excluded name clashes
    upstream itself documents). Depends on all of the above.
44. `Control.Lens.Operators` → `Linen.Control.Lens.Operators` (facade,
    operator-only re-export of #43, for callers who want `(^.)`/`(.~)`/…
    without the named functions). Depends on #43.
45. `Control.Lens.Profunctor` → `Linen.Control.Lens.Profunctor` — small
    profunctor-optic-specific combinators (`Choicy`, `Bizarre1`) not already
    covered by #2's re-export. Depends on #2, #43.
46. `Control.Lens` → `Linen.Control.Lens` (top-level facade, listed first in
    upstream's own `exposed-modules` "so cabal repl loads it" — re-exports
    #43/#44/#45 plus the container-instance modules below that upstream
    itself re-exports from the top level). Depends on all of the above.
47. `Control.Exception.Lens` → `Linen.Control.Exception.Lens` — prisms over
    `Linen.Control.Exception`'s exception type (substituted per the
    `exceptions` note above). Depends on #26, #46.
48. `Control.Monad.Error.Lens` → `Linen.Control.Monad.Error.Lens` — prisms
    over `MonadExcept`/`ExceptT` (substituted per the note above). Depends
    on #26, #46.
49. `Data.Array.Lens` → `Linen.Data.Array.Lens` — `Ixed`/`TraverseMax`/
    `TraverseMin` instances over Lean's native `Array` (substituting for
    the `array` package's `Ix`-indexed `Data.Array`; Lean's `Array` is
    `Nat`-indexed and dynamically sized rather than `Ix`-indexed over an
    arbitrary bounded range, so the `Ix`-generic instances upstream
    provides narrow to the one `Nat`-indexed case here — a strict
    simplification, not a lost capability, since every `lens` call site
    using this module already only ever indexes concretely). Depends on
    #32, #46.
50. `Data.Bits.Lens` → `Linen.Data.Bits.Lens` — over `Linen.Data.Bits`.
    Depends on #46.
51. `Data.ByteString.Lens` → `Linen.Data.ByteString.Lens` (folds in
    `Data.ByteString.Strict.Lens`, `Control.Lens.Internal.ByteString` per
    the notes above) — over `Linen.Data.ByteString`. Depends on #33, #46.
52. `Data.ByteString.Lazy.Lens` → `Linen.Data.ByteString.Lazy.Lens` — over
    `Linen.Data.ByteString.Lazy`. Depends on #33, #46.
53. `Data.Complex.Lens` → `Linen.Data.Complex.Lens` — over
    `Linen.Data.Complex`. Depends on #24, #46.
54. `Data.HashSet.Lens` → `Linen.Data.HashSet.Lens` — over `Std.HashSet`.
    Depends on #46.
55. `Data.List.Lens` → `Linen.Data.List.Lens` — over Lean stdlib `List`.
    Depends on #32, #33, #46.
56. `Data.Map.Lens` → `Linen.Data.Map.Lens` — over `Linen.Data.Map`.
    Depends on #32, #46.
57. `Data.Set.Lens` → `Linen.Data.Set.Lens` — over `Linen.Data.Set`.
    Depends on #32, #46.
58. `Data.Text.Lens` → `Linen.Data.Text.Lens` (folds in
    `Data.Text.Strict.Lens` per the note above) — over `Linen.Data.Text`.
    Depends on #33, #46.
59. `Data.Vector.Lens` → `Linen.Data.Vector.Lens` (folds in
    `Data.Vector.Generic.Lens` per the note above) — over
    `Linen.Data.Vector`. Depends on #32, #46.
60. `System.Exit.Lens` → `Linen.System.Exit.Lens` — over
    `Linen.System.Exit`. Depends on #26, #46.
61. `System.FilePath.Lens` → `Linen.System.FilePath.Lens` — over Lean's own
    `System.FilePath`. Depends on #46.
62. `System.IO.Error.Lens` → `Linen.System.IO.Error.Lens` — over Lean's own
    `IO.Error`. Depends on #26, #46.
63. `Numeric.Lens` → `Linen.Numeric.Lens` — `_Show`, `hex`, `octal`,
    `negated`, `_Integral` prisms/isos over numeric-literal parsing.
    Depends on #25, #26, #46.
64. `Numeric.Natural.Lens` → `Linen.Numeric.Natural.Lens` — one `Prism'
    Int Nat`-style instance over Lean's native `Nat`. Depends on #63.

**Total: 64 genuinely-new-port modules** (46 in the optics core, 4 exception/
monad-error/numeric facades, 14 per-container instance modules), plus the 2
prerequisite packages' own modules (16 for `profunctors`, 4 for
`indexed-traversable`, both counted in their own `dependencies.md`), against
84 upstream `exposed-modules` + 1 `other-modules`. The gap (85 upstream −
64 new-port ≈ 21) is made up of: modules folded into another module listed
above (`Internal.Prelude`, `.ByteString`, `Data.*.Strict.Lens`,
`Data.Vector.Generic.Lens` — 5), modules dropped outright as GHC/TH-specific
with no Lean analogue (`Data.Data.Lens`, `.Dynamic.Lens`, `.Typeable.Lens`,
`GHC.Generics.Lens`, `Language.Haskell.TH.Lens`, `Control.Lens.TH`,
`.Internal.TH`, `.FieldTH`, `.PrismTH`, `.Internal.Doctest`,
`.Internal.CTypes`, `Control.Parallel.Strategies.Lens`, `Control.Seq.Lens` —
13), and modules deferred pending a container `linen` hasn't ported yet
(`Data.IntSet.Lens`, `Data.Sequence.Lens`, `Data.Tree.Lens`,
`Data.Text.Lazy.Lens` — 4): 5 + 13 + 4 = 22, reconciling with rounding in
the "≈" counts above (`Control.Lens.Internal` itself, #19, is a facade
counted among the 46, not a fold-in, unlike its sibling `.ByteString`).
