# `FreerSimple` module dependencies

Topological order of every module of the [`freer-simple`](https://hackage.haskell.org/package/freer-simple-1.2.1.1)
Hackage package imported into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Topologically sorted modules

All modules below are ported (or resolved away) — kept commented out as a completed checklist.

<!-- 1. `Data.OpenUnion` (merges upstream `Data.OpenUnion` + `Data.OpenUnion.Internal`) -->
<!-- 2. `Control.Monad.Effect` (merges upstream `Control.Monad.Freer.Internal` + upstream's `Control.Monad.Freer` re-export shim) -->
<!-- 3. `Control.Monad.Effect.Reader` -->
<!-- 4. `Control.Monad.Effect.State` -->
<!-- 5. `Control.Monad.Effect.Error` -->
<!-- 6. `Control.Monad.Effect.Writer` -->
<!-- 7. `Control.Monad.Effect.NonDet` (the `NonDet` type itself lives in upstream's `Internal`; `msplit` not ported — see below) -->
<!-- 8. `Control.Monad.Effect.Coroutine` -->
<!-- 9. `Control.Monad.Effect.Fresh` -->
<!-- 10. `Control.Monad.Effect.Trace` -->
<!-- 11. *(`freer-simple` package root — no upstream module; covered by `linen`'s own root)* -->

## Own upstream dependencies

`freer-simple`'s `build-depends` are `base`, `natural-transformation`,
`template-haskell` and `transformers-base`. Resolved per AGENTS.md's precedence
rule (Lean stdlib > what `linen` already has from Haskell > new Hackage source):

- **`base`** → Lean stdlib.
- **`natural-transformation`** → folded inline. The package supplies only the
  natural-transformation type `m :~> n = forall x. m x -> n x`, which Lean
  expresses directly as the dependent function type
  `{β : Type} → m β → n β`. No wrapper structure, no separate import entry.
- **`template-haskell`** → **dropped.** It backs only
  `Control.Monad.Freer.TH`'s `makeEffect`, which generates the `send`-wrapping
  smart constructors for an effect GADT. Lean has no Template Haskell, so those
  constructors are hand-written per effect — the same treatment `README.md`
  already documents for `lens`'s `Control.Lens.TH` (`makeLenses`).
- **`transformers-base`** → **out of scope.** It backs only `MonadBase`-style
  interop instances for embedding `Eff` in an arbitrary transformer stack, which
  the core `Eff`/`interpret`/`run` machinery does not need. Should a later module
  want it, `linen` already has the equivalent capability via
  `Control.Monad.IO.Unlift` (ported from `unliftio`).

## Substitutions / deviations

- **`Data.OpenUnion` + `Data.OpenUnion.Internal` merged into one module.**
  Upstream splits a safe `Member`-typeclass interface (`Data.OpenUnion`) from an
  unsafe implementation (`Data.OpenUnion.Internal`), where `Union` is an
  `unsafeCoerce`d `(Int, Any)` pair giving O(1) dispatch under GHC. This port
  keeps the safe interface and makes it the real implementation: `Union` is an
  ordinary strictly-positive inductive indexed by the effect row, in the shape of
  `List.Mem` evidence. Lean has no reason to pay for an unsafe fast path the
  kernel cannot see through, so there is no "Internal" split to make — the same
  reasoning `Linen/System/IO.lean` records for dropping `unsafeInlineIO` and
  `Control.Monad.STM` for dropping GHC's STM primops.

- **`Data.FTCQueue` dropped.** Upstream's `Eff` stores its continuations in a
  catenable "fast type-aligned" queue purely so that left-nested `>>=` chains
  avoid quadratic continuation concatenation under GHC. That is a performance
  device, not a semantic one: this port uses the direct Freer encoding
  (`protect a | impure (Union effs β) (β → Eff effs α)`), which is behaviour- and
  type-identical and differs only in the amortised complexity of pathologically
  left-nested binds. Recorded here and in the module doc-comment as a deliberate,
  behaviour-preserving omission rather than a simplification of the port's type or
  semantics.

- **`Control.Monad.Freer.TH` dropped** — see `template-haskell` above.

- **`msplit` is not ported** (the one omission from `Control.Monad.Effect.NonDet`).
  Upstream peels off the first solution plus a computation for the rest, using a
  queue of pending branches. Its recursion resumes from a *queued* computation
  rather than a subterm of the one being traversed, so it is not structurally
  recursive, and no measure is available: `sizeOf` yields nothing for the
  continuation carried by a Freer node, and on an infinitely-branching
  computation `msplit` genuinely diverges — upstream is total only by Haskell's
  laziness. Porting it would need `partial` or a fuel parameter, both forbidden
  by AGENTS.md, so it is left out rather than faked. `makeChoiceA` covers the
  finite-search use.

- **`makeChoiceA` is specialised to `List`.** Upstream is generic over
  `Alternative f`. Lean's `Alternative` offers no way to build an arbitrary `f`
  from two branches without also assuming monoidal structure on it, so the
  handler is written at `List` — the instance real uses of upstream's version
  take anyway. `Control.Applicative.asum` folds the result into another
  `Alternative` where wanted.

- **Requests that never return answer with `Empty`.** Upstream's
  `Error e r` and `MZero :: NonDet a` are requests of *arbitrary* answer type.
  Written literally in Lean, their constructors would bind `{α : Type}`, whose
  type `Type` inhabits `Type 1` — forcing the effect into `Type 1`, which cannot
  be an effect (`Type → Type`) at all. Answering with `Empty` says the same
  thing more precisely (the continuation is unreachable) and the smart
  constructors recover an arbitrary result type via `Empty.elim`.

- **`outParam` locator classes for `Error` and `Writer`.** `throwError e`'s
  error type appears only in its `Member (Error ε) effs` constraint, so instance
  search stalls on `Member (Error ?ε) effs`. `HasError`/`HasWriter` carry that
  type as an `outParam`, so resolving against the row determines it. Same device
  as `Control.Monad.Effect.FileSystem`'s `HasFileSystem`. The `Member`-based
  `send` remains available and is what `Reader`/`State`/`Fresh`/`Trace` use.

- **No `Monoid` class for `Writer`.** Upstream's `runWriter` requires
  `Monoid w`; neither Lean's standard library nor `linen` has one. Following the
  two precedents already in this codebase, the general `runWriter` takes the
  unit and append explicitly (as `Codec.Picture.Metadata` does for `foldMap`)
  and `runWriterAppend` covers the common case via `[Append ω] [Inhabited ω]`
  (as `Data.Foldable.foldMap` does).

- **`Coroutine`'s `Status` drove the payload universe.** `Status` holds a
  `b → Eff effs (Status …)`, so it must live in `Type 1`, which means
  `Eff` has to accept a `Type 1` payload. That is why `Eff` is universe-
  polymorphic (`Type u` in, `Type (max 1 u)` out) rather than pinned to
  `Type 0`: at `u = 1` payload and computation share universe 1 and the
  circularity closes. `Eff.bindH` is the matching heterogeneous bind, since a
  handler answers at `Type 0` while the computation's result may sit higher.
  This is the "self-referential type — do the proof" case AGENTS.md calls out;
  no `partial` and no termination annotation were needed.

- **`Fresh` counts in `Nat`,** not upstream's `Int`: fresh names are never
  negative and `Nat` matches this library's convention.

- **`runTracePure` added** alongside upstream's stdout-printing `runTrace`, so
  tracing is testable without I/O (which is what lets that module's tests be
  `#guard`s).

- **Universe.** Upstream's effect row is `[* -> *]`, erased at compile time. In
  Lean, `Type → Type` itself inhabits `Type 1`, so a constructor binding
  `{eff : Type → Type}` forces both `Union` and `Eff` into `Type 1` while their
  payloads stay in `Type 0`. `Monad.{u,v}` is universe-polymorphic, so
  `Monad (Eff effs)` instantiates at `u = 0, v = 1` and `do`-notation works
  normally. This is a genuine universe bump relative to every other transformer
  in `Linen/Control/Monad/*`, none of which is indexed by a *list* of monads.

## The capability effects — no upstream counterpart

`Linen/Control/Monad/Effect/{FileSystem,HTTP,PostgreSQL,ObjectStore,Queue,
SecretStore}.lean` are **`linen`-original** and therefore not part of the
topological checklist above (same treatment as `Control.Exception.Lens` relative
to the `lens` import). They demonstrate what the ported mechanism gains from
dependent types.

`FileSystem` came first and sets the pattern, at two strengths:

1. **Permissions** — which *operations* are allowed. A `Capability` value indexes
   the effect and each operation carries a `Prop`-valued proof that the
   capability grants it, so a read-only capability makes `writeFile` fail to
   elaborate at the call site.
2. **Path scope** — which *arguments* those operations may be called on. The
   capability's `scopes` confine every operation to paths beneath a matching
   root, as a proof obligation discharged by `decide`, so
   `readFile p!"/etc/passwd"` under a sandboxed capability does not elaborate
   either. Each `Scope` carries its own operation list, so the restriction is on
   (operation, argument) *pairs*: one capability can be read-write under one
   directory and read-only under another.

Haskell's type-level effect rows — `freer-simple`'s included — can say *which
effects* are available but never with which permissions, and cannot constrain an
effect's arguments at all, short of one effect type per combination or a runtime
check the type system knows nothing about.

Two implementation notes worth recording, both discovered by construction:

- **The permission obligations are instances; the scope obligation is an
  auto-param.** An auto-param (`:= by decide`) fires before `cap` is unified from
  the expected type and fails on a metavariable, so permissions must be
  `Prop`-classes (instance resolution is postponed until `cap` is known). The
  scope obligation depends on the path argument and so cannot be keyed on `cap`
  alone; it works as an auto-param precisely because `HasFileSystem`'s `outParam`
  has already determined `cap` by the time the tactic runs.
- **Paths are `List String`, not `System.FilePath`.** Lean's
  `String.startsWith`/`String.take` do not reduce under `decide` (they get stuck
  on the slice representation), so a string-prefix scope check could not be
  discharged at elaboration time at all; `List String` prefix comparison does
  reduce. Component-wise containment is also the *correct* meaning of "under this
  directory" — `/tmp/sandbox-evil` is a string-prefix extension of
  `/tmp/sandbox` but is not inside it, and the tests pin that case. The `p!`
  macro gives literals ordinary path syntax, splitting at macro-expansion time so
  the result stays a literal list.


`HTTP` and `PostgreSQL` are the same pattern at two further instances, and both
were written to test whether it generalises. It does, with one refinement each:

- **`HTTP`** puts a method list inside each URL scope, so the *argument* half of
  the capability constrains `(method, url)` pairs rather than URLs alone — "GET
  anywhere under `/v1`, POST only to `/v1/events`" is one capability, not two.
  The `Url` type repeats the `Path` lesson twice over: a host is DNS labels and a
  path is segments, because `api.example.com.evil.com` is a string-prefix
  extension of `api.example.com` and `/v1-admin` is one of `/v1`, and neither is
  inside the other. Scheme and port are matched exactly and are part of the
  scope.
- **`PostgreSQL`** adds a third, *structural* restriction below the two: the
  database, instance and role are capability fields and the handler derives its
  connection string from them alone, so there is no obligation to discharge
  because no term of `Eff [PostgreSQL cap] α` could name an alternative. Its
  queries are an AST rather than strings for exactly the reason paths are
  component lists — a `String` of SQL is opaque to `decide` — with the added
  benefit that the rendered SQL is derived from the checked value and so cannot
  disagree with it. There is deliberately no `rawSql` escape hatch: it would
  weaken every guarantee from "cannot be written" to "cannot be written without
  lying".

One correction to the note above, found while writing these two: auto-param
failures **are** capturable by `#guard_msgs` in Lean 4.33.1, contrary to what
`FileSystemTest.lean`'s header says. The tests still assert scope rejections as
`theorem … ≠ true := by decide` rather than as message matches, because that is
the stronger statement and does not rot when a compiler version rewords its
diagnostics — but the reason given for it was wrong.


### The three cloud effects

`ObjectStore`, `Queue` and `SecretStore` came last, alongside `Linen.Cloud.*`.
They are the pattern's fourth, fifth and sixth instances, and the first written
against a **remote** backend rather than a local one. Three things they add:

- **The handler takes the backend as a parameter**, as `HTTP.runHTTPWith` does
  with its transport — so the *same* `Eff [ObjectStore cap] α` runs against S3,
  against Google Cloud Storage, or against an in-memory double. That is what
  makes a provider-agnostic program a real claim rather than a slogan, and it is
  what lets the tests exercise the effects end to end with no network and no
  credentials.
- **`Queue` reaches for the structural restriction rather than a permission.**
  SQS is one queue with receipt handles; Pub/Sub is a topic to publish to plus a
  *subscription* to pull from. Rather than a portable queue type with an optional
  receive target failing at runtime, the backend record splits into `Producer`
  and `Consumer`, so a Pub/Sub `Consumer` cannot be constructed without naming a
  subscription — the same move `PostgreSQL` makes with its connection target,
  where there is no obligation to discharge because no term could name an
  alternative.
- **`SecretStore` splits reading a value from reading metadata**, as separate
  operations with separate permission bits. `Cloud.SecretStore`'s backend record keeps
  `getValue` apart from `describe` at runtime, it being the only inbound
  plaintext path in the interface; making the split a `Prop`-class turns that
  convention into a compile-time fact, so a capability granting `describe`
  makes `getValue` fail to elaborate. This is the clearest demonstration in the
  set of a distinction a type-level effect row cannot draw at all: both
  operations have the same effect type and differ only in a *value* indexing it.

One divergence from `FileSystem` worth recording, because it looks like a bug
and is not. `ObjectStore`'s key macro `k!` does **not** filter empty segments,
while `FileSystem`'s `p!` does. An S3 key is an opaque byte string in which
`a//b`, `a/` and `/a` are three distinct keys, so filtering would make them
unaddressable; `String.splitOn "/"` and `"/".intercalate` are exact inverses on
every string, so `List String` stays a faithful representation without needing a
well-formedness field. The `ObjectStore` scope prefix is correspondingly
*stricter* than S3's own `prefix=` parameter, which is a byte prefix and so
matches `logs-2026/a` for `prefix=logs`; the component-wise check does not, and
the handler compensates by sending a delimiter-terminated prefix on the wire, so
what is enumerated cannot exceed what was authorised.
