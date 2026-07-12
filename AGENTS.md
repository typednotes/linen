# AGENTS.md

Guidance for working in the **linen** Lean library.

## Project layout

- Library sources live under `Linen/`, mirroring their module path
  (e.g. `Linen/Data/Functor.lean` is module `Linen.Data.Functor`).
- Every source module must be imported from the library root `Linen.lean`.
- Tests live under `Tests/`, mirroring the source tree with a `Test` suffix
  on the file name (e.g. `Linen/Data/Functor.lean` →
  `Tests/Linen/Data/FunctorTest.lean`), and are imported from `Tests.lean`.

## Testing

- **Every module in `Linen/` must have a counterpart under `Tests/` with
  illustrative tests.** The test module mirrors the source path (with a `Test`
  suffix) and is added to the import list in `Tests.lean`.
- Tests assert correctness with `#guard`, so building the `Tests` library runs
  every check:

  ```
  lake build Tests
  ```

- Prefer small, illustrative `#guard` examples that document intended behaviour.
  For `Prop`-valued definitions that cannot be decided by `#guard`, use
  `example ... := rfl` (or an explicit proof) to illustrate the law.

### FFI and native-library dependencies

- **Any module that links against a native C library (FFI) must build and
  pass its tests on at least macOS and Linux.** This covers both the `ffi/*.c`
  shim and the `lakefile.lean` linking logic (`pkg-config` discovery, vendored
  source, or downloaded prebuilt archives).
- CI (`.github/workflows/`) must run the full `lake build Tests` on both
  platforms whenever any FFI target is present in the build graph — a native
  dependency that only builds on the contributor's own machine is not
  considered done.
- When a native library ships no `pkg-config` file and has no package in
  Ubuntu's default apt repos (so the existing `pkgConfig`/`pkgLinkFlags`
  pattern in `lakefile.lean` doesn't apply), prefer, in order: (1) vendoring
  the library's source directly under `ffi/` when a small single- or
  few-file amalgamation exists (e.g. SQLite's `sqlite3.c`/`sqlite3.h`) — this
  avoids a platform dev-package dependency entirely and pins the exact
  version in git; (2) downloading a pinned prebuilt release archive as a
  build/CI step when no such amalgamation exists (e.g. DuckDB) — do not check
  prebuilt per-platform binaries into git.

## Keeping the main page current

`README.md` is the project's front page: the logo, badges, the feature list,
the module table, and the headline counts (modules / theorems / `#guard`
checks). **Whenever you add, remove, or substantially change a module, think
about updating `README.md`** so its feature list, module table, and stats stay
accurate.

## Importing external code

When importing external code into the library — designated by a **local path**
or a **GitHub link** (e.g. a Haskell package, a Rust crate, another Lean
project, or a single module):

- **Before porting anything, check whether it already exists — or already has
  a suitable source — in this order of precedence: Lean standard library >
  Haskell/Hackage > the new source (e.g. a Rust crate).** Search `Linen/`
  first: do not port a bespoke copy of something the stdlib or an
  already-ported Haskell package already provides in `linen`; import/reuse the
  existing definition instead. But the precedence doesn't stop at what's
  *already* ported — if the functionality isn't in `linen` yet, prefer
  bringing it in from a suitable Hackage package (following the
  Hackage-import convention below) over porting it fresh from the Rust
  source. So for a Rust crate being imported: if the Lean stdlib already has
  it, use that; else if `linen` already has it via an earlier Haskell import,
  reuse that; else if a suitable Hackage package covers it, import that
  package instead; only port directly from the Rust source what has no
  suitable Haskell counterpart. Stdlib examples: use `Id` for the identity
  monad, `Option`/`Except` rather than re-declared `Maybe`/`Either`/Rust's
  `Option`/`Result`, `· >=> ·` / `· <=< ·` for Kleisli composition,
  `List.foldlM` for `foldM`, `Functor.discard` for `void`.
- **Follow Lean standard-library principles for the module hierarchy and
  namespaces.** Place modules and choose namespaces the way the Lean stdlib
  would (e.g. `Data.…`, `Control.…`, `System.…`), not by mirroring the source
  project's layout or naming. Adapt identifiers to Lean naming conventions.
- **Lean-ify names that reference Haskell/GHC itself.** If a package, module,
  or identifier is named after Haskell-the-language or a GHC-specific concept
  (e.g. a `-hs` suffix, `GHC.*`, a name that only makes sense relative to
  Haskell), rename it to something Lean-appropriate instead of carrying the
  Haskell branding over — the same treatment as any other naming adaptation
  (e.g. `WaiAppStatic` → `WebApp.Static`, `Warp` → `Server`).
- After substitution, the result should read as idiomatic Lean built on the
  standard library — and still satisfy every rule below (tests, no `partial`,
  no `sorry`).

### Importing from Hackage

When asked to import a library from `https://hackage.haskell.org/`, first
write a dependency list to `docs/imports/<library>/dependencies.md` before
porting any code (one folder per library). The list must be in **topological
order of dependencies**: a module appears only after every module it depends
on (modules depending on nothing but existing linen parts come first). Add or
update `docs/imports/index.md` with the library-level topological order
(which library before which), linking to each library's `dependencies.md`.

Then import the dependencies in that order, **applying this same approach
recursively** to each one: before porting a dependency that itself pulls in
further Hackage libraries, first write its own topologically-ordered
dependency list under `docs/imports/`, then import those in turn.

### Importing from crates.io

The same convention applies when asked to import a Rust crate: write
`docs/imports/<crate>/dependencies.md` (topological module order) before
porting any code, list it in `docs/imports/index.md`, and recurse into its own
further dependencies the same way. The one difference from a Hackage import is
the precedence rule above — check the Lean stdlib, whatever `linen` already
has from Haskell, and whether a suitable Hackage package could be imported
instead, before porting anything fresh from the crate itself.

## Coding conventions

- **No `partial def`.** All recursion must be structural or have a proven
  termination argument — never use `partial` and never rely on a fuel
  parameter to dodge termination.
- **The requirement of proving everything (termination, etc.) must not lead to
  abusive simplifications.** If a genuine, faithful port needs a real
  termination proof (e.g. a self-referential type, mutual recursion), do that
  proof — don't weaken the port's type or behavior just to dodge the proof
  work (e.g. replacing a recursively-typed field with raw/undecoded data
  because the recursive decoder was hard to prove terminating). Simplifications
  are for cases upstream itself doesn't fully specify or that are genuinely
  out of scope (see the existing documented examples in this codebase), not a
  substitute for doing the proof.
- **No `sorry`.** Do not leave `sorry` in committed code unless it is genuinely
  unavoidable; if so, call it out explicitly.
- Prefer Lean standard-library objects over re-wrapping them (e.g. use `Id` for
  the identity functor rather than a bespoke wrapper).
- Document definitions with doc-comments; mathematical statements may use LaTeX
  (`$...$` / `$$...$$`) as in the existing modules.
- Group code into clearly labelled sections with `── … ──` comment banners.
