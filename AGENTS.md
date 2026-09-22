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

- Every code involving FFI, must be thoroughly tested.

### FFI and native-library dependencies

- **Any module that links against a native C library (FFI) must build and
  pass its tests on every axis below.** This covers both the `ffi/*.c`
  shim and the `lakefile.lean` linking logic (`pkg-config` discovery, vendored
  source, or downloaded prebuilt archives). A native dependency that only
  builds on the contributor's own machine is not considered done.
- **CI covers four axes, not two platforms.** `lean_action_ci.yml` is
  structured this way because each axis, when it was added, immediately caught
  something the others could not see. Pure-Lean modules are
  platform-independent and elan pins the compiler to one commit, so more
  distros would only re-test identical `.olean` semantics — everything that
  varies lives at the FFI boundary:

  | Axis | Job | What only it can catch |
  |---|---|---|
  | Linux x86_64 | `build` | the sealed-DuckDB path, amd64 assets |
  | Linux **arm64** | `build` | per-arch asset names and arch-specific library discovery |
  | macOS arm64 | `build` | the dylib path, two-level namespace, no sealing |
  | **Consumer** build | `consumer` | anything that depends on `linen` being the *root* package, and **executable links** — the job also builds a consumer `lean_exe`, and a `lean_lib` never links `Scrt1.o` |
  | **Unsealable** host | `unsealable` | branches that a runner with a full toolchain cannot reach |

  The last two are the ones a contributor will not think of, so they are worth
  spelling out:

  - **Consumer.** Lake elaborates a dependency's lakefile with the
    *consumer's* directory as the working directory, while target outputs go
    to the dependency's own build directory. Built standalone the two
    coincide, so this repository's own green CI proves nothing about them. A
    path resolved against the wrong one broke every Linux consumer while CI
    stayed green (0.19.1). **Any new path in `lakefile.lean` must be anchored
    to `pkg.buildDir`/`pkg.dir`, never to `IO.currentDir`.** The same blind
    spot applies to the *link*: no target in this repository links an
    executable startup object (`Scrt1.o`), so a flag that breaks only
    `lean_exe` links — the `-L<multiarch>` glibc shadowing, fixed after
    1.0.0 — was invisible here while breaking `infra`/`ledger`/`liaison`.
    The job's `consumerApp` executable is the guard: **any change to the
    link-flag recipe must keep it linking.**
  - **Unsealable host.** GitHub's Ubuntu runners ship `g++`, so the
    missing-static-libstdc++ branch was unreachable there and shipped broken
    for a release. The job runs in `debian:bookworm-slim` *without* `g++` and
    asserts the build fails, names the fix, and is not buried under a cascade
    of secondary errors. **If you add a branch that only fires when a tool is
    absent, add a job where it is absent** — an untaken branch is untested
    code, and a fallback is exactly the code that runs when things are already
    going wrong.
- **Assert on the linked artifact when the property is not observable from a
  `#guard`.** `ci/check-sealed-duckdb.sh` inspects the symbol table
  (no undefined `_Unwind_*`, no `libstdc++.so.6` in `DT_NEEDED`, the extension
  is present). Getting this wrong does not fail a build — it aborts the
  process at run time on a path tests do not reach. See `docs/linking.md`.
- When a native library ships no `pkg-config` file and has no package in
  Ubuntu's default apt repos (so the existing `pkgConfig`/`pkgLinkFlags`
  pattern in `lakefile.lean` doesn't apply), prefer, in order: (1) vendoring
  the library's source directly under `ffi/` when a small single- or
  few-file amalgamation exists (e.g. SQLite's `sqlite3.c`/`sqlite3.h`) — this
  avoids a platform dev-package dependency entirely and pins the exact
  version in git; (2) downloading a pinned prebuilt release archive as a
  build/CI step when no *usable* amalgamation exists — do not check prebuilt
  per-platform binaries into git.
- **"Usable" is the operative word, and DuckDB is the cautionary example.**
  DuckDB *does* ship an amalgamation (`libduckdb-src.zip`: `duckdb.cpp` at
  25.6MB plus three headers), so the test is not merely whether one exists.
  Its `ExtensionHelper::LoadAllExtensions` is a literal `// nop` and it
  contains no extension code at all, so vendoring it would silently drop the
  `core_functions` SQL library; it is also a single C++ translation unit, so
  no `-j` can parallelise it, and it would add ~28MB to git permanently.
  DuckDB therefore stays in tier 2 — but for those reasons, not because no
  amalgamation exists. Before choosing tier 1, check that the amalgamation is
  *feature-complete* (diff its symbols against the project's own shared
  library) and that compiling it is affordable.
- **Equally, check that a prebuilt archive is complete.** DuckDB publishes
  both `libduckdb-linux-<arch>.zip` and `static-libs-linux-<arch>.zip`; the
  `libduckdb_static.a` in the former is core-only and *not* equivalent to the
  `libduckdb.so` beside it. See `docs/linking.md` §4.2.

## Releasing

A release is cut by pushing a version tag; `.github/workflows/release.yml` does
the rest. The order matters, because three things have to agree:

1. Bump `version` in `lakefile.lean`.
2. Add a `## [x.y.z] - YYYY-MM-DD` section to `CHANGELOG.md`, moving anything
   under `[Unreleased]` into it.
3. Run `ci/check-release.sh vx.y.z` **before** tagging. It checks the tag
   against the lakefile and the CHANGELOG and prints the notes that would be
   published, so a mismatch is caught locally rather than in a workflow.
4. Commit, `git tag -a vx.y.z`, and push the tag.

The workflow then re-runs the full suite on macOS **and** Linux — a release is
never gated on whatever CI happened to run for the branch — and publishes a
GitHub release whose notes are the CHANGELOG section. A tag with a prerelease
suffix (`v0.17.0-rc1`) publishes as a prerelease, so it does not become
"latest".

**`release.yml` is narrower than `lean_action_ci.yml`, deliberately or not —
know which.** Its `test` job runs `[ubuntu-latest, macos-latest]` only: it does
**not** cover the arm64 Linux leg, the consumer build, or the unsealable host.
So a tag is gated on two of the four axes above. Two of those three uncovered
axes have each already caught a bug that reached a release (0.19.1 and the
unsealable fallback), so do not read a green release workflow as "every axis
passed" — check `lean_action_ci.yml` on the tagged commit for that.

Two things worth knowing:

- **GitHub runs the workflow file from the tagged commit**, not from `main`. A
  tag cut before a change to `release.yml` will not see that change, and a tag
  cut before the workflow existed will not trigger it at all.
- **No build artifacts are attached, deliberately.** `linen` is consumed as
  source (`require linen from git … @ "vx.y.z"`), so the tag is the artifact.
  `.olean` files are specific to one toolchain and platform, and publishing
  them would invite a dependency that silently stops matching the consumer's
  compiler.

The native dependency list lives in one place,
`.github/actions/setup-native-deps/action.yml`, used by both workflows — a
second copy is the kind of duplication that goes stale without anyone noticing.

## Keeping the main page current

`README.md` is the project's front page: the logo, badges, the feature list,
the module table, and the headline counts (modules / theorems / `#guard`
checks). **Whenever you add, remove, or substantially change a module, think
about updating `README.md`** so its feature list, module table, and stats stay
accurate.

## Importing external code

**`typednotes` is not external.** Libraries in the `typednotes` GitHub
organisation (`typednotes/infra`, `typednotes/typednotes-compiler`, …) are
first-party siblings of this one, not third-party dependencies. Moving code from
one of them into `linen` is a **move**, not an import: it gets no
`docs/imports/` entry, no dependency list and no precedence check. Move it when
it belongs here — the test is whether more than one sibling needs it, or whether
it is a building block rather than an application concern — then **edit the
sibling in the same change** to delete its copy and use `linen`'s. Two live
copies of the same code is the outcome to avoid, and leaving the sibling
untouched is what produces one. Note that the sibling may already depend on
`linen`, in which case the dependency direction is fixed and `linen` must not
import it back.

Everything below concerns genuinely **external** code — designated by a **local
path** or a **GitHub link** outside the `typednotes` organisation (e.g. a
Haskell package, a Rust crate, another Lean project, or a single module):

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

## Prove or test everything



## No half-implemented features

If a feature (a safety check, a data model that is meant to cover several
cases, an API meant to apply uniformly across a set of kinds/types, ...) is
only wired up for some of the cases it should logically cover, that is not
"done for now" — it is a trap for whoever assumes it applies uniformly. A
2026-09-10 incident in the sibling `infra` project: an ownership/tagging
system was wired up for two kinds out of many, with every other kind silently
falling back to weaker, ledger-only behaviour; the gap was invisible until it
caused a real, destructive incident. Either implement a feature completely
for every case it claims to cover in the same change, or say loudly in the
code, the docs, and to the user exactly which cases it does **not** cover
yet — never let partial coverage look complete. When only part of a feature
can be done, stop and get explicit agreement from the user on the partial
scope before shipping it, rather than deciding unilaterally that "the common
case" is good enough.
