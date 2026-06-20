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

## Importing external code

When importing external code into the library — designated by a **local path**
or a **GitHub link** (e.g. a Haskell package, another Lean project, or a single
module):

- **Replace every concept that has a Lean standard-library equivalent** with that
  equivalent, and adapt the surrounding code accordingly. Do not port a bespoke
  copy of something the stdlib already provides. Examples: use `Id` for the
  identity monad, `Option`/`Except` rather than re-declared `Maybe`/`Either`,
  `· >=> ·` / `· <=< ·` for Kleisli composition, `List.foldlM` for `foldM`,
  `Functor.discard` for `void`. Only keep what the stdlib genuinely lacks.
- **Follow Lean standard-library principles for the module hierarchy and
  namespaces.** Place modules and choose namespaces the way the Lean stdlib
  would (e.g. `Data.…`, `Control.…`, `System.…`), not by mirroring the source
  project's layout or naming. Adapt identifiers to Lean naming conventions.
- After substitution, the result should read as idiomatic Lean built on the
  standard library — and still satisfy every rule below (tests, no `partial`,
  no `sorry`).

## Coding conventions

- **No `partial def`.** All recursion must be structural or have a proven
  termination argument — never use `partial` and never rely on a fuel
  parameter to dodge termination.
- **No `sorry`.** Do not leave `sorry` in committed code unless it is genuinely
  unavoidable; if so, call it out explicitly.
- Prefer Lean standard-library objects over re-wrapping them (e.g. use `Id` for
  the identity functor rather than a bespoke wrapper).
- Document definitions with doc-comments; mathematical statements may use LaTeX
  (`$...$` / `$$...$$`) as in the existing modules.
- Group code into clearly labelled sections with `── … ──` comment banners.
