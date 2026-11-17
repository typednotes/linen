# AGENTS.md — linen

<!-- agents-md standard v4 — github.com/ngrislain/agents. Refresh with the agents-md skill. -->

Instructions for coding agents (Claude Code, Codex, pi, opencode, …) and for humans working on
this project. `CLAUDE.md` imports this file; edit this one.

## Project

`linen` is a curated standard-library companion for Lean 4: external concepts (Haskell/Hackage packages, Rust crates, …) ported in, with everything Lean already provides stripped out, plus FFI-backed modules (sockets, TLS, PostgreSQL, DuckDB, keychain, …). It is a library consumed as source (`require linen from git … @ "vx.y.z"`) by the first-party `typednotes` siblings (`infra`, `ledger`, `liaison`, `web-data`, …) and others. A change is done when every module has `LinenTest/` tests, `lake test` passes on every CI axis (Linux x86_64 and arm64, macOS arm64, consumer build, unsealable host), there is no `partial def` or `sorry`, and `README.md` (feature list, module table, counts) and `CHANGELOG.md` are current.

| Task | Command |
| --- | --- |
| Install | Lean via elan (toolchain pinned in `lean-toolchain`, `leanprover/lean4:v4.34.0`); native libs — Linux: `sudo apt-get install $(sed 's/#.*//' ci/native-deps/apt.txt \| xargs)`, macOS: `brew install libpq openssl pkg-config` plus the `PKG_CONFIG_PATH` from `.github/actions/setup-native-deps/action.yml`; Lake fetches Lean deps on `lake build` |
| Type-check | `lake build --wfail` (warnings fail the build, as in CI) |
| Test | `lake test --wfail` (builds `LinenTest`, running every `#guard`); integration: `lake exe gitfn-integration`, `lake exe gitfn-remote` (needs network + deploy key); `./ci/check-sealed-duckdb.sh` (Linux) |
| Lint / format | `TODO(agents-md): no linter or formatter is configured (no lintDriver in lakefile.lean, none in CI)` |
| Build / release | `lake build --wfail`; release: `ci/check-release.sh vX.Y.Z`, then tag (see "Releasing" below; `.github/workflows/release.yml` publishes) |
| Examples | `lake exe examples` (lists them), `lake exe examples <name> [args...]` |

Before declaring a change done, run type-check, tests and lint, and report their real output.

## Permissions: what to do freely, when asked, and only after confirming

**Do freely** — reversible and local:
- read, search, and run type-checkers, tests, linters, formatters and local builds;
- edit files in the working tree within the scope of the task, including `docs/`, `reviews/`,
  `NOTES.md`, `TODO.md`, specs and tests;
- create scratch files in git-ignored directories (`output/`, `.cache/`, `tmp/`);
- read shell history and logs to spot repetitive work (see "Suggest automating repeated work").

**Only when the user asks** — do not initiate:
- `git commit`, creating or switching branches, stashing, rebasing a local branch;
- adding, removing or upgrading dependencies (propose the change and why);
- changing the machine outside the repository — global installs, dotfiles, symlinks, services,
  shell profiles — and recording each change in the manifest (see "Manifest of system changes");
- runs that are long (> ~10 min), costly (GPU, cluster, cloud, paid APIs) or touch shared
  resources (shared caches, staging data);
- clearing `.cache/` or `output/`, or forcing a cache refresh, when the content is slow or costly
  to produce again.

**Confirm explicitly every time**, even if a similar action was approved earlier — approval does
not carry over to a new context:
- `git push` only when explicitly requested by the user for that operation. Direct pushes to
  `main`/`master` are permitted only when explicitly specified. A request specifying the push
  scope (repositories, remote, branches and any tags), including an explicit command or batch,
  counts as confirmation; do not ask again for the same operation. Approval does not extend to
  later pushes. Never force-push or rewrite pushed history;
- opening, merging or closing a PR/MR, posting review comments, tagging, releasing, publishing a
  package or image;
- deploying to any environment (dev, staging, prod);
- publishing outside the repo: Confluence, Google Docs/Sheets, Slack, email, issue trackers;
- deleting or overwriting anything not recreated by a command in this repo: branches, remote
  resources, datasets, buckets, tables, experiment outputs;
- changing infrastructure, permissions, secrets or credentials.

**Never**: commit secrets or customer data; skip hooks (`--no-verify`); rewrite pushed history;
weaken a type-check, a test or a lint rule to make it pass; claim a check passed without running it.

## Repository layout and the documents it keeps

| Path | Audience | Content | Keep in sync with |
| --- | --- | --- | --- |
| `docs/` | humans, high level | project, architecture, algorithms, code map — a hierarchy of Markdown files illustrated with SVG figures | the code, every change |
| `README.md` (root and per package) | users and contributors, close to the code | install, usage, examples, how the code is organised | the code it sits next to |
| `SPEC.md` (per module/package) | humans first | the contract the code must fulfil | types and tests check it |
| `reviews/` (git-ignored) | reviewers | reviews of commits and PRs: explanation, SVG figures, suggested comments and fixes | — (dated snapshots) |
| `output/` (git-ignored) | users of the application | large, binary or reproducible outputs of runs | — (regenerated by the CLI) |
| `.cache/` (git-ignored) | the code | cached results of slow downloads and computations | — (safe to delete) |
| `CHANGELOG.md` | users of a released artifact | one entry per version, newest on top | every release |
| `LOGBOOK.md` | humans re-running experiments | one entry per experiment, newest on top, with repeatability info | every experiment |
| `TODO.md` | everyone | planned but not executed work | when planning or finishing work |
| `NOTES.md` (anywhere) | humans and agents | things worth knowing that are not definitive documentation | when something surprising is learned |

### `docs/` — high-level documentation

- A hierarchy with an index at `docs/README.md` linking every page. Typical sections:
  `docs/project/` (goals, scope, decisions, roadmap), `docs/architecture/` (components, data
  flow, deployment), `docs/algorithms/` (methods, math, complexity, trade-offs), `docs/code/`
  (module map, key types, extension points).
- Illustrate with SVG figures stored next to the page that uses them (`docs/architecture/figures/`).
  SVG is the editable source: hand-written or generated by a committed script, legible at page
  width, in light and dark backgrounds, with a `<title>` and `<desc>`. Do not commit rendered PNGs.
- `docs/` explains *why* and *how it fits together*; `README.md` files explain *how to use and
  modify* the code next to them. Link rather than repeat; one canonical page per subject.
- Keep `docs/` in sync: a change to behaviour, architecture, an algorithm or a public interface
  updates the affected pages and figures **in the same change**. Separate measured results from
  expectations and proposals.

### `SPEC.md` — one contract per module/package

- Every module, package or component with a public surface has a `SPEC.md` beside its code: a short,
  human-readable statement of what it guarantees — purpose, inputs/outputs, invariants, error
  behaviour, performance bounds, what it explicitly does not do.
- The spec is the source of truth. Types encode as much of it as the language allows; tests check
  the rest. When code and spec disagree, fix one of them deliberately and say which.
- Number the clauses (`S1`, `S2`, …) so tests and reviews can cite them.

### `reviews/` — reviews of commits and PRs

One directory per review, `reviews/YYYY-MM-DD-<slug>/`, with a `README.md` (summary, walkthrough,
findings ranked by severity, suggested comments ready to paste, suggested fixes as diffs) and
`figures/*.svg`. A video only when the user asks for one. `reviews/` is **git-ignored**: reviews
are local working documents, not project history. Posting them anywhere requires confirmation
(see Permissions).

### `output/` — generated results, git-ignored

What an application produces that is large, not text (images, videos, models, archives, binary
tables) or reproducible from the code and its inputs goes to `output/`, which is **git-ignored**.
Organise it by command and run (`output/<command>/<run-id>/`), write next to each result what
produced it (command, parameters, commit), and make the location a CLI option with `output/` as
the default. Anything worth keeping is either reproducible from the repo (then it stays in
`output/`) or published deliberately (a release, a bucket, a document — see Permissions); small,
reviewed, hand-curated text results may be committed elsewhere, e.g. in `docs/`.

### `.cache/` — caching slow objects, git-ignored

Anything slow or costly to obtain — a large download, a long computation, a remote query — is
cached under `.cache/` (**git-ignored**), with the most idiomatic library of the language: in
Python `joblib.Memory` for function results, Parquet (or JSON for small, human-readable data) for
tables keyed by their parameters; elsewhere the equivalent (`cacache`/file caches in JS, `cached`
or file caches in Rust, …).
- The cache key covers everything that determines the result: parameters, input versions or
  checksums, and a code version. Cache only results that cannot change for a given key — never
  "the latest" of something live (pin a date, a version or a completed time window instead).
- Deleting `.cache/` must always be safe: it costs time, never correctness. Downloads are
  verified (checksum or size) before being cached.
- The CLI offers `--no-cache` and `--refresh`, and the location is configurable (config file or
  environment variable), with `.cache/` as the default.

### `CHANGELOG.md` — only if the project is released as a versioned artifact

Library, image, executable, model: one section per version, newest on top, following
[Keep a Changelog](https://keepachangelog.com) and [SemVer](https://semver.org). Unreleased changes
accumulate under `## [Unreleased]`. Bump the version and the changelog in the same change.

### `LOGBOOK.md` — only if the project runs experiments

One entry per experiment, newest on top, readable by a human who was not there: date, question,
setup, result (with numbers), conclusion, and how to repeat it — commit hash, exact command,
parameters, seeds, data snapshot, links to outputs and dashboards. Failed and inconclusive
experiments are logged too.

### `TODO.md` and `NOTES.md`

- `TODO.md`: planned but not executed work, as a checklist grouped by theme, with enough context to
  pick an item up cold. Remove items when done (the history is in git and the changelog).
- `NOTES.md`: anywhere something should be known by a human or an agent and is not definitive
  documentation — gotchas, findings, dead ends, external constraints, open questions. Promote a note
  to `docs/` or a `README.md` once it becomes definitive.

## Code

### Types: use the strongest standard checking the language offers, maximally

- Use the standard type-checker of the language/framework in its strictest practical mode, run it in
  CI, and keep it at zero errors.
  **This project:** Lean 4 (`leanprover/lean4:v4.34.0`, pinned in `lean-toolchain`), checked by
  the elaborator and kernel; `lake build --wfail` locally and in CI (and `lake test --wfail`), so
  every warning, `sorry` above all, fails the build; `autoImplicit` stays on (Lean's default) — it
  automates implicit binders, which are still fully checked. `partial def` is forbidden by the
  project rules below.
- "Strict" means settings that make the checker reject more incorrect programs. Features that only
  automate what the checker then verifies in full — type inference, Lean's `autoImplicit`
  (auto-bound implicit arguments) — are not weaknesses; leave them on, and never spend a strictness
  pass removing them.
- Make illegal states unrepresentable: precise types, enums/sum types over strings and flags, new
  types for identifiers and units, immutable data by default. Avoid `Any`, casts and ignores; when
  one is unavoidable, scope it narrowly and justify it in a comment.

### Modularity and docstrings

- Organise the code in cohesive modules with explicit, minimal public interfaces and acyclic
  dependencies. One canonical implementation per concern; delete dead paths rather than keep them.
- Every public module, class and function has a docstring written for a human user: what it is
  for, how to call it, what it returns or raises, and an example when usage is not obvious. Explain
  *why* in comments, not *what*.

### One CLI per application

An application project (anything run rather than imported) exposes **a single command-line entry
point** with a short name, e.g. `uv run <cli> <command>` in Python (a `[project.scripts]` entry),
`npx <cli>`, `cargo run --bin <cli>`. Every operation — run, train, evaluate, serve, inspect,
publish — is a subcommand of it, not a loose script.

- **Self-documented**: `<cli>`, `<cli> --help` and `<cli> <command> --help` at every level list
  the commands, arguments, defaults and an example. Running a command with no arguments prints help
  rather than doing something surprising.
- **No dead ends on missing arguments**: each argument either has a default or is asked for
  interactively. Defaults live preferably in a TOML config (`<cli>.toml`, or `[tool.<cli>]` in
  `pyproject.toml`), with hard-coded fallbacks in the code; precedence is
  flag > environment variable > config file > hard-coded default, and `--help` shows the effective
  value and where it came from.
- Interactive prompts only when stdin is a TTY; otherwise (CI, agents, pipes) fail fast with a message
  naming the missing flag. Destructive or outward-facing commands ask for confirmation and accept
  `--yes` and `--dry-run`.
- **Rich, colourful terminal UI** preferably: coloured and structured help, tables, progress bars,
  spinners, syntax-highlighted output and interactive prompts/selections (Python: `rich` +
  `rich-argparse`, Typer, Textual for full TUIs; JS: `@clack/prompts`, `ink`; Rust: `clap` with
  colours, `indicatif`, `ratatui`; Go: Charm's `bubbletea`/`lipgloss`). Degrade gracefully: plain
  output when not a TTY, and honour `NO_COLOR`.
- Large, binary or reproducible results go to `output/` (overridable with `--output`); slow
  intermediate results are cached in `.cache/` (`--no-cache`, `--refresh`) — see above.
- Data goes to stdout, logs and progress to stderr; exit codes are non-zero on failure. Offer
  `--json` where the output is consumed by other programs.

### Tests

- Tests do two jobs: **illustrate** every unit and higher-level functionality working (they are
  executable examples — readable, named after the behaviour), and **check** that the code fulfils its
  `SPEC.md` (cite clause ids, e.g. `test_s3_rejects_empty_window`).
- Cover unit, integration and end-to-end levels as relevant; prefer property-based tests for
  invariants. Every bug fix comes with a regression test. Tests are deterministic and fast by default;
  slow or networked tests are marked and opt-in.

## Working style

- Keep changes focused on the task; mention unrelated issues in `TODO.md` or `NOTES.md` instead of
  fixing them silently.
- Update code, types, tests, `SPEC.md`, `docs/`, `README.md`, `CHANGELOG.md` and figures together.
- Report faithfully: what was run, what passed, what failed, what was skipped.

### Manifest of system changes

Changes to the machine outside the repository are recorded so they can be tracked and reverted:
installing or upgrading tools globally (`brew`, `uv tool`, `pipx`, `npm -g`, `cargo install`),
editing dotfiles and configuration under `$HOME`, creating symlinks, services, launch agents or
cron jobs, changing shell profiles or system settings. (Changes inside the repo are tracked by
git; cloud resources by their own infrastructure-as-code.)
- **Where**: the XDG state directory, `${XDG_STATE_HOME:-~/.local/state}/<tool>/` — an
  application uses its own name; an agent acting by hand uses `agents/`, appending one JSON
  object per line to `manifest.jsonl`:
  `{"time": "…", "project": "…", "command": "…", "changes": ["…"], "revert": "…"}`.
- **What**: the exact command, every path or setting it changed, and what reverting needs — the
  inverse command, the backup path or the previous value. Back up a file before overwriting it.
- **Applications** that make such changes (installers, linkers, setup commands) write the
  manifest themselves and offer `history` and `revert` subcommands. A revert undoes newest first,
  skips an action whose path changed since, and is itself recorded.

### Undo log for mutating actions

An application whose actions mutate state — files, configuration, links, services, databases,
anything not regenerated by a command — keeps an **undo log**: one record per mutation, enough to
revert it.
- **At the scope of what changed**, so the log sits next to the state it can restore:

  | Scope of the mutation | Undo log |
  | --- | --- |
  | system-wide (`/etc`, `/usr/local`, system services) | `/var/lib/<app>/undo.jsonl` (Linux), `/Library/Application Support/<app>/undo.jsonl` (macOS) |
  | user-wide (`$HOME`, dotfiles, user services) | `${XDG_STATE_HOME:-~/.local/state}/<app>/undo.jsonl` |
  | project or folder (files under a working directory) | `<folder>/.<app>/undo.jsonl`, git-ignored — not in `.cache/`, which must stay safe to delete |

  An action spanning several scopes writes a record in each.
- **A record** holds: id, time, the command and run it belongs to, the target (path, key, row),
  its state before (a backup path, the previous value, or "absent") and after, and the inverse
  operation. Back up anything overwritten or deleted, next to the log (`backups/<id>/`).
- **Write-ahead**: write the record *before* mutating and mark it done after, so an interrupted
  action can still be undone or completed.
- **Undo** (`<cli> undo [id|run]`, default: the latest run): newest first, only if the target
  is still in its recorded "after" state — otherwise skip and say why; the undo is itself
  logged. Backups are kept until the user prunes them (`<cli> undo --prune …`).

### Suggest automating repeated work

Look out for tasks the user does by hand again and again, and propose to automate them.
- **Where to look**: the current session (the same commands or edits requested several times);
  the shell history (`~/.zsh_history`, `~/.bash_history`), for commands touching this project;
  CI and run logs; `LOGBOOK.md`, `NOTES.md` and `README.md` steps that are long manual sequences;
  `git log` (recurring chore commits, repeated fix-ups).
- **History and logs are sensitive**: read them locally and only for this purpose, extract
  command *patterns*, and never copy secrets, tokens, hostnames of customers or personal data into
  files, messages or suggestions.
- **Suggest, don't impose**: at a natural break, propose briefly what is repeated, how often, the
  automation (preferably a subcommand of the project's CLI; otherwise a script, a git hook, a CI
  job, a scheduled job or an agent skill) and what it saves. Implement it only if the user agrees;
  note accepted-but-deferred ideas in `TODO.md`, and do not repeat a suggestion the user declined.

## Project-specific notes

This section holds the rules that were in linen's own AGENTS.md before it adopted the
standard; they are project rules and stay in force.

### Project layout

- Library sources live under `Linen/`, mirroring their module path
  (e.g. `Linen/Data/Functor.lean` is module `Linen.Data.Functor`).
- Every source module must be imported from the library root `Linen.lean`.
- Tests live under `LinenTest/`, mirroring the source tree with a `Test` suffix
  on the file name (e.g. `Linen/Data/Functor.lean` →
  `LinenTest/Linen/Data/FunctorTest.lean`), and are imported from `LinenTest.lean`.

### Testing

- **Every module in `Linen/` must have a counterpart under `LinenTest/` with
  illustrative tests.** The test module mirrors the source path (with a `Test`
  suffix) and is added to the import list in `LinenTest.lean`.
- Tests assert correctness with `#guard`, so building the `LinenTest` library runs
  every check:

  ```
  lake test          # the package's test driver: builds `LinenTest`
  ```

- Prefer small, illustrative `#guard` examples that document intended behaviour.
  For `Prop`-valued definitions that cannot be decided by `#guard`, use
  `example ... := rfl` (or an explicit proof) to illustrate the law.

- Every code involving FFI, must be thoroughly tested.

#### FFI and native-library dependencies

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

### Releasing

A release is cut by pushing a version tag; `.github/workflows/release.yml` does
the rest. The order matters, because three things have to agree:

1. Bump `version` in `lakefile.lean`.
2. Add a `## [x.y.z] - YYYY-MM-DD` section to `CHANGELOG.md`, moving anything
   under `[Unreleased]` into it.
3. Run `ci/check-release.sh vx.y.z` **before** tagging. It checks the tag
   against the lakefile and the CHANGELOG and prints the notes that would be
   published, so a mismatch is caught locally rather than in a workflow.
4. When the user asks, commit and create the annotated local tag (confirm the tag, per
   Permissions). Push `main` and `vx.y.z` together only when explicitly requested; the
   publisher waits for successful CI on that exact release commit.

CI runs on pushes to `main` and pull requests targeting `main` (plus manual
dispatch). The release workflow requires the **latest push-to-main CI run for
the exact tag commit** to have completed successfully, and the commit to be
reachable from `main`. `ci/require-main-ci.sh` attests that full workflow:
Linux x86_64, Linux arm64, macOS, consumer builds, the unsealable host and HTTP/2
conformance. No test matrix is repeated on tags. It then publishes a GitHub
release whose notes are the CHANGELOG section. A tag with a prerelease
suffix (`v0.17.0-rc1`) publishes as a prerelease, so it does not become
"latest".

Missing/pending main CI is polled for up to two hours. Failed/cancelled CI,
invalid evidence, API errors, timeout, PR/manual results and results for another
commit block publication. A manual release retry requires an existing
version tag and attests its checked-out commit again; `GITHUB_SHA` from the
dispatch's default branch is not release evidence.

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
`.github/actions/setup-native-deps/action.yml`, used by CI and consumers — a
second copy is the kind of duplication that goes stale without anyone noticing.

### Keeping the main page current

`README.md` is the project's front page: the logo, badges, the feature list,
the module table, and the headline counts (modules / theorems / `#guard`
checks). **Whenever you add, remove, or substantially change a module, think
about updating `README.md`** so its feature list, module table, and stats stay
accurate.

### Importing external code

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

#### Importing from Hackage

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

#### Importing from crates.io

The same convention applies when asked to import a Rust crate: write
`docs/imports/<crate>/dependencies.md` (topological module order) before
porting any code, list it in `docs/imports/index.md`, and recurse into its own
further dependencies the same way. The one difference from a Hackage import is
the precedence rule above — check the Lean stdlib, whatever `linen` already
has from Haskell, and whether a suitable Hackage package could be imported
instead, before porting anything fresh from the crate itself.

### Coding conventions

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

### Prove or test everything

(Heading kept from the previous AGENTS.md, which gave it no body: every definition is
proved or tested — see "Testing" and "Coding conventions" above.)

### No half-implemented features

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
