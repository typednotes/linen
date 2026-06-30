import Lake
open System Lake DSL

package linen where
  version := v!"0.1.0"

-- ── libpq discovery (cross-platform, no hardcoded machine paths) ──
-- libpq (the PostgreSQL client library) is found differently per platform:
--   • compile flags: resolved dynamically via `pkg-config --cflags libpq`,
--     run inside the `postgres.o` build action below — this picks up the
--     keg-only Homebrew include on macOS and `/usr/include` on Linux.
--   • link flags: Lake evaluates `moreLinkArgs` purely (no IO), so we can't
--     call pkg-config there. On Linux the Ubuntu GitHub runner installs
--     `libpq-dev`, which puts libpq on the default search path, so `-lpq`
--     alone resolves. On macOS libpq is keg-only, so we add the Homebrew
--     prefixes (`-L`); a non-existent one is just a warning, so listing both
--     the Apple-Silicon and Intel locations is safe.

/-- Run `pkg-config <args>` and return its stdout split into individual flags.
    Returns `#[]` when pkg-config (or the queried package) is unavailable —
    the build then falls back to default include paths. -/
def pkgConfig (args : Array String) : IO (Array String) := do
  let out ← IO.Process.output { cmd := "pkg-config", args }
  if out.exitCode != 0 then
    return #[]
  -- pkg-config emits a space-separated line with a trailing newline; normalize
  -- whitespace to spaces, then split into individual flags.
  let normalized := (out.stdout.replace "\n" " ").replace "\t" " "
  return (normalized.splitOn " ").filter (· != "") |>.toArray

/-- Link flags for libpq. On Linux libpq is on the default search path, so a
    bare `-lpq` suffices and we avoid emitting bogus `-L` warnings in CI logs.
    On macOS (keg-only) we add the Homebrew lib prefixes. -/
def libpqLinkArgs : Array String :=
  if System.Platform.isOSX then
    #["-L/opt/homebrew/opt/libpq/lib", "-L/usr/local/opt/libpq/lib", "-lpq"]
  else
    #["-lpq"]

-- ── Native FFI (POSIX sockets + kqueue/epoll, PostgreSQL libpq) ──
-- The C shims in `ffi/` are portable across macOS and Linux. `network.c`
-- selects kqueue vs epoll via `#ifdef __APPLE__ / __linux__`; `postgres.c`
-- wraps libpq. Both compile to object files bundled into one static library
-- that Lake links into the `Linen` lib.

/-- Compile `ffi/network.c` into an object file. -/
target network.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "network.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "network.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Compile `ffi/postgres.c` (libpq bindings) into an object file.
    libpq's include path is discovered at build time via `pkg-config`. -/
target postgres.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "postgres.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "postgres.c"
  let libpqCFlags ← pkgConfig #["--cflags", "libpq"]
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ libpqCFlags
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  let postgresObj ← postgres.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "linenffi") #[networkObj, postgresObj]

@[default_target]
lean_lib Linen where
  -- Link the native socket FFI, and precompile so the test suite's `#eval`
  -- checks can call the `@[extern]` bindings through the interpreter.
  -- `moreLinkArgs` pulls in libpq (`-lpq` and its lib path) for `postgres.o`.
  needs := #[linenffi]
  moreLinkArgs := libpqLinkArgs
  precompileModules := true

lean_lib Tests where
  needs := #[linenffi]
  moreLinkArgs := libpqLinkArgs

lean_exe linen where
  root := `Main
  moreLinkArgs := libpqLinkArgs

-- Example programs live under `Examples/` and share one entrypoint:
-- `lake exe examples <name> [args...]`.
lean_lib Examples where
  globs := #[.submodules `Examples]
  moreLinkArgs := libpqLinkArgs

lean_exe examples where
  root := `Examples.Main
  moreLinkArgs := libpqLinkArgs
