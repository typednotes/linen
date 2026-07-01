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

/-- Extra library search paths, by platform. Lean's bundled linker (`ld.lld`)
    does NOT search Debian/Ubuntu's multiarch lib dir by default, so a bare
    `-lpq`/`-lssl` fails on the GitHub runner with "unable to find library".
    We add the multiarch dirs on Linux and the keg-only Homebrew prefixes on
    macOS. A non-existent `-L` is only a linker warning, so listing several
    (x86_64 + aarch64, Apple-Silicon + Intel) is safe. -/
def libSearchDirs : Array String :=
  if System.Platform.isOSX then
    #["-L/opt/homebrew/opt/libpq/lib", "-L/usr/local/opt/libpq/lib",
      "-L/opt/homebrew/opt/openssl@3/lib", "-L/usr/local/opt/openssl@3/lib"]
  else
    #["-L/usr/lib/x86_64-linux-gnu", "-L/usr/lib/aarch64-linux-gnu"]

/-- Link flags for libpq (`ffi/postgres.c`). -/
def libpqLinkArgs : Array String := libSearchDirs ++ #["-lpq"]

/-- Link flags for OpenSSL (`ffi/jose.c`). -/
def opensslLinkArgs : Array String := libSearchDirs ++ #["-lssl", "-lcrypto"]

/-- All native link flags for the linen FFI (libpq + OpenSSL). -/
def nativeLinkArgs : Array String := libpqLinkArgs ++ opensslLinkArgs

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

/-- Compile `ffi/jose.c` (OpenSSL JOSE bindings) into an object file.
    OpenSSL's include path is discovered at build time via `pkg-config`. -/
target jose.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "jose.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "jose.c"
  let opensslCFlags ← pkgConfig #["--cflags", "openssl"]
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ opensslCFlags
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  let postgresObj ← postgres.o.fetch
  let joseObj ← jose.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "linenffi") #[networkObj, postgresObj, joseObj]

@[default_target]
lean_lib Linen where
  -- Link the native socket FFI, and precompile so the test suite's `#eval`
  -- checks can call the `@[extern]` bindings through the interpreter.
  -- `moreLinkArgs` pulls in libpq (`-lpq` and its lib path) for `postgres.o`.
  needs := #[linenffi]
  moreLinkArgs := nativeLinkArgs
  precompileModules := true

lean_lib Tests where
  needs := #[linenffi]
  moreLinkArgs := nativeLinkArgs

lean_exe linen where
  root := `Main
  moreLinkArgs := nativeLinkArgs

-- Example programs live under `Examples/` and share one entrypoint:
-- `lake exe examples <name> [args...]`.
lean_lib Examples where
  globs := #[.submodules `Examples]
  moreLinkArgs := nativeLinkArgs

lean_exe examples where
  root := `Examples.Main
  moreLinkArgs := nativeLinkArgs
