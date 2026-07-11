import Lake
open System Lake DSL

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

/-- Link flags for a pkg-config package: its `--libs`, plus an explicit
    `-L<libdir>` from `--variable=libdir`.

    `pkg-config --libs` omits library directories it considers "default" (e.g.
    Debian/Ubuntu's multiarch `/usr/lib/x86_64-linux-gnu`), but Lean's bundled
    `ld.lld` does NOT search those — so a bare `-lpq`/`-lssl` fails on the
    GitHub runner with "unable to find library". `--variable=libdir` reports the
    exact directory on every platform (multiarch on Linux, the keg-only Homebrew
    prefix on macOS), so no library path is ever hardcoded. -/
def pkgLinkFlags (pkg : String) : IO (Array String) := do
  let libs ← pkgConfig #["--libs", pkg]
  let libdir ← pkgConfig #["--variable=libdir", pkg]
  return (libdir.filter (· != "")).map ("-L" ++ ·) ++ libs

/-- On macOS, `libz` ships only as a versioned dylib in `/usr/lib`
    (`libz.1.dylib`, no unversioned `libz.dylib` symlink) plus a linkable
    `.tbd` stub inside the Xcode/Command Line Tools SDK — so `-lz` only
    resolves with `-L<sdk>/usr/lib` pointing at that stub. `pkg-config
    --variable=libdir zlib` reports plain `/usr/lib`, which lacks the stub,
    so `zlib`'s link flags need this extra directory alongside the ones
    `pkgLinkFlags` already returns. On Linux this returns `#[]` (there is no
    `xcrun`), and `pkgLinkFlags`'s ordinary `-lz` resolution is sufficient. -/
def macSdkLibArgs : IO (Array String) := do
  try
    let out ← IO.Process.output { cmd := "xcrun", args := #["--show-sdk-path"] }
    if out.exitCode != 0 then
      return #[]
    return #["-L" ++ out.stdout.trimAscii.copy ++ "/usr/lib"]
  catch _ =>
    return #[]

/-- On macOS, resolve the active SDK's framework search path via `xcrun` and
    return `-F<sdk>/System/Library/Frameworks -isysroot <sdk> -L<sdk>/usr/lib`,
    so `-framework Security`/`-framework CoreFoundation` (`keychain.o`'s link
    flags) resolve against Lean's bundled `ld.lld`, which — unlike a
    system-provided `cc`/`ld` — has no default framework search path baked
    in. Returns `#[]` (a harmless no-op) if `xcrun` is unavailable, e.g. on
    Linux/Windows, where these flags are never used anyway. -/
def macSdkFrameworkArgs : IO (Array String) := do
  try
    let out ← IO.Process.output { cmd := "xcrun", args := #["--show-sdk-path"] }
    if out.exitCode != 0 then
      return #[]
    let sdk := out.stdout.trimAscii.copy
    if sdk.isEmpty then
      return #[]
    return #["-F", sdk ++ "/System/Library/Frameworks", "-isysroot", sdk, "-L", sdk ++ "/usr/lib"]
  catch _ =>
    return #[]

-- Resolve the native link flags at lakefile-elaboration time via `pkg-config`.
-- This runs on the build machine (Lake recompiles the lakefile per checkout),
-- so the Ubuntu runner gets Linux paths and dev boxes get their own — with no
-- library location hardcoded. Defines `libpqLinkArgs`, `opensslLinkArgs`,
-- and `nativeLinkArgs` as plain `Array String` literals.
open Lean Elab Command in
run_cmd do
  let mkDef (n : Name) (flags : Array String) : CommandElabM Unit := do
    let lits : Array (TSyntax `term) := flags.map (fun s => quote s)
    elabCommand (← `(def $(mkIdent n) : Array String := #[$lits,*]))
  let pq ← pkgLinkFlags "libpq"
  let ssl ← pkgLinkFlags "openssl"
  let zlib ← pkgLinkFlags "zlib"
  let macSdk ← macSdkLibArgs
  -- Keychain link flags are OS-conditional: frameworks on macOS and system
  -- libraries on Windows have no `pkg-config` file, so — unlike `libpq`/
  -- `openssl`/`zlib` above — they're picked via the pure, compile-time
  -- `System.Platform.isOSX`/`isWindows` constants (the same ones Lake's own
  -- config code, e.g. `Lake/Config/LeanLib.lean`, uses for this kind of
  -- per-platform link decision) rather than any `pkg-config` probe. Linux is
  -- the one branch with a `.pc` file (`libsecret-1`), so it still goes
  -- through `pkgLinkFlags`, which degrades to `#[]` if that package is
  -- absent — matching every other optional native dependency in this file.
  let keychainLinkArgs : Array String ←
    if System.Platform.isOSX then
      macSdkFrameworkArgs.map (· ++ #["-framework", "Security", "-framework", "CoreFoundation"])
    else if System.Platform.isWindows then
      pure #["-ladvapi32", "-lcredui"]
    else
      pkgLinkFlags "libsecret-1"
  mkDef `libpqLinkArgs pq
  mkDef `opensslLinkArgs ssl
  mkDef `zlibLinkArgs (macSdk ++ zlib)
  mkDef `keychainLinkArgs keychainLinkArgs
  mkDef `nativeLinkArgs (pq ++ ssl ++ macSdk ++ zlib ++ keychainLinkArgs)

-- `moreLinkArgs` here also flows into `ExternLib.linkArgs` (`self.pkg.moreLinkArgs`),
-- so `linenffi`'s `:shared` dynlib — loaded directly by the interpreter for `#eval` —
-- is itself linked against Homebrew's OpenSSL. Without this, `tls.o`'s `SSL_CTX_new`
-- is left as an unbound symbol that dyld's flat-namespace fallback can resolve to
-- macOS's incompatible system `libboringssl.dylib` instead, crashing on the first call.
package linen where
  version := v!"0.2.0"
  moreLinkArgs := nativeLinkArgs

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

/-- Compile `ffi/tls.c` (OpenSSL TLS bindings) into an object file.
    OpenSSL's include path is discovered at build time via `pkg-config`. -/
target tls.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "tls.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "tls.c"
  let opensslCFlags ← pkgConfig #["--cflags", "openssl"]
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ opensslCFlags
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Compile `ffi/zlib.c` (zlib inflate bindings) into an object file.
    zlib's include path is discovered at build time via `pkg-config`. -/
target zlib.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "zlib.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "zlib.c"
  let zlibCFlags ← pkgConfig #["--cflags", "zlib"]
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ zlibCFlags
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Compile `ffi/keychain.c` (OS credential-store bindings) into an object
    file. `libsecret-1`'s include path (used only by the Linux `#ifdef`
    branch) is discovered at build time via `pkg-config`; this is a no-op
    `#[]` on macOS/Windows, where that branch is never compiled. -/
target keychain.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "keychain.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "keychain.c"
  let libsecretCFlags ← pkgConfig #["--cflags", "libsecret-1"]
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ libsecretCFlags
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  let postgresObj ← postgres.o.fetch
  let joseObj ← jose.o.fetch
  let tlsObj ← tls.o.fetch
  let zlibObj ← zlib.o.fetch
  let keychainObj ← keychain.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "linenffi")
    #[networkObj, postgresObj, joseObj, tlsObj, zlibObj, keychainObj]

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
