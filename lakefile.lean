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

-- ── DuckDB discovery (downloaded pinned prebuilt archive, no pkg-config) ──
-- DuckDB ships no `.pc` file and has no small single-file amalgamation to
-- vendor (unlike SQLite's `sqlite3.c`), so per AGENTS.md's FFI precedence
-- note this is the "download a pinned prebuilt release archive" path: a
-- fixed DuckDB release version's platform archive is downloaded (if not
-- already cached) and unpacked into `.lake/duckdb/` at lakefile-elaboration
-- time, exactly like `pkgLinkFlags` shells out to `pkg-config` above — just
-- to `curl`/`unzip` instead. The archive itself is git-ignored (`.lake/` is
-- already in `.gitignore`), matching "do not check prebuilt per-platform
-- binaries into git".

/-- Pinned DuckDB release version (bump here only). `v1.5.4` is the latest
    stable release as of this port, and — unlike `v1.1.x` — its `duckdb.h`
    already exposes the instance-cache/client-context/arrow-options API
    surface `duckdb-ffi-1.5.0.0`'s `OpenConnect.hs` binds against. -/
def duckdbVersion : String := "1.5.4"

/-- Directory the pinned DuckDB archive is unpacked into (or, for
    `DUCKDB_PREFIX`, where a local install already lives). Relative to the
    package root, which is Lake's cwd when it elaborates `lakefile.lean` and
    runs `target`/`extern_lib` actions — the same assumption `pkgConfig`'s
    relative-path-free design already relies on. -/
def duckdbCacheDir : FilePath := ".lake" / "duckdb"

/-- The pinned release's platform- (and, on Linux, architecture-) specific
    asset name. Only macOS/Linux are resolved, matching the two legs of the
    CI matrix / AGENTS.md's FFI macOS+Linux testing requirement. -/
def duckdbArchiveName : IO String := do
  if System.Platform.isOSX then
    return "libduckdb-osx-universal.zip"
  else
    try
      let out ← IO.Process.output { cmd := "uname", args := #["-m"] }
      let arch := out.stdout.trimAscii.copy
      return if arch == "aarch64" || arch == "arm64" then
        "libduckdb-linux-arm64.zip"
      else
        "libduckdb-linux-amd64.zip"
    catch _ =>
      return "libduckdb-linux-amd64.zip"

/-- Make `p` absolute (relative to the current directory) if it isn't
    already, so paths baked into `-I`/`-L`/`-rpath` flags stay valid no
    matter what directory a later `lake`/built-binary invocation runs from. -/
def toAbsolute (p : FilePath) : IO FilePath := do
  if p.isAbsolute then
    return p
  else
    return (← IO.currentDir) / p

/-- Download (if not already cached) and unpack the pinned platform archive
    into `duckdbCacheDir`, returning that directory — the release archives
    are flat (`duckdb.h`, `duckdb.hpp`, `libduckdb.{dylib,so}` directly
    inside, no subdirectory). Idempotent: a `duckdb.h` already present short-
    circuits the whole check before any network call, so re-running `lake
    build` (including in CI, on a cold cache) only ever downloads once. -/
def ensureDuckdbUnpacked : IO FilePath := do
  let dir := duckdbCacheDir
  let header := dir / "duckdb.h"
  if ← header.pathExists then
    return dir
  IO.FS.createDirAll dir
  let name ← duckdbArchiveName
  let url := s!"https://github.com/duckdb/duckdb/releases/download/v{duckdbVersion}/{name}"
  let archivePath := dir / name
  IO.eprintln s!"[linen] downloading DuckDB {duckdbVersion} ({name})..."
  let curlOut ← IO.Process.output
    { cmd := "curl", args := #["-fsSL", "-o", archivePath.toString, url] }
  if curlOut.exitCode != 0 then
    throw <| IO.userError s!"failed to download {url}: {curlOut.stderr}"
  let unzipOut ← IO.Process.output
    { cmd := "unzip", args := #["-o", "-q", archivePath.toString, "-d", dir.toString] }
  if unzipOut.exitCode != 0 then
    throw <| IO.userError s!"failed to unpack {archivePath}: {unzipOut.stderr}"
  return dir

/-- Resolve DuckDB's include/lib directories: `DUCKDB_PREFIX` (e.g. a
    Homebrew `duckdb` prefix, laying out `include/duckdb.h` and
    `lib/libduckdb.dylib`) if that env var is set — skipping the download
    entirely, for local dev machines with `libduckdb` already installed some
    other way — else the pinned download-and-unpack path above (a single
    flat directory serving as both). Both are returned absolute. -/
def duckdbDirs : IO (FilePath × FilePath) := do
  match ← IO.getEnv "DUCKDB_PREFIX" with
  | some prefixDir =>
    let p ← toAbsolute (prefixDir : FilePath)
    return (p / "include", p / "lib")
  | none =>
    let dir ← toAbsolute (← ensureDuckdbUnpacked)
    return (dir, dir)

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
  -- DuckDB: downloaded pinned prebuilt archive (see the block above), not
  -- pkg-config. `-rpath` (supported by Lean's bundled `ld.lld` on both
  -- platforms, same flag spelling) is baked into every linked
  -- binary/shared-lib — including `linenffi`'s own `:shared` dynlib the
  -- interpreter dlopen's for `#eval` — so the dynamic loader finds
  -- `libduckdb.{dylib,so}` at its unpacked/`DUCKDB_PREFIX` location without
  -- `DYLD_LIBRARY_PATH`/`LD_LIBRARY_PATH`. Chosen over copying the shared
  -- library next to every build output because rpath is a one-time link-time
  -- flag applying uniformly to `lean_exe`, `Tests`' `#eval`s, and `Examples`
  -- alike, whereas copying would need repeating (and re-syncing on upgrade)
  -- for every one of those separate output locations.
  let (duckdbInc, duckdbLibDir) ← duckdbDirs
  let duckdbIncludeArgs : Array String := #["-I", duckdbInc.toString]
  let duckdbLinkArgs : Array String :=
    #["-L", duckdbLibDir.toString, "-lduckdb", "-Wl,-rpath," ++ duckdbLibDir.toString]
  mkDef `libpqLinkArgs pq
  mkDef `opensslLinkArgs ssl
  mkDef `zlibLinkArgs (macSdk ++ zlib)
  mkDef `keychainLinkArgs keychainLinkArgs
  mkDef `duckdbIncludeArgs duckdbIncludeArgs
  mkDef `duckdbLinkArgs duckdbLinkArgs
  mkDef `nativeLinkArgs (pq ++ ssl ++ macSdk ++ zlib ++ keychainLinkArgs ++ duckdbLinkArgs)

-- `moreLinkArgs` here also flows into `ExternLib.linkArgs` (`self.pkg.moreLinkArgs`),
-- so `linenffi`'s `:shared` dynlib — loaded directly by the interpreter for `#eval` —
-- is itself linked against Homebrew's OpenSSL. Without this, `tls.o`'s `SSL_CTX_new`
-- is left as an unbound symbol that dyld's flat-namespace fallback can resolve to
-- macOS's incompatible system `libboringssl.dylib` instead, crashing on the first call.
package linen where
  version := v!"0.10.0"
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

/-- Compile the vendored SQLite amalgamation (`ffi/vendor/sqlite3/sqlite3.c`)
    into an object file. No `pkg-config` probe is needed: SQLite ships as a
    small, self-contained, public-domain single `.c`/`.h` amalgamation, so it
    is vendored directly under `ffi/vendor/sqlite3/` rather than discovered on
    the host system (see `docs/imports/sqlite-simple/dependencies.md`'s
    "Native C library" section). `SQLITE_THREADSAFE=1` (serialized mode) keeps
    the default upstream ships; nothing here disables it. -/
target sqlite3.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "sqlite3.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "vendor" / "sqlite3" / "sqlite3.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString,
    "-I", (pkg.dir / "ffi" / "vendor" / "sqlite3").toString,
    "-DSQLITE_THREADSAFE=1"]
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Compile `ffi/sqlite3_shim.c` (the `@[extern]` entry points used by
    `Linen.Database.SQLite.Bindings`) into an object file. `#include`s the
    vendored `sqlite3.h` directly (no system header, no `pkg-config`). -/
target sqlite3_shim.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "sqlite3_shim.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "sqlite3_shim.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString,
    "-I", (pkg.dir / "ffi" / "vendor" / "sqlite3").toString]
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Compile `ffi/duckdb_shim.c` (the `@[extern]` entry points used by
    `Linen.Database.DuckDB.FFI.OpenConnect`, and by future `duckdb-ffi`
    modules as they're ported) into an object file. DuckDB's include path
    (`duckdb.h`) is resolved via the downloaded-pinned-archive/`DUCKDB_PREFIX`
    logic above, not `pkg-config` (see that block's comment for why). -/
target duckdb.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "duckdb.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "duckdb_shim.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString] ++ duckdbIncludeArgs
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  let postgresObj ← postgres.o.fetch
  let joseObj ← jose.o.fetch
  let tlsObj ← tls.o.fetch
  let zlibObj ← zlib.o.fetch
  let keychainObj ← keychain.o.fetch
  let sqlite3Obj ← sqlite3.o.fetch
  let sqlite3ShimObj ← sqlite3_shim.o.fetch
  let duckdbObj ← duckdb.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "linenffi")
    #[networkObj, postgresObj, joseObj, tlsObj, zlibObj, keychainObj, sqlite3Obj, sqlite3ShimObj,
      duckdbObj]

@[default_target]
lean_lib Linen where
  -- Link the native socket FFI, and precompile so the test suite's `#eval`
  -- checks can call the `@[extern]` bindings through the interpreter.
  -- `needs := #[linenffi]` alone already pulls in every native link flag
  -- (`nativeLinkArgs`, via `package linen`'s `moreLinkArgs` feeding
  -- `linenffi`'s `ExternLib.linkArgs`) — do NOT also set `moreLinkArgs`
  -- here: that would add the same flags a second time to this target's own
  -- link command, which is exactly what caused every per-module `:dynlib`
  -- link (and every downstream consumer's link) to pass `-rpath` twice,
  -- triggering `ld64.lld: warning: duplicate -rpath ... ignored`.
  needs := #[linenffi]
  precompileModules := true

lean_lib Tests where
  -- `Tests.Linen.Database.DuckDB.FFI.TestSupport` (a `Tests`-tree module, not
  -- a `Linen` one) declares its own `@[extern]` bindings for later test
  -- files' `#eval`s to call through the interpreter — same reason `Linen`
  -- itself precompiles, just one level down. See `Linen`'s comment above for
  -- why `moreLinkArgs` is deliberately NOT also set here.
  needs := #[linenffi]
  precompileModules := true

lean_exe linen where
  root := `Main
  needs := #[linenffi]

-- Example programs live under `Examples/` and share one entrypoint:
-- `lake exe examples <name> [args...]`.
lean_lib Examples where
  globs := #[.submodules `Examples]
  needs := #[linenffi]

lean_exe examples where
  root := `Examples.Main
  needs := #[linenffi]
