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
    CI matrix / AGENTS.md's FFI macOS+Linux testing requirement.

    **The two platforms deliberately take different assets.**

    macOS takes `libduckdb-osx-universal.zip`, whose `libduckdb.dylib` is
    linked dynamically — two-level namespace makes it immune to the unwinder
    problem described at `duckdbLinkArgs` below.

    Linux takes **`static-libs-linux-*.zip`**, not `libduckdb-linux-*.zip`,
    because Linux links DuckDB statically and *sealed*. This is not the
    obvious choice and is worth stating plainly: `libduckdb-linux-amd64.zip`
    also contains a `libduckdb_static.a`, but that archive is **not
    feature-equivalent to the `libduckdb.so` beside it**. It is a core-only
    build — it leaves `duckdb::ExtensionHelper::LoadAllExtensions` undefined
    and contains none of the `core_functions` extension
    (`CoreFunctionsExtension`/`DateTruncFun`/`ListValueFun`: 0 symbols there
    versus 10/16/4 in the shared library), so sealing it would hand Linux
    users a DuckDB missing much of the SQL function library. Bumping the
    DuckDB version does not help: v1.5.5 ships the identical gap in that
    asset.

    `static-libs-linux-*.zip` is the complete static build — `libduckdb_static.a`
    plus `libcore_functions_extension.a`,
    `libduckdb_generated_extension_loader.a` (which is what defines
    `LoadAllExtensions`), the parquet/json/icu/autocomplete/tpcds extensions,
    and DuckDB's vendored third-party archives. Linking all of them leaves no
    undefined DuckDB symbol except four weak `duckdb_zstd::ZSTD_trace_*` hooks
    and one re2 thread-local initialiser, none of which the shared library
    defines either. -/
def duckdbArchiveName : IO String := do
  if System.Platform.isOSX then
    return "libduckdb-osx-universal.zip"
  else
    try
      let out ← IO.Process.output { cmd := "uname", args := #["-m"] }
      let arch := out.stdout.trimAscii.copy
      return if arch == "aarch64" || arch == "arm64" then
        "static-libs-linux-arm64.zip"
      else
        "static-libs-linux-amd64.zip"
    catch _ =>
      return "static-libs-linux-amd64.zip"

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
  -- On Linux the short-circuit also checks for an archive that only the
  -- `static-libs-*` asset carries. A cache left behind by an earlier revision
  -- (which fetched `libduckdb-linux-*.zip`) has `duckdb.h` too, and would
  -- otherwise be accepted and then quietly fall back to dynamic linking —
  -- reinstating the aborts the sealed build exists to prevent.
  let cacheComplete ← do
    if !(← header.pathExists) then
      pure false
    else if System.Platform.isOSX then
      pure true
    else
      (dir / "libcore_functions_extension.a").pathExists
  if cacheComplete then
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

/-- Whether this is a Linux build — the one platform needing the sealed-DSO
    treatment below. Kept as a named predicate so the several places that
    branch on it read the same way. -/
def isLinuxBuild : Bool := !System.Platform.isOSX && !System.Platform.isWindows

/-- Ask `g++` for the absolute path of one of its static runtime archives.
    `-print-file-name` echoes the bare name straight back when it cannot
    locate the file, which is reported here as `none` rather than as a path
    that does not exist. -/
def gccStaticArchive (name : String) : IO (Option FilePath) := do
  try
    let out ← IO.Process.output { cmd := "g++", args := #["-print-file-name=" ++ name] }
    if out.exitCode != 0 then return none
    let p := out.stdout.trimAscii.copy
    if p == name then return none
    let fp : FilePath := p
    if ← fp.pathExists then return some fp else return none
  catch _ =>
    return none

/-- Every `.a` in `duckdbLibDir`, sorted for a reproducible link line. On
    Linux this is the whole of `static-libs-linux-*.zip`: DuckDB's core
    archive, its extensions (`core_functions` above all) and its vendored
    third-party archives. Taking the directory wholesale rather than naming
    archives individually means a DuckDB upgrade that adds or renames one
    needs no change here. -/
def duckdbLibArchives (duckdbLibDir : FilePath) : IO (Array FilePath) := do
  let entries ← duckdbLibDir.readDir
  let archives := entries.filterMap fun e =>
    if e.path.extension == some "a" then some e.path else none
  return archives.qsort (fun a b => a.toString < b.toString)

/-- The archives sealed into `libduckdb_sealed.so` on Linux: all of DuckDB's,
    then the C++ runtime and the unwinder. `none` if any piece is missing, in
    which case the caller falls back to linking DuckDB dynamically and says so
    loudly.

    See `duckdbLinkArgs`' comment in the `run_cmd` block below for *why* Linux
    needs this at all. -/
def duckdbSealedArchives (duckdbLibDir : FilePath) : IO (Option (Array String)) := do
  let ddArchives ← duckdbLibArchives duckdbLibDir
  unless ddArchives.any (·.fileName == some "libduckdb_static.a") do
    IO.eprintln s!"[linen] WARNING: no libduckdb_static.a under {duckdbLibDir}, \
      so DuckDB cannot be sealed into a self-contained shared library on this \
      Linux build. Falling back to linking libduckdb dynamically — on which \
      every DuckDB error path aborts the process under Lean (see \
      lakefile.lean's duckdbLinkArgs comment). Unset DUCKDB_PREFIX to use the \
      pinned static-libs release archive."
    return none
  -- Sealing without the `core_functions` extension would silently cost Linux
  -- much of the SQL function library, so its absence is a hard stop rather
  -- than something to discover at query time.
  unless ddArchives.any (·.fileName == some "libcore_functions_extension.a") do
    IO.eprintln s!"[linen] WARNING: libduckdb_static.a is present under \
      {duckdbLibDir} but libcore_functions_extension.a is not, which means \
      this is the core-only static build from `libduckdb-linux-*.zip` rather \
      than the complete one from `static-libs-linux-*.zip`. Sealing it would \
      produce a DuckDB missing much of the SQL function library, so falling \
      back to dynamic linking instead."
    return none
  let some stdcxx ← gccStaticArchive "libstdc++.a"
    | IO.eprintln "[linen] WARNING: no static libstdc++.a (install g++ / \
        libstdc++-dev); falling back to a dynamically linked libduckdb, on \
        which every DuckDB error path aborts the process under Lean."
      return none
  let some gccEh ← gccStaticArchive "libgcc_eh.a"
    | IO.eprintln "[linen] WARNING: no static libgcc_eh.a; falling back to a \
        dynamically linked libduckdb, on which every DuckDB error path aborts \
        the process under Lean."
      return none
  let some gccA ← gccStaticArchive "libgcc.a"
    | IO.eprintln "[linen] WARNING: no static libgcc.a; falling back to a \
        dynamically linked libduckdb, on which every DuckDB error path aborts \
        the process under Lean."
      return none
  -- `--start-group` because these archives are mutually recursive: the
  -- extension loader calls into core, core calls back into the extensions, and
  -- a single left-to-right pass resolves only some of it.
  let group := #["-Wl,--start-group"] ++ ddArchives.map (·.toString)
    ++ #[stdcxx.toString, gccEh.toString, gccA.toString, "-Wl,--end-group"]
  -- Localize every symbol taken from an archive, naming them explicitly
  -- rather than using `ALL`, so this cannot start hiding whatever else `leanc`
  -- happens to put on the link line.
  let basenames := (ddArchives.map (fun p => p.fileName.getD "")
    ++ #["libstdc++.a", "libgcc_eh.a", "libgcc.a"]).filter (· != "")
  let excludeArg := "-Wl,--exclude-libs," ++ String.intercalate ":" basenames.toList
  return some (group ++ #[excludeArg])

/-- Refuse to continue when DuckDB cannot be sealed on Linux.

    `duckdbSealedArchives` warns and answers `none` when a piece is missing, and
    the dynamic linking that follows is not a degraded-but-working
    configuration: it is the one on which *every* DuckDB error path aborts the
    process (see `duckdbLinkArgs`). There is nothing to fall back *to*, so
    continuing only moves the failure somewhere that names neither the cause nor
    the fix.

    This was a warning for one release because CI could not reach it — GitHub's
    Ubuntu runners ship `g++`, so the static libstdc++ is always present. On a
    minimal Debian, a RHEL-family image, or a container built
    `--no-install-recommends` it is reachable, and a warning in four thousand
    lines of build output is not a signal.

    The escape hatch is explicit and has to be typed. Someone deliberately
    building a subset that never touches a DuckDB error path is a real case and
    should not be blocked — but they should say so rather than inherit it from a
    missing package. -/
def requireSealedOrExplain (isLinux : Bool) (sealed : Option (Array String))
    (hasSharedDuckdb : Bool) : IO Unit := do
  if isLinux && sealed.isNone then
    match ← IO.getEnv "LINEN_ALLOW_UNSEALED_DUCKDB" with
    | some "1" =>
      -- The opt-out only leads anywhere if there is a shared library to link.
      -- The pinned Linux asset is `static-libs-linux-*.zip`, which carries
      -- archives and no `libduckdb.so`, so without `DUCKDB_PREFIX` the
      -- "fallback" is an `unable to find library -lduckdb` several thousand
      -- lines later — a worse failure than the one it was meant to bypass.
      if !hasSharedDuckdb then
        throw <| IO.userError "\
[linen] LINEN_ALLOW_UNSEALED_DUCKDB=1 was set, but there is no shared libduckdb \
to link against: the pinned Linux asset is `static-libs-linux-*.zip`, which \
contains static archives only.\n\
\n\
  The unsealed path therefore needs a DuckDB you supply yourself:\n\
      DUCKDB_PREFIX=/path/to/duckdb   # with lib/libduckdb.so and include/duckdb.h\n\
\n\
  Otherwise install a static libstdc++ and let the build seal DuckDB \
properly — see the message this opt-out was meant to bypass, and \
docs/linking.md section 4."
      IO.eprintln "[linen] WARNING: LINEN_ALLOW_UNSEALED_DUCKDB=1 — linking \
DuckDB dynamically. Every DuckDB error path will abort the process under Lean \
on Linux. See docs/linking.md section 4."
    | _ =>
      throw <| IO.userError "\
[linen] Cannot seal DuckDB on this Linux host, and an unsealed build aborts the \
process on every DuckDB error path rather than returning an error.\n\
\n\
  Install the pieces named in the WARNING above. On Debian/Ubuntu:\n\
      apt-get install -y g++            # brings libstdc++-*-dev\n\
  On RHEL/Fedora:\n\
      dnf install -y gcc-c++ libstdc++-static\n\
\n\
  If DUCKDB_PREFIX is set, unset it: the pinned static-libs archive is what \
carries the static libraries.\n\
\n\
  To build anyway — only sensible if nothing will touch a DuckDB error path — \
set LINEN_ALLOW_UNSEALED_DUCKDB=1.\n\
\n\
  Background: docs/linking.md section 4, and leanprover/lean4#15112."

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
  --
  -- On **Linux** DuckDB is *not* linked dynamically. It is sealed into one
  -- self-contained `libduckdb_sealed.so` (see `duckdbSealedLib` below)
  -- together with a static libstdc++ and libgcc, every symbol of which is
  -- localized, and everything then links against that one shared object.
  --
  -- The reason is a Lean-toolchain interaction, not a DuckDB bug. Lean's Linux
  -- toolchain statically links LLVM's libunwind into `libleanshared.so` and
  -- exports only *part* of the unwind ABI at default visibility: 10 symbols,
  -- where the system `libstdc++.so.6` imports 11. The four it does not export
  -- — `_Unwind_GetIPInfo`, `_Unwind_GetDataRelBase`, `_Unwind_GetTextRelBase`,
  -- `_Unwind_Resume_or_Rethrow` — fall through to `libgcc_s.so.1`. Since
  -- `libleanshared.so` is a `DT_NEEDED` of `bin/lean`, it precedes
  -- `libstdc++.so.6`/`libgcc_s.so.1` in the global lookup scope, so a
  -- dynamically linked libduckdb unwinds through *two* unwinders: LLVM's
  -- raises the exception, then libstdc++'s personality routine calls libgcc's
  -- `_Unwind_GetIPInfo` on an LLVM-layout context, reads a garbage IP, matches
  -- no landing pad, and calls `std::terminate` — even though DuckDB's own
  -- `catch (...)` sits in the very frame being unwound. Every DuckDB error
  -- path therefore aborted the process with SIGABRT (`terminate called after
  -- throwing an instance of 'duckdb::…Exception'` / `terminate called
  -- recursively`), which is what nine `Tests/Linen/Database/DuckDB/**` modules
  -- hit on Linux and none hit on macOS.
  --
  -- Measured, not guessed: from plain C with no Lean in the process the
  -- identical calls return `DuckDBError` normally, and `LD_PRELOAD`ing the one
  -- complete unwinder made all nine modules pass. Sealing achieves the same
  -- thing without asking anyone to set an environment variable — DuckDB's
  -- throw, its catch, its C++ runtime and its unwinder all end up inside one
  -- shared object, bound at link time and invisible to the global scope, so
  -- Lean's partial export cannot interpose any of it. Verified on the built
  -- artifact: no undefined `_Unwind_*`/`__cxa_throw`/`__cxa_begin_catch`/
  -- `__gxx_personality_v0` remain, and no `libstdc++.so.6` in `DT_NEEDED`.
  --
  -- macOS needs none of this: `libduckdb.dylib` is two-level-namespace bound
  -- directly to `/usr/lib/libc++.1.dylib`, so nothing in the process can
  -- interpose its C++ runtime or unwinder in the first place.
  let (duckdbInc, duckdbLibDir) ← duckdbDirs
  let duckdbIncludeArgs : Array String := #["-I", duckdbInc.toString]
  let duckdbDynamicLinkArgs : Array String :=
    #["-L", duckdbLibDir.toString, "-lduckdb", "-Wl,-rpath," ++ duckdbLibDir.toString]
  let sealedArchives ← if isLinuxBuild then duckdbSealedArchives duckdbLibDir else pure none
  -- Whether a *shared* libduckdb exists to fall back to. On Linux the pinned
  -- asset is `static-libs-linux-*.zip`, which contains archives only — so the
  -- unsealed path has nothing to link and is available solely to someone who
  -- supplied their own DuckDB through `DUCKDB_PREFIX`.
  let hasSharedDuckdb ← (duckdbLibDir / "libduckdb.so").pathExists
  -- `IO.currentDir` here is the **workspace** root, not this package's own
  -- directory: when `linen` is a dependency, Lake elaborates this lakefile with
  -- the *consumer's* root as the working directory. Every other path in this
  -- block is content with that — the DuckDB download and its `-I`/`-L` all live
  -- under the workspace's `.lake/duckdb` and agree with each other — but
  -- `duckdbSealedLib` is a Lake *target*, and Lake writes a target's output to
  -- `pkg.buildDir`, which is always *this* package's directory. So the `-L`
  -- naming that library must be derived from this file's location rather than
  -- from the working directory, or the two name different trees.
  --
  -- They did, and it broke every Linux consumer while this project's own CI
  -- stayed green: built standalone, the workspace root *is* the package
  -- directory and the two coincide. As a dependency they diverge, and the
  -- consumer's link fails with
  --
  --     ld.lld: error: unable to find library -lduckdb_sealed
  --
  -- moments after its log says `✔ Built linen/duckdbSealedLib` — the library
  -- exists, one directory tree over from where the linker was told to look.
  -- Standalone CI cannot catch this by construction; only a consumer build can.
  --
  -- `getFileName` is absolute here (Lake passes the resolved lakefile path), so
  -- `toAbsolute` is belt-and-braces rather than load-bearing.
  let pkgDir : FilePath := (FilePath.mk (← getFileName)).parent.getD "."
  let sealedLibDir ← toAbsolute (pkgDir / ".lake" / "build" / "ffi")
  let duckdbLinkArgs : Array String :=
    if sealedArchives.isSome then
      #["-L", sealedLibDir.toString, "-lduckdb_sealed",
        "-Wl,-rpath," ++ sealedLibDir.toString]
    else
      duckdbDynamicLinkArgs
  mkDef `libpqLinkArgs pq
  mkDef `opensslLinkArgs ssl
  mkDef `zlibLinkArgs (macSdk ++ zlib)
  mkDef `keychainLinkArgs keychainLinkArgs
  mkDef `duckdbIncludeArgs duckdbIncludeArgs
  mkDef `duckdbLinkArgs duckdbLinkArgs
  -- The pieces the sealed DSO is built from, and whether to build it at all.
  mkDef `duckdbSealedLinkArgs (sealedArchives.getD #[])
  elabCommand (← `(def $(mkIdent `duckdbUsesSealedLib) : Bool :=
    $(quote sealedArchives.isSome)))
  mkDef `nativeLinkArgs (pq ++ ssl ++ macSdk ++ zlib ++ keychainLinkArgs ++ duckdbLinkArgs)
  -- **Last**, after every `mkDef`. Throwing earlier aborts the rest of this
  -- block, so the definitions below it are never generated and the real message
  -- arrives buried under four `Unknown identifier nativeLinkArgs` errors that
  -- make it look as though the lakefile is broken rather than the host.
  requireSealedOrExplain isLinuxBuild sealedArchives hasSharedDuckdb

-- `moreLinkArgs` here also flows into `ExternLib.linkArgs` (`self.pkg.moreLinkArgs`),
-- so `linenffi`'s `:shared` dynlib — loaded directly by the interpreter for `#eval` —
-- is itself linked against Homebrew's OpenSSL. Without this, `tls.o`'s `SSL_CTX_new`
-- is left as an unbound symbol that dyld's flat-namespace fallback can resolve to
-- macOS's incompatible system `libboringssl.dylib` instead, crashing on the first call.
package linen where
  version := v!"1.0.0"
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

/-- Link `duckdb.o` plus DuckDB's static archive, a static libstdc++ and a
    static libgcc into one self-contained shared library, localizing every
    symbol that came from an archive.

    Linux only — see `duckdbLinkArgs`' comment above for the split-unwinder
    problem this exists to solve. Two details matter:

    - `--exclude-libs` names every archive explicitly instead of using `ALL`.
      Object files are unaffected by `--exclude-libs`, so the shim's 269
      `linen_duckdb_*` entry points keep default visibility and stay exported
      while DuckDB's own `duckdb_*` symbols and the whole C++ runtime become
      local. `ALL` would behave the same for `duckdb.o` today, but would also
      silently localize whatever archives `leanc` itself adds to the link
      line, which is not ours to decide.
    - Lean's own symbols (`lean_alloc_external`, …) are deliberately left
      undefined here, exactly as they are in the per-module `:dynlib`s the
      interpreter loads; they resolve against the already-loaded
      `libleanshared.so` at `dlopen` time.

    This is one shared library, not a static archive folded into every
    consumer, because `Tests` is `precompileModules`-enabled: a static DuckDB
    would be linked into each of the ~20 DuckDB test modules' `:dynlib`s
    separately. (`Linen` itself no longer precompiles — see its comment
    below — but `Tests` still does, so the reason stands.) -/
target duckdbSealedLib pkg : Dynlib := do
  let soFile := pkg.buildDir / "ffi" / nameToSharedLib "duckdb_sealed"
  let objJob ← duckdb.o.fetch
  -- `duckdbSealedLinkArgs` already carries the `--start-group`ed archives and
  -- the matching `--exclude-libs`; see `duckdbSealedArchives`.
  --
  -- These go in `traceArgs`, NOT `weakArgs`: Lake includes `traceArgs` in the
  -- build's input trace and deliberately excludes `weakArgs`. With the archive
  -- list in `weakArgs`, changing *which* archives get sealed did not invalidate
  -- the cached library, so CI (whose `.lake` is restored from cache) silently
  -- relinked nothing and kept a sealed library built from the earlier,
  -- core-only archive — which then failed at load with `undefined symbol:
  -- duckdb::ExtensionHelper::LoadAllExtensions`. The archive set is a real
  -- input to this artifact and has to be traced like one.
  buildSharedLib "duckdb_sealed" soFile #[objJob] #[]
    (traceArgs := duckdbSealedLinkArgs ++ #["-fPIC", "-lm", "-ldl", "-lpthread"])
    (linker := "leanc")

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
  let mut objs :=
    #[networkObj, postgresObj, joseObj, tlsObj, zlibObj, keychainObj, sqlite3Obj, sqlite3ShimObj]
  let staticLibFile := pkg.staticLibDir / nameToStaticLib "linenffi"
  if duckdbUsesSealedLib then
    -- `duckdb.o` must NOT also go into this static archive: it is already
    -- inside the sealed library, and a static copy would win at link time,
    -- pulling in the dynamic `libduckdb.so` again and reinstating the very
    -- abort the sealing removes.
    --
    -- The sealed library is sequenced *before* this archive's own job with
    -- `bindM`, not merely fetched alongside it. Everything that links against
    -- `linenffi` passes `-lduckdb_sealed` (via `nativeLinkArgs`), starting
    -- with `linenffi`'s own `:shared` facet — and a bare
    -- `let _ ← duckdbSealedLib.fetch` only *schedules* that build rather than
    -- waiting for it, which lost the race and failed the `:shared` link with
    -- `ld.lld: error: unable to find library -lduckdb_sealed`.
    let sealedJob ← duckdbSealedLib.fetch
    sealedJob.bindM fun _ => buildStaticLib staticLibFile objs
  else
    objs := objs.push (← duckdb.o.fetch)
    buildStaticLib staticLibFile objs

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
  -- **Deliberately not precompiled.** `precompileModules := true` here forces
  -- `Linen:shared`, a whole-library artifact, so no consumer can link against
  -- any module without first compiling all ~770 of them. Measured on a clean
  -- checkout, a consumer whose only import is `Linen.Data.Functor` (a leaf
  -- with no Linen imports) took 2333 jobs and 5m04s; with this `false`, 16
  -- jobs and 13s. README's "linking against linen" section already warns
  -- consumers off `precompileModules` for exactly this reason — setting it
  -- here imposed on every dependent the cost that warning describes.
  --
  -- `Tests` below still precompiles, and that is what keeps `Linen:shared`
  -- built for the `#eval`s that call `@[extern]` bindings through the
  -- interpreter. A consumer needing the same sets it on its own library, as
  -- `lean_action_ci.yml`'s consumer job does.
  --
  -- Splitting `linenffi` per subsystem and giving each `lean_lib` only the
  -- archives it reaches was tried and reverted: Lake builds every
  -- `extern_lib` in a dependency package regardless of any library's `needs`,
  -- so a pure-tree consumer still compiled all nine objects and still fetched
  -- DuckDB. The split cost eight extra targets and bought nothing measurable.
  precompileModules := false

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
