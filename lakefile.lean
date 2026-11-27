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
    prefix on macOS), so no library path is ever hardcoded.

    **Adding that `-L` back is only safe when nothing links an executable.**
    This library's own `Linen`/`LinenTest` targets never do (see the `lean_exe`
    note below), but every consumer that links a `lean_exe` does, and on Linux
    the directory is fatal there: `/usr/lib/<multiarch>` holds the *system*
    `libc.so`, so `-lc` resolves to it while Lean's vendored `Scrt1.o` still
    references the `__libc_csu_init`/`__libc_csu_fini` compat symbols glibc
    2.34 removed — an undefined-symbol link failure inside the C runtime,
    nowhere near anything `linen` wrote. Use `pkgAbsoluteLibs` on such a
    platform instead. Kept for macOS, where Homebrew is keg-only (there is no
    system multiarch directory to shadow) and the explicit `-L` is required. -/
def pkgLinkFlags (pkg : String) : IO (Array String) := do
  let libs ← pkgConfig #["--libs", pkg]
  let libdir ← pkgConfig #["--variable=libdir", pkg]
  return (libdir.filter (· != "")).map ("-L" ++ ·) ++ libs

/-- Link flags for a pkg-config package that name each library file outright
    (`<libdir>/libfoo.so`, or `.dylib` on macOS), rather than adding its
    directory to the linker's search path.

    This is the executable-safe form of `pkgLinkFlags`: naming the file links
    exactly the intended library and adds nothing to the `-L` search order, so
    the glibc Lean bundles keeps winning `-lc` and its vendored `Scrt1.o` still
    resolves. Falls back to the bare `-lfoo` for that entry if the file is
    absent, so a distro with an unusual layout still gets a chance.

    Mirrors `infra/lakefile.lean`'s `pkgAbsoluteLibs`, the version already
    exercised by a Linux CI that links executables. -/
def pkgAbsoluteLibs (pkg : String) : IO (Array String) := do
  let libs ← pkgConfig #["--libs", pkg]
  let libdirs ← pkgConfig #["--variable=libdir", pkg]
  let libdir : Option String := (libdirs.filter (· != ""))[0]?
  let ext := if System.Platform.isOSX then "dylib" else "so"
  let mut out : Array String := #[]
  for tok in libs do
    if tok.startsWith "-L" then
      continue                                  -- deliberately dropped
    else if tok.startsWith "-l" then
      let name := (tok.drop 2).toString
      match libdir with
      | some d =>
        let candidate : FilePath := (d : FilePath) / s!"lib{name}.{ext}"
        if ← candidate.pathExists then
          out := out.push candidate.toString
        else
          out := out.push tok
      | none => out := out.push tok
    else
      out := out.push tok
  return out

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
-- already cached) and unpacked into linen's `.lake/duckdb/` only when a
-- build requests DuckDB. The archive itself is git-ignored (`.lake/` is
-- already in `.gitignore`), matching "do not check prebuilt per-platform
-- binaries into git".

/-- Pinned DuckDB release version (bump here only). `v1.5.4` is the latest
    stable release as of this port, and — unlike `v1.1.x` — its `duckdb.h`
    already exposes the instance-cache/client-context/arrow-options API
    surface `duckdb-ffi-1.5.0.0`'s `OpenConnect.hs` binds against. -/
def duckdbVersion : String := "1.5.4"

/-- DuckDB's pinned archive cache, relative to linen's package directory. -/
def duckdbCacheDir : FilePath := ".lake" / "duckdb"

/-- The pinned release's platform- (and, on Linux, architecture-) specific
    asset name. Only macOS/Linux are resolved, matching the two legs of the
    CI matrix / AGENTS.md's FFI macOS+Linux testing requirement.

    **The two platforms deliberately take different assets.**

    macOS takes `libduckdb-osx-universal.zip`, whose `libduckdb.dylib` is
    linked dynamically — two-level namespace makes it immune to the unwinder
    problem described in `docs/linking.md`.

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
def ensureDuckdbUnpacked (dir : FilePath) : IO FilePath := do
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
def duckdbDirs (pkgDir : FilePath) : IO (FilePath × FilePath) := do
  match ← IO.getEnv "DUCKDB_PREFIX" with
  | some prefixDir =>
    let p ← toAbsolute (prefixDir : FilePath)
    return (p / "include", p / "lib")
  | none =>
    let dir ← ensureDuckdbUnpacked (pkgDir / duckdbCacheDir)
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

    See `docs/linking.md` for why Linux needs this. -/
def duckdbSealedArchives (duckdbLibDir : FilePath) : IO (Option (Array String)) := do
  let ddArchives ← duckdbLibArchives duckdbLibDir
  unless ddArchives.any (·.fileName == some "libduckdb_static.a") do
    IO.eprintln s!"[linen] WARNING: no libduckdb_static.a under {duckdbLibDir}, \
      so DuckDB cannot be sealed into a self-contained shared library on this \
      Linux build. Falling back to linking libduckdb dynamically — on which \
      every DuckDB error path aborts the process under Lean (see \
      docs/linking.md). Unset DUCKDB_PREFIX to use the \
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

/-- Parse `nm` output into `(symbol, isWeak)` pairs. `nm` prints one symbol
    per line with two shapes — undefined: `                 U name` (letter,
    name), defined: `0000000000000000 T name` (address, letter, name) — and
    marks weak symbols with a lowercase/uppercase `w`/`v` letter. Weak
    undefined references never break a link, so the audit below exempt them
    (DuckDB's own *shared* library leaves its `ZSTD_trace_*` hooks and
    `__gmon_start__` undefined the same way).

    `tolerant := true` (used only when scanning the *toolchain's* libraries
    for what they define) answers `#[]` if `nm` rejects a file — a toolchain
    ships a few non-ELF `.so` files that are really linker scripts, whose
    symbols the real library beside them contributes anyway. The scan of the
    sealed library itself is strict: a failure there must not be swallowed. -/
def nmSymbolPairs (args : Array String) (tolerant : Bool := false)
    : IO (Array (String × Bool)) := do
  let out ← IO.Process.output { cmd := "nm", args }
  if out.exitCode != 0 then
    if tolerant then
      return #[]
    throw <| IO.userError s!"nm {String.intercalate " " args.toList} failed: \
      {out.stderr}"
  let isWeakLetter (l : String) : Bool := l == "w" || l == "W" || l == "v" || l == "V"
  let mut pairs : Array (String × Bool) := #[]
  for line in out.stdout.splitOn "\n" do
    let fields := (line.splitOn " ").filter (· != "")
    match fields with
    | [letter, name] => pairs := pairs.push (name, isWeakLetter letter)
    | [_addr, letter, name] => pairs := pairs.push (name, isWeakLetter letter)
    | _ => pure ()
  return pairs

/-- Audit the linked `libduckdb_sealed.so` for undefined symbols that no
    executable link will be able to resolve.

    The sealed library deliberately keeps undefined references that are fine
    for the links it participates in: Lean's own symbols (`lean_*`, resolved
    by `libleanshared.so`, which `leanc` puts on every link line) and the
    glibc/pthread/libm set Lean's bundled glibc provides. It also —
    *undeliberately*, inherited from the host `libstdc++`/`libgcc` archives —
    keeps references to host-glibc symbols Lean's bundled glibc predates
    (see `ffi/duckdb_glibc_compat.c`). A *shared library* link does not check
    any of this, which is why every `lean_lib` and `LinenTest` build on Linux
    stayed green; an *executable* link does, under `ld.lld`'s
    `--no-allow-shlib-undefined`, and a consumer's `lean_exe` referencing
    DuckDB failed with `undefined reference: __isoc23_strtoul` while this
    repository's CI ran green — its consumer job kept DuckDB out of its test
    executable for exactly this reason.

    So this is not a warning-shaped gap. After every sealed-library link, the
    audit collects every non-weak undefined dynamic symbol and checks it
    against everything the toolchain itself can resolve: the `.so`/`.a`
    exports under Lean's system library directory (`<toolchain>/lib`, plus
    its `glibc/` and `lean/` subdirectories — the exact set `leanc` places on
    every link line it performs). Anything left over is a hard build failure
    naming the symbols and `ffi/duckdb_glibc_compat.c`, rather than a
    consumer's `ld.lld` error three steps removed.

    A versioned reference (`malloc@GLIBC_2.17`) must match a definition with
    that exact version (`nm` renders a *default* version as `malloc@@…`,
    normalized here); an unversioned one may bind to any definition of the
    name. Measured on Ubuntu 24.04 (gcc 13) against the toolchain's ~2.28
    glibc: exactly `__isoc23_strtoul`, `__libc_single_threaded` and
    `_dl_find_object` fail the check — the three `duckdb_glibc_compat.c`
    exists to define. -/
def auditSealedDuckdbLib (soFile : FilePath) (toolchainLibDirs : Array FilePath)
    : IO Unit := do
  let undef ← nmSymbolPairs #["-D", "--undefined-only", soFile.toString]
  let mut defs : Array String := #[]
  for dir in toolchainLibDirs do
    unless ← dir.pathExists do
      continue
    for entry in (← dir.readDir) do
      let ext := entry.path.extension.getD ""
      unless ext == "so" || ext == "a" do
        continue
      for (n, _) in (← nmSymbolPairs #["-D", "--defined-only", entry.path.toString] (tolerant := true)) do
        defs := defs.push (n.replace "@@" "@")
      for (n, _) in (← nmSymbolPairs #["--defined-only", entry.path.toString] (tolerant := true)) do
        defs := defs.push (n.replace "@@" "@")
  let stripVersion (n : String) : String := (n.splitOn "@").headD n
  let defExact := defs
  let defBase := defs.map stripVersion
  let mut missing : Array String := #[]
  for (n, weak) in undef do
    if weak then
      continue
    let n := n.replace "@@" "@"
    let ok :=
      if n.contains "@" then
        defExact.contains n
      else
        defBase.contains (stripVersion n)
    unless ok do
      missing := missing.push n
  unless missing.isEmpty do
    throw <| IO.userError s!"\
[linen] libduckdb_sealed.so references symbols Lean's bundled glibc does not \
provide:\n\
{missing.foldl (fun a n => a ++ "    " ++ n ++ "\n") ""}\
The host C++ runtime archives sealed in (libstdc++.a/libgcc*.a) were built \
against a newer glibc than the toolchain's. Every *executable* link will \
fail under ld.lld's --no-allow-shlib-undefined (library links will not, \
which is why this builds green until a consumer's lean_exe hits it).\n\
\n\
  Extend ffi/duckdb_glibc_compat.c with a shim for each symbol above — \
each must delegate to the older-glibc equivalent or answer the \
conservative constant, never guess at real behaviour. See that file's \
header for the three already covered and why each is safe.\n\
\n\
  Background: docs/linking.md section 4."

/-- Refuse to continue when DuckDB cannot be sealed on Linux.

    `duckdbSealedArchives` warns and answers `none` when a piece is missing, and
    the dynamic linking that follows is not a degraded-but-working
    configuration: it is the one on which *every* DuckDB error path aborts the
    process (see `docs/linking.md`). There is nothing to fall back *to*, so
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

package linen where
  version := v!"4.1.0"
  -- `lake test` builds `LinenTest`, whose `#guard`s run as it elaborates.
  testDriver := "LinenTest"

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

-- A shared shim owns its platform flags; Lake propagates its Dynlib through
-- imports. Use an absolute identity on Unix so dependents need no rpath.
private def buildFFIBridge (pkg : Package) (name : String) (obj : Job FilePath)
    (args : Array String) : FetchM (Job Dynlib) := do
  let out := pkg.buildDir / "ffi" / nameToSharedLib name
  if Platform.isWindows then
    buildLeanSharedLib name out #[obj] #[] (traceArgs := args)
  else if Platform.isOSX then
    let cc ← getLeanCc
    buildSharedLib name out #[obj] #[]
      (traceArgs := #["-Wl,-install_name," ++ out.toString, "-Wl,-undefined,dynamic_lookup"] ++ args)
      (linker := cc.toString)
  else
    buildSharedLib name out #[obj] #[]
      (traceArgs := #["-Wl,-soname," ++ out.toString] ++ args) (linker := "leanc")

/-- Build the PostgreSQL shim and link to the discovered libpq. -/
target postgresNative pkg : Dynlib := do
  let args ← if Platform.isOSX then pkgLinkFlags "libpq" else pkgAbsoluteLibs "libpq"
  buildFFIBridge pkg "linen_postgres" (← postgres.o.fetch) ((← macSdkLibArgs) ++ args)

/-- Build the zlib shim and link to the system zlib (SDK stub on macOS). -/
target zlibNative pkg : Dynlib := do
  let args ← if Platform.isOSX then pkgLinkFlags "zlib" else pkgAbsoluteLibs "zlib"
  buildFFIBridge pkg "linen_zlib" (← zlib.o.fetch) ((← macSdkLibArgs) ++ args)

/-- Build the Keychain shim and link to the platform credential store. -/
target keychainNative pkg : Dynlib := do
  let args ← if Platform.isOSX then
      macSdkFrameworkArgs.map (· ++ #["-framework", "Security", "-framework", "CoreFoundation"])
    else if Platform.isWindows then pure #["-ladvapi32", "-lcredui"]
    else pkgAbsoluteLibs "libsecret-1"
  buildFFIBridge pkg "linen_keychain" (← keychain.o.fetch) args

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

private structure DuckDBBuild where
  includeDir : FilePath
  libDir : FilePath
  sealedArchives : Option (Array String)

/-- Discover or download DuckDB only when the DuckDB library requests it. -/
target duckdbConfig pkg : DuckDBBuild := do
  let (includeDir, libDir) ← duckdbDirs pkg.dir
  let sealedArchives ← if isLinuxBuild then duckdbSealedArchives libDir else pure none
  requireSealedOrExplain isLinuxBuild sealedArchives (← (libDir / "libduckdb.so").pathExists)
  return Job.pure { includeDir, libDir, sealedArchives }

/-- Compile `ffi/duckdb_shim.c` (the `@[extern]` entry points used by
    `Linen.Database.DuckDB.FFI.OpenConnect`, and by future `duckdb-ffi`
    modules as they're ported) into an object file. DuckDB's include path
    (`duckdb.h`) is resolved via the downloaded-pinned-archive/`DUCKDB_PREFIX`
    logic above, not `pkg-config` (see that block's comment for why). -/
target duckdb.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "duckdb.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "duckdb_shim.c"
  let config ← (← duckdbConfig.fetch).await
  let headerJob ← inputTextFile (config.includeDir / "duckdb.h")
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile (srcJob.zipWith (fun src _ => src) headerJob) weakArgs
    (traceArgs := #["-I", config.includeDir.toString, "-O2", "-fPIC"])
    (extraDepTrace := getLeanTrace)

/-- Compile `ffi/duckdb_glibc_compat.c` — the shims for the handful of
    host-glibc symbols the sealed-in C++ runtime references but Lean's
    bundled glibc predates (measured: `__isoc23_strtoul`,
    `__libc_single_threaded`, `_dl_find_object`) — into an object file.
    Only ever linked into `libduckdb_sealed.so` (see `duckdbSealedLib`),
    never into `linenffi`, so a host whose runtime needs none of them pays
    nothing.
    `-fvisibility=hidden` is what keeps the shims private to that one
    library: the references live inside it, resolve at its link time, and
    export nothing that could interpose a real glibc symbol. See the C
    file's header for what each shim does and why each is safe. -/
target duckdb_compat.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "duckdb_compat.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "duckdb_glibc_compat.c"
  buildO oFile srcJob #[] (traceArgs := #["-O2", "-fPIC", "-fvisibility=hidden"])
    (extraDepTrace := getLeanTrace)

/-- Link `duckdb.o` plus DuckDB's static archive, a static libstdc++ and a
    static libgcc into one self-contained shared library, localizing every
    symbol that came from an archive.

    Linux only — see `docs/linking.md` for the split-unwinder
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
    consumer, because `LinenTest` is `precompileModules`-enabled: a static DuckDB
    would be linked into each of the ~20 DuckDB test modules' `:dynlib`s
    separately. (`Linen` itself no longer precompiles — see its comment
    below — but `LinenTest` still does, so the reason stands.)

    The glibc-compat object (`duckdb_compat.o`) links in beside the shim so
    the host C++ runtime's references to newer-glibc symbols — the ones that
    made every consumer *executable* link on Linux fail under
    `--no-allow-shlib-undefined` while library links stayed green — resolve
    inside this library. `linenffi`'s action below then audits the linked
    artifact for any the shim does not cover, so a future host runtime that
    references some newer symbol again is a loud, named failure here rather
    than a consumer's `ld.lld` error. -/
target duckdbSealedLib pkg : Dynlib := do
  let soFile := pkg.buildDir / "ffi" / nameToSharedLib "duckdb_sealed"
  let config ← (← duckdbConfig.fetch).await
  let some archives := config.sealedArchives
    | error "DuckDB sealing is unavailable on this host"
  let objJob ← duckdb.o.fetch
  let compatJob ← duckdb_compat.o.fetch
  -- `archives` already carries the `--start-group`ed archives and
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
  let sealed ← buildSharedLib "duckdb_sealed" soFile #[objJob, compatJob] #[]
    (traceArgs := archives ++ #["-Wl,-soname," ++ soFile.toString, "-fPIC", "-lm", "-ldl", "-lpthread"])
    (linker := "leanc")
  let sysLibDir ← getLeanSystemLibDir
  sealed.mapM fun lib => do
    liftM <| auditSealedDuckdbLib lib.path #[sysLibDir, sysLibDir / "glibc", sysLibDir / "lean"]
    return lib

/-- Build the DuckDB shim as an import-scoped native dependency. On macOS
    and the explicit unsealed Linux path, only the shim lives in this DSO. -/
target duckdbNative pkg : Dynlib := do
  let config ← (← duckdbConfig.fetch).await
  if config.sealedArchives.isSome then
    duckdbSealedLib.fetch
  else
    let name := "linen_duckdb"
    let out := pkg.buildDir / "ffi" / nameToSharedLib name
    let lib := config.libDir / nameToSharedLib "duckdb"
    let libJob ← inputBinFile lib
    let obj ← duckdb.o.fetch
    let identity := if Platform.isOSX then
      #["-Wl,-install_name," ++ out.toString, "-Wl,-undefined,dynamic_lookup"]
    else #["-Wl,-soname," ++ out.toString]
    let linker ← getLeanCc
    buildSharedLib name out #[obj.zipWith (fun path _ => path) libJob] #[]
      (traceArgs := identity ++ (← macSdkLibArgs) ++
        #[lib.toString, "-Wl,-rpath," ++ config.libDir.toString])
      (linker := linker.toString)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  let joseObj ← jose.o.fetch
  let tlsObj ← tls.o.fetch
  let sqlite3Obj ← sqlite3.o.fetch
  let sqlite3ShimObj ← sqlite3_shim.o.fetch
  -- OpenSSL is supplied by Lean's linker. Adding the system OpenSSL could
  -- select an incompatible libc or TLS implementation.
  let objs := #[networkObj, joseObj, tlsObj, sqlite3Obj, sqlite3ShimObj]
  let staticLibFile := pkg.staticLibDir / nameToStaticLib "linenffi"
  buildStaticLib staticLibFile objs

-- DuckDB modules import ordinary Linen modules, so the umbrella cannot
-- belong to their shared dependency: it would introduce a shared-facet cycle.
lean_lib LinenCore where
  roots := #[]
  globs := #[
    `Linen.CDP.*, `Linen.Cloud.*, `Linen.Codec.+, `Linen.Control.+,
    .one `Linen.Crypto.AES, .one `Linen.Crypto.ConstantTime, `Linen.Crypto.JOSE.+,
    .one `Linen.Crypto.MD5, .one `Linen.Crypto.RC4, .one `Linen.Crypto.SecureRandom,
    .one `Linen.Crypto.SHA1, .one `Linen.Crypto.SHA256, .one `Linen.Crypto.SigV4,
    `Linen.Data.+, `Linen.DataFrame.+, `Linen.Database.Redis.*,
    `Linen.Database.SQL.+, `Linen.Database.SQLite.*,
    `Linen.Graphics.+, `Linen.Network.+, `Linen.Numeric.+, `Linen.Options.+,
    `Linen.PostgREST.+, `Linen.System.Console.+, `Linen.System.Exit.*,
    `Linen.System.FilePath.+, `Linen.System.Git.+, `Linen.System.GitFn.*,
    .one `Linen.System.IO, .one `Linen.System.LakeLog, `Linen.System.Log.+,
    `Linen.System.Posix.+, .one `Linen.System.Process, .one `Linen.System.TimeManager,
    .one `Linen.System.Worker, `Linen.System.Cell.*, `Linen.Text.+, `Linen.Time.+, `Linen.Web.+]
  needs := #[linenffi]
  -- Keep ordinary imports cheap; callers can opt into precompilation when
  -- they use `#eval` on imported FFI bindings.
  precompileModules := false

-- These small libraries own their imports exclusively, so their shared
-- facets cannot form a cycle with LinenCore.
lean_lib LinenPostgreSQL where
  roots := #[`Linen.Database.PostgreSQL.LibPQ]
  needs := #[postgresNative]
  moreLinkLibs := #[postgresNative]

lean_lib LinenZlib where
  roots := #[`Linen.Crypto.Zlib]
  globs := #[.submodules `Linen.Crypto.Zlib]
  needs := #[zlibNative]
  moreLinkLibs := #[zlibNative]

lean_lib LinenKeychain where
  roots := #[]
  globs := #[.one `Linen.System.Keychain]
  needs := #[keychainNative]
  moreLinkLibs := #[keychainNative]

-- This is a library dependency, not an `extern_lib` (package-wide).
lean_lib LinenDuckDB where
  roots := #[`Linen.Database.DuckDB]
  globs := #[.submodules `Linen.Database.DuckDB]
  needs := #[duckdbNative]
  moreLinkLibs := #[duckdbNative]

@[default_target]
lean_lib Linen where
  roots := #[]
  globs := #[.one `Linen]

lean_lib LinenTest where
  -- `LinenTest.Linen.Database.DuckDB.FFI.TestSupport` (a `LinenTest`-tree module, not
  -- a `Linen` one) declares its own `@[extern]` bindings for later test
  -- files' `#eval`s to call through the interpreter.
  needs := #[linenffi]
  precompileModules := true

-- **Not a default target, deliberately.** `lake build`/`lean-action` link only
-- `lean_lib Linen`, and a *library* link produces `.olean`s and a `:shared`
-- dynlib, never the executable startup object (`Scrt1.o`). That left the whole
-- "does an executable link?" question untested here: the `-L<multiarch>` in
-- `pkgLinkFlags` shadowed the bundled glibc and broke every consumer's
-- `lean_exe` on Linux (`undefined symbol: __libc_csu_init`) while this repo's
-- CI stayed green. `infra/lakefile.lean` documented exactly that gap; the
-- `consumer-exe` job in `.github/workflows/lean_action_ci.yml` is what now
-- closes it, by linking a consumer executable that names libpq's file outright.
--
-- The consumer job tests both an executable that uses the sealed library and
-- one that does not import or link DuckDB.
lean_exe linen where
  root := `Main
  needs := #[linenffi]

-- The `System.GitFn` integration test: it creates a local fixture repository,
-- fetches and checks it with the secure policy, builds workers with nested
-- Lake builds and calls them over stdio and HTTP. It needs `git` and the
-- pinned toolchain and takes minutes, so it is an executable CI runs
-- (`lake exe gitfn-integration`), not a `#guard`. `supportInterpreter`:
-- parsing imports library environments, whose initializers must be able to run.
lean_exe «gitfn-integration» where
  root := `Integration.GitFn
  needs := #[linenffi]
  supportInterpreter := true

-- `System.GitFn` against a real, private GitHub repository (`typednotes/test`
-- at a pinned commit), which carries its own expectations (`gitfn.json`).
-- Needs network access and a credential for it; CI runs it with the
-- repository's deploy key, when the `GITFN_DEPLOY_KEY` secret is set
-- (`lake exe gitfn-remote`).
lean_exe «gitfn-remote» where
  root := `Integration.GitFnRemote
  needs := #[linenffi]
  supportInterpreter := true

-- The consumer imports just the contract. Both transports use the separate worker.
lean_lib CellFixture where
  roots := #[`Integration.CellContract]

lean_exe «cell-integration» where
  root := `Integration.Cell
  needs := #[linenffi]

lean_exe «cell-worker-fixture» where
  root := `Integration.CellWorker
  needs := #[linenffi]

-- Example programs live under `Examples/` and share one entrypoint:
-- `lake exe examples <name> [args...]`.
lean_lib Examples where
  globs := #[.submodules `Examples]
  needs := #[linenffi]

lean_exe examples where
  root := `Examples.Main
  needs := #[linenffi]
