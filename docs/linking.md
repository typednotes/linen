# Native linking in `linen`

Reference for the native (FFI) dependencies: how each is obtained, how they are
linked, and the C++ exception/unwinder constraint that governs DuckDB's linkage
on Linux.

Read §3–§4 before linking a native library whose implementation is **C++**.
Plain C dependencies are unaffected by the whole of §3.

Measurements in this document were taken on Lean v4.33.1 (the export counts
in §5.1 also hold on v4.34.0, which this project now uses), DuckDB v1.5.4,
Ubuntu 24.04 (`libstdc++.so.6.0.33`) and macOS on Apple silicon. Statements
derived by reasoning rather than measurement are marked **(inference)**.

---

## 1. Static vs dynamic linking

**Static linking** (`libfoo.a`, an *archive*) resolves at build time. The
linker copies the archive members that satisfy undefined symbols into the
output. Afterwards there is no separate `libfoo`.

**Dynamic linking** (`libfoo.so`, `libfoo.dylib`) defers resolution. The output
records a `DT_NEEDED` entry naming the library and undefined entries naming the
symbols. At load time the dynamic loader finds the library and binds each
reference. That binding step is **symbol resolution**, and §3 is a failure of
it.

|                             | Static (`.a`)                   | Dynamic (`.so` / `.dylib`)              |
| --------------------------- | ------------------------------- | --------------------------------------- |
| Resolved                    | build time                      | load time, every run                    |
| What ships                  | code copied into the output     | a dependency record + the library file  |
| Output size                 | larger, self-contained          | smaller; library shared between programs |
| Updating the dependency     | rebuild                         | replace one file                        |
| Symbol conflicts possible   | no, once linked                 | **yes** — see §3                        |

Dynamic linking is the default for system libraries. Static linking is used
when a dependency must be hermetic — self-contained and unaffected by what else
is loaded in the process.

## 2. Position-independent code (PIC)

A shared library's load address is chosen by the loader and varies per run
(ASLR), so its code must not contain absolute addresses. PIC code refers to
symbols PC-relatively or through the global offset table; non-PIC code uses
absolute relocations such as `R_X86_64_32`/`R_X86_64_32S` and cannot be linked
into a shared object:

```
relocation R_X86_64_32S against `.rodata' can not be used when making a shared
object; recompile with -fPIC
```

Static archives are not always built with `-fPIC`, so linking one into a shared
library requires checking. For `libduckdb_static.a`, relocation types in
`.rela.text` of its largest members:

```
{'PLT32': 29550, 'PC32': 7857, 'REX_GOTPCRELX': 1409, 'GOTPCREL': 29}
non-PIC absolute (32/32S): 0
```

All relocations are PC-relative or GOT-based; there are zero absolute ones. The
archive is therefore PIC and can be linked into a shared object. Ubuntu's
`libstdc++.a` and `libgcc.a` also link successfully into one.

## 3. C++ exceptions across a dynamic-linking boundary

### 3.1 What a `throw` requires

Reaching a `catch` requires walking the stack frame by frame, testing each
frame's handler table and running destructors. Two separate libraries
cooperate:

- the **C++ runtime** — `libstdc++` (GCC) or `libc++abi` (LLVM) — decides which
  handler matches, in the *personality routine* `__gxx_personality_v0`;
- the **unwinder** — `libgcc_s` (GNU) or LLVM's `libunwind` — walks the stack
  and exposes the `_Unwind_*` ABI.

During a throw the personality routine receives an opaque `_Unwind_Context *`
from the unwinder and calls back into the unwinder with it
(`_Unwind_GetIPInfo`, `_Unwind_GetLanguageSpecificData`, …). **The layout of
that context is private to each unwinder implementation.** The two
implementations share function names, so they are link-compatible, but their
contexts are not interchangeable.

### 3.2 The measured symbol split

- `libleanshared.so` statically links LLVM's libunwind and exports **10**
  `_Unwind_*` symbols at default visibility:
  `DeleteException, ForcedUnwind, GetGR, GetIP, GetLanguageSpecificData,
  GetRegionStart, RaiseException, Resume, SetGR, SetIP`.
- Ubuntu's `libstdc++.so.6` imports **11**.
- The four not exported by `libleanshared.so` are **`_Unwind_GetIPInfo`**,
  `_Unwind_GetDataRelBase`, `_Unwind_GetTextRelBase` and
  `_Unwind_Resume_or_Rethrow`. These resolve to `libgcc_s.so.1`, which
  `libstdc++` is built against.
- `libleanshared.so` exports no `__cxa_*` or `__gxx_personality_v0`; the
  unwinder is the only C++ runtime component it exposes.

On Linux, symbol resolution uses one **global lookup scope**: an ordered list
searched front to back, first match wins — the executable, then its
`DT_NEEDED` libraries breadth-first, then objects `dlopen`ed later.
`libleanshared.so` is a direct `DT_NEEDED` of `bin/lean`, while
`libstdc++.so.6` and `libgcc_s.so.1` enter only as dependencies of
`libduckdb.so`, so `libleanshared.so` precedes them.

Consequently, when DuckDB throws:

1. `libstdc++`'s `__cxa_throw` calls `_Unwind_RaiseException`, which resolves
   to LLVM's unwinder in `libleanshared.so`.
2. LLVM's unwinder constructs an LLVM-layout `_Unwind_Context` and passes it to
   `libstdc++`'s personality routine.
3. The personality routine calls `_Unwind_GetIPInfo`, which resolves to
   libgcc's unwinder.
4. libgcc reads an LLVM-layout context and returns an invalid instruction
   pointer.
5. No frame's handler table matches, so no handler is found and
   `std::terminate` runs.

The handler exists: `duckdb_appender_create_ext`'s cold section calls
`__cxa_begin_catch` and `duckdb::ErrorData::ErrorData(std::exception const&)`,
and `duckdb::Exception` derives from `std::runtime_error`.

Two observations distinguish this from other causes:

- The identical calls from a plain C program linked against the same
  `libduckdb.so`, with Lean absent from the process, return `DuckDBError` for
  every error path tested. Inside `bin/lean` they abort.
- The abort message prints the exception type but **no `what():` line**.
  libstdc++'s verbose terminate handler prints the type via a non-virtual call,
  then rethrows internally to obtain `what()`; that rethrow fails the same way
  and re-enters the handler, producing `terminate called recursively`.

### 3.3 macOS is unaffected

Mach-O uses a **two-level namespace**: each import records the symbol *and* the
library expected to provide it. `libduckdb.dylib` is bound to
`/usr/lib/libc++.1.dylib`, so its unwinder calls cannot be resolved elsewhere.
Lean's build scripts state this asymmetry directly:
`prepare-llvm-macos.sh` notes that system libc++ means "there's no danger of
conflicts", while `prepare-llvm-linux.sh` carries the comment that static
linking "breaks cross-library C++ exceptions".

## 4. How `linen` links DuckDB

- **macOS**: `libduckdb.dylib` from `libduckdb-osx-universal.zip`, linked
  dynamically with an `-rpath` to the unpack directory. §3 does not apply.
- **Linux**: DuckDB is linked statically into one sealed shared library.

### 4.1 The sealed shared library

`libduckdb_sealed.so` (target `duckdbSealedLib` in `lakefile.lean`) contains
`duckdb.o` — the `ffi/duckdb_shim.c` entry points — plus every `.a` from the
unpacked DuckDB archive, a static `libstdc++.a`, and static
`libgcc_eh.a`/`libgcc.a`. Two linker options matter:

- `-Wl,--start-group … -Wl,--end-group` around the archives. DuckDB's core and
  its extension loader reference each other, so a single left-to-right pass
  does not resolve everything.
- `-Wl,--exclude-libs,<archives>`, which marks every symbol taken from those
  archives as local to the output. Those symbols are then not exported, so they
  cannot be resolved from the global scope, and intra-library references bind
  at link time.

**Where it is written, and where the `-L` looks, must be derived the same
way.** The target writes to `pkg.buildDir / "ffi"` — always this package's own
directory — while the flags are computed during lakefile elaboration, where
`IO.currentDir` is the *workspace* root and so is the consumer's directory
whenever `linen` is a dependency. The `-L` is therefore derived from the
lakefile's own path (`getFileName`), not from the working directory. Deriving
it from the working directory is what shipped in 0.17.0–0.19.0 and made every
Linux consumer fail to link, while every standalone build — including this
project's CI, where the two paths are the same directory — passed. A change
here is only really tested by building a consumer.

`--exclude-libs` names the archives explicitly rather than using `ALL`.
`--exclude-libs` applies only to archive members, so `duckdb.o`'s entry points
would retain default visibility under `ALL` as well; naming the archives keeps
the option from also localizing archives that `leanc` adds to the link line.

Verified on the linked artifact by `ci/check-sealed-duckdb.sh` (§4.5):

| property | value |
| --- | --- |
| undefined `_Unwind_*` / `__cxa_throw` / `__cxa_begin_catch` / `__gxx_personality_v0` | none |
| `libstdc++.so.6` in `DT_NEEDED` | absent |
| undefined `duckdb::`/`duckdb_*` symbols | none beyond the weak hooks below |
| exported `duckdb_*` | 0 (localized) |
| exported `linen_duckdb_*` | 269 |

The exceptions are four weak `duckdb_zstd::ZSTD_trace_*` hooks and one re2
thread-local initialiser, which DuckDB's own shared library also leaves
undefined.

Because the exception path is entirely internal to this library, the outcome
does not depend on what `libleanshared.so` exports.

### 4.2 Alternative: `LD_PRELOAD`

`LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libgcc_s.so.1` also resolves §3, and was
verified to make all nine affected test modules pass with Lean's own exception
handling intact. Preloaded objects are searched before the executable's
`DT_NEEDED` entries, so all 11 `_Unwind_*` symbols resolve to one complete
implementation.

|                                 | `LD_PRELOAD`                                        | Sealed library                       |
| ------------------------------- | --------------------------------------------------- | ------------------------------------ |
| Fix lives in                    | the runtime environment                             | the artifact                         |
| Scope of effect                 | the whole process                                   | one shared library                   |
| Depends on Lean's export set    | yes                                                 | no                                   |
| Covers a future C++ dependency  | yes, process-wide                                   | no — per dependency                  |
| Build requirements              | none                                                | static `libstdc++.a`/`libgcc.a`, `g++` |
| Runtime requirements            | `libgcc_s.so.1` at a distro-specific path           | none                                 |
| If misconfigured                | glibc warns on stderr and continues                 | build fails with a named cause       |
| Effect on downstream consumers  | each consumer must set it                           | none                                 |

`linen` is consumed as source (`require linen from git … @ "vx.y.z"`), so an
environment variable would have to be set by every downstream project and its
CI, and omitting it produces a process abort rather than a build error. The
sealed library is therefore what ships. `LD_PRELOAD` remains useful as a
diagnostic: if a future C++ dependency aborts this way, preloading
`libgcc_s.so.1` distinguishes a split unwinder from other causes without
changing the build.

### 4.3 Release asset selection

DuckDB publishes two Linux archives containing a static library, and they are
not equivalent.

`libduckdb-linux-<arch>.zip` contains `libduckdb.so` and a **core-only**
`libduckdb_static.a`. That archive leaves
`duckdb::ExtensionHelper::LoadAllExtensions(duckdb::DuckDB&)` undefined —
referenced but defined nowhere in it — and contains no extension code. Across
the archive: 63 259 symbols defined, 378 referenced-but-undefined, of which
exactly one is in the `duckdb::` namespace. Symbol counts against the shared
library in the same zip:

| symbol | `libduckdb_static.a` | `libduckdb.so` |
| --- | --- | --- |
| `CoreFunctionsExtension` | 0 | 10 |
| `DateTruncFun` | 0 | 16 |
| `ListValueFun` | 0 | 4 |

`static-libs-linux-<arch>.zip` is the complete static build:
`libduckdb_static.a` plus `libcore_functions_extension.a`,
`libduckdb_generated_extension_loader.a` (which defines `LoadAllExtensions`),
the parquet/json/icu/autocomplete/tpcds extensions, and DuckDB's vendored
third-party archives — 101 204 symbols defined in total. `lakefile.lean`
fetches this asset on Linux.

Bumping the DuckDB version does not substitute for this: v1.5.5 ships the same
core-only archive in `libduckdb-linux-<arch>.zip`.

Note that a *shared library* link permits undefined symbols. A missing archive
is therefore not a link error; it appears at load time as
`symbol lookup error: … undefined symbol: …`. Hence §4.5.

### 4.4 Pinning and updates

`linen` does not use a system DuckDB, before or after sealing. The version is
pinned by `duckdbVersion` in `lakefile.lean`, the archive is downloaded into
`.lake/duckdb` (not committed), and the library is found through that path. A
system DuckDB upgrade is not picked up in either linkage. Updating means
changing `duckdbVersion` and rebuilding.

What sealing changes:

- The library file can no longer be replaced in place; a relink is required.
- `DUCKDB_PREFIX` (which points the build at an existing DuckDB install) will
  usually supply only a shared library, with no static archives. The build then
  emits a `[linen] WARNING` naming the missing piece and falls back to dynamic
  linking, which is subject to §3.
- DuckDB security updates require a `duckdbVersion` bump and a `linen` release
  rather than a system package upgrade. This follows from pinning, not from
  sealing.

### 4.5 Build-time verification

`ci/check-sealed-duckdb.sh` runs in both workflows after `lake build Tests` and
asserts the properties tabulated in §4.1 against the linked library. A symbol
table is not observable from a `#guard`, and the two failure modes seen in
practice — a missing archive (§4.3) and a stale artifact (below) — produce a
runtime `symbol lookup error` in an unrelated module rather than a build error.

The stale-artifact case is a Lake detail worth knowing: `buildSharedLib`'s
`weakArgs` are **excluded** from the build's input trace, while `traceArgs` are
included. The archive list determines the library's contents and therefore
belongs in `traceArgs`; in `weakArgs`, changing which archives are sealed does
not invalidate the cached library, and CI restores `.lake` from cache.

## 5. Lean toolchain details

### 5.1 The export set is stable across releases

`script/prepare-llvm-linux.sh` contains

```
-DLEAN_CXX_STDLIB='-Wl,-Bstatic -lc++ -lc++abi -Wl,-Bdynamic'
```

byte-identical at `v4.0.0`, `v4.8.0`, `v4.15.0`, `v4.20.0`, `v4.25.0`,
`v4.30.0`, `v4.33.1`, `v4.34.0-rc2` and `master`, dating from commit
`b23627378060` (2021-10-28). Measured from the official release tarballs:

| Lean release             | `_Unwind_*` exported by `libleanshared.so` |
| ------------------------ | ------------------------------------------ |
| v4.0.0 (2023-09)         | 10                                         |
| v4.8.0 (2024-06)         | 10                                         |
| v4.15.0 (2025-01)        | 10                                         |
| v4.33.1 x86_64 (2026-08) | 10                                         |
| v4.33.1 aarch64          | 10                                         |
| v4.34.0-rc2              | 10                                         |
| v4.34.0 (2026-09)        | 10                                         |

Same ten names in v4.34.0 as in every release before it — `_Unwind_GetIPInfo`
is still absent — so upgrading the toolchain does not remove the need for §4's
sealing.

Changing Lean version does not affect §3.

### 5.2 Why ten

LLVM's libunwind splits its entry points across two objects:
`libunwind/src/UnwindLevel1.c` defines the ten listed in §3.2;
`libunwind/src/UnwindLevel1-gcc-ext.c` defines the GCC extensions, including
all four that are missing. libc++abi references the former, so
`UnwindLevel1.o` is pulled out of the static archive; nothing references the
GCC extensions, so `UnwindLevel1-gcc-ext.o` is not. The symbols remain visible
despite Lean's `-fvisibility=hidden` because libunwind marks its entry points
`__attribute__((visibility("default")))` unless `_LIBUNWIND_HIDE_SYMBOLS` is
defined.

It follows that libc++abi requires only those ten, so an all-LLVM stack has no
split. **(inference)** A Lean built against a dynamic `libstdc++` would also
have no split, since libgcc provides all eleven symbols together;
`src/CMakeLists.txt` defaults `LEAN_CXX_STDLIB` to `-lstdc++`, and the static
libc++ is an override applied in the release script. Untested.

### 5.3 Upstream status

`leanprover/lean-llvm`'s build workflow passes
`-DLIBCXX_HERMETIC_STATIC_LIBRARY=ON -DLIBCXXABI_HERMETIC_STATIC_LIBRARY=ON`
but not `-DLIBUNWIND_HIDE_SYMBOLS=ON`.

[lean4#3500](https://github.com/leanprover/lean4/pull/3500), "fix: C++
exceptions across shared libraries on Linux" (2024-02-26), addressed the same
class of problem for Lean's own code by making `libgcc_s` dynamic, leaving
`-Wl,-Bstatic -lunwind` in place.

A search of leanprover/lean4 issues and PRs for `_Unwind`, "cross-library
exceptions", `libgcc_s` and "static libc++" returns no matches, and
`--exclude-libs` and `version-script` do not appear in the repository. No
upstream issue covering this exists as of 2026-09-10.

## 6. FFI inventory

One shim per native dependency, under `ffi/`.

| Shim | Native library | Acquisition | Source in git | Implementation |
| --- | --- | --- | --- | --- |
| `network.c` | none — POSIX sockets, kqueue/epoll | libc / syscalls | n/a | C |
| `postgres.c` | libpq | `pkg-config libpq` | no | C |
| `jose.c` | OpenSSL (libcrypto) | `pkg-config openssl` | no | C |
| `tls.c` | OpenSSL (libssl, libcrypto) | `pkg-config openssl` | no | C |
| `zlib.c` | zlib | `pkg-config zlib` | no | C |
| `keychain.c` | macOS: Security, CoreFoundation<br>Linux: libsecret-1<br>Windows: advapi32, credui | `-framework` / `pkg-config libsecret-1` / `-l` | no | C |
| `sqlite3_shim.c` | SQLite 3.51.0 | vendored amalgamation | **yes** | C |
| `duckdb_shim.c` | DuckDB 1.5.4 | pinned prebuilt archive → `.lake/duckdb` | no | **C++** |

DuckDB is the only dependency whose implementation is C++, and the only one
subject to §3. C has no exceptions, so no C dependency requires an unwinder.

Reproducibility differs by tier: SQLite is fixed in git; DuckDB is
version-pinned in `lakefile.lean`; libpq, OpenSSL, zlib and libsecret are
whatever the build machine provides.

### 6.1 Embedded and client-server dependencies

`linen` binds both architectures, which determines where SQL functions execute.

| Shim | Architecture | SQL executes in | Library size |
| --- | --- | --- | --- |
| `postgres.c` → libpq | client-server | a separate `postgres` process | 0.3 MB |
| `sqlite3_shim.c` → SQLite | embedded | the calling process | 9.6 MB of source |
| `duckdb_shim.c` → DuckDB | embedded | the calling process | 67 MB (Linux `.so`) |

libpq contains no query engine; it serializes SQL onto a socket, and its
connection string is a network address
(`host=localhost port=5432 dbname=mydb user=myuser`). DuckDB and SQLite are
embedded — `openDatabase` takes `path : Option String`, a file or `none` for
in-memory — so parsing, planning and execution all occur in the calling
process, and every SQL function must be compiled into the linked library.

DuckDB's documentation uses "client" for a language binding, not a network
client. Its Rust binding is described as "an ergonomic wrapper over the DuckDB
C API", opens databases with `Connection::open_in_memory()` or
`Connection::open(path)`, and by default compiles DuckDB from bundled source
(`bundled` feature) or links a system DuckDB. Its extensions are separate cargo
features — `json` and `parquet` imply `bundled`; `autocomplete`, `icu`, `tpch`
and `tpcds` statically link their extension and imply `bundled-cmake` — so the
core-versus-extensions decision in §4.3 and §6.3 applies to every DuckDB
binding.

### 6.2 Why SQLite is vendored

Recorded in `docs/imports/sqlite-simple/dependencies.md`:

1. `direct-sqlite`, the package ported, bundles the amalgamation and compiles
   it in-tree; linking a system library is an opt-in flag, off by default.
2. SQLite ships a single self-contained public-domain `.c`/`.h` pair, its
   recommended distribution form.
3. Vendoring removes the `libsqlite3-dev`/Homebrew dependency, the CI install
   step and a `pkg-config` probe, making the SQLite build identical on any
   platform with a C compiler.
4. It fixes the version and the compile flags in git. Host SQLite versions vary
   and behaviour depends on compile-time options; `lakefile.lean` sets
   `SQLITE_THREADSAFE=1` explicitly.

`AGENTS.md` frames vendoring as the answer for a library that ships no
`pkg-config` file and has no apt package. That precondition does not hold for
SQLite — `sqlite3.pc` and `libsqlite3-dev` both exist — so the operative
criteria are the four above.

### 6.3 Why DuckDB is not vendored

DuckDB publishes an amalgamation: `libduckdb-src.zip`, four files, `duckdb.cpp`
at 25.6 MB plus `duckdb.hpp`, `duckdb.h` and `duckdb_extension.h`. Two
measurements rule it out.

**The released amalgamation is core-only.** It contains no extension code —
zero occurrences of `CoreFunctionsExtension`, `DateTruncFun`, `ListValueFun`,
`ParquetExtension` or `JSONExtension` — and defines the loader as:

```cpp
void ExtensionHelper::LoadAllExtensions(DuckDB &db) {
	// nop
}
```

The amalgamation carries a *hook* rather than the code. It defines
`DUCKDB_EXTENSION_CORE_FUNCTIONS_LINKED false` and, when that is switched on,
`#include`s `core_functions_extension.hpp` — a header the bundle does not
contain — above an upstream comment reading "TODO: rewrite package_build.py to
allow also loading out-of-tree extensions in non-cmake builds". So the
amalgamation route cannot supply extensions as shipped.

DuckDB's Rust binding shows the same boundary from the other side: its
`autocomplete`, `icu`, `tpch` and `tpcds` features each statically link an
extension and all imply `bundled-cmake`, which is git-checkout-only, whereas
the plain `bundled` (amalgamation, via `cc`) feature does not offer them.
Extensions therefore require DuckDB's CMake build and a full source tree, not
an amalgamation.

Compiling the released amalgamation and running SQL against the result:

| works | fails |
| --- | --- |
| CREATE / INSERT / SELECT / WHERE | `sum`, `avg` |
| JOIN, GROUP BY, ORDER, LIMIT | `abs`, `round` |
| window functions, CTEs, subqueries | `sin`, `sqrt`, `pow` |
| `count(*)`, `count(v)`, `min`, `max` | `date_trunc`, `date_part` |
| LIKE, `upper`, `substr`, `length` | `list_value`, `list_transform` |
| `regexp_matches`, `strftime` | `stddev`, `median`, `string_agg` |
| CSV reading | `read_parquet`, JSON type |

DuckDB's core is the engine — parser, optimizer, execution, storage — and
elementary scalar and aggregate functions live in the `core_functions`
extension, which ordinary distributions carry. The error suggests
`INSTALL core_functions; LOAD core_functions;`, which requires network access
and an extension repository.

**A single 25 MB C++ translation unit is expensive.** Measured with
`clang++ -std=c++17 -O2 -fPIC -c duckdb.cpp` on Apple silicon:

| | |
| --- | --- |
| wall time | 2 min 46 s |
| peak RSS | 8.0 GB |
| object size | 41.5 MB |

Being one translation unit, this parallelises across neither cores nor memory.
GitHub's `ubuntu-latest` provides ~16 GB and `macos-latest` roughly half.
Vendoring would also add ~28 MB of C++ to the repository, against ~40 MB
downloaded by the current approach.

Against the vendored SQLite amalgamation, compiled with the same flags the
lakefile uses (`-O2 -fPIC -DSQLITE_THREADSAFE=1`) on the same machine:

| | source | wall time | peak RSS | object |
| --- | --- | --- | --- | --- |
| SQLite (C) | 9.0 MB | **7.2 s** | **0.4 GB** | 1.4 MB |
| DuckDB amalgamation (C++) | 25.6 MB | **166 s** | **8.0 GB** | 41.5 MB |
| ratio | 2.8x | 23x | 20x | 30x |

2.8x the source but 23x the time and 20x the memory: the cost is C++ template
instantiation, not file size. This is why vendoring works for one and not the
other, and it is the measurement to repeat before vendoring anything else.

Note also that the figures above are for the *core-only* amalgamation. A build
including the extensions is necessarily larger, so its cost is at least this
**(inference)**, and 8.0 GB already approaches or exceeds a `macos-latest`
runner.

**Building a fuller amalgamation ourselves is not a way out.**
`scripts/amalgamation.py` excludes extensions deliberately: its
`compile_directories` covers `src/`, third-party and `extension/loader` only,
and it skips every `*_extension.hpp` include from `extension_helper.cpp` along
with `generated_extension_loader.hpp` and `generated_extension_headers.hpp` —
which is why `LoadAllExtensions` comes out as `// nop`. The `--extended` flag
only exports more headers for out-of-tree extension authors. Including
extensions would mean patching that script, reimplementing the CMake step that
generates `generated_extension_headers.hpp`, and re-doing both on every
upgrade — and the script has no split option, so the result is a *larger*
single translation unit, i.e. worse on the constraint that actually binds.

### The from-source alternative, measured

DuckDB's full source tree built with CMake is the only complete from-source
route, and it is the option its Rust binding uses for extensions. Measured on
the same machine at `--parallel 4` (to model a 4-vCPU runner), configured
`-DBUILD_SHELL=0 -DBUILD_UNITTESTS=0 -DBUILD_EXTENSIONS="json;parquet;icu"`:

| step | time |
| --- | --- |
| shallow clone of `v1.5.4` | 11 s |
| CMake configure | 17 s |
| compile (490 objects) | **245 s** |
| total | **262 s** |

The result passes the §6.3 SQL matrix in full — `sum`, `avg`, `date_trunc`,
`list_value`, `stddev`, `median`, `string_agg` and the rest all work — and
builds `libcore_functions_extension.a` alongside json, parquet and icu.
Unlike the single translation unit, this parallelises across cores.

So this route is not prohibitively slow. Its costs are elsewhere:

- **~120 MB vendored**, after pruning `data/` (122 MB), `test/` (47 MB) and
  `benchmark/` (6 MB) from the 294 MB tree — `src/` 24 MB, `extension/` 60 MB,
  `third_party/` 33 MB. Roughly 12x the vendored SQLite, in every clone.
- **Owning a DuckDB build**: the CMake invocation, its extension selection, and
  re-verification on every upgrade.
- **Build time on every cold CI build, on every matrix leg.** The figure above is
  from fast local cores; a runner would be slower **(inference)**.

What it would buy is real: full functionality, no build-time download, and —
since the compiler would be ours — `-stdlib=libc++` to match Lean's runtime,
removing the cause of §3 rather than isolating it and allowing §4.1's sealing
machinery and §4.5's check to be deleted.

A variant worth noting: *downloading* the pinned source tree instead of
committing it keeps the compiler control, and therefore the removal of §3,
without the ~120 MB in git.

The prebuilt approach in §4.1–§4.3 was chosen against this trade. It is
recorded here with figures so the choice can be revisited on evidence rather
than re-litigated from estimates.

## 7. Adding an FFI dependency

1. **If the library is plain C**, §3 does not apply. Use `pkg-config`
   discovery or a vendored amalgamation, as the existing shims do.
2. **If its implementation is C++ and it is linked into the process**, assume
   §3 applies until measured otherwise, and exercise an **error path** in CI on
   both platforms. A happy-path test does not reach the unwinder.
3. **Wrap every `extern "C"` entry point** in
   `catch (const std::exception&)` / `catch (...)` and return an error value.
   Note that this is not sufficient against §3, where the handler is present
   but not found.
4. **Prefer, in order**: process isolation — running the dependency as a
   subprocess, as Lean itself does for its SAT solver in
   `Lean/Meta/Tactic/BVDecide/External.lean` via `IO.Process.spawn`, which puts
   the dependency's exceptions in another process entirely; then a sealed or
   symbol-localized library (§4.1); then a documented runtime requirement
   (§4.2).
5. **Check a static archive is complete** before designing around it: diff its
   symbol table against the project's own shared library (§4.3).
6. **Check an amalgamation is feature-complete**, and measure its compile cost,
   before vendoring it (§6.3).
7. **Verify linkage properties on the artifact**, not in Lean. Symbol tables,
   `DT_NEEDED` entries and visibility are invisible to `#guard` (§4.5).

## References

- LLVM libunwind: `libunwind/src/UnwindLevel1.c`,
  `libunwind/src/UnwindLevel1-gcc-ext.c`, `libunwind/src/config.h`
- Lean: `script/prepare-llvm-linux.sh`, `script/prepare-llvm-macos.sh`,
  `src/CMakeLists.txt` (`LEAN_CXX_STDLIB`),
  `Lean/Meta/Tactic/BVDecide/External.lean` (subprocess pattern),
  [lean4#3500](https://github.com/leanprover/lean4/pull/3500)
- DuckDB: [C API](https://duckdb.org/docs/current/clients/c/overview),
  [Rust client](https://duckdb.org/docs/current/clients/rust/overview),
  release assets for `v1.5.4`
- `linen`: `lakefile.lean` (`duckdbArchiveName`, `duckdbSealedArchives`,
  `duckdbSealedLib`, `duckdbLinkArgs`), `ci/check-sealed-duckdb.sh`,
  `docs/imports/sqlite-simple/dependencies.md`
