# Native linking in `linen`

Why this document exists: on 2026-09-10, nine `Tests/Linen/Database/DuckDB/**`
modules aborted on Linux — and only on Linux — with

```
terminate called after throwing an instance of 'duckdb::CatalogException'
terminate called recursively
error: Lean exited with code 134
```

on code paths that returned an ordinary error value on macOS. The cause turned
out to have nothing to do with DuckDB and everything to do with how Lean, C++
runtimes and dynamic linking interact on Linux. The fix is in
`lakefile.lean`'s `duckdbLinkArgs`/`duckdbSealedLib`; this page explains the
concepts behind it, so the next person to add an FFI dependency can recognise
the same trap before it costs them a day.

Read this if you are about to link a native library — especially a **C++** one.

---

## 1. Static vs dynamic linking

A library can join your program at two different moments.

**Static linking** (`libfoo.a`, an *archive*) happens at build time. The
linker copies the machine code you actually use out of the archive and into
your output file. Afterwards there is no `libfoo` — there is just your binary,
with foo's code inside it.

**Dynamic linking** (`libfoo.so` on Linux, `libfoo.dylib` on macOS) defers the
work. Your output records a *note*: "I need `libfoo.so`, and I need the symbols
`foo_open`, `foo_close`, …". At program start the **dynamic loader** reads those
notes, finds the libraries, and patches every reference to point at the real
addresses. This last step is **symbol resolution**, and it is where our bug
lived.

|                                   | Static (`.a`)                     | Dynamic (`.so` / `.dylib`)              |
| --------------------------------- | --------------------------------- | --------------------------------------- |
| Resolved                          | Build time, permanently           | Every run, by the loader                |
| What ships                        | Code copied into your binary      | A dependency note + the separate file   |
| Size on disk                      | Larger binary, self-contained     | Smaller binary, library shared          |
| Patch the dependency (e.g. a CVE) | Rebuild and redistribute          | Replace one file                        |
| Multiple programs using it        | Each carries its own copy         | One copy in memory, shared              |
| Symbol conflicts                  | Impossible once linked            | **Possible** — see §3                   |
| Debug/crash reports               | Everything in one place           | Needs the right library version present |

Neither is "better". Dynamic linking is the sane default for system libraries
you don't control (libc, OpenSSL, libpq). Static linking is the right answer
when you need a dependency to be *hermetic* — pinned, self-contained, and
immune to whatever else is loaded in the process. Our DuckDB fix needs exactly
that, for the reason in §3.

## 2. What "PIC" means, and why it gated the fix

**PIC = Position-Independent Code.**

Machine code has to refer to things — a function to call, a string constant.
There are two ways to write those references:

- **Absolute**: "jump to address `0x4051a0`". Simple, but only correct if the
  code is loaded at the exact address the linker assumed.
- **Relative**: "jump to 300 bytes past wherever I currently am", or "look the
  address up in my table". Works anywhere.

PIC is code written the second way. It matters because a shared library
**cannot know its load address in advance**: the loader places it wherever
there is room, and on modern systems ASLR deliberately randomises that on every
run. So all code inside a `.so` must be PIC — hence `-fPIC` when compiling
anything destined for a shared library.

Static archives, by contrast, are traditionally built *without* `-fPIC`,
because they were meant for executables, which do load at a predictable
address. This is why you sometimes see:

```
relocation R_X86_64_32S against `.rodata' can not be used when making a shared
object; recompile with -fPIC
```

**Why this gated our fix.** The fix requires putting DuckDB's *static* archive
*inside* a shared library — legal only if that archive is PIC. Rather than
discover that after rewriting the build, it was checked up front by parsing the
archive's relocation types:

```
$ # relocation types in .rela.text of the largest members
{'PLT32': 29550, 'PC32': 7857, 'REX_GOTPCRELX': 1409, 'GOTPCREL': 29}
  non-PIC absolute (32/32S): 0    GOT-based (PIC): 1438
```

Every relocation is PC-relative (`PC32`, `PLT32`) or goes through the global
offset table (`GOTPCREL`, `REX_GOTPCRELX`); there are **zero** absolute
`R_X86_64_32`/`32S` relocations. `libduckdb_static.a` is PIC, so the plan was
viable. Ubuntu's `libstdc++.a` and `libgcc.a` turned out to be usable the same
way.

## 3. The bug: two unwinders in one process

### What a C++ `throw` actually needs

`throw` is not a jump. To get from a `throw` to the matching `catch`, the
runtime must walk back up the call stack, and at each frame ask "does this
function have a handler for this type?", running destructors on the way. The
component that does the walking is the **unwinder**, and it is a *separate
library* from the C++ standard library:

- the **C++ runtime** (`libstdc++` for GCC, `libc++abi` for LLVM) decides
  *which* handler matches — that logic lives in a function called the
  *personality routine*, `__gxx_personality_v0`;
- the **unwinder** (`libgcc_s` for GNU, LLVM's `libunwind` for LLVM) does the
  mechanical stack walking and exposes an ABI of `_Unwind_*` functions.

The two talk constantly during a throw. The personality routine receives an
opaque `_Unwind_Context *` from the unwinder and calls back into the unwinder —
`_Unwind_GetIPInfo` to ask "which instruction are we at?",
`_Unwind_GetLanguageSpecificData` to find the frame's handler table, and so on.

That context is **an internal data structure whose layout is private to each
unwinder implementation**. GNU's and LLVM's are not compatible. Both implement
the same *function names*, so they are link-compatible; their *contexts* are
not. Mixing them within a single throw is undefined behaviour, and in practice
means reading garbage.

### How the two got mixed

Measured on Ubuntu with Lean v4.33.1:

- `libleanshared.so` statically links LLVM's `libunwind` and **exports 10**
  `_Unwind_*` symbols at default visibility.
- The system `libstdc++.so.6` **imports 11**.
- The four it does not export are `_Unwind_GetIPInfo`,
  `_Unwind_GetDataRelBase`, `_Unwind_GetTextRelBase` and
  `_Unwind_Resume_or_Rethrow`. (`_Unwind_GetIPInfo` is the one that hurts.)

Now add §1's symbol resolution. On Linux, resolution uses a single **global
lookup scope**: an ordered list of loaded objects, searched front to back, first
match wins. The order is the executable, then its `DT_NEEDED` libraries
breadth-first, then anything `dlopen`ed later. `libleanshared.so` is a direct
`DT_NEEDED` of `bin/lean`, while `libstdc++.so.6` and `libgcc_s.so.1` enter
only as dependencies of `libduckdb.so`. So `libleanshared.so` is **ahead** of
them.

The result, when DuckDB throws:

1. `libstdc++`'s `__cxa_throw` calls `_Unwind_RaiseException` — resolved to
   **LLVM's** unwinder, inside `libleanshared.so`.
2. LLVM's unwinder builds an **LLVM-layout** `_Unwind_Context` and hands it to
   `libstdc++`'s personality routine.
3. The personality routine calls `_Unwind_GetIPInfo`, which Lean does *not*
   export — so it resolves to **libgcc's** unwinder.
4. libgcc reads an LLVM-layout context as if it were its own, and returns a
   garbage instruction pointer.
5. No frame's handler table matches that address. The runtime concludes there
   is no handler anywhere and calls `std::terminate`.

DuckDB's own `catch (...)` was in the very frame being unwound. Verified in the
disassembly: `duckdb_appender_create_ext`'s cold section calls
`__cxa_begin_catch` and `duckdb::ErrorData::ErrorData(std::exception const&)`,
and `duckdb::Exception` derives from `std::runtime_error`. The handler existed
and was never found.

Two details corroborate the mechanism rather than merely being consistent with
it:

- **The same calls from plain C work.** A C program linked against the same
  pinned `libduckdb.so`, with no Lean in the process, returns `DuckDBError`
  from all four error paths. Put Lean in the process and the identical call
  aborts.
- **The error printed no `what():` line.** libstdc++'s verbose terminate
  handler prints the exception type (via a non-virtual call — works), then
  *re-throws internally* to recover `what()`. That re-throw hits the same
  broken unwinding, re-enters the terminate handler, and trips its recursion
  guard — which is exactly the `terminate called recursively` in the log.

### Why macOS never saw it

Mach-O uses a **two-level namespace**: each import records *both* the symbol
name and the library it is expected to come from. `libduckdb.dylib` is bound
directly to `/usr/lib/libc++.1.dylib`, so its unwinding calls always reach the
one runtime it was built against. Nothing in the process can interpose them.
Lean's own build scripts note this asymmetry — the macOS script says system
libc++ means "there's no danger of conflicts", while the Linux script carries a
comment that static linking "breaks cross-library C++ exceptions".

This is the general shape of Linux-only FFI bugs: a flat global namespace where
load order decides, versus a namespace that records provenance.

## 4. The fix: seal the dependency

Three options were on the table.

1. **`LD_PRELOAD=libgcc_s.so.1`.** Preloaded objects are searched before the
   executable's `DT_NEEDED`, so all 11 `_Unwind_*` resolve to the one complete
   unwinder. Verified to fix all nine modules, and Lean's own exception
   handling still worked. Rejected as the *only* fix: it is an environment
   variable, so CI would go green while every Linux consumer of these modules
   still aborted — green CI for a broken feature.
2. **Static-link DuckDB into everything.** Correct in principle, but
   `precompileModules := true` for both `Linen` and `Tests` means link flags
   reach *every* module's `:dynlib`. Around twenty DuckDB test modules would
   each embed the archive separately.
3. **Seal DuckDB into one shared library** — what `linen` does.

`libduckdb_sealed.so` contains `duckdb.o` (our shim), `libduckdb_static.a`, a
static `libstdc++.a` and a static `libgcc.a`/`libgcc_eh.a`, linked with:

```
-Wl,--exclude-libs,libduckdb_static.a:libstdc++.a:libgcc.a:libgcc_eh.a
```

`--exclude-libs` marks every symbol taken from those archives as **local** to
the resulting shared object. Two consequences, and both are the point:

- Those symbols are no longer *exported*, so they never enter the global lookup
  scope and cannot be interposed by Lean or anything else.
- References to them from inside the same object are bound **at link time**
  rather than going through the global scope at load time.

DuckDB's throw, its catch, its C++ runtime and its unwinder therefore all live
inside one object and only ever talk to each other. Lean's partial export
becomes irrelevant — which also means **a Lean upgrade or downgrade cannot
reintroduce this**, unlike the `LD_PRELOAD` workaround, which depends on Lean's
exact symbol set.

Verified on the built artifact:

- no undefined `_Unwind_*`, `__cxa_throw`, `__cxa_begin_catch` or
  `__gxx_personality_v0` remain — nothing left to interpose;
- no `libstdc++.so.6` in `DT_NEEDED` — the C++ runtime is fully absorbed;
- DuckDB's own `duckdb_*` symbols: 0 exported (localized);
- the shim's 269 `linen_duckdb_*` entry points: still exported.

That last line is why the flag names four archives instead of `ALL`.
`--exclude-libs,ALL` would also hide `duckdb.o`'s entry points and break every
`@[extern]` binding above it. Object files listed directly are unaffected by
`--exclude-libs` — only archive members are — so the shim stays visible while
everything it links against goes local.

**macOS keeps linking `libduckdb.dylib` dynamically.** It has no such problem,
and a platform needs a fix only for a problem it has.

### 4.1 Sealing vs `LD_PRELOAD`, in detail

Both approaches were verified to fix all nine modules. They are worth
contrasting carefully, because they attack the problem from opposite ends and
the choice is not obvious.

**`LD_PRELOAD` unifies the process on one unwinder.** Preloaded objects are
inserted into the global lookup scope immediately after the executable and
*before* its `DT_NEEDED` libraries. Since `libgcc_s.so.1` defines **all 11**
`_Unwind_*` symbols, putting it in front means every request finds a complete,
self-consistent implementation there and the split disappears:

```
search order →  [ bin/lean ]  [ libgcc_s.so.1 ]  [ libleanshared.so ]  [ libstdc++.so.6 ]  …
                               ^^^^^^^^^^^^^^^^
                               all 11 _Unwind_* resolve here, consistently
```

Lean's LLVM libunwind is still in the process; it simply stops being consulted
for these symbols. That is safe only because both implementations honour the
same `_Unwind_*` ABI — the incompatibility is in the *context layout*, and with
one implementation answering every call no foreign context is ever built.
(`Tests.Linen.Control.ExceptionTest` was checked under the preload and still
passed.)

**Sealing removes DuckDB from the global scope entirely.** Nothing is
reordered; the symbols simply are not there to resolve:

```
libduckdb_sealed.so
┌────────────────────────────────────────────────────────────┐
│ duckdb.o            → exports linen_duckdb_* (269 symbols) │
│ libduckdb_static.a  ┐                                      │
│ libstdc++.a         ├ local: bound at link time,           │
│ libgcc.a/libgcc_eh.a┘ never entered into the global scope   │
└────────────────────────────────────────────────────────────┘
global scope is never consulted for _Unwind_*/__cxa_* on DuckDB's behalf
```

The differences that actually matter:

|                                | `LD_PRELOAD=libgcc_s.so.1`                                     | Sealed DSO                                            |
| ------------------------------ | -------------------------------------------------------------- | ----------------------------------------------------- |
| Where the fix lives            | The **environment** the program runs in                        | The **artifact** itself                               |
| Can it be forgotten?           | Yes — and then it aborts on the first DuckDB error              | No — it is a property of the file                     |
| Blast radius                   | **Whole process**: changes symbol resolution for everything     | Just this one shared object                            |
| Depends on Lean's symbol set   | Yes — it exists to counteract Lean's specific partial export    | No — immune to what Lean exports                      |
| Survives a Lean up/downgrade   | Probably, but unverified per version                            | Yes, by construction                                  |
| Covers a *future* C++ FFI dep  | **Yes, automatically** — the fix is process-wide                | **No** — each dependency must be sealed separately     |
| Build-time requirements        | None                                                            | Static `libstdc++.a`/`libgcc.a` + `g++` on the builder |
| Runtime requirements           | `libgcc_s.so.1` at a distro/arch-specific path                  | None                                                  |
| Cost                           | One line                                                        | ~80 lines of build logic, a ~39MB library, slower link |
| Failure mode if misconfigured  | glibc warns on stderr and **continues** → silently broken again | Build warns loudly and falls back                     |
| Effect on downstream consumers | **Viral**: every consumer and their CI must set it              | Nothing to do; their build handles it                  |

Two entries deserve emphasis because they cut in opposite directions.

**In `LD_PRELOAD`'s favour:** it is the more *general* fix. It repairs the
process-wide hazard, so a future C++ FFI dependency — say a library binding
LLVM or Arrow — would be covered for free. Sealing is per-dependency: each new
C++ library that throws needs its own seal. Neither approach fixes the actual
root cause, which is upstream in Lean.

**Decisively against it:** `linen` is consumed **as source**
(`require linen from git … @ "vx.y.z"`), so the requirement would be viral.
Every downstream project, and every downstream CI job, would have to know to
set an environment variable — and the consequence of not knowing is not a build
error but a process abort partway through, at the first DuckDB error path,
which is exactly the kind of failure nobody connects back to a missing env var.
CI would have been green while the feature was broken for everyone who
consumed it. `AGENTS.md` calls this out directly: never let partial coverage
look complete.

So the sealed DSO is what ships. `LD_PRELOAD` remains useful as a **diagnostic**:
if a future C++ dependency starts aborting this way, preloading `libgcc_s.so.1`
is the fastest way to confirm the cause is a split unwinder before doing any
build work. That is precisely how it was used here.

They are not mutually exclusive, but with the seal in place the preload is
redundant for DuckDB, and adding a process-wide symbol-resolution override that
nothing needs is not free of risk.

### 4.2 The catch: DuckDB's static archive is not feature-equivalent

**Status: the sealing mechanism works; applying it to DuckDB is blocked
upstream.** Recorded here because the trap is not DuckDB-specific and cost
real time to find.

Sealing was verified end to end — the DSO links with `leanc`, has no undefined
unwind/ABI symbols, and exports only the shim's entry points. But when the
sealed library was loaded, it failed with:

```
symbol lookup error: libduckdb_sealed.so: undefined symbol:
  _ZN6duckdb15ExtensionHelper17LoadAllExtensionsERNS_6DuckDBE
```

that is, `duckdb::ExtensionHelper::LoadAllExtensions(duckdb::DuckDB&)`. The
obvious diagnosis — a static-archive member-ordering problem, fixable with
`--start-group` or `--whole-archive` — is **wrong**. The symbol is referenced
but *never defined anywhere* in `libduckdb_static.a`, while the shared
`libduckdb.so` does define it. Counting across the whole archive: 63 259
symbols defined, 378 referenced-but-undefined, of which exactly **one** is in
the `duckdb::` namespace (this one); the other 377 are libstdc++/libc symbols
that the static C++ runtime supplies.

A no-op stub would link. It would also be a **silent feature downgrade on
Linux only**, because the archive does not merely fail to *load* the bundled
extensions — it does not *contain* them:

| symbol                   | `libduckdb_static.a` | `libduckdb.so` |
| ------------------------ | -------------------- | -------------- |
| `CoreFunctionsExtension` | **0**                | 10             |
| `DateTruncFun`           | **0**                | 16             |
| `ListValueFun`           | **0**                | 4              |

DuckDB's `core_functions` extension carries a large part of the SQL function
library. A sealed static build would therefore give Linux users a DuckDB
missing functions that macOS users have — precisely the "partial coverage
looking complete" that `AGENTS.md` forbids, and not something to decide
unilaterally.

**The lesson generalises:** a project shipping both `libfoo.so` and
`libfoo_static.a` in one archive does not necessarily ship the *same library*
twice. Diff the symbol tables before building a design on the static one.

### The failure mode to remember

If the sealed library cannot be built — `DUCKDB_PREFIX` pointing at an install
with no static archive, or no static `libstdc++.a` on the system — the build
prints a loud warning naming the missing piece and falls back to dynamic
linking. It does **not** fail silently, because the silent outcome is binaries
that abort on their first DuckDB error. Per `AGENTS.md`: never let partial
coverage look complete.

---

## 5. Does this depend on the Lean version?

**No. Not on any released version, and not on the architecture.** This was
checked rather than assumed, because "upgrade Lean" would have been a far
cheaper fix than anything in §4.

The flag responsible lives in `script/prepare-llvm-linux.sh`:

```
-DLEAN_CXX_STDLIB='-Wl,-Bstatic -lc++ -lc++abi -Wl,-Bdynamic'
```

It is **byte-identical** at `v4.0.0`, `v4.8.0`, `v4.15.0`, `v4.20.0`,
`v4.25.0`, `v4.30.0`, `v4.33.1`, `v4.34.0-rc2` and `master`, dating from commit
`b23627378060` (2021-10-28, *"feat: link external dependencies statically
again"*). The resulting export set was measured directly from the official
release tarballs:

| Lean release             | `_Unwind_*` exported by `libleanshared.so` |
| ------------------------ | ------------------------------------------ |
| v4.0.0 (2023-09)         | 10                                         |
| v4.8.0 (2024-06)         | 10                                         |
| v4.15.0 (2025-01)        | 10                                         |
| v4.33.1 x86_64 (2026-08) | 10                                         |
| v4.33.1 **aarch64**      | 10                                         |
| v4.34.0-rc2 (newest)     | 10                                         |

Same ten names every time. **Upgrading will not fix it, and downgrading will
not either.**

### Why exactly ten

Not arbitrary — it falls out of static-archive member selection. LLVM's
libunwind splits its entry points across two object files:

- `libunwind/src/UnwindLevel1.c` defines exactly the ten observed.
- `libunwind/src/UnwindLevel1-gcc-ext.c` defines the GCC extensions —
  including all four that go missing: `_Unwind_GetIPInfo`,
  `_Unwind_GetDataRelBase`, `_Unwind_GetTextRelBase`,
  `_Unwind_Resume_or_Rethrow`.

libc++abi references `_Unwind_RaiseException` and friends, so `UnwindLevel1.o`
is pulled out of the archive. *Nothing* references the GCC extensions, so
`UnwindLevel1-gcc-ext.o` is never pulled in. Ten in, four missing, by
construction. They stay visible despite Lean compiling with
`-fvisibility=hidden` because libunwind marks its entry points
`__attribute__((visibility("default")))` unless `_LIBUNWIND_HIDE_SYMBOLS` is
set.

### It is an upstream bug, and no issue exists for it

`leanprover/lean-llvm`'s build workflow already contains, under the comment
*"hide libc++ symbols in libleanshared"*:

```
-DLIBCXX_HERMETIC_STATIC_LIBRARY=ON -DLIBCXXABI_HERMETIC_STATIC_LIBRARY=ON
```

but **not** `-DLIBUNWIND_HIDE_SYMBOLS=ON`. Two of the three C++ runtime
components were deliberately hidden and the third was missed; the unwinder is
the only part that leaks.

Lean has been bitten by this class of bug in its own code before:
[lean4#3500](https://github.com/leanprover/lean4/pull/3500), *"fix: C++
exceptions across shared libraries on Linux"* (2024-02-26), fixed it by making
`libgcc_s` **dynamic** and added the comment *"general clang++ dependency,
breaks cross-library C++ exceptions if linked statically"*. That fixed Lean's
own half and left `-Wl,-Bstatic -lunwind` in place — the half that harms
third-party C++ FFI.

Searching leanprover/lean4 issues and PRs for `_Unwind`, "cross-library
exceptions", `libgcc_s` and "static libc++" returns **nothing**, and
`--exclude-libs`/`version-script` appear nowhere in the repository. **No
upstream issue exists; one should be filed**, proposing
`-DLIBUNWIND_HIDE_SYMBOLS=ON` alongside the two flags already present and
citing #3500 as precedent. (Caveat: an unrelated project measured that hiding
the exports *alone* turned their `SIGABRT` into a `SIGSEGV`, so the upstream fix
may need pairing with `--unwindlib=libgcc`. Untested for Lean.)

### Three other projects hit this independently, all recently

None filed upstream either, and each chose a different workaround — useful,
because it maps the solution space:

- **`Dragon-Hatcher/vampire-tactic-lean`** — the same bug with a different C++
  library. Its lakefile comment describes the mechanism exactly as §3 does and
  ends *"going uncaught past the `catch` in `MainLoop::run` that is sitting
  right there in the stack. Every goal fails this way, at the moment of
  success."* Its fix has the same shape as ours: static `libstdc++.a` +
  `libgcc_eh.a` + `libgcc.a` by absolute path from `c++ -print-file-name`.
- **`emberian/dregg`** — exploits **symbol versioning** instead: Lean's
  definitions are *unversioned* while libstdc++'s references are
  `@GCC_3.0`/`@GCC_3.3`, so emitting `-lgcc_s` first should let the versioned
  definition win. **Not applicable here**, and the reason is worth recording:
  `libduckdb.so` already lists `libgcc_s.so.1` in its own `DT_NEEDED`, so a
  properly versioned libgcc_s is *already* in this process and Lean still wins.
  Scope order beats versioning — which is also why `LD_PRELOAD` works (it gets
  ahead of `libleanshared`) and merely linking `-lgcc_s` would not.
- **`mcpp-community/mcpp` PR #598** — the same arithmetic in a SYCL artifact.

## 6. How other Lean packages handle native linking

Surveyed from the Reservoir index (829 packages), ranked by stars.

**Most Lean packages have no native code at all.** Of the top 25 by stars,
**23 compile or link nothing native**; only LeanCopilot (C++) and SciLean (C)
do. Across the top 100, 11 of 98 do native work — 6 C, 3 C++, 2 Rust.

So "FFI in Lean is mostly plain C" holds as a plurality — but it is weaker than
it sounds, because the most-starred FFI package in the ecosystem is C++, and
**every documented linking disaster in the ecosystem is a C++ one.** Plain C
libraries skip this whole chapter for one reason: C has no exceptions, so
nothing ever needs the unwinder. `ffi/postgres.c`, `ffi/jose.c`, `ffi/zlib.c`
and `linen`'s other shims are safe for exactly that reason. DuckDB was the
first dependency here whose *internals* throw.

Notable practice:

- **`leanprover/soplex-ffi`** (a Lean FRO repo) hit the sibling of our bug —
  Lean's bundled GMP interposing on a vendored GMP, crashing inside
  `mpq_init` — and fixed it with the same tool we use:
  `#[gmpVendorLib.toString, "-Wl,--exclude-libs,libgmp.a"]`, so *"the dynamic
  loader cannot interpose any GMP symbol from Lean's bundled GMP"*. It compiles
  its C++ bridge `-stdlib=libc++` to ABI-match Lean's runtime, and — worth
  copying — ships an explicit **C++ exception ABI self-test** that throws,
  catches via the `std::exception` base and checks `.what()` survived,
  *"turning the silent ABI risk into a CI failure."*
- **`lean-dojo/LeanCopilot`**, the most-starred FFI package, links CTranslate2
  and OpenBLAS dynamically, has **no `try`/`catch`/`noexcept` anywhere** in its
  shim, and 8 reachable `throw`s behind 12 `extern "C"` entry points. It is
  plausibly exposed to this bug latently and has simply never reported it. Its
  issue #196 documents the trap in the naive fix: you cannot statically link
  both libstdc++ and libc++, because both define the same Itanium-ABI-mangled
  symbols — a hard duplicate-symbol error.
- **`abdoo8080/lean-cvc5`** is the reference for doing C++ FFI right: pinned
  prebuilt **static** archives, `-stdlib=libc++`, and all 3525 lines of shim
  wrapped in `catch (...)` returning an `Except`.
- **Lean 4 core sidesteps C++ FFI entirely**: it vendors CaDiCaL but builds it
  as a **standalone executable** driven over `IO.Process.spawn`, parsing
  stdout. Process isolation means CaDiCaL's exceptions can never reach the Lean
  process. Worth remembering as a real option when a C++ dependency is hostile.
- **`V-Sekai-fire/datasource-duckdb`**, another DuckDB binding, independently
  arrived at sealing — a self-contained archive with *"all non-`duckdb_*`
  symbols localized"*.

**There is no official guidance on any of this.** The Lean reference manual's
FFI chapter contains zero occurrences of `C++`, `extern "C"` or "static
librar"; `rpath` appears nowhere in Lean's docs; Lake's README never mentions
C++ and marks `extern_lib` deprecated without documenting how to link a system
library; and Lake's own C++ FFI example is currently disabled in its CI. Two
undocumented facts worth knowing: `leanc --print-ldflags` already emits
`-lstdc++` (Linux) / `-lc++` (macOS), and **Lake never emits `-rpath`** — which
is why `linen` passes it explicitly.

## 7. Practical guidance for adding an FFI dependency to `linen`

1. **Is it plain C?** Then none of §3–§5 applies. Use `pkg-config` discovery or
   vendored sources as the existing shims do, and move on.
2. **Does it throw C++ exceptions?** Assume it will abort on Linux until proven
   otherwise, and exercise an **error path** in CI on both platforms — the
   happy path will not reveal this.
3. **Never let exceptions cross the FFI boundary.** Wrap every `extern "C"`
   entry point in `catch (const std::exception&)` / `catch (...)` and return an
   error value. Note this alone would *not* have saved us: the handler must
   still be *found*, and §3 is about it not being found.
4. **Prefer, in order:** process isolation (as Lean core does) > a
   sealed/localized library (§4) > a documented runtime requirement.
5. **Test the ABI, not just the feature.** soplex-ffi's throw/catch/`.what()`
   self-test turns a silent ABI hazard into a build failure. `linen` should
   grow one for any throwing library it links.
6. **Check a static archive is complete before designing around it.** Ours was
   not — see §4.2.
