#!/usr/bin/env bash
#
# Verify that Linux's sealed DuckDB library is genuinely self-contained.
#
#   ci/check-sealed-duckdb.sh
#
# On Linux, `lakefile.lean` seals DuckDB into one `libduckdb_sealed.so` — the
# DuckDB archives plus a static libstdc++/libgcc, every archive symbol
# localized — because Lean's `libleanshared.so` exports a *partial* `_Unwind_*`
# ABI that otherwise splits C++ unwinding across two incompatible unwinders and
# aborts the process on every DuckDB error path. See `docs/linking.md` for the
# full story.
#
# That seal is a property of a linked binary, not of the source, and two
# different ways of getting it wrong have already reached CI:
#
#   1. The sealed library linked fine but left `LoadAllExtensions` undefined,
#      because the archive it lives in was not on the link line. A *shared*
#      library link permits undefined symbols, so this is not a link error —
#      it surfaces much later as `symbol lookup error` while building an
#      unrelated test module.
#   2. The archive list was passed via Lake's `weakArgs`, which is excluded
#      from the build trace, so changing which archives got sealed did not
#      invalidate the cached library and CI kept a stale one.
#
# Both are invisible in the source and cheap to detect in the artifact, which
# is what this script does. It is deliberately a build-time check rather than a
# `#guard`: no Lean-level test can observe a symbol table.
#
# macOS exits successfully and immediately: it links `libduckdb.dylib`
# dynamically and needs no seal, because Mach-O's two-level namespace makes its
# C++ runtime impossible to interpose.

set -euo pipefail

SO=".lake/build/ffi/libduckdb_sealed.so"

if [ "$(uname -s)" != "Linux" ]; then
  echo "not Linux — macOS links libduckdb.dylib dynamically and needs no seal"
  exit 0
fi

if [ ! -f "$SO" ]; then
  echo "ERROR: $SO does not exist." >&2
  echo >&2
  echo "On Linux, DuckDB must be sealed into that library. Its absence means" >&2
  echo "lakefile.lean fell back to linking libduckdb dynamically — on which" >&2
  echo "every DuckDB error path aborts the process under Lean. Look for a" >&2
  echo "'[linen] WARNING' line earlier in the build naming the missing piece." >&2
  exit 1
fi

fail=0

# ── 1. Nothing left for libleanshared.so to interpose ──────────────────────
#
# The whole point of sealing. If any of these are still undefined, the dynamic
# loader will resolve them from the global scope, where Lean's partial unwinder
# is ahead of libgcc's complete one.
leaked="$(nm -D --undefined-only "$SO" \
  | grep -oE '_Unwind_[A-Za-z_]+|__cxa_throw|__cxa_begin_catch|__cxa_rethrow|__gxx_personality_v0' \
  | sort -u || true)"
if [ -n "$leaked" ]; then
  echo "ERROR: exception-handling symbols are still undefined in $SO:" >&2
  echo "$leaked" | sed 's/^/  /' >&2
  echo "  => these would resolve against libleanshared.so's partial unwinder." >&2
  fail=1
fi

# ── 2. The C++ runtime is absorbed, not borrowed ───────────────────────────
if readelf -d "$SO" | grep -qE 'NEEDED.*libstdc\+\+'; then
  echo "ERROR: $SO still has a DT_NEEDED on libstdc++.so.6." >&2
  echo "  => the static libstdc++ was not linked in; the seal is incomplete." >&2
  fail=1
fi

# ── 3. No undefined DuckDB symbols ─────────────────────────────────────────
#
# Catches an incomplete static archive set — the failure mode that
# `libduckdb-linux-*.zip`'s core-only `libduckdb_static.a` produces. The
# exclusions are the optional hooks that DuckDB's own *shared* library also
# leaves undefined: zstd's tracing callbacks and re2's thread-local
# initialiser.
dd_undef="$(nm -D --undefined-only "$SO" | awk '{print $NF}' \
  | grep -E '6duckdb|duckdb_' \
  | grep -vE 'ZSTD_trace_|duckdb_re25hooks' || true)"
if [ -n "$dd_undef" ]; then
  echo "ERROR: DuckDB symbols are undefined in $SO:" >&2
  echo "$dd_undef" | sed 's/^/  /' >&2
  echo "  => an archive defining them was not on the link line. If this is" >&2
  echo "     ExtensionHelper::LoadAllExtensions, the build used the core-only" >&2
  echo "     libduckdb_static.a from libduckdb-linux-*.zip instead of the" >&2
  echo "     complete set from static-libs-linux-*.zip." >&2
  fail=1
fi

# ── 4. The shim's entry points are still exported ──────────────────────────
#
# The counterpart risk to sealing too little: sealing too much.
# `--exclude-libs,ALL` would localize `duckdb.o`'s own entry points and break
# every `@[extern]` binding above them.
entry_points="$(nm -D --defined-only "$SO" | grep -c ' T linen_duckdb_' || true)"
if [ "$entry_points" -lt 100 ]; then
  echo "ERROR: only $entry_points exported linen_duckdb_* entry points in $SO" >&2
  echo "  => expected ~269. --exclude-libs may be hiding duckdb.o's symbols," >&2
  echo "     which would break every @[extern] binding in Linen.Database.DuckDB." >&2
  fail=1
fi

# ── 4b. And core_functions really is inside ────────────────────────────────
#
# Localized, so it must be looked for among *all* symbols rather than the
# dynamic ones. Its absence is the silent feature downgrade that made the
# core-only archive unacceptable in the first place.
#
# Not `grep -q`: it exits at the first match, closing the pipe mid-stream,
# so `nm` — with tens of thousands of symbols still to write — dies of
# SIGPIPE, and under `set -o pipefail` that turns a *pass* into a false
# "no CoreFunctionsExtension". It raced green on some hosts and failed
# reproducibly on others (observed in Docker: nm exits 141 every run).
# Filtering to /dev/null reads the whole stream, so `nm` always finishes.
if ! nm "$SO" 2>/dev/null | grep 'CoreFunctionsExtension' >/dev/null; then
  echo "ERROR: no CoreFunctionsExtension symbols in $SO." >&2
  echo "  => Linux would get a DuckDB missing much of the SQL function" >&2
  echo "     library while macOS gets the full one." >&2
  fail=1
fi

# ── 4c. Nothing undefined that a host glibc has but the bundled one lacks ──
#
# The third way this library has broken downstream: the host C++ runtime
# sealed in references symbols newer than the glibc Lean bundles, which no
# *library* link ever notices (shared libraries may keep undefined
# references) but every *executable* link rejects under
# `--no-allow-shlib-undefined`. That is the one link shape this repository's
# own targets never produce, so it shipped for a release and only a
# consumer's `lean_exe` saw it. `ffi/duckdb_glibc_compat.c` defines the
# measured set (`__isoc23_strtoul`, `__libc_single_threaded`,
# `_dl_find_object`); this asserts they are really resolved in the artifact,
# and lakefile.lean's `auditSealedDuckdbLib` does the complete per-host check
# (any non-weak undefined symbol outside the toolchain's own libraries) at
# build time, so a *future* host runtime naming new symbols fails the build
# here rather than a consumer's link.
glibc_undef="$(nm -D --undefined-only "$SO" | awk '{print $NF}' \
  | grep -E '^(__isoc23_|__libc_single_threaded$|_dl_find_object$)' || true)"
if [ -n "$glibc_undef" ]; then
  echo "ERROR: host-glibc symbols left undefined in $SO:" >&2
  echo "$glibc_undef" | sed 's/^/  /' >&2
  echo "  => every consumer executable link fails on these under" >&2
  echo "     --no-allow-shlib-undefined. ffi/duckdb_glibc_compat.c should" >&2
  echo "     have resolved them — see its header, and lakefile.lean's" >&2
  echo "     auditSealedDuckdbLib." >&2
  fail=1
fi

if [ "$fail" -ne 0 ]; then
  echo >&2
  echo "See docs/linking.md for what the seal is and why it matters." >&2
  exit 1
fi

echo "sealed DuckDB library OK:"
echo "  - no undefined unwinder/C++ ABI symbols (nothing to interpose)"
echo "  - no libstdc++.so.6 in DT_NEEDED (runtime absorbed)"
echo "  - no undefined DuckDB symbols (archive set complete)"
echo "  - $entry_points exported linen_duckdb_* entry points"
echo "  - core_functions extension present"
echo "  - no host-glibc symbols left undefined (executable links survive)"
