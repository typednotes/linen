/*
 * ffi/duckdb_glibc_compat.c — glibc shims for the sealed DuckDB library
 *
 * On Linux, `lakefile.lean` seals DuckDB into `libduckdb_sealed.so` together
 * with the *host's* static `libstdc++.a`/`libgcc_eh.a`/`libgcc.a` (see
 * `duckdbSealedLib` there). Those host archives are built against the host
 * distribution's glibc, which is newer than the glibc Lean bundles, so they
 * reference a handful of symbols that did not exist when Lean's glibc was cut
 * (measured on Ubuntu 24.04 / gcc 13, against the toolchain's glibc ~2.28):
 *
 *   __isoc23_strtoul       (glibc 2.38)   from libstdc++.a
 *   __libc_single_threaded (glibc 2.32)   from libstdc++.a
 *   _dl_find_object        (glibc 2.35)   from libgcc_eh.a
 *
 * Leaving them undefined does not fail the *sealed library's own* link —
 * shared libraries may keep undefined symbols — and it does not fail
 * library-shaped links either, which is why every `lean_lib`/`Tests` build
 * on Linux stayed green. It fails one link shape only: an *executable*
 * (`lean_exe`), because `ld.lld` checks shared-library references there
 * under `--no-allow-shlib-undefined` — so every consumer that links a
 * DuckDB-referencing executable on Linux failed with
 *
 *   ld.lld: error: undefined reference: __isoc23_strtoul
 *   >>> referenced by libduckdb_sealed.so (disallowed by
 *   >>> --no-allow-shlib-undefined)
 *
 * while linen's own CI never noticed: its consumer job deliberately kept
 * DuckDB out of its test executable because of exactly this (see
 * `lean_exe linen`'s comment in lakefile.lean).
 *
 * The definitions below close that gap. Each one is a shim whose behaviour is
 * exactly what the *old* glibc Lean bundles would have provided, so the
 * sealed library behaves identically on every host:
 *
 *   __isoc23_* — glibc's C23 `strtol` family. Under pre-C23 rules `strtoul`
 *     et al. simply don't accept binary `0b` literals; delegating to them is
 *     precisely the pre-2.38 semantics (glibc itself aliases the isoc23
 *     names to the same code when the C23 difference is absent). The whole
 *     family is provided, not just the one seen on Ubuntu 24.04, because a
 *     different host libstdc++ may call any of its siblings.
 *   __libc_single_threaded — a glibc flag callers consult only to skip
 *     locking when the process is single-threaded. `0` ("possibly
 *     multi-threaded") is always safe; it just takes the locked path.
 *   _dl_find_object — glibc's (and ld.so's) fast unwinding-table lookup,
 *     used by libgcc's exception unwinder. Returning -1 ("not found") makes
 *     libgcc fall back to its own `dl_iterate_phdr` walk — slower, and
 *     exactly what every pre-2.35 system does; DuckDB's catch blocks still
 *     run, which is the property the sealed library exists to protect.
 *
 * Every definition is `hidden`: the references that need them are inside the
 * sealed library itself and resolve at its link time, and nothing is
 * exported, so the definitions can neither interpose nor be interposed at
 * run time on a host whose real glibc *does* provide the symbols.
 *
 * `lakefile.lean`'s `auditSealedDuckdbLib` re-derives the missing-symbol set
 * from the linked artifact on every build, so a *future* host C++ runtime
 * that references some newer symbol again is a loud, named build failure
 * rather than a consumer's `ld.lld` error — if it fires, extend this file.
 */

#include <inttypes.h>
#include <stdlib.h>

__attribute__((visibility("hidden")))
long __isoc23_strtol(const char *nptr, char **endptr, int base) {
  return strtol(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
long long __isoc23_strtoll(const char *nptr, char **endptr, int base) {
  return strtoll(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
unsigned long __isoc23_strtoul(const char *nptr, char **endptr, int base) {
  return strtoul(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
unsigned long long __isoc23_strtoull(const char *nptr, char **endptr, int base) {
  return strtoull(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
intmax_t __isoc23_strtoimax(const char *nptr, char **endptr, int base) {
  return strtoimax(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
uintmax_t __isoc23_strtoumax(const char *nptr, char **endptr, int base) {
  return strtoumax(nptr, endptr, base);
}

__attribute__((visibility("hidden")))
char __libc_single_threaded = 0;

__attribute__((visibility("hidden")))
int _dl_find_object(void *pc, void *result) {
  (void)pc;
  (void)result;
  return -1;
}
