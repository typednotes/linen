/*
 * ffi/sqlite3_shim.c — SQLite3 FFI for Lean 4
 *
 * Wraps the vendored SQLite amalgamation (ffi/vendor/sqlite3/sqlite3.c) for
 * Linen.Database.SQLite.Bindings. Follows the same lean_alloc_external
 * pattern as ffi/postgres.c: two opaque external classes (Database,
 * Statement) wrapping `sqlite3 *`/`sqlite3_stmt *`, with GC finalizers that
 * release the underlying native resource if the caller never did.
 *
 * Scope: the core `sqlite3_*` entry points needed by the raw-binding layer
 * (`Linen.Database.SQLite.Bindings`) and the thin/ public wrappers built on
 * it (`Direct`, `SQLite3`). Two callback-based entry points remain out of
 * scope — `sqlite3_trace` and the row callback of `sqlite3_exec` — since
 * nothing in this port needs them; `exec` here always passes a NULL row
 * callback, matching `sqlite3_exec`'s documented behaviour of simply not
 * invoking any callback in that case.
 *
 * `sqlite3_create_function_v2` (for
 * `Linen.Database.SQLite.Simple.Function`, module #16 of
 * `docs/imports/sqlite-simple/dependencies.md`) *does* need a Lean closure
 * invoked from a C callback — the first such case in this codebase's ffi
 * sources — so this file adds that one piece of machinery (see the
 * "USER-DEFINED SCALAR FUNCTIONS" section below): the registered Lean
 * closure is kept alive as SQLite's opaque per-function user-data pointer
 * (retained with `lean_inc_ref`, released via SQLite's own `xDestroy`
 * callback), and `sqlite3_create_function_v2`'s `xFunc` trampoline invokes
 * it synchronously with `lean_apply_4` on the same thread SQLite calls back
 * on — safe here because SQLite's callback happens synchronously inside
 * `sqlite3_step`, i.e. on the same Lean-runtime thread that made the call,
 * never on a separate OS thread.
 *
 * Platform: macOS and Linux (any platform with a C compiler — SQLite is
 * vendored, not discovered via pkg-config).
 */

#include <lean/lean.h>
#include "sqlite3.h"
#include <string.h>
#include <stdlib.h>
#include <stdatomic.h>

/* ────────────────────────────────────────────────────────────
 * External classes for sqlite3* and sqlite3_stmt*
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_sqlite3_db_class = NULL;
static lean_external_class *g_linen_sqlite3_stmt_class = NULL;

typedef struct {
    sqlite3 *db;
} linen_sqlite3_db_t;

typedef struct {
    sqlite3_stmt *stmt;
} linen_sqlite3_stmt_t;

static void linen_sqlite3_db_finalizer(void *ptr) {
    linen_sqlite3_db_t *d = (linen_sqlite3_db_t *)ptr;
    if (d) {
        if (d->db) sqlite3_close_v2(d->db);
        free(d);
    }
}

static void linen_sqlite3_stmt_finalizer(void *ptr) {
    linen_sqlite3_stmt_t *s = (linen_sqlite3_stmt_t *)ptr;
    if (s) {
        if (s->stmt) sqlite3_finalize(s->stmt);
        free(s);
    }
}

static void linen_noop_foreach_sqlite3(void *mod, b_lean_obj_arg fn) {
    /* no sub-objects to traverse */
}

/*
 * FuncContext / FuncArgs: ephemeral, *borrowed*-pointer wrappers around a
 * scalar function callback's `sqlite3_context*`/`sqlite3_value**`. Unlike
 * Database/Statement above, these never own the underlying SQLite resource
 * (it belongs to the one `sqlite3_step` call that is invoking the
 * callback), so their finalizers only free the small Lean-side wrapper
 * struct, never call into SQLite.
 */

static lean_external_class *g_linen_sqlite3_funcctx_class = NULL;
static lean_external_class *g_linen_sqlite3_funcargs_class = NULL;

typedef struct {
    sqlite3_context *ctx;
} linen_sqlite3_funcctx_t;

typedef struct {
    sqlite3_value **argv;
} linen_sqlite3_funcargs_t;

static void linen_sqlite3_funcctx_finalizer(void *ptr) { free(ptr); }
static void linen_sqlite3_funcargs_finalizer(void *ptr) { free(ptr); }

static atomic_int g_linen_sqlite3_classes_initialized = 0;

static void linen_sqlite3_ensure_classes_initialized(void) {
    if (atomic_load_explicit(&g_linen_sqlite3_classes_initialized, memory_order_acquire))
        return;
    g_linen_sqlite3_db_class = lean_register_external_class(
        &linen_sqlite3_db_finalizer, &linen_noop_foreach_sqlite3);
    g_linen_sqlite3_stmt_class = lean_register_external_class(
        &linen_sqlite3_stmt_finalizer, &linen_noop_foreach_sqlite3);
    g_linen_sqlite3_funcctx_class = lean_register_external_class(
        &linen_sqlite3_funcctx_finalizer, &linen_noop_foreach_sqlite3);
    g_linen_sqlite3_funcargs_class = lean_register_external_class(
        &linen_sqlite3_funcargs_finalizer, &linen_noop_foreach_sqlite3);
    atomic_store_explicit(&g_linen_sqlite3_classes_initialized, 1, memory_order_release);
}

static inline lean_obj_res mk_sqlite3_funcctx(sqlite3_context *ctx) {
    linen_sqlite3_ensure_classes_initialized();
    linen_sqlite3_funcctx_t *wrapper = malloc(sizeof(linen_sqlite3_funcctx_t));
    if (!wrapper) return NULL;
    wrapper->ctx = ctx;
    return lean_alloc_external(g_linen_sqlite3_funcctx_class, wrapper);
}

static inline sqlite3_context *get_sqlite3_funcctx(b_lean_obj_arg obj) {
    linen_sqlite3_funcctx_t *wrapper = (linen_sqlite3_funcctx_t *)lean_get_external_data(obj);
    return wrapper->ctx;
}

static inline lean_obj_res mk_sqlite3_funcargs(sqlite3_value **argv) {
    linen_sqlite3_ensure_classes_initialized();
    linen_sqlite3_funcargs_t *wrapper = malloc(sizeof(linen_sqlite3_funcargs_t));
    if (!wrapper) return NULL;
    wrapper->argv = argv;
    return lean_alloc_external(g_linen_sqlite3_funcargs_class, wrapper);
}

static inline sqlite3_value **get_sqlite3_funcargs(b_lean_obj_arg obj) {
    linen_sqlite3_funcargs_t *wrapper = (linen_sqlite3_funcargs_t *)lean_get_external_data(obj);
    return wrapper->argv;
}

/* ────────────────────────────────────────────────────────────
 * Helpers: wrap/unwrap external objects
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_sqlite3_db(sqlite3 *db) {
    linen_sqlite3_ensure_classes_initialized();
    linen_sqlite3_db_t *wrapper = malloc(sizeof(linen_sqlite3_db_t));
    if (!wrapper) {
        sqlite3_close_v2(db);
        return NULL;
    }
    wrapper->db = db;
    return lean_alloc_external(g_linen_sqlite3_db_class, wrapper);
}

static inline sqlite3 *get_sqlite3_db(b_lean_obj_arg obj) {
    linen_sqlite3_db_t *wrapper = (linen_sqlite3_db_t *)lean_get_external_data(obj);
    return wrapper->db;
}

static inline lean_obj_res mk_sqlite3_stmt(sqlite3_stmt *stmt) {
    linen_sqlite3_ensure_classes_initialized();
    linen_sqlite3_stmt_t *wrapper = malloc(sizeof(linen_sqlite3_stmt_t));
    if (!wrapper) {
        sqlite3_finalize(stmt);
        return NULL;
    }
    wrapper->stmt = stmt;
    return lean_alloc_external(g_linen_sqlite3_stmt_class, wrapper);
}

static inline sqlite3_stmt *get_sqlite3_stmt(b_lean_obj_arg obj) {
    linen_sqlite3_stmt_t *wrapper = (linen_sqlite3_stmt_t *)lean_get_external_data(obj);
    return wrapper->stmt;
}

/* ────────────────────────────────────────────────────────────
 * Helper: make Lean Option.none / Option.some / Prod pairs
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_option_none(void) { return lean_box(0); }

static inline lean_obj_res mk_option_some(lean_obj_arg val) {
    lean_object *opt = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(opt, 0, val);
    return opt;
}

static inline lean_obj_res mk_pair(lean_obj_arg fst, lean_obj_arg snd) {
    lean_object *p = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(p, 0, fst);
    lean_ctor_set(p, 1, snd);
    return p;
}

static inline lean_obj_res mk_string_or_empty(const char *s) {
    if (!s) s = "";
    return lean_mk_string_from_bytes(s, strlen(s));
}

/* ================================================================
 * CONNECTION MANAGEMENT
 * ================================================================ */

/*
 * @[extern "linen_sqlite3_open"]
 * opaque openRaw : @& String -> IO (Int32 x Database)
 *
 * sqlite3_open_v2 with READWRITE|CREATE flags (the default `sqlite3_open`
 * behaviour). Like PQconnectdb, this sets the sqlite3* handle even when the
 * open fails (per <http://sqlite.org/c3ref/open.html>: "Whether or not an
 * error occurs when it is opened, resources associated with the database
 * connection handle should be released by passing it to sqlite3_close()") —
 * so the returned pair always carries a usable `Database` for the caller to
 * inspect via `errmsg`, mirroring `Linen.Database.PostgreSQL.LibPQ.connect`.
 * Only a NULL db pointer (documented as possible on malloc failure) becomes
 * a thrown IO error, since there would be nothing to wrap.
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_open(
    b_lean_obj_arg path_obj,
    lean_obj_arg world
) {
    linen_sqlite3_ensure_classes_initialized();
    const char *path = lean_string_cstr(path_obj);
    sqlite3 *db = NULL;
    int rc = sqlite3_open_v2(path, &db,
        SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, NULL);

    if (!db) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("sqlite3_open_v2 returned NULL")));
    }

    lean_obj_res dbObj = mk_sqlite3_db(db);
    if (!dbObj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for sqlite3 wrapper")));
    }
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), dbObj));
}

/*
 * @[extern "linen_sqlite3_close"]
 * opaque closeRaw : Database -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_close(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    linen_sqlite3_db_t *wrapper = (linen_sqlite3_db_t *)lean_get_external_data(db_obj);
    int rc = SQLITE_OK;
    if (wrapper->db) {
        rc = sqlite3_close_v2(wrapper->db);
        if (rc == SQLITE_OK) wrapper->db = NULL;
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_sqlite3_errmsg"]
 * opaque errmsg : @& Database -> IO String
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_errmsg(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    if (!db) return lean_io_result_mk_ok(mk_string_or_empty(""));
    return lean_io_result_mk_ok(mk_string_or_empty(sqlite3_errmsg(db)));
}

/*
 * @[extern "linen_sqlite3_interrupt"]
 * opaque interrupt : @& Database -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_interrupt(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    if (db) sqlite3_interrupt(db);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_sqlite3_get_autocommit"]
 * opaque getAutocommit : @& Database -> IO UInt8
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_get_autocommit(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    int ac = db ? sqlite3_get_autocommit(db) : 1;
    return lean_io_result_mk_ok(lean_box((uint8_t)(ac != 0)));
}

/* ================================================================
 * SIMPLE QUERY EXECUTION
 * ================================================================ */

/*
 * @[extern "linen_sqlite3_exec"]
 * opaque execRaw : @& Database -> @& String -> IO (Int32 x String)
 *
 * No row callback is passed (see file header): this executes every
 * statement in `sql` for its side effects only, exactly like
 * `sqlite3_exec(db, sql, NULL, NULL, &errmsg)`.
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_exec(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg sql_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    const char *sql = lean_string_cstr(sql_obj);
    char *errmsg = NULL;
    int rc = sqlite3_exec(db, sql, NULL, NULL, &errmsg);
    lean_obj_res msgObj = mk_string_or_empty(errmsg);
    if (errmsg) sqlite3_free(errmsg);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), msgObj));
}

/* ================================================================
 * STATEMENT MANAGEMENT
 * ================================================================ */

/*
 * @[extern "linen_sqlite3_prepare"]
 * opaque prepareRaw : @& Database -> @& String -> IO (Int32 x Option Statement)
 *
 * If `sql` has no statements, sqlite3_prepare_v2 returns SQLITE_OK with a
 * NULL stmt — surfaced here as `(0, none)`, matching
 * <http://www.sqlite.org/c3ref/prepare.html>.
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_prepare(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg sql_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    const char *sql = lean_string_cstr(sql_obj);
    sqlite3_stmt *stmt = NULL;
    int rc = sqlite3_prepare_v2(db, sql, -1, &stmt, NULL);

    lean_obj_res stmtOpt;
    if (!stmt) {
        stmtOpt = mk_option_none();
    } else {
        lean_obj_res stmtObj = mk_sqlite3_stmt(stmt);
        if (!stmtObj) {
            return lean_io_result_mk_error(
                lean_mk_io_user_error(lean_mk_string("malloc failed for sqlite3_stmt wrapper")));
        }
        stmtOpt = mk_option_some(stmtObj);
    }
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), stmtOpt));
}

/*
 * @[extern "linen_sqlite3_step"]
 * opaque step : @& Statement -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_step(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_step(stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_sqlite3_reset"]
 * opaque reset : @& Statement -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_reset(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_reset(stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_sqlite3_finalize"]
 * opaque finalizeRaw : Statement -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_finalize(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    linen_sqlite3_stmt_t *wrapper = (linen_sqlite3_stmt_t *)lean_get_external_data(stmt_obj);
    int rc = SQLITE_OK;
    if (wrapper->stmt) {
        rc = sqlite3_finalize(wrapper->stmt);
        wrapper->stmt = NULL;
    }
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_sqlite3_clear_bindings"]
 * opaque clearBindings : @& Statement -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_clear_bindings(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_clear_bindings(stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * PARAMETER AND COLUMN INFORMATION
 * ================================================================ */

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_parameter_count(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int n = sqlite3_bind_parameter_count(stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)n));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_parameter_name(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const char *name = sqlite3_bind_parameter_name(stmt, (int)idx);
    if (!name) return lean_io_result_mk_ok(mk_option_none());
    return lean_io_result_mk_ok(mk_option_some(mk_string_or_empty(name)));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_parameter_index(
    b_lean_obj_arg stmt_obj,
    b_lean_obj_arg name_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const char *name = lean_string_cstr(name_obj);
    int idx = sqlite3_bind_parameter_index(stmt, name);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)idx));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_count(
    b_lean_obj_arg stmt_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int n = sqlite3_column_count(stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)n));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_name(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const char *name = sqlite3_column_name(stmt, (int)idx);
    if (!name) return lean_io_result_mk_ok(mk_option_none());
    return lean_io_result_mk_ok(mk_option_some(mk_string_or_empty(name)));
}

/* ================================================================
 * BINDING VALUES TO PREPARED STATEMENTS
 *
 * All use SQLITE_TRANSIENT so SQLite copies the value immediately: the
 * source String/ByteArray is only borrowed (@&) for the duration of the
 * call, exactly mirroring `Linen.Database.PostgreSQL.LibPQ`'s parameter
 * handling and upstream's own use of `c_SQLITE_TRANSIENT`.
 * ================================================================ */

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_int64(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    int64_t value,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_bind_int64(stmt, (int)idx, (sqlite3_int64)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_double(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    double value,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_bind_double(stmt, (int)idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_text(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    b_lean_obj_arg value_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const char *value = lean_string_cstr(value_obj);
    size_t len = lean_string_size(value_obj) - 1; /* exclude NUL terminator */
    int rc = sqlite3_bind_text(stmt, (int)idx, value, (int)len, SQLITE_TRANSIENT);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_blob(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    b_lean_obj_arg value_obj,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    size_t len = lean_sarray_size(value_obj);
    const uint8_t *value = lean_sarray_cptr(value_obj);
    int rc = sqlite3_bind_blob(stmt, (int)idx, value, (int)len, SQLITE_TRANSIENT);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_bind_null(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int rc = sqlite3_bind_null(stmt, (int)idx);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * RESULT VALUES FROM A QUERY
 * ================================================================ */

LEAN_EXPORT lean_obj_res linen_sqlite3_column_type(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    int t = sqlite3_column_type(stmt, (int)idx);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)t));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_int64(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    sqlite3_int64 v = sqlite3_column_int64(stmt, (int)idx);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)v));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_double(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    double v = sqlite3_column_double(stmt, (int)idx);
    return lean_io_result_mk_ok(lean_box_float(v));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_text(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const unsigned char *text = sqlite3_column_text(stmt, (int)idx);
    int len = sqlite3_column_bytes(stmt, (int)idx);
    if (!text) len = 0;
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes((const char *)(text ? text : (const unsigned char *)""), (size_t)len));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_column_blob(
    b_lean_obj_arg stmt_obj,
    uint32_t idx,
    lean_obj_arg world
) {
    sqlite3_stmt *stmt = get_sqlite3_stmt(stmt_obj);
    const void *blob = sqlite3_column_blob(stmt, (int)idx);
    int len = sqlite3_column_bytes(stmt, (int)idx);
    lean_obj_res arr = lean_alloc_sarray(1, (size_t)len, (size_t)len);
    if (len > 0 && blob) {
        memcpy(lean_sarray_cptr(arr), blob, (size_t)len);
    }
    return lean_io_result_mk_ok(arr);
}

/* ================================================================
 * RESULT STATISTICS
 * ================================================================ */

LEAN_EXPORT lean_obj_res linen_sqlite3_last_insert_rowid(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    sqlite3_int64 rowid = db ? sqlite3_last_insert_rowid(db) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)rowid));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_changes(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    sqlite3_int64 n = db ? sqlite3_changes64(db) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_total_changes(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    sqlite3_int64 n = db ? sqlite3_total_changes64(db) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/* ================================================================
 * USER-DEFINED SCALAR FUNCTIONS
 *
 * See the file header for why this is the one place in this codebase's ffi
 * sources that calls a Lean closure back from a C callback.
 * ================================================================ */

/*
 * The xFunc trampoline SQLite invokes once per row for a registered scalar
 * function. `sqlite3_user_data(ctx)` recovers the Lean closure
 * (`FuncContext -> FuncArgs -> UInt32 -> IO Unit`) stored at registration
 * time; it is retained (lean_inc_ref'd) here so the stored reference
 * remains valid for the *next* call too, then applied via lean_apply_4
 * (three real arguments plus the IO "world" token). Any Lean-side failure
 * (an `IO` error, e.g. from an uncaught `Ok.errors` inside a `FromField`
 * conversion) falls back to reporting SQL NULL, matching upstream's own
 * catch-all behaviour.
 */
static void linen_sqlite3_xfunc_trampoline(sqlite3_context *ctx, int argc, sqlite3_value **argv) {
    lean_object *closure = (lean_object *)sqlite3_user_data(ctx);
    if (!closure) { sqlite3_result_null(ctx); return; }
    lean_inc_ref(closure);

    lean_obj_res ctxObj = mk_sqlite3_funcctx(ctx);
    lean_obj_res argsObj = mk_sqlite3_funcargs(argv);
    if (!ctxObj || !argsObj) { sqlite3_result_null(ctx); return; }

    lean_object *result = lean_apply_4(closure, ctxObj, argsObj,
        lean_box_uint32((uint32_t)argc), lean_box(0));
    if (lean_io_result_is_error(result)) {
        sqlite3_result_null(ctx);
    }
    lean_dec_ref(result);
}

/* Invoked by SQLite when a function registration is replaced or removed
 * (including at `sqlite3_close`), or by our own `deleteFunction`. Releases
 * this port's reference to the stored closure. */
static void linen_sqlite3_xdestroy_trampoline(void *pApp) {
    if (pApp) lean_dec_ref((lean_object *)pApp);
}

/*
 * @[extern "linen_sqlite3_create_function"]
 * opaque createFunctionRaw :
 *   @& Database -> @& String -> Int32 -> Bool ->
 *   (FuncContext -> FuncArgs -> UInt32 -> IO Unit) -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_create_function(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg name_obj,
    int32_t nArg,
    uint8_t deterministic,
    lean_obj_arg f_obj,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    const char *name = lean_string_cstr(name_obj);
    int eTextRep = SQLITE_UTF8 | (deterministic ? SQLITE_DETERMINISTIC : 0);

    lean_inc_ref(f_obj); /* the C side now owns one persistent reference */
    int rc = sqlite3_create_function_v2(
        db, name, (int)nArg, eTextRep, (void *)f_obj,
        &linen_sqlite3_xfunc_trampoline, NULL, NULL,
        &linen_sqlite3_xdestroy_trampoline);
    if (rc != SQLITE_OK) {
        /* registration failed: xDestroy will not be called by SQLite, so
         * release the reference we just took ourselves. */
        lean_dec_ref(f_obj);
    }
    lean_dec_ref(f_obj); /* release this call's own borrowed reference to f_obj */
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_sqlite3_delete_function"]
 * opaque deleteFunctionRaw : @& Database -> @& String -> Int32 -> IO UInt32
 *
 * Removing a registration is `sqlite3_create_function_v2` with every
 * function pointer NULL for the same `(name, nArg, textRep)` triple; SQLite
 * calls the *originally*-registered xDestroy on the *originally*-registered
 * pApp as part of removing it, which is what releases our closure
 * reference (see `linen_sqlite3_xdestroy_trampoline`).
 */
LEAN_EXPORT lean_obj_res linen_sqlite3_delete_function(
    b_lean_obj_arg db_obj,
    b_lean_obj_arg name_obj,
    int32_t nArg,
    lean_obj_arg world
) {
    sqlite3 *db = get_sqlite3_db(db_obj);
    const char *name = lean_string_cstr(name_obj);
    int rc = sqlite3_create_function_v2(
        db, name, (int)nArg, SQLITE_UTF8, NULL, NULL, NULL, NULL, NULL);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ---- Argument accessors (FuncArgs, 0-based) ---- */

LEAN_EXPORT lean_obj_res linen_sqlite3_func_arg_type(
    b_lean_obj_arg args_obj, uint32_t idx, lean_obj_arg world
) {
    sqlite3_value **argv = get_sqlite3_funcargs(args_obj);
    int t = sqlite3_value_type(argv[idx]);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)t));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_arg_int64(
    b_lean_obj_arg args_obj, uint32_t idx, lean_obj_arg world
) {
    sqlite3_value **argv = get_sqlite3_funcargs(args_obj);
    sqlite3_int64 v = sqlite3_value_int64(argv[idx]);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)v));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_arg_double(
    b_lean_obj_arg args_obj, uint32_t idx, lean_obj_arg world
) {
    sqlite3_value **argv = get_sqlite3_funcargs(args_obj);
    double v = sqlite3_value_double(argv[idx]);
    return lean_io_result_mk_ok(lean_box_float(v));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_arg_text(
    b_lean_obj_arg args_obj, uint32_t idx, lean_obj_arg world
) {
    sqlite3_value **argv = get_sqlite3_funcargs(args_obj);
    const unsigned char *text = sqlite3_value_text(argv[idx]);
    int len = sqlite3_value_bytes(argv[idx]);
    if (!text) len = 0;
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes((const char *)(text ? text : (const unsigned char *)""), (size_t)len));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_arg_blob(
    b_lean_obj_arg args_obj, uint32_t idx, lean_obj_arg world
) {
    sqlite3_value **argv = get_sqlite3_funcargs(args_obj);
    const void *blob = sqlite3_value_blob(argv[idx]);
    int len = sqlite3_value_bytes(argv[idx]);
    lean_obj_res arr = lean_alloc_sarray(1, (size_t)len, (size_t)len);
    if (len > 0 && blob) {
        memcpy(lean_sarray_cptr(arr), blob, (size_t)len);
    }
    return lean_io_result_mk_ok(arr);
}

/* ---- Result setters (FuncContext) ---- */

LEAN_EXPORT lean_obj_res linen_sqlite3_func_result_int64(
    b_lean_obj_arg ctx_obj, int64_t value, lean_obj_arg world
) {
    sqlite3_context *ctx = get_sqlite3_funcctx(ctx_obj);
    sqlite3_result_int64(ctx, (sqlite3_int64)value);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_result_double(
    b_lean_obj_arg ctx_obj, double value, lean_obj_arg world
) {
    sqlite3_context *ctx = get_sqlite3_funcctx(ctx_obj);
    sqlite3_result_double(ctx, value);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_result_text(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    sqlite3_context *ctx = get_sqlite3_funcctx(ctx_obj);
    const char *value = lean_string_cstr(value_obj);
    size_t len = lean_string_size(value_obj) - 1;
    sqlite3_result_text(ctx, value, (int)len, SQLITE_TRANSIENT);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_result_blob(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    sqlite3_context *ctx = get_sqlite3_funcctx(ctx_obj);
    size_t len = lean_sarray_size(value_obj);
    const uint8_t *value = lean_sarray_cptr(value_obj);
    sqlite3_result_blob(ctx, value, (int)len, SQLITE_TRANSIENT);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res linen_sqlite3_func_result_null(
    b_lean_obj_arg ctx_obj, lean_obj_arg world
) {
    sqlite3_context *ctx = get_sqlite3_funcctx(ctx_obj);
    sqlite3_result_null(ctx);
    return lean_io_result_mk_ok(lean_box(0));
}
