/*
 * ffi/postgres.c — PostgreSQL libpq FFI for Lean 4
 *
 * Wraps libpq's PGconn and PGresult objects for PostgreSQL client support.
 * Follows the same lean_alloc_external pattern as ffi/network.c and ffi/tls.c.
 *
 * Features:
 * - Connection management (connect, close, status)
 * - Query execution (exec, exec_params, prepare, exec_prepared)
 * - Result inspection (ntuples, nfields, getvalue, getisnull, fname, ftype)
 * - String escaping (escape_literal, escape_identifier)
 * - LISTEN/NOTIFY support (consume_input, notifies)
 * - Transaction status inspection
 * - Proper resource cleanup via GC finalizer
 *
 * Platform: macOS and Linux. Requires libpq (PostgreSQL client library).
 */

#include <lean/lean.h>
#include <libpq-fe.h>
#include <string.h>
#include <stdlib.h>
#include <stdatomic.h>

/* ────────────────────────────────────────────────────────────
 * External classes for PGconn and PGresult
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_pg_conn_class = NULL;
static lean_external_class *g_linen_pg_result_class = NULL;

typedef struct {
    PGconn *conn;
} linen_pg_conn_t;

typedef struct {
    PGresult *result;
} linen_pg_result_t;

static void linen_pg_conn_finalizer(void *ptr) {
    linen_pg_conn_t *c = (linen_pg_conn_t *)ptr;
    if (c) {
        if (c->conn) PQfinish(c->conn);
        free(c);
    }
}

static void linen_pg_result_finalizer(void *ptr) {
    linen_pg_result_t *r = (linen_pg_result_t *)ptr;
    if (r) {
        if (r->result) PQclear(r->result);
        free(r);
    }
}

static void linen_noop_foreach_pg(void *mod, b_lean_obj_arg fn) {
    /* no sub-objects to traverse */
}

/* Ensure external classes are registered (lazy, thread-safe via atomic).
 * Uses a simple atomic flag to ensure registration happens exactly once.
 * The Lean runtime must be initialized before this is first called. */
static atomic_int g_linen_pg_classes_initialized = 0;

static void linen_pg_ensure_classes_initialized(void) {
    if (atomic_load_explicit(&g_linen_pg_classes_initialized, memory_order_acquire))
        return;
    g_linen_pg_conn_class = lean_register_external_class(
        &linen_pg_conn_finalizer, &linen_noop_foreach_pg);
    g_linen_pg_result_class = lean_register_external_class(
        &linen_pg_result_finalizer, &linen_noop_foreach_pg);
    atomic_store_explicit(&g_linen_pg_classes_initialized, 1, memory_order_release);
}

/* ────────────────────────────────────────────────────────────
 * Helpers: wrap/unwrap external objects
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_pg_conn(PGconn *conn) {
    linen_pg_ensure_classes_initialized();
    linen_pg_conn_t *wrapper = malloc(sizeof(linen_pg_conn_t));
    if (!wrapper) {
        PQfinish(conn);
        return NULL; /* caller must check */
    }
    wrapper->conn = conn;
    return lean_alloc_external(g_linen_pg_conn_class, wrapper);
}

static inline PGconn *get_pg_conn(b_lean_obj_arg obj) {
    linen_pg_conn_t *wrapper = (linen_pg_conn_t *)lean_get_external_data(obj);
    return wrapper->conn;
}

static inline lean_obj_res mk_pg_result(PGresult *result) {
    linen_pg_ensure_classes_initialized();
    linen_pg_result_t *wrapper = malloc(sizeof(linen_pg_result_t));
    if (!wrapper) {
        PQclear(result);
        return NULL; /* caller must check */
    }
    wrapper->result = result;
    return lean_alloc_external(g_linen_pg_result_class, wrapper);
}

static inline PGresult *get_pg_result(b_lean_obj_arg obj) {
    linen_pg_result_t *wrapper = (linen_pg_result_t *)lean_get_external_data(obj);
    return wrapper->result;
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean IO error from a message string
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_pg_io_error(const char *msg) {
    return lean_io_result_mk_error(
        lean_mk_io_user_error(lean_mk_string(msg)));
}

/* Helper: make a Lean IO error from a PGconn error message */
static inline lean_obj_res mk_pg_conn_error(PGconn *conn) {
    const char *msg = PQerrorMessage(conn);
    return mk_pg_io_error(msg ? msg : "unknown PostgreSQL error");
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean pair (Prod)
 *   Lean encodes (a, b) as ctor(0, 2, 0) with fields [a, b]
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_pair(lean_obj_arg fst, lean_obj_arg snd) {
    lean_object *p = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(p, 0, fst);
    lean_ctor_set(p, 1, snd);
    return p;
}

/* ────────────────────────────────────────────────────────────
 * Helper: make Option.none and Option.some
 *   none = lean_box(0)
 *   some(x) = ctor(1, 1, 0) with field [x]
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_option_none(void) {
    return lean_box(0);
}

static inline lean_obj_res mk_option_some(lean_obj_arg val) {
    lean_object *opt = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(opt, 0, val);
    return opt;
}

/* Build a libpq paramValues array (text format) from a Lean
 * `Array (Option String)`: `none` becomes a NULL entry (SQL NULL), `some s`
 * becomes a pointer into `s`'s UTF-8 data. The pointers borrow from
 * `params_obj`, which the caller keeps alive for the duration of the libpq
 * call (it's a `@&`-borrowed argument, never deallocated mid-call).
 * `*out_count` is always set; the returned array must be `free`d unless
 * NULL, which only happens when the array is empty. */
static const char **build_param_values(b_lean_obj_arg params_obj, size_t *out_count) {
    size_t n = lean_array_size(params_obj);
    *out_count = n;
    if (n == 0) return NULL;
    const char **values = (const char **)calloc(n, sizeof(const char *));
    if (!values) return NULL;
    for (size_t i = 0; i < n; i++) {
        lean_object *opt = lean_array_get_core(params_obj, i);
        if (lean_obj_tag(opt) == 0) {
            values[i] = NULL; /* Option.none -> SQL NULL */
        } else {
            values[i] = lean_string_cstr(lean_ctor_get(opt, 0)); /* Option.some s */
        }
    }
    return values;
}

/* ================================================================
 * CONNECTION MANAGEMENT
 * ================================================================ */

/*
 * @[extern "linen_pg_connect"]
 * opaque pgConnectImpl : @& String -> IO PgConn
 *
 * Opens a new connection to a PostgreSQL server using the given
 * connection info string. Always returns a PgConn handle — including one
 * with a bad `PQstatus` (e.g. authentication failure, unreachable host) —
 * mirroring PQconnectdb itself, which only fails to produce a connection
 * object on allocation failure. Callers must check `status`/`errorMessage`
 * (as `Database.SQL.Connection.acquire` does) rather than relying on this
 * throwing for a rejected connection; a thrown `IO` error here would give a
 * failed connection no `PgConn` for the caller to inspect, matching how
 * `exec`/`exec_params` only throw when `PQexec*` returns NULL, never for a
 * bad `PQresultStatus`.
 */
LEAN_EXPORT lean_obj_res linen_pg_connect(
    b_lean_obj_arg conninfo_obj,
    lean_obj_arg world
) {
    linen_pg_ensure_classes_initialized();

    const char *conninfo = lean_string_cstr(conninfo_obj);
    PGconn *conn = PQconnectdb(conninfo);

    if (!conn) {
        return mk_pg_io_error("PQconnectdb returned NULL");
    }

    lean_obj_res obj = mk_pg_conn(conn);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGconn wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_pg_status"]
 * opaque pgStatusImpl : @& PgConn -> IO UInt8
 *
 * Returns the connection status:
 *   0 = CONNECTION_OK
 *   1 = CONNECTION_BAD
 *   (other values for async connection states)
 */
LEAN_EXPORT lean_obj_res linen_pg_status(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    ConnStatusType status = PQstatus(conn);
    return lean_io_result_mk_ok(lean_box((uint8_t)status));
}

/*
 * @[extern "linen_pg_error_message"]
 * opaque pgErrorMessageImpl : @& PgConn -> IO String
 *
 * Returns the most recent error message for the connection.
 */
LEAN_EXPORT lean_obj_res linen_pg_error_message(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    const char *msg = PQerrorMessage(conn);
    if (!msg) msg = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(msg, strlen(msg)));
}

/*
 * @[extern "linen_pg_close"]
 * opaque pgCloseImpl : PgConn -> IO Unit
 *
 * Explicitly closes a connection before GC reclaims it.
 * Safe to call multiple times (idempotent).
 *
 * `conn` is declared `@&` (borrowed) on the Lean side, so the caller keeps
 * its own reference and the compiler-generated call site decrements it
 * exactly once, on its own, after this returns. This function used to take
 * `conn_obj` as owned (`lean_obj_arg`) and additionally call
 * `lean_dec_ref(conn_obj)` itself — a double decrement, since the borrowed
 * call site's own decrement still ran afterwards. Once the object's
 * refcount reached zero here, the external-class finalizer freed the
 * wrapper (and its heap block); the caller's later decrement then touched
 * already-freed memory, corrupting the allocator's free list. That
 * corruption doesn't crash immediately — it manifests later, whenever the
 * allocator next collects that thread's freed blocks (typically at thread
 * exit), which is why this surfaced as a `mi_heap_collect_ex` segfault well
 * after a successful, correct run rather than at the `close` call site.
 */
LEAN_EXPORT lean_obj_res linen_pg_close(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_pg_conn_t *wrapper = (linen_pg_conn_t *)lean_get_external_data(conn_obj);
    if (wrapper->conn) {
        PQfinish(wrapper->conn);
        wrapper->conn = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * QUERY EXECUTION
 * ================================================================ */

/*
 * @[extern "linen_pg_exec"]
 * opaque pgExecImpl : @& PgConn -> @& String -> IO PgResult
 *
 * Executes a simple SQL query string and returns the result.
 */
LEAN_EXPORT lean_obj_res linen_pg_exec(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg query_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_exec: connection is closed");
    }

    const char *query = lean_string_cstr(query_obj);
    PGresult *result = PQexec(conn, query);

    if (!result) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res obj = mk_pg_result(result);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGresult wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_pg_exec_params"]
 * opaque pgExecParamsImpl : @& PgConn -> @& String -> @& Array (Option String)
 *     -> IO PgResult
 *
 * Executes a parameterized SQL query in text format.
 *   conn    — connection handle
 *   query   — SQL with $1, $2, ... placeholders
 *   params  — parameter values; `none` encodes a SQL NULL
 *
 * (Previously this took 7 parameters — nparams, separate paramValues/
 * paramLengths/paramFormats arrays, and resultFormat — matching a design
 * this project never adopted on the Lean side, where `execParams` has
 * always taken a single `Array (Option String)`. Since the `@[extern]`
 * caller only ever passed the 3 arguments Lean declares, the extra
 * parameters here were reading whatever was left on the stack/in
 * registers — silently invalid, and fatal (`lean_is_array` assertion) the
 * moment a real query supplied any parameters.)
 */
LEAN_EXPORT lean_obj_res linen_pg_exec_params(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg query_obj,
    b_lean_obj_arg params_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_exec_params: connection is closed");
    }

    const char *query = lean_string_cstr(query_obj);

    size_t nparams = 0;
    const char **param_values = build_param_values(params_obj, &nparams);
    if (nparams > 0 && !param_values) {
        return mk_pg_io_error("pg_exec_params: malloc failed");
    }

    PGresult *result = PQexecParams(
        conn, query, (int)nparams,
        NULL, /* paramTypes — let the server infer */
        param_values,
        NULL, /* paramLengths — ignored for text-format parameters */
        NULL, /* paramFormats — NULL means every parameter is text */
        0     /* resultFormat — text */
    );

    free(param_values);

    if (!result) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res obj = mk_pg_result(result);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGresult wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_pg_prepare"]
 * opaque pgPrepareImpl : @& PgConn -> @& String -> @& String -> IO PgResult
 *
 * Creates a prepared statement, letting the server infer parameter types.
 *   conn      — connection handle
 *   stmtName  — name for the prepared statement (empty string = unnamed)
 *   query     — SQL with $1, $2, ... placeholders
 *
 * (Previously took a 4th `nparams : UInt32` argument the Lean side never
 * declared or passed — the same real-vs-declared-arity mismatch as
 * `linen_pg_exec_params` above, just never triggered since nothing calls
 * `prepare` yet.)
 */
LEAN_EXPORT lean_obj_res linen_pg_prepare(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg stmt_name_obj,
    b_lean_obj_arg query_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_prepare: connection is closed");
    }

    const char *stmt_name = lean_string_cstr(stmt_name_obj);
    const char *query = lean_string_cstr(query_obj);

    PGresult *result = PQprepare(
        conn, stmt_name, query, 0, /* nparams — 0 lets the server infer types */
        NULL /* paramTypes — let the server infer */
    );

    if (!result) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res obj = mk_pg_result(result);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGresult wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_pg_exec_prepared"]
 * opaque pgExecPreparedImpl : @& PgConn -> @& String -> @& Array (Option String)
 *     -> IO PgResult
 *
 * Executes a previously prepared statement in text format.
 *   conn      — connection handle
 *   stmtName  — name of the prepared statement
 *   params    — parameter values; `none` encodes a SQL NULL
 *
 * (Same real-vs-declared-arity mismatch as `linen_pg_exec_params` above —
 * this took 6 parameters here against 2 declared on the Lean side; fixed the
 * same way, and equally unreachable in practice since nothing calls
 * `execPrepared` yet.)
 */
LEAN_EXPORT lean_obj_res linen_pg_exec_prepared(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg stmt_name_obj,
    b_lean_obj_arg params_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_exec_prepared: connection is closed");
    }

    const char *stmt_name = lean_string_cstr(stmt_name_obj);

    size_t nparams = 0;
    const char **param_values = build_param_values(params_obj, &nparams);
    if (nparams > 0 && !param_values) {
        return mk_pg_io_error("pg_exec_prepared: malloc failed");
    }

    PGresult *result = PQexecPrepared(
        conn, stmt_name, (int)nparams,
        param_values,
        NULL, /* paramLengths — ignored for text-format parameters */
        NULL, /* paramFormats — NULL means every parameter is text */
        0     /* resultFormat — text */
    );

    free(param_values);

    if (!result) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res obj = mk_pg_result(result);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGresult wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/* ================================================================
 * RESULT INSPECTION
 * ================================================================ */

/*
 * @[extern "linen_pg_result_status"]
 * opaque pgResultStatusImpl : @& PgResult -> IO UInt8
 *
 * Returns the result status code:
 *   0 = PGRES_EMPTY_QUERY
 *   1 = PGRES_COMMAND_OK
 *   2 = PGRES_TUPLES_OK
 *   3 = PGRES_COPY_OUT
 *   4 = PGRES_COPY_IN
 *   5 = PGRES_BAD_RESPONSE
 *   6 = PGRES_NONFATAL_ERROR
 *   7 = PGRES_FATAL_ERROR
 */
LEAN_EXPORT lean_obj_res linen_pg_result_status(
    b_lean_obj_arg result_obj,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    ExecStatusType status = PQresultStatus(result);
    return lean_io_result_mk_ok(lean_box((uint8_t)status));
}

/*
 * @[extern "linen_pg_result_error_message"]
 * opaque pgResultErrorMessageImpl : @& PgResult -> IO String
 *
 * Returns the error message associated with the result, or empty string.
 */
LEAN_EXPORT lean_obj_res linen_pg_result_error_message(
    b_lean_obj_arg result_obj,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    const char *msg = PQresultErrorMessage(result);
    if (!msg) msg = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(msg, strlen(msg)));
}

/*
 * @[extern "linen_pg_ntuples"]
 * opaque pgNtuplesImpl : @& PgResult -> IO UInt32
 *
 * Returns the number of rows in the result.
 */
LEAN_EXPORT lean_obj_res linen_pg_ntuples(
    b_lean_obj_arg result_obj,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int n = PQntuples(result);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)n));
}

/*
 * @[extern "linen_pg_nfields"]
 * opaque pgNfieldsImpl : @& PgResult -> IO UInt32
 *
 * Returns the number of columns in the result.
 */
LEAN_EXPORT lean_obj_res linen_pg_nfields(
    b_lean_obj_arg result_obj,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int n = PQnfields(result);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)n));
}

/*
 * @[extern "linen_pg_getvalue"]
 * opaque pgGetvalueImpl : @& PgResult -> UInt32 -> UInt32 -> IO String
 *
 * Returns the value of a field (row, col) as a string.
 * Returns empty string for NULL values — use getisnull to distinguish.
 */
LEAN_EXPORT lean_obj_res linen_pg_getvalue(
    b_lean_obj_arg result_obj,
    uint32_t row,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    const char *val = PQgetvalue(result, (int)row, (int)col);
    if (!val) val = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(val, strlen(val)));
}

/*
 * @[extern "linen_pg_getisnull"]
 * opaque pgGetisnullImpl : @& PgResult -> UInt32 -> UInt32 -> IO UInt8
 *
 * Returns 1 if the field (row, col) is NULL, 0 otherwise.
 */
LEAN_EXPORT lean_obj_res linen_pg_getisnull(
    b_lean_obj_arg result_obj,
    uint32_t row,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int is_null = PQgetisnull(result, (int)row, (int)col);
    return lean_io_result_mk_ok(lean_box((uint8_t)is_null));
}

/*
 * @[extern "linen_pg_fname"]
 * opaque pgFnameImpl : @& PgResult -> UInt32 -> IO String
 *
 * Returns the column name for the given column number.
 */
LEAN_EXPORT lean_obj_res linen_pg_fname(
    b_lean_obj_arg result_obj,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    const char *name = PQfname(result, (int)col);
    if (!name) name = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(name, strlen(name)));
}

/*
 * @[extern "linen_pg_ftype"]
 * opaque pgFtypeImpl : @& PgResult -> UInt32 -> IO UInt32
 *
 * Returns the OID of the column type for the given column number.
 */
LEAN_EXPORT lean_obj_res linen_pg_ftype(
    b_lean_obj_arg result_obj,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    Oid oid = PQftype(result, (int)col);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)oid));
}

/*
 * @[extern "linen_pg_cmd_tuples"]
 * opaque pgCmdTuplesImpl : @& PgResult -> IO String
 *
 * Returns the number of rows affected by the command as a string.
 * For INSERT, UPDATE, DELETE commands. Returns "" for other commands.
 */
LEAN_EXPORT lean_obj_res linen_pg_cmd_tuples(
    b_lean_obj_arg result_obj,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    const char *tuples = PQcmdTuples(result);
    if (!tuples) tuples = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(tuples, strlen(tuples)));
}

/* ================================================================
 * RESULT CLEANUP
 * ================================================================ */

/*
 * @[extern "linen_pg_clear"]
 * opaque pgClearImpl : PgResult -> IO Unit
 *
 * Explicitly frees a result before GC reclaims it.
 * Safe to call multiple times (idempotent).
 */
LEAN_EXPORT lean_obj_res linen_pg_clear(
    lean_obj_arg result_obj,
    lean_obj_arg world
) {
    linen_pg_result_t *wrapper = (linen_pg_result_t *)lean_get_external_data(result_obj);
    if (wrapper->result) {
        PQclear(wrapper->result);
        wrapper->result = NULL;
    }
    lean_dec_ref(result_obj);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * STRING ESCAPING
 * ================================================================ */

/*
 * @[extern "linen_pg_escape_literal"]
 * opaque pgEscapeLiteralImpl : @& PgConn -> @& String -> IO String
 *
 * Escapes a string for use as a SQL literal, including surrounding
 * single quotes. The caller does NOT need to add quotes.
 * Returns an error if the connection is invalid.
 */
LEAN_EXPORT lean_obj_res linen_pg_escape_literal(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg str_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_escape_literal: connection is closed");
    }

    const char *str = lean_string_cstr(str_obj);
    size_t len = lean_string_size(str_obj) - 1; /* exclude null terminator */

    char *escaped = PQescapeLiteral(conn, str, len);
    if (!escaped) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res result = lean_mk_string_from_bytes(escaped, strlen(escaped));
    PQfreemem(escaped);
    return lean_io_result_mk_ok(result);
}

/*
 * @[extern "linen_pg_escape_identifier"]
 * opaque pgEscapeIdentifierImpl : @& PgConn -> @& String -> IO String
 *
 * Escapes a string for use as a SQL identifier (table name, column name),
 * including surrounding double quotes.
 * Returns an error if the connection is invalid.
 */
LEAN_EXPORT lean_obj_res linen_pg_escape_identifier(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg str_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_escape_identifier: connection is closed");
    }

    const char *str = lean_string_cstr(str_obj);
    size_t len = lean_string_size(str_obj) - 1; /* exclude null terminator */

    char *escaped = PQescapeIdentifier(conn, str, len);
    if (!escaped) {
        return mk_pg_conn_error(conn);
    }

    lean_obj_res result = lean_mk_string_from_bytes(escaped, strlen(escaped));
    PQfreemem(escaped);
    return lean_io_result_mk_ok(result);
}

/* ================================================================
 * LISTEN/NOTIFY
 * ================================================================ */

/*
 * @[extern "linen_pg_consume_input"]
 * opaque pgConsumeInputImpl : @& PgConn -> IO UInt8
 *
 * Consumes any available input from the server.
 * Returns 1 on success, 0 on failure.
 * Must be called before pgNotifies to process pending notifications.
 */
LEAN_EXPORT lean_obj_res linen_pg_consume_input(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_consume_input: connection is closed");
    }

    int ok = PQconsumeInput(conn);
    return lean_io_result_mk_ok(lean_box((uint8_t)ok));
}

/*
 * @[extern "linen_pg_notifies"]
 * opaque pgNotifiesImpl : @& PgConn -> IO (Option (String x String x UInt32))
 *
 * Returns the next pending notification, if any.
 * The triple contains (channel_name, payload, notifying_pid).
 * Returns Option.none if no notification is pending.
 *
 * Typical usage: call pgConsumeInput first, then pgNotifies in a loop
 * until it returns none.
 */
LEAN_EXPORT lean_obj_res linen_pg_notifies(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_notifies: connection is closed");
    }

    PGnotify *notify = PQnotifies(conn);
    if (!notify) {
        return lean_io_result_mk_ok(mk_option_none());
    }

    /* Build the triple: (channel, payload, pid)
     * Lean encodes (a, b, c) as (a, (b, c)) = Prod(a, Prod(b, c)) */
    lean_obj_res channel = lean_mk_string_from_bytes(
        notify->relname, strlen(notify->relname));
    lean_obj_res payload = lean_mk_string_from_bytes(
        notify->extra, strlen(notify->extra));
    lean_obj_res pid = lean_box_uint32((uint32_t)notify->be_pid);

    lean_obj_res inner_pair = mk_pair(payload, pid);
    lean_obj_res triple = mk_pair(channel, inner_pair);

    PQfreemem(notify);

    return lean_io_result_mk_ok(mk_option_some(triple));
}

/* ================================================================
 * TRANSACTION STATUS
 * ================================================================ */

/*
 * @[extern "linen_pg_transaction_status"]
 * opaque pgTransactionStatusImpl : @& PgConn -> IO UInt8
 *
 * Returns the current in-transaction status of the connection:
 *   0 = PQTRANS_IDLE       — not in a transaction
 *   1 = PQTRANS_ACTIVE     — command in progress
 *   2 = PQTRANS_INTRANS    — idle, in a valid transaction block
 *   3 = PQTRANS_INERROR    — idle, in a failed transaction block
 *   4 = PQTRANS_UNKNOWN    — connection is bad
 */
LEAN_EXPORT lean_obj_res linen_pg_transaction_status(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    PGTransactionStatusType status = PQtransactionStatus(conn);
    return lean_io_result_mk_ok(lean_box((uint8_t)status));
}

/* ================================================================
 * CONNECTION INFO
 * ================================================================ */

/*
 * @[extern "linen_pg_server_version"]
 * opaque pgServerVersionImpl : @& PgConn -> IO UInt32
 *
 * Returns the server version as an integer (e.g. 150004 for 15.4).
 */
LEAN_EXPORT lean_obj_res linen_pg_server_version(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    int ver = PQserverVersion(conn);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ver));
}

/*
 * @[extern "linen_pg_socket"]
 * opaque pgSocketImpl : @& PgConn -> IO Int32
 *
 * Returns the file descriptor of the connection socket.
 * Useful for integrating with event loops (epoll/kqueue).
 * Returns -1 if the connection is not open.
 */
LEAN_EXPORT lean_obj_res linen_pg_socket(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    int fd = PQsocket(conn);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fd));
}

/*
 * @[extern "linen_pg_db"]
 * opaque pgDbImpl : @& PgConn -> IO String
 *
 * Returns the database name of the connection.
 */
LEAN_EXPORT lean_obj_res linen_pg_db(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    const char *db = PQdb(conn);
    if (!db) db = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(db, strlen(db)));
}

/*
 * @[extern "linen_pg_user"]
 * opaque pgUserImpl : @& PgConn -> IO String
 *
 * Returns the user name of the connection.
 */
LEAN_EXPORT lean_obj_res linen_pg_user(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    const char *user = PQuser(conn);
    if (!user) user = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(user, strlen(user)));
}

/*
 * @[extern "linen_pg_host"]
 * opaque pgHostImpl : @& PgConn -> IO String
 *
 * Returns the server host name of the connection.
 */
LEAN_EXPORT lean_obj_res linen_pg_host(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    const char *host = PQhost(conn);
    if (!host) host = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(host, strlen(host)));
}

/*
 * @[extern "linen_pg_port"]
 * opaque pgPortImpl : @& PgConn -> IO String
 *
 * Returns the port of the connection.
 */
LEAN_EXPORT lean_obj_res linen_pg_port(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    const char *port = PQport(conn);
    if (!port) port = "";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(port, strlen(port)));
}

/* ================================================================
 * RESULT FIELD SIZE / BINARY DATA
 * ================================================================ */

/*
 * @[extern "linen_pg_getlength"]
 * opaque pgGetlengthImpl : @& PgResult -> UInt32 -> UInt32 -> IO UInt32
 *
 * Returns the actual length of a field value in bytes.
 * Useful for binary format results.
 */
LEAN_EXPORT lean_obj_res linen_pg_getlength(
    b_lean_obj_arg result_obj,
    uint32_t row,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int len = PQgetlength(result, (int)row, (int)col);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)len));
}

/*
 * @[extern "linen_pg_fformat"]
 * opaque pgFformatImpl : @& PgResult -> UInt32 -> IO UInt32
 *
 * Returns the format code for the given column:
 *   0 = text format
 *   1 = binary format
 */
LEAN_EXPORT lean_obj_res linen_pg_fformat(
    b_lean_obj_arg result_obj,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int fmt = PQfformat(result, (int)col);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)fmt));
}

/*
 * @[extern "linen_pg_getvalue_bytes"]
 * opaque pgGetvalueBytesImpl : @& PgResult -> UInt32 -> UInt32 -> IO ByteArray
 *
 * Returns the raw bytes of a field value as a ByteArray.
 * Useful for binary format results or when you need the exact bytes.
 */
LEAN_EXPORT lean_obj_res linen_pg_getvalue_bytes(
    b_lean_obj_arg result_obj,
    uint32_t row,
    uint32_t col,
    lean_obj_arg world
) {
    PGresult *result = get_pg_result(result_obj);
    int len = PQgetlength(result, (int)row, (int)col);
    const char *val = PQgetvalue(result, (int)row, (int)col);

    lean_obj_res arr = lean_alloc_sarray(1, (size_t)len, (size_t)len);
    if (len > 0 && val) {
        memcpy(lean_sarray_cptr(arr), val, (size_t)len);
    }
    return lean_io_result_mk_ok(arr);
}

/* ================================================================
 * COPY PROTOCOL SUPPORT
 * ================================================================ */

/*
 * @[extern "linen_pg_put_copy_data"]
 * opaque pgPutCopyDataImpl : @& PgConn -> @& String -> IO UInt8
 *
 * Sends data to the server during COPY IN state.
 * Returns 1 on success, 0 if the data was not sent (non-blocking mode,
 * would block), -1 on error.
 */
LEAN_EXPORT lean_obj_res linen_pg_put_copy_data(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg data_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_put_copy_data: connection is closed");
    }

    const char *data = lean_string_cstr(data_obj);
    int len = (int)(lean_string_size(data_obj) - 1);

    int ret = PQputCopyData(conn, data, len);
    if (ret == -1) {
        return mk_pg_conn_error(conn);
    }
    return lean_io_result_mk_ok(lean_box((uint8_t)ret));
}

/*
 * @[extern "linen_pg_put_copy_end"]
 * opaque pgPutCopyEndImpl : @& PgConn -> @& String -> IO UInt8
 *
 * Signals the end of COPY IN data.
 * Pass an empty string for success, or an error message to abort.
 * Returns 1 on success, 0 if would block, -1 on error.
 */
LEAN_EXPORT lean_obj_res linen_pg_put_copy_end(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg errmsg_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_put_copy_end: connection is closed");
    }

    const char *errmsg = lean_string_cstr(errmsg_obj);
    /* Pass NULL for normal completion, error message string for abort */
    const char *msg = (strlen(errmsg) == 0) ? NULL : errmsg;

    int ret = PQputCopyEnd(conn, msg);
    if (ret == -1) {
        return mk_pg_conn_error(conn);
    }
    return lean_io_result_mk_ok(lean_box((uint8_t)ret));
}

/* ================================================================
 * NON-BLOCKING / ASYNC CONNECTION SUPPORT
 * ================================================================ */

/*
 * @[extern "linen_pg_connect_start"]
 * opaque pgConnectStartImpl : @& String -> IO PgConn
 *
 * Begins an asynchronous connection to a PostgreSQL server.
 * Returns immediately with a PgConn handle.
 * Use pgConnectPoll to drive the connection to completion.
 */
LEAN_EXPORT lean_obj_res linen_pg_connect_start(
    b_lean_obj_arg conninfo_obj,
    lean_obj_arg world
) {
    linen_pg_ensure_classes_initialized();

    const char *conninfo = lean_string_cstr(conninfo_obj);
    PGconn *conn = PQconnectStart(conninfo);

    if (!conn) {
        return mk_pg_io_error("PQconnectStart returned NULL");
    }

    if (PQstatus(conn) == CONNECTION_BAD) {
        lean_obj_res err = mk_pg_conn_error(conn);
        PQfinish(conn);
        return err;
    }

    lean_obj_res obj = mk_pg_conn(conn);
    if (!obj) {
        return mk_pg_io_error("malloc failed for PGconn wrapper");
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_pg_connect_poll"]
 * opaque pgConnectPollImpl : @& PgConn -> IO UInt8
 *
 * Drives an asynchronous connection attempt.
 * Returns:
 *   0 = PGRES_POLLING_FAILED
 *   1 = PGRES_POLLING_READING  — wait for socket to be readable
 *   2 = PGRES_POLLING_WRITING  — wait for socket to be writable
 *   3 = PGRES_POLLING_OK       — connection established
 */
LEAN_EXPORT lean_obj_res linen_pg_connect_poll(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    PostgresPollingStatusType status = PQconnectPoll(conn);
    return lean_io_result_mk_ok(lean_box((uint8_t)status));
}

/*
 * @[extern "linen_pg_set_nonblocking"]
 * opaque pgSetNonblockingImpl : @& PgConn -> UInt8 -> IO UInt8
 *
 * Sets the non-blocking mode of the connection.
 *   arg = 1 for non-blocking, 0 for blocking
 * Returns 0 on success, -1 on error.
 */
LEAN_EXPORT lean_obj_res linen_pg_set_nonblocking(
    b_lean_obj_arg conn_obj,
    uint8_t arg,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_set_nonblocking: connection is closed");
    }

    int ret = PQsetnonblocking(conn, (int)arg);
    return lean_io_result_mk_ok(lean_box((uint8_t)(ret == 0 ? 0 : 1)));
}

/*
 * @[extern "linen_pg_is_nonblocking"]
 * opaque pgIsNonblockingImpl : @& PgConn -> IO UInt8
 *
 * Returns 1 if the connection is in non-blocking mode, 0 otherwise.
 */
LEAN_EXPORT lean_obj_res linen_pg_is_nonblocking(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    int nb = PQisnonblocking(conn);
    return lean_io_result_mk_ok(lean_box((uint8_t)nb));
}

/* ================================================================
 * RESET (RECONNECT)
 * ================================================================ */

/*
 * @[extern "linen_pg_reset"]
 * opaque pgResetImpl : @& PgConn -> IO Unit
 *
 * Resets the connection to the server, using the same parameters
 * that were used to connect originally. Blocks until reconnected.
 */
LEAN_EXPORT lean_obj_res linen_pg_reset(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    PGconn *conn = get_pg_conn(conn_obj);
    if (!conn) {
        return mk_pg_io_error("pg_reset: connection is closed");
    }

    PQreset(conn);

    if (PQstatus(conn) != CONNECTION_OK) {
        return mk_pg_conn_error(conn);
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * RESULT STATUS STRING
 * ================================================================ */

/*
 * @[extern "linen_pg_res_status"]
 * opaque pgResStatusImpl : UInt8 -> IO String
 *
 * Converts a result status code to a human-readable string.
 */
LEAN_EXPORT lean_obj_res linen_pg_res_status(
    uint8_t status,
    lean_obj_arg world
) {
    const char *str = PQresStatus((ExecStatusType)status);
    if (!str) str = "UNKNOWN";
    return lean_io_result_mk_ok(
        lean_mk_string_from_bytes(str, strlen(str)));
}
