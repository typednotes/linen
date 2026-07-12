/*
 * ffi/duckdb_shim.c — DuckDB FFI for Lean 4
 *
 * Wraps DuckDB's C API (`duckdb.h`, from the pinned prebuilt archive
 * `lakefile.lean` downloads/unpacks — see that file's "DuckDB discovery"
 * section) for `Linen.Database.DuckDB.FFI.OpenConnect`. Follows the same
 * `lean_alloc_external` pattern as `ffi/sqlite3_shim.c`: one opaque external
 * class per owning handle type (`Database`, `Connection`, `InstanceCache`,
 * `ClientContext`, `ArrowOptions`, `Value`), each with a GC finalizer that
 * releases the underlying native resource if the caller never explicitly did
 * — and each explicit "close"/"destroy" entry point below is itself
 * idempotent (nulls out the wrapped pointer after releasing it), mirroring
 * `ffi/sqlite3_shim.c`'s `linen_sqlite3_close`.
 *
 * Scope: the entry points bound by `Database.DuckDB.FFI.OpenConnect` (module
 * #2 of `docs/imports/duckdb-ffi/dependencies.md`) plus, as of this
 * revision, `Database.DuckDB.FFI.{Appender,BindValues,Catalog,Configuration,
 * DataChunk}` (modules #2-#6, all depending on nothing but `Types`) —
 * `duckdb_open`/`duckdb_open_ext`/`duckdb_close`/`duckdb_connect`/
 * `duckdb_interrupt`/`duckdb_query_progress`/`duckdb_disconnect`/the
 * instance-cache and client-context/Arrow-options accessors/
 * `duckdb_library_version`/`duckdb_get_table_names`, the full bulk-append
 * API, prepared-statement parameter binding, catalog inspection,
 * `duckdb_config` management, and data-chunk/column access.
 * `duckdb_open_ext`/`duckdb_get_or_create_from_cache` still always pass a
 * NULL `duckdb_config`: even though `Configuration` is now ported, wiring a
 * real `Config` through `OpenConnect`'s own entry points is left to
 * whoever ports `duckdb-simple`'s config-plumbing layer on top of both
 * modules — this is the same kind of documented, incremental scope-growth
 * as `Linen/Database/DuckDB/FFI/Types.lean`'s doc comment describes, not a
 * behavior-weakening simplification (DuckDB's own API contract treats a NULL
 * config as "use the default configuration", a real and fully-specified
 * behavior, not a gap this port papers over).
 *
 * As of this revision also covers `Database.DuckDB.FFI.{ErrorData,
 * ExecutePrepared,FileSystem,Helpers,Logging}` (modules #7/#8/#9/#10/#11,
 * all depending on nothing but `Types`): structured-error accessors,
 * executing a bound prepared statement, the virtual/attached file-system
 * API, assorted small helpers (malloc/free, `duckdb_string_t` accessors,
 * date/time decomposition, numeric conversions), and custom log-storage
 * registration. `Logging` is the one addition here that calls back into a
 * Lean closure from native code — see the `LOGGING` section below for the
 * trampoline pair this needed (`linen_duckdb_log_write_trampoline`/
 * `linen_duckdb_log_delete_trampoline`), modeled on `ffi/sqlite3_shim.c`'s
 * own `xFunc`/`xDestroy` pair for `sqlite3_create_function_v2`.
 *
 * A handful of `TEST SUPPORT` entry points near the end of this file back
 * small helpers used only by this batch's own `Tests/` — `duckdb_query`
 * (for DDL/DML setup, e.g. `CREATE TABLE`/`ATTACH`) and `duckdb_prepare`/
 * `duckdb_destroy_prepare` (to obtain a real `PreparedStatement` to bind
 * against) — neither of which belongs to any of the five modules above.
 * They exist purely so this batch's tests can exercise real DuckDB behavior
 * end-to-end without waiting on `Database.DuckDB.FFI.QueryExecution`/
 * `PreparedStatements` (out of scope for this batch) to be ported first;
 * whoever ports those two modules later is free to supersede/rename them.
 *
 * Platform: macOS and Linux (any platform `lakefile.lean`'s DuckDB-archive
 * resolution supports).
 */

#include <lean/lean.h>
#include "duckdb.h"
#include <string.h>
#include <stdlib.h>
#include <stdatomic.h>

/* ────────────────────────────────────────────────────────────
 * External classes for the owning handle types
 * ──────────────────────────────────────────────────────────── */

typedef struct { duckdb_database db; } linen_duckdb_database_t;
typedef struct { duckdb_connection conn; } linen_duckdb_connection_t;
typedef struct { duckdb_instance_cache cache; } linen_duckdb_instance_cache_t;
typedef struct { duckdb_client_context ctx; } linen_duckdb_client_context_t;
typedef struct { duckdb_arrow_options opts; } linen_duckdb_arrow_options_t;
typedef struct { duckdb_value val; } linen_duckdb_value_t;
typedef struct { duckdb_appender app; } linen_duckdb_appender_t;
typedef struct { duckdb_prepared_statement stmt; } linen_duckdb_prepared_statement_t;
typedef struct { duckdb_data_chunk chunk; } linen_duckdb_data_chunk_t;
/* Non-owning: a `duckdb_vector` is a borrowed pointer into its parent
 * `DataChunk` (see `duckdb.h`'s own doc comment on
 * `duckdb_data_chunk_get_vector`) — this wrapper's finalizer frees only
 * itself, never `vec`. */
typedef struct { duckdb_vector vec; } linen_duckdb_vector_t;
typedef struct { duckdb_logical_type type; } linen_duckdb_logical_type_t;
typedef struct { duckdb_config config; } linen_duckdb_config_t;
typedef struct { duckdb_config_option option; } linen_duckdb_config_option_t;
typedef struct { duckdb_error_data err; } linen_duckdb_error_data_t;
typedef struct { duckdb_catalog catalog; } linen_duckdb_catalog_t;
typedef struct { duckdb_catalog_entry entry; } linen_duckdb_catalog_entry_t;
/* `duckdb_result` is a flat by-value struct (not a pointer-typedef): embed
 * it by value in the wrapper itself, per `Types.lean`'s doc comment on
 * `ResultHandle`. */
typedef struct { duckdb_result result; } linen_duckdb_result_t;
typedef struct { duckdb_file_system fs; } linen_duckdb_file_system_t;
typedef struct { duckdb_file_open_options opts; } linen_duckdb_file_open_options_t;
typedef struct { duckdb_file_handle handle; } linen_duckdb_file_handle_t;
typedef struct { duckdb_log_storage storage; } linen_duckdb_log_storage_t;
/* Wraps a bare `void *` from `duckdb_malloc`. */
typedef struct { void *ptr; } linen_duckdb_raw_memory_t;
typedef struct { duckdb_scalar_function fn; } linen_duckdb_scalar_function_t;
typedef struct { duckdb_scalar_function_set set; } linen_duckdb_scalar_function_set_t;
/* Non-owning: the `duckdb_data_chunk` a scalar function's native callback
 * receives as its input is owned by DuckDB itself, released once the
 * callback returns — never by this wrapper. See `Types.lean`'s doc comment
 * on `BorrowedDataChunkHandle`. */
typedef struct { duckdb_data_chunk chunk; } linen_duckdb_borrowed_data_chunk_t;
/* Non-owning: a validity-mask pointer is owned by its parent `Vector`. */
typedef struct { uint64_t *mask; } linen_duckdb_validity_mask_t;

static lean_external_class *g_linen_duckdb_database_class = NULL;
static lean_external_class *g_linen_duckdb_connection_class = NULL;
static lean_external_class *g_linen_duckdb_instance_cache_class = NULL;
static lean_external_class *g_linen_duckdb_client_context_class = NULL;
static lean_external_class *g_linen_duckdb_arrow_options_class = NULL;
static lean_external_class *g_linen_duckdb_value_class = NULL;
static lean_external_class *g_linen_duckdb_appender_class = NULL;
static lean_external_class *g_linen_duckdb_prepared_statement_class = NULL;
static lean_external_class *g_linen_duckdb_data_chunk_class = NULL;
static lean_external_class *g_linen_duckdb_vector_class = NULL;
static lean_external_class *g_linen_duckdb_logical_type_class = NULL;
static lean_external_class *g_linen_duckdb_config_class = NULL;
static lean_external_class *g_linen_duckdb_config_option_class = NULL;
static lean_external_class *g_linen_duckdb_error_data_class = NULL;
static lean_external_class *g_linen_duckdb_catalog_class = NULL;
static lean_external_class *g_linen_duckdb_catalog_entry_class = NULL;
static lean_external_class *g_linen_duckdb_result_class = NULL;
static lean_external_class *g_linen_duckdb_file_system_class = NULL;
static lean_external_class *g_linen_duckdb_file_open_options_class = NULL;
static lean_external_class *g_linen_duckdb_file_handle_class = NULL;
static lean_external_class *g_linen_duckdb_log_storage_class = NULL;
static lean_external_class *g_linen_duckdb_raw_memory_class = NULL;
static lean_external_class *g_linen_duckdb_scalar_function_class = NULL;
static lean_external_class *g_linen_duckdb_scalar_function_set_class = NULL;
static lean_external_class *g_linen_duckdb_borrowed_data_chunk_class = NULL;
static lean_external_class *g_linen_duckdb_validity_mask_class = NULL;

static void linen_duckdb_noop_foreach(void *mod, b_lean_obj_arg fn) {
    /* no sub-objects to traverse */
}

static void linen_duckdb_database_finalizer(void *ptr) {
    linen_duckdb_database_t *d = (linen_duckdb_database_t *)ptr;
    if (d) {
        if (d->db) duckdb_close(&d->db);
        free(d);
    }
}

static void linen_duckdb_connection_finalizer(void *ptr) {
    linen_duckdb_connection_t *c = (linen_duckdb_connection_t *)ptr;
    if (c) {
        if (c->conn) duckdb_disconnect(&c->conn);
        free(c);
    }
}

static void linen_duckdb_instance_cache_finalizer(void *ptr) {
    linen_duckdb_instance_cache_t *c = (linen_duckdb_instance_cache_t *)ptr;
    if (c) {
        if (c->cache) duckdb_destroy_instance_cache(&c->cache);
        free(c);
    }
}

static void linen_duckdb_client_context_finalizer(void *ptr) {
    linen_duckdb_client_context_t *c = (linen_duckdb_client_context_t *)ptr;
    if (c) {
        if (c->ctx) duckdb_destroy_client_context(&c->ctx);
        free(c);
    }
}

static void linen_duckdb_arrow_options_finalizer(void *ptr) {
    linen_duckdb_arrow_options_t *a = (linen_duckdb_arrow_options_t *)ptr;
    if (a) {
        if (a->opts) duckdb_destroy_arrow_options(&a->opts);
        free(a);
    }
}

static void linen_duckdb_value_finalizer(void *ptr) {
    linen_duckdb_value_t *v = (linen_duckdb_value_t *)ptr;
    if (v) {
        if (v->val) duckdb_destroy_value(&v->val);
        free(v);
    }
}

static void linen_duckdb_appender_finalizer(void *ptr) {
    linen_duckdb_appender_t *a = (linen_duckdb_appender_t *)ptr;
    if (a) {
        if (a->app) duckdb_appender_destroy(&a->app);
        free(a);
    }
}

static void linen_duckdb_prepared_statement_finalizer(void *ptr) {
    linen_duckdb_prepared_statement_t *s = (linen_duckdb_prepared_statement_t *)ptr;
    if (s) {
        if (s->stmt) duckdb_destroy_prepare(&s->stmt);
        free(s);
    }
}

static void linen_duckdb_data_chunk_finalizer(void *ptr) {
    linen_duckdb_data_chunk_t *c = (linen_duckdb_data_chunk_t *)ptr;
    if (c) {
        if (c->chunk) duckdb_destroy_data_chunk(&c->chunk);
        free(c);
    }
}

static void linen_duckdb_vector_finalizer(void *ptr) {
    /* Non-owning: never destroy the wrapped `duckdb_vector` itself. */
    linen_duckdb_vector_t *v = (linen_duckdb_vector_t *)ptr;
    if (v) free(v);
}

static void linen_duckdb_logical_type_finalizer(void *ptr) {
    linen_duckdb_logical_type_t *t = (linen_duckdb_logical_type_t *)ptr;
    if (t) {
        if (t->type) duckdb_destroy_logical_type(&t->type);
        free(t);
    }
}

static void linen_duckdb_config_finalizer(void *ptr) {
    linen_duckdb_config_t *c = (linen_duckdb_config_t *)ptr;
    if (c) {
        if (c->config) duckdb_destroy_config(&c->config);
        free(c);
    }
}

static void linen_duckdb_config_option_finalizer(void *ptr) {
    linen_duckdb_config_option_t *o = (linen_duckdb_config_option_t *)ptr;
    if (o) {
        if (o->option) duckdb_destroy_config_option(&o->option);
        free(o);
    }
}

static void linen_duckdb_error_data_finalizer(void *ptr) {
    linen_duckdb_error_data_t *e = (linen_duckdb_error_data_t *)ptr;
    if (e) {
        if (e->err) duckdb_destroy_error_data(&e->err);
        free(e);
    }
}

static void linen_duckdb_catalog_finalizer(void *ptr) {
    linen_duckdb_catalog_t *c = (linen_duckdb_catalog_t *)ptr;
    if (c) {
        if (c->catalog) duckdb_destroy_catalog(&c->catalog);
        free(c);
    }
}

static void linen_duckdb_catalog_entry_finalizer(void *ptr) {
    linen_duckdb_catalog_entry_t *e = (linen_duckdb_catalog_entry_t *)ptr;
    if (e) {
        if (e->entry) duckdb_destroy_catalog_entry(&e->entry);
        free(e);
    }
}

static void linen_duckdb_result_finalizer(void *ptr) {
    linen_duckdb_result_t *r = (linen_duckdb_result_t *)ptr;
    if (r) {
        duckdb_destroy_result(&r->result);
        free(r);
    }
}

static void linen_duckdb_file_system_finalizer(void *ptr) {
    linen_duckdb_file_system_t *f = (linen_duckdb_file_system_t *)ptr;
    if (f) {
        if (f->fs) duckdb_destroy_file_system(&f->fs);
        free(f);
    }
}

static void linen_duckdb_file_open_options_finalizer(void *ptr) {
    linen_duckdb_file_open_options_t *o = (linen_duckdb_file_open_options_t *)ptr;
    if (o) {
        if (o->opts) duckdb_destroy_file_open_options(&o->opts);
        free(o);
    }
}

static void linen_duckdb_file_handle_finalizer(void *ptr) {
    linen_duckdb_file_handle_t *h = (linen_duckdb_file_handle_t *)ptr;
    if (h) {
        if (h->handle) duckdb_destroy_file_handle(&h->handle);
        free(h);
    }
}

static void linen_duckdb_log_storage_finalizer(void *ptr) {
    linen_duckdb_log_storage_t *s = (linen_duckdb_log_storage_t *)ptr;
    if (s) {
        if (s->storage) duckdb_destroy_log_storage(&s->storage);
        free(s);
    }
}

static void linen_duckdb_raw_memory_finalizer(void *ptr) {
    linen_duckdb_raw_memory_t *m = (linen_duckdb_raw_memory_t *)ptr;
    if (m) {
        if (m->ptr) duckdb_free(m->ptr);
        free(m);
    }
}

static void linen_duckdb_scalar_function_finalizer(void *ptr) {
    linen_duckdb_scalar_function_t *f = (linen_duckdb_scalar_function_t *)ptr;
    if (f) {
        if (f->fn) duckdb_destroy_scalar_function(&f->fn);
        free(f);
    }
}

static void linen_duckdb_scalar_function_set_finalizer(void *ptr) {
    linen_duckdb_scalar_function_set_t *s = (linen_duckdb_scalar_function_set_t *)ptr;
    if (s) {
        if (s->set) duckdb_destroy_scalar_function_set(&s->set);
        free(s);
    }
}

static void linen_duckdb_borrowed_data_chunk_finalizer(void *ptr) {
    /* Non-owning: never destroy the wrapped `duckdb_data_chunk` itself. */
    linen_duckdb_borrowed_data_chunk_t *c = (linen_duckdb_borrowed_data_chunk_t *)ptr;
    if (c) free(c);
}

static void linen_duckdb_validity_mask_finalizer(void *ptr) {
    /* Non-owning: never free the wrapped mask pointer itself. */
    linen_duckdb_validity_mask_t *m = (linen_duckdb_validity_mask_t *)ptr;
    if (m) free(m);
}

static atomic_int g_linen_duckdb_classes_initialized = 0;

static void linen_duckdb_ensure_classes_initialized(void) {
    if (atomic_load_explicit(&g_linen_duckdb_classes_initialized, memory_order_acquire))
        return;
    g_linen_duckdb_database_class = lean_register_external_class(
        &linen_duckdb_database_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_connection_class = lean_register_external_class(
        &linen_duckdb_connection_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_instance_cache_class = lean_register_external_class(
        &linen_duckdb_instance_cache_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_client_context_class = lean_register_external_class(
        &linen_duckdb_client_context_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_arrow_options_class = lean_register_external_class(
        &linen_duckdb_arrow_options_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_value_class = lean_register_external_class(
        &linen_duckdb_value_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_appender_class = lean_register_external_class(
        &linen_duckdb_appender_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_prepared_statement_class = lean_register_external_class(
        &linen_duckdb_prepared_statement_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_data_chunk_class = lean_register_external_class(
        &linen_duckdb_data_chunk_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_vector_class = lean_register_external_class(
        &linen_duckdb_vector_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_logical_type_class = lean_register_external_class(
        &linen_duckdb_logical_type_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_config_class = lean_register_external_class(
        &linen_duckdb_config_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_config_option_class = lean_register_external_class(
        &linen_duckdb_config_option_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_error_data_class = lean_register_external_class(
        &linen_duckdb_error_data_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_catalog_class = lean_register_external_class(
        &linen_duckdb_catalog_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_catalog_entry_class = lean_register_external_class(
        &linen_duckdb_catalog_entry_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_result_class = lean_register_external_class(
        &linen_duckdb_result_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_file_system_class = lean_register_external_class(
        &linen_duckdb_file_system_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_file_open_options_class = lean_register_external_class(
        &linen_duckdb_file_open_options_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_file_handle_class = lean_register_external_class(
        &linen_duckdb_file_handle_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_log_storage_class = lean_register_external_class(
        &linen_duckdb_log_storage_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_raw_memory_class = lean_register_external_class(
        &linen_duckdb_raw_memory_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_scalar_function_class = lean_register_external_class(
        &linen_duckdb_scalar_function_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_scalar_function_set_class = lean_register_external_class(
        &linen_duckdb_scalar_function_set_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_borrowed_data_chunk_class = lean_register_external_class(
        &linen_duckdb_borrowed_data_chunk_finalizer, &linen_duckdb_noop_foreach);
    g_linen_duckdb_validity_mask_class = lean_register_external_class(
        &linen_duckdb_validity_mask_finalizer, &linen_duckdb_noop_foreach);
    atomic_store_explicit(&g_linen_duckdb_classes_initialized, 1, memory_order_release);
}

/* ────────────────────────────────────────────────────────────
 * Helpers: wrap/unwrap external objects
 * ──────────────────────────────────────────────────────────── */

static inline lean_obj_res mk_duckdb_database(duckdb_database db) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_database_t *w = malloc(sizeof(linen_duckdb_database_t));
    if (!w) { if (db) duckdb_close(&db); return NULL; }
    w->db = db;
    return lean_alloc_external(g_linen_duckdb_database_class, w);
}

static inline lean_obj_res mk_duckdb_connection(duckdb_connection conn) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_connection_t *w = malloc(sizeof(linen_duckdb_connection_t));
    if (!w) { if (conn) duckdb_disconnect(&conn); return NULL; }
    w->conn = conn;
    return lean_alloc_external(g_linen_duckdb_connection_class, w);
}

static inline lean_obj_res mk_duckdb_instance_cache(duckdb_instance_cache cache) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_instance_cache_t *w = malloc(sizeof(linen_duckdb_instance_cache_t));
    if (!w) { if (cache) duckdb_destroy_instance_cache(&cache); return NULL; }
    w->cache = cache;
    return lean_alloc_external(g_linen_duckdb_instance_cache_class, w);
}

static inline lean_obj_res mk_duckdb_client_context(duckdb_client_context ctx) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_client_context_t *w = malloc(sizeof(linen_duckdb_client_context_t));
    if (!w) { if (ctx) duckdb_destroy_client_context(&ctx); return NULL; }
    w->ctx = ctx;
    return lean_alloc_external(g_linen_duckdb_client_context_class, w);
}

static inline lean_obj_res mk_duckdb_arrow_options(duckdb_arrow_options opts) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_arrow_options_t *w = malloc(sizeof(linen_duckdb_arrow_options_t));
    if (!w) { if (opts) duckdb_destroy_arrow_options(&opts); return NULL; }
    w->opts = opts;
    return lean_alloc_external(g_linen_duckdb_arrow_options_class, w);
}

static inline lean_obj_res mk_duckdb_value(duckdb_value val) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_value_t *w = malloc(sizeof(linen_duckdb_value_t));
    if (!w) { if (val) duckdb_destroy_value(&val); return NULL; }
    w->val = val;
    return lean_alloc_external(g_linen_duckdb_value_class, w);
}

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

/* `path_obj`/`query_obj` here are `Option String`: `.none` (`lean_box(0)`)
 * means "pass NULL", `.some s` means "pass s's C string". `mk_cstring_opt`
 * peeks the constructor tag without consuming a reference, matching how
 * `b_lean_obj_arg` borrowed arguments are read elsewhere in this file. */
static inline const char *unwrap_cstring_opt(b_lean_obj_arg opt) {
    if (lean_obj_tag(opt) == 0) return NULL; /* .none */
    return lean_string_cstr(lean_ctor_get(opt, 0));
}

/* Wraps a nullable `CString` that the caller must release with
 * `duckdb_free` (e.g. `duckdb_logical_type_get_alias`,
 * `duckdb_enum_dictionary_value`, `duckdb_struct_type_child_name`,
 * `duckdb_union_type_member_name`, `duckdb_parameter_name`,
 * `duckdb_prepared_statement_column_name`) into `Option String`, copying the
 * bytes into a fresh Lean string and freeing the native one. */
static inline lean_obj_res mk_string_opt_owned_free(char *s) {
    if (!s) return mk_option_none();
    lean_obj_res str = mk_string_or_empty(s);
    duckdb_free(s);
    return mk_option_some(str);
}

/* Wraps a nullable `CString` owned by something else (e.g. a `duckdb_result`
 * or `duckdb_prepared_statement`, per `duckdb_column_name`'s/
 * `duckdb_prepare_error`'s own doc comments) into `Option String`, without
 * freeing it. */
static inline lean_obj_res mk_string_opt_borrowed(const char *s) {
    if (!s) return mk_option_none();
    return mk_option_some(mk_string_or_empty(s));
}

static inline lean_obj_res mk_duckdb_appender(duckdb_appender app) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_appender_t *w = malloc(sizeof(linen_duckdb_appender_t));
    if (!w) { if (app) duckdb_appender_destroy(&app); return NULL; }
    w->app = app;
    return lean_alloc_external(g_linen_duckdb_appender_class, w);
}

static inline lean_obj_res mk_duckdb_prepared_statement(duckdb_prepared_statement stmt) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_prepared_statement_t *w = malloc(sizeof(linen_duckdb_prepared_statement_t));
    if (!w) { if (stmt) duckdb_destroy_prepare(&stmt); return NULL; }
    w->stmt = stmt;
    return lean_alloc_external(g_linen_duckdb_prepared_statement_class, w);
}

static inline lean_obj_res mk_duckdb_data_chunk(duckdb_data_chunk chunk) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_data_chunk_t *w = malloc(sizeof(linen_duckdb_data_chunk_t));
    if (!w) { if (chunk) duckdb_destroy_data_chunk(&chunk); return NULL; }
    w->chunk = chunk;
    return lean_alloc_external(g_linen_duckdb_data_chunk_class, w);
}

static inline lean_obj_res mk_duckdb_vector(duckdb_vector vec) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_vector_t *w = malloc(sizeof(linen_duckdb_vector_t));
    if (!w) return NULL; /* non-owning: nothing to release on malloc failure */
    w->vec = vec;
    return lean_alloc_external(g_linen_duckdb_vector_class, w);
}

static inline lean_obj_res mk_duckdb_logical_type(duckdb_logical_type type) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_logical_type_t *w = malloc(sizeof(linen_duckdb_logical_type_t));
    if (!w) { if (type) duckdb_destroy_logical_type(&type); return NULL; }
    w->type = type;
    return lean_alloc_external(g_linen_duckdb_logical_type_class, w);
}

static inline lean_obj_res mk_duckdb_config(duckdb_config config) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_config_t *w = malloc(sizeof(linen_duckdb_config_t));
    if (!w) { if (config) duckdb_destroy_config(&config); return NULL; }
    w->config = config;
    return lean_alloc_external(g_linen_duckdb_config_class, w);
}

static inline lean_obj_res mk_duckdb_config_option(duckdb_config_option option) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_config_option_t *w = malloc(sizeof(linen_duckdb_config_option_t));
    if (!w) { if (option) duckdb_destroy_config_option(&option); return NULL; }
    w->option = option;
    return lean_alloc_external(g_linen_duckdb_config_option_class, w);
}

static inline lean_obj_res mk_duckdb_error_data(duckdb_error_data err) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_error_data_t *w = malloc(sizeof(linen_duckdb_error_data_t));
    if (!w) { if (err) duckdb_destroy_error_data(&err); return NULL; }
    w->err = err;
    return lean_alloc_external(g_linen_duckdb_error_data_class, w);
}

static inline lean_obj_res mk_duckdb_catalog(duckdb_catalog catalog) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_catalog_t *w = malloc(sizeof(linen_duckdb_catalog_t));
    if (!w) { if (catalog) duckdb_destroy_catalog(&catalog); return NULL; }
    w->catalog = catalog;
    return lean_alloc_external(g_linen_duckdb_catalog_class, w);
}

static inline lean_obj_res mk_duckdb_catalog_entry(duckdb_catalog_entry entry) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_catalog_entry_t *w = malloc(sizeof(linen_duckdb_catalog_entry_t));
    if (!w) { if (entry) duckdb_destroy_catalog_entry(&entry); return NULL; }
    w->entry = entry;
    return lean_alloc_external(g_linen_duckdb_catalog_entry_class, w);
}

static inline lean_obj_res mk_duckdb_result_wrapper(void) {
    /* Caller fills in ->result via `duckdb_execute_prepared(..., &w->result)`
     * before this object escapes; always succeeds in creating the wrapper
     * itself (malloc failure aside), since `duckdb_execute_prepared`
     * populates its `out_result` even on failure (see
     * `ExecutePrepared.lean`'s doc comment). */
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_result_t *w = malloc(sizeof(linen_duckdb_result_t));
    if (!w) return NULL;
    memset(&w->result, 0, sizeof(w->result));
    return lean_alloc_external(g_linen_duckdb_result_class, w);
}

static inline lean_obj_res mk_duckdb_file_system(duckdb_file_system fs) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_file_system_t *w = malloc(sizeof(linen_duckdb_file_system_t));
    if (!w) { if (fs) duckdb_destroy_file_system(&fs); return NULL; }
    w->fs = fs;
    return lean_alloc_external(g_linen_duckdb_file_system_class, w);
}

static inline lean_obj_res mk_duckdb_file_open_options(duckdb_file_open_options opts) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_file_open_options_t *w = malloc(sizeof(linen_duckdb_file_open_options_t));
    if (!w) { if (opts) duckdb_destroy_file_open_options(&opts); return NULL; }
    w->opts = opts;
    return lean_alloc_external(g_linen_duckdb_file_open_options_class, w);
}

static inline lean_obj_res mk_duckdb_file_handle(duckdb_file_handle handle) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_file_handle_t *w = malloc(sizeof(linen_duckdb_file_handle_t));
    if (!w) { if (handle) duckdb_destroy_file_handle(&handle); return NULL; }
    w->handle = handle;
    return lean_alloc_external(g_linen_duckdb_file_handle_class, w);
}

static inline lean_obj_res mk_duckdb_log_storage(duckdb_log_storage storage) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_log_storage_t *w = malloc(sizeof(linen_duckdb_log_storage_t));
    if (!w) { if (storage) duckdb_destroy_log_storage(&storage); return NULL; }
    w->storage = storage;
    return lean_alloc_external(g_linen_duckdb_log_storage_class, w);
}

static inline lean_obj_res mk_duckdb_raw_memory(void *ptr) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_raw_memory_t *w = malloc(sizeof(linen_duckdb_raw_memory_t));
    if (!w) { if (ptr) duckdb_free(ptr); return NULL; }
    w->ptr = ptr;
    return lean_alloc_external(g_linen_duckdb_raw_memory_class, w);
}

static inline lean_obj_res mk_duckdb_scalar_function(duckdb_scalar_function fn) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_scalar_function_t *w = malloc(sizeof(linen_duckdb_scalar_function_t));
    if (!w) { if (fn) duckdb_destroy_scalar_function(&fn); return NULL; }
    w->fn = fn;
    return lean_alloc_external(g_linen_duckdb_scalar_function_class, w);
}

static inline lean_obj_res mk_duckdb_scalar_function_set(duckdb_scalar_function_set set) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_scalar_function_set_t *w = malloc(sizeof(linen_duckdb_scalar_function_set_t));
    if (!w) { if (set) duckdb_destroy_scalar_function_set(&set); return NULL; }
    w->set = set;
    return lean_alloc_external(g_linen_duckdb_scalar_function_set_class, w);
}

static inline lean_obj_res mk_duckdb_borrowed_data_chunk(duckdb_data_chunk chunk) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_borrowed_data_chunk_t *w = malloc(sizeof(linen_duckdb_borrowed_data_chunk_t));
    if (!w) return NULL; /* non-owning: nothing to release on malloc failure */
    w->chunk = chunk;
    return lean_alloc_external(g_linen_duckdb_borrowed_data_chunk_class, w);
}

static inline lean_obj_res mk_duckdb_validity_mask(uint64_t *mask) {
    linen_duckdb_ensure_classes_initialized();
    linen_duckdb_validity_mask_t *w = malloc(sizeof(linen_duckdb_validity_mask_t));
    if (!w) return NULL; /* non-owning: nothing to release on malloc failure */
    w->mask = mask;
    return lean_alloc_external(g_linen_duckdb_validity_mask_class, w);
}

/* Build a transient `duckdb_logical_type[]` from a Lean `Array LogicalType`.
 * Every element borrows its wrapped pointer from `arr_obj` (kept alive by
 * the caller for the duration of the call); the returned C array itself
 * must be `free`d by the caller (it never owns the individual logical
 * types). Returns NULL for an empty array (a valid zero-column case, per
 * `duckdb.h`'s own doc comment on `duckdb_create_data_chunk`). */
static duckdb_logical_type *build_logical_type_array(b_lean_obj_arg arr_obj, size_t *out_count) {
    size_t n = lean_array_size(arr_obj);
    *out_count = n;
    if (n == 0) return NULL;
    duckdb_logical_type *types = (duckdb_logical_type *)calloc(n, sizeof(duckdb_logical_type));
    if (!types) return NULL;
    for (size_t i = 0; i < n; i++) {
        lean_object *elem = lean_array_get_core(arr_obj, i);
        linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(elem);
        types[i] = tw->type;
    }
    return types;
}

/* Build a transient, NULL-terminated `const char *[]` from a Lean
 * `Array String`, for `duckdb_appender_create_query`'s `column_names`
 * parameter. Every pointer borrows from `arr_obj`; the returned array
 * itself must be `free`d by the caller. Returns NULL for an empty array. */
static const char **build_cstring_array(b_lean_obj_arg arr_obj, size_t *out_count) {
    size_t n = lean_array_size(arr_obj);
    *out_count = n;
    if (n == 0) return NULL;
    const char **strs = (const char **)calloc(n, sizeof(const char *));
    if (!strs) return NULL;
    for (size_t i = 0; i < n; i++) {
        strs[i] = lean_string_cstr(lean_array_get_core(arr_obj, i));
    }
    return strs;
}

/* ================================================================
 * INSTANCE CACHE
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_instance_cache"]
 * opaque createInstanceCache : IO InstanceCache
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_instance_cache(lean_obj_arg world) {
    duckdb_instance_cache cache = duckdb_create_instance_cache();
    lean_obj_res obj = mk_duckdb_instance_cache(cache);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_instance_cache wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_get_or_create_from_cache"]
 * opaque getOrCreateFromCacheRaw :
 *   @& InstanceCache -> @& Option String -> IO (UInt32 x (Option Database x Option String))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_get_or_create_from_cache(
    b_lean_obj_arg cache_obj,
    b_lean_obj_arg path_obj,
    lean_obj_arg world
) {
    linen_duckdb_instance_cache_t *cw = (linen_duckdb_instance_cache_t *)lean_get_external_data(cache_obj);
    const char *path = unwrap_cstring_opt(path_obj);
    duckdb_database db = NULL;
    char *err = NULL;
    duckdb_state rc = duckdb_get_or_create_from_cache(cw->cache, path, &db, NULL, &err);

    lean_obj_res dbOpt = db ? mk_option_some(mk_duckdb_database(db)) : mk_option_none();
    lean_obj_res errOpt = err ? mk_option_some(mk_string_or_empty(err)) : mk_option_none();
    if (err) duckdb_free(err);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), mk_pair(dbOpt, errOpt)));
}

/*
 * @[extern "linen_duckdb_destroy_instance_cache"]
 * opaque destroyInstanceCache : InstanceCache -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_instance_cache(
    b_lean_obj_arg cache_obj,
    lean_obj_arg world
) {
    linen_duckdb_instance_cache_t *cw = (linen_duckdb_instance_cache_t *)lean_get_external_data(cache_obj);
    if (cw->cache) {
        duckdb_destroy_instance_cache(&cw->cache);
        cw->cache = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * OPEN / CONNECT / CLOSE / DISCONNECT
 * ================================================================ */

/*
 * @[extern "linen_duckdb_open"]
 * opaque openRaw : @& Option String -> IO (UInt32 x Option Database)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_open(
    b_lean_obj_arg path_obj,
    lean_obj_arg world
) {
    const char *path = unwrap_cstring_opt(path_obj);
    duckdb_database db = NULL;
    duckdb_state rc = duckdb_open(path, &db);
    lean_obj_res dbOpt = db ? mk_option_some(mk_duckdb_database(db)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), dbOpt));
}

/*
 * @[extern "linen_duckdb_open_ext"]
 * opaque openExtRaw : @& Option String -> IO (UInt32 x (Option Database x Option String))
 *
 * Always passes a NULL `duckdb_config` — see the file header for why.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_open_ext(
    b_lean_obj_arg path_obj,
    lean_obj_arg world
) {
    const char *path = unwrap_cstring_opt(path_obj);
    duckdb_database db = NULL;
    char *err = NULL;
    duckdb_state rc = duckdb_open_ext(path, &db, NULL, &err);
    lean_obj_res dbOpt = db ? mk_option_some(mk_duckdb_database(db)) : mk_option_none();
    lean_obj_res errOpt = err ? mk_option_some(mk_string_or_empty(err)) : mk_option_none();
    if (err) duckdb_free(err);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), mk_pair(dbOpt, errOpt)));
}

/*
 * @[extern "linen_duckdb_close"]
 * opaque close : Database -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_close(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    linen_duckdb_database_t *dw = (linen_duckdb_database_t *)lean_get_external_data(db_obj);
    if (dw->db) {
        duckdb_close(&dw->db);
        dw->db = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_connect"]
 * opaque connectRaw : @& Database -> IO (UInt32 x Option Connection)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_connect(
    b_lean_obj_arg db_obj,
    lean_obj_arg world
) {
    linen_duckdb_database_t *dw = (linen_duckdb_database_t *)lean_get_external_data(db_obj);
    duckdb_connection conn = NULL;
    duckdb_state rc = duckdb_connect(dw->db, &conn);
    lean_obj_res connOpt = conn ? mk_option_some(mk_duckdb_connection(conn)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), connOpt));
}

/*
 * @[extern "linen_duckdb_interrupt"]
 * opaque interrupt : @& Connection -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_interrupt(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    if (cw->conn) duckdb_interrupt(cw->conn);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_query_progress"]
 * opaque queryProgressRaw : @& Connection -> IO (Float x (UInt64 x UInt64))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_query_progress(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    duckdb_query_progress_type progress = { .percentage = -1.0, .rows_processed = 0, .total_rows_to_process = 0 };
    if (cw->conn) progress = duckdb_query_progress(cw->conn);
    lean_obj_res rows = mk_pair(
        lean_box_uint64((uint64_t)progress.rows_processed),
        lean_box_uint64((uint64_t)progress.total_rows_to_process));
    return lean_io_result_mk_ok(mk_pair(lean_box_float(progress.percentage), rows));
}

/*
 * @[extern "linen_duckdb_disconnect"]
 * opaque disconnect : Connection -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_disconnect(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    if (cw->conn) {
        duckdb_disconnect(&cw->conn);
        cw->conn = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * CLIENT CONTEXT / ARROW OPTIONS
 * ================================================================ */

/*
 * @[extern "linen_duckdb_connection_get_client_context"]
 * opaque connectionGetClientContext : @& Connection -> IO ClientContext
 */
LEAN_EXPORT lean_obj_res linen_duckdb_connection_get_client_context(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    duckdb_client_context ctx = NULL;
    duckdb_connection_get_client_context(cw->conn, &ctx);
    lean_obj_res obj = mk_duckdb_client_context(ctx);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_client_context wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_connection_get_arrow_options"]
 * opaque connectionGetArrowOptions : @& Connection -> IO ArrowOptions
 */
LEAN_EXPORT lean_obj_res linen_duckdb_connection_get_arrow_options(
    b_lean_obj_arg conn_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    duckdb_arrow_options opts = NULL;
    duckdb_connection_get_arrow_options(cw->conn, &opts);
    lean_obj_res obj = mk_duckdb_arrow_options(opts);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_arrow_options wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_client_context_get_connection_id"]
 * opaque clientContextGetConnectionId : @& ClientContext -> IO UInt64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_client_context_get_connection_id(
    b_lean_obj_arg ctx_obj,
    lean_obj_arg world
) {
    linen_duckdb_client_context_t *cw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    idx_t connId = cw->ctx ? duckdb_client_context_get_connection_id(cw->ctx) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)connId));
}

/*
 * @[extern "linen_duckdb_destroy_client_context"]
 * opaque destroyClientContext : ClientContext -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_client_context(
    b_lean_obj_arg ctx_obj,
    lean_obj_arg world
) {
    linen_duckdb_client_context_t *cw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    if (cw->ctx) {
        duckdb_destroy_client_context(&cw->ctx);
        cw->ctx = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_destroy_arrow_options"]
 * opaque destroyArrowOptions : ArrowOptions -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_arrow_options(
    b_lean_obj_arg opts_obj,
    lean_obj_arg world
) {
    linen_duckdb_arrow_options_t *ow = (linen_duckdb_arrow_options_t *)lean_get_external_data(opts_obj);
    if (ow->opts) {
        duckdb_destroy_arrow_options(&ow->opts);
        ow->opts = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * MISC
 * ================================================================ */

/*
 * @[extern "linen_duckdb_library_version"]
 * opaque libraryVersion : IO String
 */
LEAN_EXPORT lean_obj_res linen_duckdb_library_version(lean_obj_arg world) {
    const char *v = duckdb_library_version();
    return lean_io_result_mk_ok(mk_string_or_empty(v));
}

/*
 * @[extern "linen_duckdb_get_table_names"]
 * opaque getTableNamesRaw : @& Connection -> @& String -> UInt8 -> IO Value
 */
LEAN_EXPORT lean_obj_res linen_duckdb_get_table_names(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg query_obj,
    uint8_t qualified,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    duckdb_value val = duckdb_get_table_names(cw->conn, query, (bool)qualified);
    lean_obj_res obj = mk_duckdb_value(val);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_value wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/* ================================================================
 * APPENDER
 * ================================================================ */

/*
 * @[extern "linen_duckdb_appender_create"]
 * opaque createRaw : @& Connection -> @& Option String -> @& String -> IO (UInt32 x Option Appender)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_create(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg schema_obj,
    b_lean_obj_arg table_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *schema = unwrap_cstring_opt(schema_obj);
    const char *table = lean_string_cstr(table_obj);
    duckdb_appender app = NULL;
    duckdb_state rc = duckdb_appender_create(cw->conn, schema, table, &app);
    lean_obj_res appOpt = app ? mk_option_some(mk_duckdb_appender(app)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), appOpt));
}

/*
 * @[extern "linen_duckdb_appender_create_ext"]
 * opaque createExtRaw :
 *   @& Connection -> @& Option String -> @& Option String -> @& String ->
 *     IO (UInt32 x Option Appender)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_create_ext(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg catalog_obj,
    b_lean_obj_arg schema_obj,
    b_lean_obj_arg table_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *catalog = unwrap_cstring_opt(catalog_obj);
    const char *schema = unwrap_cstring_opt(schema_obj);
    const char *table = lean_string_cstr(table_obj);
    duckdb_appender app = NULL;
    duckdb_state rc = duckdb_appender_create_ext(cw->conn, catalog, schema, table, &app);
    lean_obj_res appOpt = app ? mk_option_some(mk_duckdb_appender(app)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), appOpt));
}

/*
 * @[extern "linen_duckdb_appender_create_query"]
 * opaque createQueryRaw :
 *   @& Connection -> @& String -> @& Array LogicalType -> @& Option String ->
 *     @& Option (Array String) -> IO (UInt32 x Option Appender)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_create_query(
    b_lean_obj_arg conn_obj,
    b_lean_obj_arg query_obj,
    b_lean_obj_arg types_obj,
    b_lean_obj_arg table_name_obj,
    b_lean_obj_arg column_names_obj,
    lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    size_t type_count = 0;
    duckdb_logical_type *types = build_logical_type_array(types_obj, &type_count);
    const char *table_name = unwrap_cstring_opt(table_name_obj);
    const char **column_names = NULL;
    if (lean_obj_tag(column_names_obj) != 0) { /* .some names */
        size_t name_count = 0;
        column_names = build_cstring_array(lean_ctor_get(column_names_obj, 0), &name_count);
    }
    duckdb_appender app = NULL;
    duckdb_state rc = duckdb_appender_create_query(
        cw->conn, query, (idx_t)type_count, types, table_name, column_names, &app);
    if (types) free(types);
    if (column_names) free(column_names);
    lean_obj_res appOpt = app ? mk_option_some(mk_duckdb_appender(app)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), appOpt));
}

/*
 * @[extern "linen_duckdb_appender_column_count"]
 * opaque columnCount : Appender -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_column_count(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    idx_t n = aw->app ? duckdb_appender_column_count(aw->app) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_appender_column_type"]
 * opaque columnType : @& Appender -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_column_type(
    b_lean_obj_arg app_obj,
    uint64_t col_idx,
    lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_logical_type type = duckdb_appender_column_type(aw->app, (idx_t)col_idx);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_appender_error_data"]
 * opaque errorData : Appender -> IO ErrorData
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_error_data(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_error_data err = duckdb_appender_error_data(aw->app);
    lean_obj_res obj = mk_duckdb_error_data(err);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_error_data wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_appender_clear"]
 * opaque clearRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_clear(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = aw->app ? duckdb_appender_clear(aw->app) : DuckDBError;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_flush"]
 * opaque flushRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_flush(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = aw->app ? duckdb_appender_flush(aw->app) : DuckDBError;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_close"]
 * opaque closeRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_close(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = aw->app ? duckdb_appender_close(aw->app) : DuckDBError;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_destroy"]
 * opaque destroy : Appender -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_destroy(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    if (aw->app) {
        duckdb_appender_destroy(&aw->app);
        aw->app = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_appender_add_column"]
 * opaque addColumnRaw : @& Appender -> @& String -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_add_column(
    b_lean_obj_arg app_obj,
    b_lean_obj_arg name_obj,
    lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_state rc = duckdb_appender_add_column(aw->app, name);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_clear_columns"]
 * opaque clearColumnsRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_clear_columns(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_appender_clear_columns(aw->app);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_begin_row"]
 * opaque beginRowRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_begin_row(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_appender_begin_row(aw->app);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_appender_end_row"]
 * opaque endRowRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_appender_end_row(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_appender_end_row(aw->app);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_default"]
 * opaque appendDefaultRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_default(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_default(aw->app);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_default_to_chunk"]
 * opaque appendDefaultToChunkRaw : @& Appender -> @& DataChunk -> Idx -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_default_to_chunk(
    b_lean_obj_arg app_obj,
    b_lean_obj_arg chunk_obj,
    uint64_t col,
    uint64_t row,
    lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    duckdb_state rc = duckdb_append_default_to_chunk(aw->app, cw->chunk, (idx_t)col, (idx_t)row);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_null"]
 * opaque appendNullRaw : Appender -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_null(b_lean_obj_arg app_obj, lean_obj_arg world) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_null(aw->app);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_value"]
 * opaque appendValueRaw : @& Appender -> @& Value -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_value(
    b_lean_obj_arg app_obj,
    b_lean_obj_arg value_obj,
    lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    linen_duckdb_value_t *vw = (linen_duckdb_value_t *)lean_get_external_data(value_obj);
    duckdb_state rc = duckdb_append_value(aw->app, vw->val);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_data_chunk"]
 * opaque appendDataChunkRaw : @& Appender -> @& DataChunk -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_data_chunk(
    b_lean_obj_arg app_obj,
    b_lean_obj_arg chunk_obj,
    lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    duckdb_state rc = duckdb_append_data_chunk(aw->app, cw->chunk);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_bool"]
 * opaque appendBoolRaw : @& Appender -> UInt8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_bool(
    b_lean_obj_arg app_obj, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_bool(aw->app, (bool)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_int8"]
 * opaque appendInt8Raw : @& Appender -> Int8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_int8(
    b_lean_obj_arg app_obj, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_int8(aw->app, (int8_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_int16"]
 * opaque appendInt16Raw : @& Appender -> Int16 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_int16(
    b_lean_obj_arg app_obj, uint16_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_int16(aw->app, (int16_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_int32"]
 * opaque appendInt32Raw : @& Appender -> Int32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_int32(
    b_lean_obj_arg app_obj, uint32_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_int32(aw->app, (int32_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_int64"]
 * opaque appendInt64Raw : @& Appender -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_int64(
    b_lean_obj_arg app_obj, uint64_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_int64(aw->app, (int64_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_hugeint"]
 * opaque appendHugeIntRaw : @& Appender -> UInt64 -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_hugeint(
    b_lean_obj_arg app_obj, uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_hugeint value = { .lower = lower, .upper = (int64_t)upper };
    duckdb_state rc = duckdb_append_hugeint(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_uint8"]
 * opaque appendUInt8Raw : @& Appender -> UInt8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_uint8(
    b_lean_obj_arg app_obj, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_uint8(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_uint16"]
 * opaque appendUInt16Raw : @& Appender -> UInt16 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_uint16(
    b_lean_obj_arg app_obj, uint16_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_uint16(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_uint32"]
 * opaque appendUInt32Raw : @& Appender -> UInt32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_uint32(
    b_lean_obj_arg app_obj, uint32_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_uint32(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_uint64"]
 * opaque appendUInt64Raw : @& Appender -> UInt64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_uint64(
    b_lean_obj_arg app_obj, uint64_t value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_uint64(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_uhugeint"]
 * opaque appendUHugeIntRaw : @& Appender -> UInt64 -> UInt64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_uhugeint(
    b_lean_obj_arg app_obj, uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_uhugeint value = { .lower = lower, .upper = upper };
    duckdb_state rc = duckdb_append_uhugeint(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_float"]
 * opaque appendFloatRaw : @& Appender -> Float32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_float(
    b_lean_obj_arg app_obj, float value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_float(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_double"]
 * opaque appendDoubleRaw : @& Appender -> Float -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_double(
    b_lean_obj_arg app_obj, double value, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_state rc = duckdb_append_double(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_date"]
 * opaque appendDateRaw : @& Appender -> @& Date -> IO UInt32
 *
 * `Date` is a single-field structure (`{ days : Int32 }`), which Lean's ABI
 * unboxes to a plain `uint32_t` scalar argument regardless of `@&` — see
 * the module doc comments in `Appender.lean`/`BindValues.lean` for the
 * experiment that established this. Same treatment for `Time`/`Timestamp`
 * below.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_date(
    b_lean_obj_arg app_obj, uint32_t days, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_date value = { .days = (int32_t)days };
    duckdb_state rc = duckdb_append_date(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_time"]
 * opaque appendTimeRaw : @& Appender -> @& Time -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_time(
    b_lean_obj_arg app_obj, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_time value = { .micros = (int64_t)micros };
    duckdb_state rc = duckdb_append_time(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_timestamp"]
 * opaque appendTimestampRaw : @& Appender -> @& Timestamp -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_timestamp(
    b_lean_obj_arg app_obj, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_timestamp value = { .micros = (int64_t)micros };
    duckdb_state rc = duckdb_append_timestamp(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_interval"]
 * opaque appendIntervalRaw : @& Appender -> Int32 -> Int32 -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_interval(
    b_lean_obj_arg app_obj, uint32_t months, uint32_t days, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    duckdb_interval value = { .months = (int32_t)months, .days = (int32_t)days, .micros = (int64_t)micros };
    duckdb_state rc = duckdb_append_interval(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_varchar"]
 * opaque appendVarcharRaw : @& Appender -> @& String -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_varchar(
    b_lean_obj_arg app_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    const char *value = lean_string_cstr(value_obj);
    duckdb_state rc = duckdb_append_varchar(aw->app, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_varchar_length"]
 * opaque appendVarcharLengthRaw : @& Appender -> @& String -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_varchar_length(
    b_lean_obj_arg app_obj, b_lean_obj_arg value_obj, uint64_t length, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    const char *value = lean_string_cstr(value_obj);
    duckdb_state rc = duckdb_append_varchar_length(aw->app, value, (idx_t)length);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_append_blob"]
 * opaque appendBlobRaw : @& Appender -> @& ByteArray -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_append_blob(
    b_lean_obj_arg app_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_appender_t *aw = (linen_duckdb_appender_t *)lean_get_external_data(app_obj);
    const uint8_t *data = lean_sarray_cptr(value_obj);
    size_t len = lean_sarray_size(value_obj);
    duckdb_state rc = duckdb_append_blob(aw->app, data, (idx_t)len);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * BIND VALUES
 * ================================================================ */

/*
 * @[extern "linen_duckdb_bind_value"]
 * opaque bindValueRaw : @& PreparedStatement -> Idx -> @& Value -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_value(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    linen_duckdb_value_t *vw = (linen_duckdb_value_t *)lean_get_external_data(value_obj);
    duckdb_state rc = duckdb_bind_value(sw->stmt, (idx_t)param_idx, vw->val);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_parameter_index"]
 * opaque bindParameterIndexRaw : @& PreparedStatement -> @& String -> IO (UInt32 x Option Idx)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_parameter_index(
    b_lean_obj_arg stmt_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    const char *name = lean_string_cstr(name_obj);
    idx_t param_idx = 0;
    duckdb_state rc = duckdb_bind_parameter_index(sw->stmt, &param_idx, name);
    lean_obj_res idxOpt = (rc == DuckDBSuccess) ? mk_option_some(lean_box_uint64((uint64_t)param_idx)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), idxOpt));
}

/*
 * @[extern "linen_duckdb_bind_boolean"]
 * opaque bindBooleanRaw : @& PreparedStatement -> Idx -> UInt8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_boolean(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_boolean(sw->stmt, (idx_t)param_idx, (bool)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_int8"]
 * opaque bindInt8Raw : @& PreparedStatement -> Idx -> Int8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_int8(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_int8(sw->stmt, (idx_t)param_idx, (int8_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_int16"]
 * opaque bindInt16Raw : @& PreparedStatement -> Idx -> Int16 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_int16(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint16_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_int16(sw->stmt, (idx_t)param_idx, (int16_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_int32"]
 * opaque bindInt32Raw : @& PreparedStatement -> Idx -> Int32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_int32(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint32_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_int32(sw->stmt, (idx_t)param_idx, (int32_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_int64"]
 * opaque bindInt64Raw : @& PreparedStatement -> Idx -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_int64(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_int64(sw->stmt, (idx_t)param_idx, (int64_t)value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_hugeint"]
 * opaque bindHugeIntRaw : @& PreparedStatement -> Idx -> UInt64 -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_hugeint(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_hugeint value = { .lower = lower, .upper = (int64_t)upper };
    duckdb_state rc = duckdb_bind_hugeint(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_uhugeint"]
 * opaque bindUHugeIntRaw : @& PreparedStatement -> Idx -> UInt64 -> UInt64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_uhugeint(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_uhugeint value = { .lower = lower, .upper = upper };
    duckdb_state rc = duckdb_bind_uhugeint(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_decimal"]
 * opaque bindDecimalRaw : @& PreparedStatement -> Idx -> UInt8 -> UInt8 -> UInt64 -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_decimal(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint8_t width, uint8_t scale,
    uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_decimal value = { .width = width, .scale = scale,
                              .value = { .lower = lower, .upper = (int64_t)upper } };
    duckdb_state rc = duckdb_bind_decimal(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_uint8"]
 * opaque bindUInt8Raw : @& PreparedStatement -> Idx -> UInt8 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_uint8(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_uint8(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_uint16"]
 * opaque bindUInt16Raw : @& PreparedStatement -> Idx -> UInt16 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_uint16(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint16_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_uint16(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_uint32"]
 * opaque bindUInt32Raw : @& PreparedStatement -> Idx -> UInt32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_uint32(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint32_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_uint32(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_uint64"]
 * opaque bindUInt64Raw : @& PreparedStatement -> Idx -> UInt64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_uint64(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_uint64(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_float"]
 * opaque bindFloatRaw : @& PreparedStatement -> Idx -> Float32 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_float(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, float value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_float(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_double"]
 * opaque bindDoubleRaw : @& PreparedStatement -> Idx -> Float -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_double(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, double value, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_double(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_date"]
 * opaque bindDateRaw : @& PreparedStatement -> Idx -> @& Date -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_date(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint32_t days, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_date value = { .days = (int32_t)days };
    duckdb_state rc = duckdb_bind_date(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_time"]
 * opaque bindTimeRaw : @& PreparedStatement -> Idx -> @& Time -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_time(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_time value = { .micros = (int64_t)micros };
    duckdb_state rc = duckdb_bind_time(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_timestamp"]
 * opaque bindTimestampRaw : @& PreparedStatement -> Idx -> @& Timestamp -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_timestamp(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_timestamp value = { .micros = (int64_t)micros };
    duckdb_state rc = duckdb_bind_timestamp(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_timestamp_tz"]
 * opaque bindTimestampTzRaw : @& PreparedStatement -> Idx -> @& Timestamp -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_timestamp_tz(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint64_t micros, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_timestamp value = { .micros = (int64_t)micros };
    duckdb_state rc = duckdb_bind_timestamp_tz(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_interval"]
 * opaque bindIntervalRaw : @& PreparedStatement -> Idx -> Int32 -> Int32 -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_interval(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, uint32_t months, uint32_t days, uint64_t micros,
    lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_interval value = { .months = (int32_t)months, .days = (int32_t)days, .micros = (int64_t)micros };
    duckdb_state rc = duckdb_bind_interval(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_varchar"]
 * opaque bindVarcharRaw : @& PreparedStatement -> Idx -> @& String -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_varchar(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    const char *value = lean_string_cstr(value_obj);
    duckdb_state rc = duckdb_bind_varchar(sw->stmt, (idx_t)param_idx, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_varchar_length"]
 * opaque bindVarcharLengthRaw : @& PreparedStatement -> Idx -> @& String -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_varchar_length(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, b_lean_obj_arg value_obj, uint64_t length,
    lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    const char *value = lean_string_cstr(value_obj);
    duckdb_state rc = duckdb_bind_varchar_length(sw->stmt, (idx_t)param_idx, value, (idx_t)length);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_blob"]
 * opaque bindBlobRaw : @& PreparedStatement -> Idx -> @& ByteArray -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_blob(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    const uint8_t *data = lean_sarray_cptr(value_obj);
    size_t len = lean_sarray_size(value_obj);
    duckdb_state rc = duckdb_bind_blob(sw->stmt, (idx_t)param_idx, data, (idx_t)len);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_bind_null"]
 * opaque bindNullRaw : @& PreparedStatement -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_bind_null(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_bind_null(sw->stmt, (idx_t)param_idx);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * CATALOG
 * ================================================================ */

/*
 * @[extern "linen_duckdb_client_context_get_catalog"]
 * opaque clientContextGetCatalogRaw : @& ClientContext -> @& String -> IO (Option Catalog)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_client_context_get_catalog(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_client_context_t *cw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_catalog catalog = duckdb_client_context_get_catalog(cw->ctx, name);
    lean_obj_res catOpt = catalog ? mk_option_some(mk_duckdb_catalog(catalog)) : mk_option_none();
    return lean_io_result_mk_ok(catOpt);
}

/*
 * @[extern "linen_duckdb_catalog_get_type_name"]
 * opaque catalogGetTypeName : Catalog -> IO String
 */
LEAN_EXPORT lean_obj_res linen_duckdb_catalog_get_type_name(b_lean_obj_arg cat_obj, lean_obj_arg world) {
    linen_duckdb_catalog_t *cw = (linen_duckdb_catalog_t *)lean_get_external_data(cat_obj);
    const char *name = cw->catalog ? duckdb_catalog_get_type_name(cw->catalog) : NULL;
    return lean_io_result_mk_ok(mk_string_or_empty(name));
}

/*
 * @[extern "linen_duckdb_catalog_get_entry"]
 * opaque catalogGetEntryRaw :
 *   @& Catalog -> @& ClientContext -> UInt32 -> @& String -> @& String -> IO (Option CatalogEntry)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_catalog_get_entry(
    b_lean_obj_arg cat_obj, b_lean_obj_arg ctx_obj, uint32_t entry_type,
    b_lean_obj_arg schema_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_catalog_t *cw = (linen_duckdb_catalog_t *)lean_get_external_data(cat_obj);
    linen_duckdb_client_context_t *xw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    const char *schema = lean_string_cstr(schema_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_catalog_entry entry = duckdb_catalog_get_entry(
        cw->catalog, xw->ctx, (duckdb_catalog_entry_type)entry_type, schema, name);
    lean_obj_res entryOpt = entry ? mk_option_some(mk_duckdb_catalog_entry(entry)) : mk_option_none();
    return lean_io_result_mk_ok(entryOpt);
}

/*
 * @[extern "linen_duckdb_destroy_catalog"]
 * opaque destroyCatalog : Catalog -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_catalog(b_lean_obj_arg cat_obj, lean_obj_arg world) {
    linen_duckdb_catalog_t *cw = (linen_duckdb_catalog_t *)lean_get_external_data(cat_obj);
    if (cw->catalog) {
        duckdb_destroy_catalog(&cw->catalog);
        cw->catalog = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_catalog_entry_get_type"]
 * opaque catalogEntryGetTypeRaw : CatalogEntry -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_catalog_entry_get_type(b_lean_obj_arg entry_obj, lean_obj_arg world) {
    linen_duckdb_catalog_entry_t *ew = (linen_duckdb_catalog_entry_t *)lean_get_external_data(entry_obj);
    duckdb_catalog_entry_type t = ew->entry ? duckdb_catalog_entry_get_type(ew->entry) : DUCKDB_CATALOG_ENTRY_TYPE_INVALID;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)t));
}

/*
 * @[extern "linen_duckdb_catalog_entry_get_name"]
 * opaque catalogEntryGetName : CatalogEntry -> IO String
 */
LEAN_EXPORT lean_obj_res linen_duckdb_catalog_entry_get_name(b_lean_obj_arg entry_obj, lean_obj_arg world) {
    linen_duckdb_catalog_entry_t *ew = (linen_duckdb_catalog_entry_t *)lean_get_external_data(entry_obj);
    const char *name = ew->entry ? duckdb_catalog_entry_get_name(ew->entry) : NULL;
    return lean_io_result_mk_ok(mk_string_or_empty(name));
}

/*
 * @[extern "linen_duckdb_destroy_catalog_entry"]
 * opaque destroyCatalogEntry : CatalogEntry -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_catalog_entry(b_lean_obj_arg entry_obj, lean_obj_arg world) {
    linen_duckdb_catalog_entry_t *ew = (linen_duckdb_catalog_entry_t *)lean_get_external_data(entry_obj);
    if (ew->entry) {
        duckdb_destroy_catalog_entry(&ew->entry);
        ew->entry = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * CONFIGURATION
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_config"]
 * opaque createConfigRaw : IO (UInt32 x Option Config)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_config(lean_obj_arg world) {
    duckdb_config config = NULL;
    duckdb_state rc = duckdb_create_config(&config);
    lean_obj_res cfgOpt = config ? mk_option_some(mk_duckdb_config(config)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), cfgOpt));
}

/*
 * @[extern "linen_duckdb_config_count"]
 * opaque configCount : IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_count(lean_obj_arg world) {
    size_t n = duckdb_config_count();
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_get_config_flag"]
 * opaque getConfigFlagRaw : Idx -> IO (UInt32 x Option String x Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_get_config_flag(uint64_t index, lean_obj_arg world) {
    const char *name = NULL;
    const char *description = NULL;
    duckdb_state rc = duckdb_get_config_flag((size_t)index, &name, &description);
    lean_obj_res nameOpt = name ? mk_option_some(mk_string_or_empty(name)) : mk_option_none();
    lean_obj_res descOpt = description ? mk_option_some(mk_string_or_empty(description)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), mk_pair(nameOpt, descOpt)));
}

/*
 * @[extern "linen_duckdb_set_config"]
 * opaque setConfigRaw : @& Config -> @& String -> @& String -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_set_config(
    b_lean_obj_arg cfg_obj, b_lean_obj_arg name_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_config_t *cw = (linen_duckdb_config_t *)lean_get_external_data(cfg_obj);
    const char *name = lean_string_cstr(name_obj);
    const char *value = lean_string_cstr(value_obj);
    duckdb_state rc = duckdb_set_config(cw->config, name, value);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_destroy_config"]
 * opaque destroyConfig : Config -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_config(b_lean_obj_arg cfg_obj, lean_obj_arg world) {
    linen_duckdb_config_t *cw = (linen_duckdb_config_t *)lean_get_external_data(cfg_obj);
    if (cw->config) {
        duckdb_destroy_config(&cw->config);
        cw->config = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_create_config_option"]
 * opaque createConfigOption : IO ConfigOption
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_config_option(lean_obj_arg world) {
    duckdb_config_option option = duckdb_create_config_option();
    lean_obj_res obj = mk_duckdb_config_option(option);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_config_option wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_config_option"]
 * opaque destroyConfigOption : ConfigOption -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_config_option(b_lean_obj_arg opt_obj, lean_obj_arg world) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    if (ow->option) {
        duckdb_destroy_config_option(&ow->option);
        ow->option = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_config_option_set_name"]
 * opaque configOptionSetName : @& ConfigOption -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_option_set_name(
    b_lean_obj_arg opt_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_config_option_set_name(ow->option, name);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_config_option_set_type"]
 * opaque configOptionSetType : @& ConfigOption -> @& LogicalType -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_option_set_type(
    b_lean_obj_arg opt_obj, b_lean_obj_arg type_obj, lean_obj_arg world
) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(type_obj);
    duckdb_config_option_set_type(ow->option, tw->type);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_config_option_set_default_value"]
 * opaque configOptionSetDefaultValue : @& ConfigOption -> @& Value -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_option_set_default_value(
    b_lean_obj_arg opt_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    linen_duckdb_value_t *vw = (linen_duckdb_value_t *)lean_get_external_data(value_obj);
    duckdb_config_option_set_default_value(ow->option, vw->val);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_config_option_set_default_scope"]
 * opaque configOptionSetDefaultScopeRaw : @& ConfigOption -> UInt32 -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_option_set_default_scope(
    b_lean_obj_arg opt_obj, uint32_t scope, lean_obj_arg world
) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    duckdb_config_option_set_default_scope(ow->option, (duckdb_config_option_scope)scope);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_config_option_set_description"]
 * opaque configOptionSetDescription : @& ConfigOption -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_config_option_set_description(
    b_lean_obj_arg opt_obj, b_lean_obj_arg description_obj, lean_obj_arg world
) {
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    const char *description = lean_string_cstr(description_obj);
    duckdb_config_option_set_description(ow->option, description);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_register_config_option"]
 * opaque registerConfigOptionRaw : @& Connection -> @& ConfigOption -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_register_config_option(
    b_lean_obj_arg conn_obj, b_lean_obj_arg opt_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    linen_duckdb_config_option_t *ow = (linen_duckdb_config_option_t *)lean_get_external_data(opt_obj);
    duckdb_state rc = duckdb_register_config_option(cw->conn, ow->option);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_client_context_get_config_option"]
 * opaque clientContextGetConfigOptionRaw : @& ClientContext -> @& String -> IO (Value x UInt32)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_client_context_get_config_option(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_client_context_t *cw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_config_option_scope scope = DUCKDB_CONFIG_OPTION_SCOPE_INVALID;
    duckdb_value val = duckdb_client_context_get_config_option(cw->ctx, name, &scope);
    lean_obj_res obj = mk_duckdb_value(val);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_value wrapper")));
    }
    return lean_io_result_mk_ok(mk_pair(obj, lean_box_uint32((uint32_t)scope)));
}

/* ================================================================
 * DATA CHUNK
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_data_chunk"]
 * opaque createDataChunk : @& Array LogicalType -> IO DataChunk
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_data_chunk(b_lean_obj_arg types_obj, lean_obj_arg world) {
    size_t type_count = 0;
    duckdb_logical_type *types = build_logical_type_array(types_obj, &type_count);
    duckdb_data_chunk chunk = duckdb_create_data_chunk(types, (idx_t)type_count);
    if (types) free(types);
    lean_obj_res obj = mk_duckdb_data_chunk(chunk);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_data_chunk wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_data_chunk"]
 * opaque destroy : DataChunk -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_data_chunk(b_lean_obj_arg chunk_obj, lean_obj_arg world) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    if (cw->chunk) {
        duckdb_destroy_data_chunk(&cw->chunk);
        cw->chunk = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_data_chunk_reset"]
 * opaque reset : DataChunk -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_data_chunk_reset(b_lean_obj_arg chunk_obj, lean_obj_arg world) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    if (cw->chunk) duckdb_data_chunk_reset(cw->chunk);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_data_chunk_get_column_count"]
 * opaque getColumnCount : DataChunk -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_data_chunk_get_column_count(b_lean_obj_arg chunk_obj, lean_obj_arg world) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    idx_t n = cw->chunk ? duckdb_data_chunk_get_column_count(cw->chunk) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_data_chunk_get_vector"]
 * opaque getVector : @& DataChunk -> Idx -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_data_chunk_get_vector(
    b_lean_obj_arg chunk_obj, uint64_t col_idx, lean_obj_arg world
) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    duckdb_vector vec = duckdb_data_chunk_get_vector(cw->chunk, (idx_t)col_idx);
    lean_obj_res obj = mk_duckdb_vector(vec);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_data_chunk_get_size"]
 * opaque getSize : DataChunk -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_data_chunk_get_size(b_lean_obj_arg chunk_obj, lean_obj_arg world) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    idx_t n = cw->chunk ? duckdb_data_chunk_get_size(cw->chunk) : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_data_chunk_set_size"]
 * opaque setSize : @& DataChunk -> Idx -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_data_chunk_set_size(
    b_lean_obj_arg chunk_obj, uint64_t size, lean_obj_arg world
) {
    linen_duckdb_data_chunk_t *cw = (linen_duckdb_data_chunk_t *)lean_get_external_data(chunk_obj);
    if (cw->chunk) duckdb_data_chunk_set_size(cw->chunk, (idx_t)size);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * ERROR DATA
 * ================================================================ */

/*
 * @[extern "linen_duckdb_error_data_create"]
 * opaque createRaw : UInt32 -> @& String -> IO ErrorData
 */
LEAN_EXPORT lean_obj_res linen_duckdb_error_data_create(
    uint32_t type, b_lean_obj_arg message_obj, lean_obj_arg world
) {
    const char *message = lean_string_cstr(message_obj);
    duckdb_error_data err = duckdb_create_error_data((duckdb_error_type)type, message);
    lean_obj_res obj = mk_duckdb_error_data(err);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_error_data wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_error_data_destroy"]
 * opaque destroy : ErrorData -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_error_data_destroy(b_lean_obj_arg err_obj, lean_obj_arg world) {
    linen_duckdb_error_data_t *ew = (linen_duckdb_error_data_t *)lean_get_external_data(err_obj);
    if (ew->err) {
        duckdb_destroy_error_data(&ew->err);
        ew->err = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_error_data_error_type"]
 * opaque errorTypeRaw : @& ErrorData -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_error_data_error_type(b_lean_obj_arg err_obj, lean_obj_arg world) {
    linen_duckdb_error_data_t *ew = (linen_duckdb_error_data_t *)lean_get_external_data(err_obj);
    duckdb_error_type t = ew->err ? duckdb_error_data_error_type(ew->err) : DUCKDB_ERROR_INVALID;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)t));
}

/*
 * @[extern "linen_duckdb_error_data_message"]
 * opaque message : @& ErrorData -> IO String
 */
LEAN_EXPORT lean_obj_res linen_duckdb_error_data_message(b_lean_obj_arg err_obj, lean_obj_arg world) {
    linen_duckdb_error_data_t *ew = (linen_duckdb_error_data_t *)lean_get_external_data(err_obj);
    const char *msg = ew->err ? duckdb_error_data_message(ew->err) : NULL;
    return lean_io_result_mk_ok(mk_string_or_empty(msg));
}

/*
 * @[extern "linen_duckdb_error_data_has_error"]
 * opaque hasError : @& ErrorData -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_error_data_has_error(b_lean_obj_arg err_obj, lean_obj_arg world) {
    linen_duckdb_error_data_t *ew = (linen_duckdb_error_data_t *)lean_get_external_data(err_obj);
    bool has = ew->err ? duckdb_error_data_has_error(ew->err) : false;
    return lean_io_result_mk_ok(lean_box(has ? 1 : 0));
}

/* ================================================================
 * EXECUTE PREPARED
 * ================================================================ */

/*
 * @[extern "linen_duckdb_execute_prepared"]
 * opaque executeRaw : @& PreparedStatement -> IO (UInt32 x Result)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_execute_prepared(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw =
        (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    lean_obj_res resultObj = mk_duckdb_result_wrapper();
    if (!resultObj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_result wrapper")));
    }
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(resultObj);
    duckdb_state rc = duckdb_execute_prepared(sw->stmt, &rw->result);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), resultObj));
}

/*
 * @[extern "linen_duckdb_destroy_result"]
 * opaque destroy : Result -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_result(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_destroy_result(&rw->result);
    memset(&rw->result, 0, sizeof(rw->result));
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * FILE SYSTEM
 * ================================================================ */

/*
 * @[extern "linen_duckdb_client_context_get_file_system"]
 * opaque getFileSystem : @& ClientContext -> IO FileSystem
 */
LEAN_EXPORT lean_obj_res linen_duckdb_client_context_get_file_system(
    b_lean_obj_arg ctx_obj, lean_obj_arg world
) {
    linen_duckdb_client_context_t *cw = (linen_duckdb_client_context_t *)lean_get_external_data(ctx_obj);
    duckdb_file_system fs = duckdb_client_context_get_file_system(cw->ctx);
    lean_obj_res obj = mk_duckdb_file_system(fs);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_file_system wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_file_system"]
 * opaque destroy : FileSystem -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_file_system(b_lean_obj_arg fs_obj, lean_obj_arg world) {
    linen_duckdb_file_system_t *fw = (linen_duckdb_file_system_t *)lean_get_external_data(fs_obj);
    if (fw->fs) {
        duckdb_destroy_file_system(&fw->fs);
        fw->fs = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_file_system_error_data"]
 * opaque errorData : @& FileSystem -> IO ErrorData
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_system_error_data(b_lean_obj_arg fs_obj, lean_obj_arg world) {
    linen_duckdb_file_system_t *fw = (linen_duckdb_file_system_t *)lean_get_external_data(fs_obj);
    duckdb_error_data err = duckdb_file_system_error_data(fw->fs);
    lean_obj_res obj = mk_duckdb_error_data(err);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_error_data wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_file_open_options"]
 * opaque createOpenOptions : IO FileOpenOptions
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_file_open_options(lean_obj_arg world) {
    duckdb_file_open_options opts = duckdb_create_file_open_options();
    lean_obj_res obj = mk_duckdb_file_open_options(opts);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_file_open_options wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_file_open_options_set_flag"]
 * opaque setOpenFlagRaw : @& FileOpenOptions -> UInt32 -> Bool -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_open_options_set_flag(
    b_lean_obj_arg opts_obj, uint32_t flag, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_file_open_options_t *ow =
        (linen_duckdb_file_open_options_t *)lean_get_external_data(opts_obj);
    duckdb_state rc =
        duckdb_file_open_options_set_flag(ow->opts, (duckdb_file_flag)flag, value != 0);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_destroy_file_open_options"]
 * opaque destroyOpenOptions : FileOpenOptions -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_file_open_options(
    b_lean_obj_arg opts_obj, lean_obj_arg world
) {
    linen_duckdb_file_open_options_t *ow =
        (linen_duckdb_file_open_options_t *)lean_get_external_data(opts_obj);
    if (ow->opts) {
        duckdb_destroy_file_open_options(&ow->opts);
        ow->opts = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_file_system_open"]
 * opaque openRaw : @& FileSystem -> @& String -> @& FileOpenOptions -> IO (UInt32 x Option FileHandle)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_system_open(
    b_lean_obj_arg fs_obj, b_lean_obj_arg path_obj, b_lean_obj_arg opts_obj, lean_obj_arg world
) {
    linen_duckdb_file_system_t *fw = (linen_duckdb_file_system_t *)lean_get_external_data(fs_obj);
    linen_duckdb_file_open_options_t *ow =
        (linen_duckdb_file_open_options_t *)lean_get_external_data(opts_obj);
    const char *path = lean_string_cstr(path_obj);
    duckdb_file_handle handle = NULL;
    duckdb_state rc = duckdb_file_system_open(fw->fs, path, ow->opts, &handle);
    lean_obj_res handleOpt = handle ? mk_option_some(mk_duckdb_file_handle(handle)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), handleOpt));
}

/*
 * @[extern "linen_duckdb_destroy_file_handle"]
 * opaque destroyFileHandle : FileHandle -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_file_handle(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    if (hw->handle) {
        duckdb_destroy_file_handle(&hw->handle);
        hw->handle = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_file_handle_error_data"]
 * opaque handleErrorData : @& FileHandle -> IO ErrorData
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_error_data(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    duckdb_error_data err = duckdb_file_handle_error_data(hw->handle);
    lean_obj_res obj = mk_duckdb_error_data(err);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_error_data wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_file_handle_read"]
 * opaque read : @& FileHandle -> Int64 -> IO (Int64 x ByteArray)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_read(
    b_lean_obj_arg fh_obj, uint64_t size, lean_obj_arg world
) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    int64_t requested = (int64_t)size;
    size_t bufSize = requested > 0 ? (size_t)requested : 0;
    uint8_t *buf = bufSize ? (uint8_t *)malloc(bufSize) : NULL;
    int64_t n = duckdb_file_handle_read(hw->handle, buf, requested);
    size_t copySize = (n > 0 && (size_t)n <= bufSize) ? (size_t)n : 0;
    lean_obj_res bytesObj = lean_alloc_sarray(1, copySize, copySize);
    if (copySize > 0) memcpy(lean_sarray_cptr(bytesObj), buf, copySize);
    if (buf) free(buf);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint64((uint64_t)n), bytesObj));
}

/*
 * @[extern "linen_duckdb_file_handle_write"]
 * opaque write : @& FileHandle -> @& ByteArray -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_write(
    b_lean_obj_arg fh_obj, b_lean_obj_arg data_obj, lean_obj_arg world
) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    const uint8_t *data = lean_sarray_cptr(data_obj);
    size_t len = lean_sarray_size(data_obj);
    int64_t n = duckdb_file_handle_write(hw->handle, data, (int64_t)len);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_file_handle_tell"]
 * opaque tell : @& FileHandle -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_tell(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    int64_t pos = duckdb_file_handle_tell(hw->handle);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)pos));
}

/*
 * @[extern "linen_duckdb_file_handle_size"]
 * opaque size : @& FileHandle -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_size(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    int64_t sz = duckdb_file_handle_size(hw->handle);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)sz));
}

/*
 * @[extern "linen_duckdb_file_handle_seek"]
 * opaque seekRaw : @& FileHandle -> Int64 -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_seek(
    b_lean_obj_arg fh_obj, uint64_t position, lean_obj_arg world
) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    duckdb_state rc = duckdb_file_handle_seek(hw->handle, (int64_t)position);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_file_handle_sync"]
 * opaque syncRaw : @& FileHandle -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_sync(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    duckdb_state rc = duckdb_file_handle_sync(hw->handle);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_file_handle_close"]
 * opaque closeRaw : @& FileHandle -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_file_handle_close(b_lean_obj_arg fh_obj, lean_obj_arg world) {
    linen_duckdb_file_handle_t *hw = (linen_duckdb_file_handle_t *)lean_get_external_data(fh_obj);
    duckdb_state rc = duckdb_file_handle_close(hw->handle);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * HELPERS
 * ================================================================ */

/*
 * @[extern "linen_duckdb_malloc"]
 * opaque malloc : UInt64 -> IO RawMemory
 */
LEAN_EXPORT lean_obj_res linen_duckdb_malloc(uint64_t size, lean_obj_arg world) {
    void *ptr = duckdb_malloc((size_t)size);
    lean_obj_res obj = mk_duckdb_raw_memory(ptr);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_malloc wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_free"]
 * opaque free : RawMemory -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_free(b_lean_obj_arg mem_obj, lean_obj_arg world) {
    linen_duckdb_raw_memory_t *mw = (linen_duckdb_raw_memory_t *)lean_get_external_data(mem_obj);
    if (mw->ptr) {
        duckdb_free(mw->ptr);
        mw->ptr = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_size"]
 * opaque vectorSize : IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_size(lean_obj_arg world) {
    idx_t n = duckdb_vector_size();
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_valid_utf8_check"]
 * opaque validUtf8Check : @& ByteArray -> IO (Option ErrorData)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_valid_utf8_check(b_lean_obj_arg bytes_obj, lean_obj_arg world) {
    const uint8_t *data = lean_sarray_cptr(bytes_obj);
    size_t len = lean_sarray_size(bytes_obj);
    duckdb_error_data err = duckdb_valid_utf8_check((const char *)data, (idx_t)len);
    if (!err) return lean_io_result_mk_ok(mk_option_none());
    lean_obj_res obj = mk_duckdb_error_data(err);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_error_data wrapper")));
    }
    return lean_io_result_mk_ok(mk_option_some(obj));
}

/* Copy a 16-byte `duckdb_string_t` image out of a Lean `ByteArray`. Callers
 * must have already checked `lean_sarray_size(bytes_obj) == 16`; a
 * mismatched size zero-fills the struct (a caller-contract violation, not a
 * DuckDB-level error). */
static inline void unpack_string_t(b_lean_obj_arg bytes_obj, duckdb_string_t *out) {
    memset(out, 0, sizeof(*out));
    size_t len = lean_sarray_size(bytes_obj);
    if (len == sizeof(duckdb_string_t)) {
        memcpy(out, lean_sarray_cptr(bytes_obj), sizeof(duckdb_string_t));
    }
}

/*
 * @[extern "linen_duckdb_string_is_inlined"]
 * opaque stringIsInlined : @& ByteArray -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_string_is_inlined(b_lean_obj_arg bytes_obj, lean_obj_arg world) {
    duckdb_string_t s;
    unpack_string_t(bytes_obj, &s);
    bool inlined = duckdb_string_is_inlined(s);
    return lean_io_result_mk_ok(lean_box(inlined ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_string_t_length"]
 * opaque stringTLength : @& ByteArray -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_string_t_length(b_lean_obj_arg bytes_obj, lean_obj_arg world) {
    duckdb_string_t s;
    unpack_string_t(bytes_obj, &s);
    uint32_t len = duckdb_string_t_length(s);
    return lean_io_result_mk_ok(lean_box_uint32(len));
}

/*
 * @[extern "linen_duckdb_string_t_data"]
 * opaque stringTData : @& ByteArray -> IO String
 */
LEAN_EXPORT lean_obj_res linen_duckdb_string_t_data(b_lean_obj_arg bytes_obj, lean_obj_arg world) {
    duckdb_string_t s;
    unpack_string_t(bytes_obj, &s);
    uint32_t len = duckdb_string_t_length(s);
    const char *ptr = duckdb_string_t_data(&s);
    if (!ptr) return lean_io_result_mk_ok(mk_string_or_empty(NULL));
    return lean_io_result_mk_ok(lean_mk_string_from_bytes(ptr, (size_t)len));
}

/*
 * @[extern "linen_duckdb_from_date"]
 * opaque fromDateRaw : Int32 -> IO (Int32 x (Int8 x Int8))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_from_date(uint32_t days, lean_obj_arg world) {
    duckdb_date date = { .days = (int32_t)days };
    duckdb_date_struct ds = duckdb_from_date(date);
    lean_obj_res inner = mk_pair(lean_box((uint8_t)ds.month), lean_box((uint8_t)ds.day));
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)ds.year), inner));
}

/*
 * @[extern "linen_duckdb_to_date"]
 * opaque toDateRaw : Int32 -> Int8 -> Int8 -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_to_date(
    uint32_t year, uint8_t month, uint8_t day, lean_obj_arg world
) {
    duckdb_date_struct ds;
    ds.year = (int32_t)year;
    ds.month = (int8_t)month;
    ds.day = (int8_t)day;
    duckdb_date date = duckdb_to_date(ds);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)date.days));
}

/*
 * @[extern "linen_duckdb_is_finite_date"]
 * opaque isFiniteDate : Int32 -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_is_finite_date(uint32_t days, lean_obj_arg world) {
    duckdb_date date = { .days = (int32_t)days };
    bool finite = duckdb_is_finite_date(date);
    return lean_io_result_mk_ok(lean_box(finite ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_from_time"]
 * opaque fromTimeRaw : Int64 -> IO (Int8 x (Int8 x (Int8 x Int32)))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_from_time(uint64_t micros, lean_obj_arg world) {
    duckdb_time time = { .micros = (int64_t)micros };
    duckdb_time_struct ts = duckdb_from_time(time);
    lean_obj_res tail = mk_pair(lean_box((uint8_t)ts.sec), lean_box_uint32((uint32_t)ts.micros));
    lean_obj_res mid = mk_pair(lean_box((uint8_t)ts.min), tail);
    return lean_io_result_mk_ok(mk_pair(lean_box((uint8_t)ts.hour), mid));
}

/*
 * @[extern "linen_duckdb_create_time_tz"]
 * opaque createTimeTz : Int64 -> Int32 -> IO TimeTz
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_time_tz(
    uint64_t micros, uint32_t offset, lean_obj_arg world
) {
    duckdb_time_tz tz = duckdb_create_time_tz((int64_t)micros, (int32_t)offset);
    return lean_io_result_mk_ok(lean_box_uint64(tz.bits));
}

/*
 * @[extern "linen_duckdb_from_time_tz"]
 * opaque fromTimeTzRaw : UInt64 -> IO (Int8 x (Int8 x (Int8 x (Int32 x Int32))))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_from_time_tz(uint64_t bits, lean_obj_arg world) {
    duckdb_time_tz tz = { .bits = bits };
    duckdb_time_tz_struct tts = duckdb_from_time_tz(tz);
    lean_obj_res tail = mk_pair(lean_box_uint32((uint32_t)tts.time.micros),
        lean_box_uint32((uint32_t)tts.offset));
    lean_obj_res t3 = mk_pair(lean_box((uint8_t)tts.time.sec), tail);
    lean_obj_res t2 = mk_pair(lean_box((uint8_t)tts.time.min), t3);
    return lean_io_result_mk_ok(mk_pair(lean_box((uint8_t)tts.time.hour), t2));
}

/*
 * @[extern "linen_duckdb_to_time"]
 * opaque toTimeRaw : Int8 -> Int8 -> Int8 -> Int32 -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_to_time(
    uint8_t hour, uint8_t min, uint8_t sec, uint32_t micros, lean_obj_arg world
) {
    duckdb_time_struct ts;
    ts.hour = (int8_t)hour;
    ts.min = (int8_t)min;
    ts.sec = (int8_t)sec;
    ts.micros = (int32_t)micros;
    duckdb_time time = duckdb_to_time(ts);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)time.micros));
}

/*
 * @[extern "linen_duckdb_from_timestamp"]
 * opaque fromTimestampRaw :
 *   Int64 -> IO (Int32 x (Int8 x (Int8 x (Int8 x (Int8 x (Int8 x Int32))))))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_from_timestamp(uint64_t micros, lean_obj_arg world) {
    duckdb_timestamp ts = { .micros = (int64_t)micros };
    duckdb_timestamp_struct tss = duckdb_from_timestamp(ts);
    lean_obj_res tail = mk_pair(lean_box((uint8_t)tss.time.sec), lean_box_uint32((uint32_t)tss.time.micros));
    lean_obj_res t3 = mk_pair(lean_box((uint8_t)tss.time.min), tail);
    lean_obj_res t2 = mk_pair(lean_box((uint8_t)tss.time.hour), t3);
    lean_obj_res t1 = mk_pair(lean_box((uint8_t)tss.date.day), t2);
    lean_obj_res t0 = mk_pair(lean_box((uint8_t)tss.date.month), t1);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)tss.date.year), t0));
}

/*
 * @[extern "linen_duckdb_to_timestamp"]
 * opaque toTimestampRaw :
 *   Int32 -> Int8 -> Int8 -> Int8 -> Int8 -> Int8 -> Int32 -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_to_timestamp(
    uint32_t year, uint8_t month, uint8_t day, uint8_t hour, uint8_t min, uint8_t sec,
    uint32_t micros, lean_obj_arg world
) {
    duckdb_timestamp_struct tss;
    tss.date.year = (int32_t)year;
    tss.date.month = (int8_t)month;
    tss.date.day = (int8_t)day;
    tss.time.hour = (int8_t)hour;
    tss.time.min = (int8_t)min;
    tss.time.sec = (int8_t)sec;
    tss.time.micros = (int32_t)micros;
    duckdb_timestamp ts = duckdb_to_timestamp(tss);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)ts.micros));
}

/*
 * @[extern "linen_duckdb_is_finite_timestamp"]
 * opaque isFiniteTimestamp : Int64 -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_is_finite_timestamp(uint64_t micros, lean_obj_arg world) {
    duckdb_timestamp ts = { .micros = (int64_t)micros };
    bool finite = duckdb_is_finite_timestamp(ts);
    return lean_io_result_mk_ok(lean_box(finite ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_is_finite_timestamp_s"]
 * opaque isFiniteTimestampSeconds : Int64 -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_is_finite_timestamp_s(uint64_t seconds, lean_obj_arg world) {
    duckdb_timestamp_s ts = { .seconds = (int64_t)seconds };
    bool finite = duckdb_is_finite_timestamp_s(ts);
    return lean_io_result_mk_ok(lean_box(finite ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_is_finite_timestamp_ms"]
 * opaque isFiniteTimestampMillis : Int64 -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_is_finite_timestamp_ms(uint64_t millis, lean_obj_arg world) {
    duckdb_timestamp_ms ts = { .millis = (int64_t)millis };
    bool finite = duckdb_is_finite_timestamp_ms(ts);
    return lean_io_result_mk_ok(lean_box(finite ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_is_finite_timestamp_ns"]
 * opaque isFiniteTimestampNanos : Int64 -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_is_finite_timestamp_ns(uint64_t nanos, lean_obj_arg world) {
    duckdb_timestamp_ns ts = { .nanos = (int64_t)nanos };
    bool finite = duckdb_is_finite_timestamp_ns(ts);
    return lean_io_result_mk_ok(lean_box(finite ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_hugeint_to_double"]
 * opaque hugeIntToDoubleRaw : UInt64 -> Int64 -> IO Float
 */
LEAN_EXPORT lean_obj_res linen_duckdb_hugeint_to_double(
    uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    duckdb_hugeint h = { .lower = lower, .upper = (int64_t)upper };
    double d = duckdb_hugeint_to_double(h);
    return lean_io_result_mk_ok(lean_box_float(d));
}

/*
 * @[extern "linen_duckdb_double_to_hugeint"]
 * opaque doubleToHugeIntRaw : Float -> IO (UInt64 x Int64)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_double_to_hugeint(double value, lean_obj_arg world) {
    duckdb_hugeint h = duckdb_double_to_hugeint(value);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint64(h.lower), lean_box_uint64((uint64_t)h.upper)));
}

/*
 * @[extern "linen_duckdb_uhugeint_to_double"]
 * opaque uHugeIntToDoubleRaw : UInt64 -> UInt64 -> IO Float
 */
LEAN_EXPORT lean_obj_res linen_duckdb_uhugeint_to_double(
    uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    duckdb_uhugeint h = { .lower = lower, .upper = upper };
    double d = duckdb_uhugeint_to_double(h);
    return lean_io_result_mk_ok(lean_box_float(d));
}

/*
 * @[extern "linen_duckdb_double_to_uhugeint"]
 * opaque doubleToUHugeIntRaw : Float -> IO (UInt64 x UInt64)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_double_to_uhugeint(double value, lean_obj_arg world) {
    duckdb_uhugeint h = duckdb_double_to_uhugeint(value);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint64(h.lower), lean_box_uint64(h.upper)));
}

/*
 * @[extern "linen_duckdb_double_to_decimal"]
 * opaque doubleToDecimalRaw : Float -> UInt8 -> UInt8 -> IO (UInt8 x (UInt8 x (UInt64 x Int64)))
 */
LEAN_EXPORT lean_obj_res linen_duckdb_double_to_decimal(
    double value, uint8_t width, uint8_t scale, lean_obj_arg world
) {
    duckdb_decimal dec = duckdb_double_to_decimal(value, width, scale);
    lean_obj_res tail = mk_pair(lean_box_uint64(dec.value.lower), lean_box_uint64((uint64_t)dec.value.upper));
    lean_obj_res mid = mk_pair(lean_box(dec.scale), tail);
    return lean_io_result_mk_ok(mk_pair(lean_box(dec.width), mid));
}

/*
 * @[extern "linen_duckdb_decimal_to_double"]
 * opaque decimalToDoubleRaw : UInt8 -> UInt8 -> UInt64 -> Int64 -> IO Float
 */
LEAN_EXPORT lean_obj_res linen_duckdb_decimal_to_double(
    uint8_t width, uint8_t scale, uint64_t lower, uint64_t upper, lean_obj_arg world
) {
    duckdb_decimal dec;
    dec.width = width;
    dec.scale = scale;
    dec.value.lower = lower;
    dec.value.upper = (int64_t)upper;
    double d = duckdb_decimal_to_double(dec);
    return lean_io_result_mk_ok(lean_box_float(d));
}

/* ================================================================
 * LOGGING
 *
 * See `Logging.lean`'s module doc comment for the overall design (DuckDB's
 * own `extra_data`/`delete_callback` slots stand in for a
 * `sqlite3_user_data(ctx)`-style lookup).
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_log_storage"]
 * opaque create : IO LogStorage
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_log_storage(lean_obj_arg world) {
    duckdb_log_storage storage = duckdb_create_log_storage();
    lean_obj_res obj = mk_duckdb_log_storage(storage);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_log_storage wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_log_storage"]
 * opaque destroy : LogStorage -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_log_storage(b_lean_obj_arg ls_obj, lean_obj_arg world) {
    linen_duckdb_log_storage_t *sw = (linen_duckdb_log_storage_t *)lean_get_external_data(ls_obj);
    if (sw->storage) {
        duckdb_destroy_log_storage(&sw->storage);
        sw->storage = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* The fixed write trampoline installed on every `LogStorage` this port
 * creates. `extra_data` is the Lean closure stored (and `lean_inc_ref`'d)
 * by `linen_duckdb_log_storage_set_extra_data` below; it is retained again
 * here for the duration of this specific call (matching
 * `ffi/sqlite3_shim.c`'s `linen_sqlite3_xfunc_trampoline`), then applied via
 * `lean_apply_5` (four real arguments plus the IO "world" token). Any
 * Lean-side failure is silently swallowed — there is no meaningful way to
 * surface an error back through DuckDB's own `void`-returning callback
 * type. */
static void linen_duckdb_log_write_trampoline(
    void *extra_data, duckdb_timestamp *timestamp, const char *level, const char *log_type,
    const char *log_message
) {
    lean_object *closure = (lean_object *)extra_data;
    if (!closure) return;
    lean_inc_ref(closure);
    int64_t micros = timestamp ? timestamp->micros : 0;
    lean_obj_res tsObj = lean_box_uint64((uint64_t)micros);
    lean_obj_res levelObj = mk_string_or_empty(level);
    lean_obj_res typeObj = mk_string_or_empty(log_type);
    lean_obj_res msgObj = mk_string_or_empty(log_message);
    lean_object *result = lean_apply_5(closure, tsObj, levelObj, typeObj, msgObj, lean_box(0));
    lean_dec_ref(result);
}

/* Invoked by DuckDB when `logStorage`'s extra data is replaced or when
 * `logStorage` itself is destroyed. Releases this port's reference to the
 * stored closure. */
static void linen_duckdb_log_delete_trampoline(void *extra_data) {
    if (extra_data) lean_dec_ref((lean_object *)extra_data);
}

/*
 * @[extern "linen_duckdb_log_storage_set_write_log_entry"]
 * opaque setWriteLogEntry : LogStorage -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_log_storage_set_write_log_entry(
    b_lean_obj_arg ls_obj, lean_obj_arg world
) {
    linen_duckdb_log_storage_t *sw = (linen_duckdb_log_storage_t *)lean_get_external_data(ls_obj);
    duckdb_log_storage_set_write_log_entry(sw->storage, &linen_duckdb_log_write_trampoline);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_log_storage_set_extra_data"]
 * opaque setExtraData :
 *   @& LogStorage -> (Int64 -> String -> String -> String -> IO Unit) -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_log_storage_set_extra_data(
    b_lean_obj_arg ls_obj, lean_obj_arg onWrite_obj, lean_obj_arg world
) {
    linen_duckdb_log_storage_t *sw = (linen_duckdb_log_storage_t *)lean_get_external_data(ls_obj);
    lean_inc_ref(onWrite_obj); /* the C side now owns one persistent reference */
    duckdb_log_storage_set_extra_data(
        sw->storage, (void *)onWrite_obj, &linen_duckdb_log_delete_trampoline);
    lean_dec_ref(onWrite_obj); /* release this call's own borrowed reference */
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_log_storage_set_name"]
 * opaque setName : @& LogStorage -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_log_storage_set_name(
    b_lean_obj_arg ls_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_log_storage_t *sw = (linen_duckdb_log_storage_t *)lean_get_external_data(ls_obj);
    const char *name = lean_string_cstr(name_obj);
    duckdb_log_storage_set_name(sw->storage, name);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_register_log_storage"]
 * opaque registerLogStorageRaw : @& Database -> @& LogStorage -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_register_log_storage(
    b_lean_obj_arg db_obj, b_lean_obj_arg ls_obj, lean_obj_arg world
) {
    linen_duckdb_database_t *dw = (linen_duckdb_database_t *)lean_get_external_data(db_obj);
    linen_duckdb_log_storage_t *sw = (linen_duckdb_log_storage_t *)lean_get_external_data(ls_obj);
    duckdb_state rc = duckdb_register_log_storage(dw->db, sw->storage);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/* ================================================================
 * LOGICAL TYPES
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_logical_type"]
 * opaque createRaw : UInt32 -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_logical_type(uint32_t ty, lean_obj_arg world) {
    duckdb_logical_type type = duckdb_create_logical_type((duckdb_type)ty);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_logical_type_get_alias"]
 * opaque getAlias : @& LogicalType -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_logical_type_get_alias(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    char *alias = duckdb_logical_type_get_alias(tw->type);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(alias));
}

/*
 * @[extern "linen_duckdb_logical_type_set_alias"]
 * opaque setAlias : @& LogicalType -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_logical_type_set_alias(
    b_lean_obj_arg ty_obj, b_lean_obj_arg alias_obj, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type_set_alias(tw->type, lean_string_cstr(alias_obj));
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_create_list_type"]
 * opaque createListType : @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_list_type(b_lean_obj_arg child_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *cw = (linen_duckdb_logical_type_t *)lean_get_external_data(child_obj);
    duckdb_logical_type type = duckdb_create_list_type(cw->type);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_array_type"]
 * opaque createArrayType : @& LogicalType -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_array_type(
    b_lean_obj_arg child_obj, uint64_t array_size, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *cw = (linen_duckdb_logical_type_t *)lean_get_external_data(child_obj);
    duckdb_logical_type type = duckdb_create_array_type(cw->type, array_size);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_map_type"]
 * opaque createMapType : @& LogicalType -> @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_map_type(
    b_lean_obj_arg key_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *kw = (linen_duckdb_logical_type_t *)lean_get_external_data(key_obj);
    linen_duckdb_logical_type_t *vw = (linen_duckdb_logical_type_t *)lean_get_external_data(value_obj);
    duckdb_logical_type type = duckdb_create_map_type(kw->type, vw->type);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_union_type"]
 * opaque createUnionType : @& Array LogicalType -> @& Array String -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_union_type(
    b_lean_obj_arg types_obj, b_lean_obj_arg names_obj, lean_obj_arg world
) {
    size_t n = 0;
    duckdb_logical_type *types = build_logical_type_array(types_obj, &n);
    size_t nn = 0;
    const char **names = build_cstring_array(names_obj, &nn);
    duckdb_logical_type type = duckdb_create_union_type(types, names, n);
    if (types) free(types);
    if (names) free(names);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_struct_type"]
 * opaque createStructType : @& Array LogicalType -> @& Array String -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_struct_type(
    b_lean_obj_arg types_obj, b_lean_obj_arg names_obj, lean_obj_arg world
) {
    size_t n = 0;
    duckdb_logical_type *types = build_logical_type_array(types_obj, &n);
    size_t nn = 0;
    const char **names = build_cstring_array(names_obj, &nn);
    duckdb_logical_type type = duckdb_create_struct_type(types, names, n);
    if (types) free(types);
    if (names) free(names);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_enum_type"]
 * opaque createEnumType : @& Array String -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_enum_type(b_lean_obj_arg names_obj, lean_obj_arg world) {
    size_t n = 0;
    const char **names = build_cstring_array(names_obj, &n);
    duckdb_logical_type type = duckdb_create_enum_type(names, n);
    if (names) free(names);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_create_decimal_type"]
 * opaque createDecimalType : UInt8 -> UInt8 -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_decimal_type(
    uint8_t width, uint8_t scale, lean_obj_arg world
) {
    duckdb_logical_type type = duckdb_create_decimal_type(width, scale);
    lean_obj_res obj = mk_duckdb_logical_type(type);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_get_type_id_raw"]
 * opaque getTypeIdRaw : @& LogicalType -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_get_type_id_raw(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_type id = duckdb_get_type_id(tw->type);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)id));
}

/*
 * @[extern "linen_duckdb_decimal_width"]
 * opaque decimalWidth : @& LogicalType -> IO UInt8
 */
LEAN_EXPORT lean_obj_res linen_duckdb_decimal_width(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    uint8_t width = duckdb_decimal_width(tw->type);
    return lean_io_result_mk_ok(lean_box(width));
}

/*
 * @[extern "linen_duckdb_decimal_scale"]
 * opaque decimalScale : @& LogicalType -> IO UInt8
 */
LEAN_EXPORT lean_obj_res linen_duckdb_decimal_scale(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    uint8_t scale = duckdb_decimal_scale(tw->type);
    return lean_io_result_mk_ok(lean_box(scale));
}

/*
 * @[extern "linen_duckdb_decimal_internal_type_raw"]
 * opaque decimalInternalTypeRaw : @& LogicalType -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_decimal_internal_type_raw(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_type id = duckdb_decimal_internal_type(tw->type);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)id));
}

/*
 * @[extern "linen_duckdb_enum_internal_type_raw"]
 * opaque enumInternalTypeRaw : @& LogicalType -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_enum_internal_type_raw(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_type id = duckdb_enum_internal_type(tw->type);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)id));
}

/*
 * @[extern "linen_duckdb_enum_dictionary_size"]
 * opaque enumDictionarySize : @& LogicalType -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_enum_dictionary_size(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    uint32_t size = duckdb_enum_dictionary_size(tw->type);
    return lean_io_result_mk_ok(lean_box_uint32(size));
}

/*
 * @[extern "linen_duckdb_enum_dictionary_value"]
 * opaque enumDictionaryValue : @& LogicalType -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_enum_dictionary_value(
    b_lean_obj_arg ty_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    char *value = duckdb_enum_dictionary_value(tw->type, index);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(value));
}

/*
 * @[extern "linen_duckdb_list_type_child_type"]
 * opaque listTypeChildType : @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_list_type_child_type(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type child = duckdb_list_type_child_type(tw->type);
    lean_obj_res obj = mk_duckdb_logical_type(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_array_type_child_type"]
 * opaque arrayTypeChildType : @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_array_type_child_type(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type child = duckdb_array_type_child_type(tw->type);
    lean_obj_res obj = mk_duckdb_logical_type(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_array_type_array_size"]
 * opaque arrayTypeArraySize : @& LogicalType -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_array_type_array_size(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    idx_t size = duckdb_array_type_array_size(tw->type);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)size));
}

/*
 * @[extern "linen_duckdb_map_type_key_type"]
 * opaque mapTypeKeyType : @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_map_type_key_type(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type key = duckdb_map_type_key_type(tw->type);
    lean_obj_res obj = mk_duckdb_logical_type(key);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_map_type_value_type"]
 * opaque mapTypeValueType : @& LogicalType -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_map_type_value_type(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type value = duckdb_map_type_value_type(tw->type);
    lean_obj_res obj = mk_duckdb_logical_type(value);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_struct_type_child_count"]
 * opaque structTypeChildCount : @& LogicalType -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_struct_type_child_count(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    idx_t count = duckdb_struct_type_child_count(tw->type);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)count));
}

/*
 * @[extern "linen_duckdb_struct_type_child_name"]
 * opaque structTypeChildName : @& LogicalType -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_struct_type_child_name(
    b_lean_obj_arg ty_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    char *name = duckdb_struct_type_child_name(tw->type, index);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(name));
}

/*
 * @[extern "linen_duckdb_struct_type_child_type"]
 * opaque structTypeChildType : @& LogicalType -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_struct_type_child_type(
    b_lean_obj_arg ty_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type child = duckdb_struct_type_child_type(tw->type, index);
    lean_obj_res obj = mk_duckdb_logical_type(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_union_type_member_count"]
 * opaque unionTypeMemberCount : @& LogicalType -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_union_type_member_count(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    idx_t count = duckdb_union_type_member_count(tw->type);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)count));
}

/*
 * @[extern "linen_duckdb_union_type_member_name"]
 * opaque unionTypeMemberName : @& LogicalType -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_union_type_member_name(
    b_lean_obj_arg ty_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    char *name = duckdb_union_type_member_name(tw->type, index);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(name));
}

/*
 * @[extern "linen_duckdb_union_type_member_type"]
 * opaque unionTypeMemberType : @& LogicalType -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_union_type_member_type(
    b_lean_obj_arg ty_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_logical_type member = duckdb_union_type_member_type(tw->type, index);
    lean_obj_res obj = mk_duckdb_logical_type(member);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_logical_type"]
 * opaque destroy : LogicalType -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_logical_type(b_lean_obj_arg ty_obj, lean_obj_arg world) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    if (tw->type) {
        duckdb_destroy_logical_type(&tw->type);
        tw->type = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * PREPARED STATEMENTS
 * ================================================================ */

/*
 * @[extern "linen_duckdb_prepare"]
 * opaque prepareRaw : @& Connection -> @& String -> IO (UInt32 x PreparedStatement)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepare(
    b_lean_obj_arg conn_obj, b_lean_obj_arg query_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    duckdb_prepared_statement stmt = NULL;
    duckdb_state rc = duckdb_prepare(cw->conn, query, &stmt);
    lean_obj_res obj = mk_duckdb_prepared_statement(stmt);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_prepared_statement wrapper")));
    }
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), obj));
}

/*
 * @[extern "linen_duckdb_destroy_prepare"]
 * opaque destroy : PreparedStatement -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_prepare(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    if (sw->stmt) {
        duckdb_destroy_prepare(&sw->stmt);
        sw->stmt = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_prepare_error"]
 * opaque error : @& PreparedStatement -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepare_error(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    const char *err = duckdb_prepare_error(sw->stmt);
    return lean_io_result_mk_ok(mk_string_opt_borrowed(err));
}

/*
 * @[extern "linen_duckdb_nparams"]
 * opaque nparams : @& PreparedStatement -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_nparams(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    idx_t n = duckdb_nparams(sw->stmt);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_parameter_name"]
 * opaque parameterName : @& PreparedStatement -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_parameter_name(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    char *name = (char *)duckdb_parameter_name(sw->stmt, param_idx);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(name));
}

/*
 * @[extern "linen_duckdb_param_type_raw"]
 * opaque paramTypeRaw : @& PreparedStatement -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_param_type_raw(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_type ty = duckdb_param_type(sw->stmt, param_idx);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/*
 * @[extern "linen_duckdb_param_logical_type"]
 * opaque paramLogicalType : @& PreparedStatement -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_param_logical_type(
    b_lean_obj_arg stmt_obj, uint64_t param_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_logical_type ty = duckdb_param_logical_type(sw->stmt, param_idx);
    lean_obj_res obj = mk_duckdb_logical_type(ty);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_clear_bindings_raw"]
 * opaque clearBindingsRaw : @& PreparedStatement -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_clear_bindings_raw(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_state rc = duckdb_clear_bindings(sw->stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_prepared_statement_type_raw"]
 * opaque statementTypeRaw : @& PreparedStatement -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepared_statement_type_raw(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_statement_type ty = duckdb_prepared_statement_type(sw->stmt);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/*
 * @[extern "linen_duckdb_prepared_statement_column_count"]
 * opaque columnCount : @& PreparedStatement -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepared_statement_column_count(
    b_lean_obj_arg stmt_obj, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    idx_t count = duckdb_prepared_statement_column_count(sw->stmt);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)count));
}

/*
 * @[extern "linen_duckdb_prepared_statement_column_name"]
 * opaque columnName : @& PreparedStatement -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepared_statement_column_name(
    b_lean_obj_arg stmt_obj, uint64_t col_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    char *name = (char *)duckdb_prepared_statement_column_name(sw->stmt, col_idx);
    return lean_io_result_mk_ok(mk_string_opt_owned_free(name));
}

/*
 * @[extern "linen_duckdb_prepared_statement_column_logical_type"]
 * opaque columnLogicalType : @& PreparedStatement -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepared_statement_column_logical_type(
    b_lean_obj_arg stmt_obj, uint64_t col_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_logical_type ty = duckdb_prepared_statement_column_logical_type(sw->stmt, col_idx);
    lean_obj_res obj = mk_duckdb_logical_type(ty);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_prepared_statement_column_type_raw"]
 * opaque columnTypeRaw : @& PreparedStatement -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_prepared_statement_column_type_raw(
    b_lean_obj_arg stmt_obj, uint64_t col_idx, lean_obj_arg world
) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    duckdb_type ty = duckdb_prepared_statement_column_type(sw->stmt, col_idx);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/* ================================================================
 * QUERY EXECUTION
 * ================================================================ */

/*
 * @[extern "linen_duckdb_query"]
 * opaque queryRaw : @& Connection -> @& String -> IO (UInt32 x Types.Result)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_query(
    b_lean_obj_arg conn_obj, b_lean_obj_arg query_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    lean_obj_res obj = mk_duckdb_result_wrapper();
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_result wrapper")));
    }
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(obj);
    duckdb_state rc = duckdb_query(cw->conn, query, &rw->result);
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), obj));
}

/*
 * @[extern "linen_duckdb_column_name"]
 * opaque columnName : @& Types.Result -> Idx -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_column_name(
    b_lean_obj_arg result_obj, uint64_t col, lean_obj_arg world
) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    const char *name = duckdb_column_name(&rw->result, col);
    return lean_io_result_mk_ok(mk_string_opt_borrowed(name));
}

/*
 * @[extern "linen_duckdb_column_type_raw"]
 * opaque columnTypeRaw : @& Types.Result -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_column_type_raw(
    b_lean_obj_arg result_obj, uint64_t col, lean_obj_arg world
) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_type ty = duckdb_column_type(&rw->result, col);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/*
 * @[extern "linen_duckdb_result_statement_type_raw"]
 * opaque statementTypeRaw : @& Types.Result -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_result_statement_type_raw(
    b_lean_obj_arg result_obj, lean_obj_arg world
) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_statement_type ty = duckdb_result_statement_type(rw->result);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/*
 * @[extern "linen_duckdb_column_logical_type"]
 * opaque columnLogicalType : @& Types.Result -> Idx -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_column_logical_type(
    b_lean_obj_arg result_obj, uint64_t col, lean_obj_arg world
) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_logical_type ty = duckdb_column_logical_type(&rw->result, col);
    lean_obj_res obj = mk_duckdb_logical_type(ty);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_result_get_arrow_options"]
 * opaque resultGetArrowOptions : @& Types.Result -> IO ArrowOptions
 */
LEAN_EXPORT lean_obj_res linen_duckdb_result_get_arrow_options(
    b_lean_obj_arg result_obj, lean_obj_arg world
) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_arrow_options opts = duckdb_result_get_arrow_options(&rw->result);
    lean_obj_res obj = mk_duckdb_arrow_options(opts);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_arrow_options wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_column_count"]
 * opaque columnCount : @& Types.Result -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_column_count(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    idx_t count = duckdb_column_count(&rw->result);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)count));
}

/*
 * @[extern "linen_duckdb_rows_changed"]
 * opaque rowsChanged : @& Types.Result -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_rows_changed(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    idx_t n = duckdb_rows_changed(&rw->result);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}

/*
 * @[extern "linen_duckdb_result_error"]
 * opaque resultError : @& Types.Result -> IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_result_error(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    const char *err = duckdb_result_error(&rw->result);
    return lean_io_result_mk_ok(mk_string_opt_borrowed(err));
}

/*
 * @[extern "linen_duckdb_result_error_type_raw"]
 * opaque resultErrorTypeRaw : @& Types.Result -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_result_error_type_raw(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_error_type ty = duckdb_result_error_type(&rw->result);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)ty));
}

/*
 * @[extern "linen_duckdb_fetch_chunk"]
 * opaque fetchChunk : @& Types.Result -> IO (Option DataChunk)
 *
 * `duckdb_fetch_chunk` (unlike `duckdb_stream_fetch_chunk`, which requires a
 * `duckdb_pending_prepared_streaming`-created result) works on any
 * materialized `duckdb_result` — the one returned by plain `duckdb_query`/
 * `duckdb_execute_prepared` included, per `duckdb.h`'s own doc comment and
 * `duckdb-simple`'s real `collectRows`/`streamNextRow` usage (both call it
 * directly on an ordinary query result). It is *not* gated behind
 * `DUCKDB_API_NO_DEPRECATED`, unlike the sibling `duckdb_result_get_chunk`/
 * `duckdb_result_chunk_count` pair — this is the only non-deprecated way to
 * walk a materialized result's rows. Load-bearing for
 * `Linen.Database.DuckDB.Simple` (module #17 of `duckdb-simple`); added
 * here rather than during the original `duckdb-ffi` port because that
 * port's own scope decision mis-filed it under the excluded
 * `StreamingResult` Haskell module (see `docs/imports/duckdb-ffi/
 * dependencies.md`'s corrected note).
 */
LEAN_EXPORT lean_obj_res linen_duckdb_fetch_chunk(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    duckdb_data_chunk chunk = duckdb_fetch_chunk(rw->result);
    if (!chunk) return lean_io_result_mk_ok(mk_option_none());
    lean_obj_res obj = mk_duckdb_data_chunk(chunk);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_data_chunk wrapper")));
    }
    return lean_io_result_mk_ok(mk_option_some(obj));
}

/* ================================================================
 * SCALAR FUNCTIONS
 *
 * See `ScalarFunctions.lean`'s module doc comment for the overall design —
 * the trampoline pair below is modeled directly on the `LOGGING` section's
 * `linen_duckdb_log_write_trampoline`/`linen_duckdb_log_delete_trampoline`.
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_scalar_function"]
 * opaque create : IO ScalarFunction
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_scalar_function(lean_obj_arg world) {
    duckdb_scalar_function fn = duckdb_create_scalar_function();
    lean_obj_res obj = mk_duckdb_scalar_function(fn);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_scalar_function wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_scalar_function"]
 * opaque destroy : ScalarFunction -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_scalar_function(b_lean_obj_arg fn_obj, lean_obj_arg world) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    if (fw->fn) {
        duckdb_destroy_scalar_function(&fw->fn);
        fw->fn = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_name"]
 * opaque setName : @& ScalarFunction -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_name(
    b_lean_obj_arg fn_obj, b_lean_obj_arg name_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_scalar_function_set_name(fw->fn, lean_string_cstr(name_obj));
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_varargs"]
 * opaque setVarargs : @& ScalarFunction -> @& LogicalType -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_varargs(
    b_lean_obj_arg fn_obj, b_lean_obj_arg ty_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_scalar_function_set_varargs(fw->fn, tw->type);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_special_handling"]
 * opaque setSpecialHandling : @& ScalarFunction -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_special_handling(
    b_lean_obj_arg fn_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_scalar_function_set_special_handling(fw->fn);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_volatile"]
 * opaque setVolatile : @& ScalarFunction -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_volatile(b_lean_obj_arg fn_obj, lean_obj_arg world) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_scalar_function_set_volatile(fw->fn);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_add_parameter"]
 * opaque addParameter : @& ScalarFunction -> @& LogicalType -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_add_parameter(
    b_lean_obj_arg fn_obj, b_lean_obj_arg ty_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_scalar_function_add_parameter(fw->fn, tw->type);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_return_type"]
 * opaque setReturnType : @& ScalarFunction -> @& LogicalType -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_return_type(
    b_lean_obj_arg fn_obj, b_lean_obj_arg ty_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_scalar_function_set_return_type(fw->fn, tw->type);
    return lean_io_result_mk_ok(lean_box(0));
}

/* The fixed native implementation installed on every `ScalarFunction` this
 * port builds. `extra_data` (retrieved via
 * `duckdb_scalar_function_get_extra_info`) is the Lean closure stored (and
 * `lean_inc_ref`'d) by `linen_duckdb_scalar_function_set_extra_info` below.
 * `input`/`output` are both borrowed (owned by DuckDB, not this program):
 * `input` is wrapped as a `BorrowedDataChunk`, `output` reuses the
 * already-non-owning `Vector` wrapper. Any Lean-side failure is silently
 * swallowed, exactly as `LOGGING`'s write trampoline already documents. */
static void linen_duckdb_scalar_function_call_trampoline(
    duckdb_function_info info, duckdb_data_chunk input, duckdb_vector output
) {
    lean_object *closure = (lean_object *)duckdb_scalar_function_get_extra_info(info);
    if (!closure) return;
    lean_inc_ref(closure);
    lean_obj_res inputObj = mk_duckdb_borrowed_data_chunk(input);
    lean_obj_res outputObj = mk_duckdb_vector(output);
    if (!inputObj || !outputObj) return;
    lean_object *result = lean_apply_3(closure, inputObj, outputObj, lean_box(0));
    lean_dec_ref(result);
}

/* Invoked by DuckDB when `fn`'s extra data is replaced or when `fn` itself
 * is destroyed. Releases this port's reference to the stored closure. */
static void linen_duckdb_scalar_function_delete_trampoline(void *extra_data) {
    if (extra_data) lean_dec_ref((lean_object *)extra_data);
}

/*
 * @[extern "linen_duckdb_scalar_function_set_extra_info"]
 * opaque setOnCall : @& ScalarFunction -> (BorrowedDataChunk -> Vector -> IO Unit) -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_extra_info(
    b_lean_obj_arg fn_obj, lean_obj_arg onCall_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    lean_inc_ref(onCall_obj); /* the C side now owns one persistent reference */
    duckdb_scalar_function_set_extra_info(
        fw->fn, (void *)onCall_obj, &linen_duckdb_scalar_function_delete_trampoline);
    lean_dec_ref(onCall_obj); /* release this call's own borrowed reference */
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_scalar_function_set_function"]
 * opaque setFunction : @& ScalarFunction -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_set_function(b_lean_obj_arg fn_obj, lean_obj_arg world) {
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_scalar_function_set_function(fw->fn, &linen_duckdb_scalar_function_call_trampoline);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_register_scalar_function_raw"]
 * opaque registerRaw : @& Connection -> @& ScalarFunction -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_register_scalar_function_raw(
    b_lean_obj_arg conn_obj, b_lean_obj_arg fn_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_state rc = duckdb_register_scalar_function(cw->conn, fw->fn);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_create_scalar_function_set"]
 * opaque createSet : @& String -> IO ScalarFunctionSet
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_scalar_function_set(
    b_lean_obj_arg name_obj, lean_obj_arg world
) {
    duckdb_scalar_function_set set = duckdb_create_scalar_function_set(lean_string_cstr(name_obj));
    lean_obj_res obj = mk_duckdb_scalar_function_set(set);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_scalar_function_set wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_scalar_function_set"]
 * opaque destroySet : ScalarFunctionSet -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_scalar_function_set(b_lean_obj_arg set_obj, lean_obj_arg world) {
    linen_duckdb_scalar_function_set_t *sw = (linen_duckdb_scalar_function_set_t *)lean_get_external_data(set_obj);
    if (sw->set) {
        duckdb_destroy_scalar_function_set(&sw->set);
        sw->set = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_add_scalar_function_to_set_raw"]
 * opaque addToSetRaw : @& ScalarFunctionSet -> @& ScalarFunction -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_add_scalar_function_to_set_raw(
    b_lean_obj_arg set_obj, b_lean_obj_arg fn_obj, lean_obj_arg world
) {
    linen_duckdb_scalar_function_set_t *sw = (linen_duckdb_scalar_function_set_t *)lean_get_external_data(set_obj);
    linen_duckdb_scalar_function_t *fw = (linen_duckdb_scalar_function_t *)lean_get_external_data(fn_obj);
    duckdb_state rc = duckdb_add_scalar_function_to_set(sw->set, fw->fn);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_register_scalar_function_set_raw"]
 * opaque registerSetRaw : @& Connection -> @& ScalarFunctionSet -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_register_scalar_function_set_raw(
    b_lean_obj_arg conn_obj, b_lean_obj_arg set_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    linen_duckdb_scalar_function_set_t *sw = (linen_duckdb_scalar_function_set_t *)lean_get_external_data(set_obj);
    duckdb_state rc = duckdb_register_scalar_function_set(cw->conn, sw->set);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_scalar_function_input_get_size"]
 * opaque inputSize : @& BorrowedDataChunk -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_input_get_size(
    b_lean_obj_arg input_obj, lean_obj_arg world
) {
    linen_duckdb_borrowed_data_chunk_t *iw =
        (linen_duckdb_borrowed_data_chunk_t *)lean_get_external_data(input_obj);
    idx_t size = duckdb_data_chunk_get_size(iw->chunk);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)size));
}

/*
 * @[extern "linen_duckdb_scalar_function_input_get_column_count"]
 * opaque inputColumnCount : @& BorrowedDataChunk -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_input_get_column_count(
    b_lean_obj_arg input_obj, lean_obj_arg world
) {
    linen_duckdb_borrowed_data_chunk_t *iw =
        (linen_duckdb_borrowed_data_chunk_t *)lean_get_external_data(input_obj);
    idx_t count = duckdb_data_chunk_get_column_count(iw->chunk);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)count));
}

/*
 * @[extern "linen_duckdb_scalar_function_input_get_vector"]
 * opaque inputVector : @& BorrowedDataChunk -> Idx -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_scalar_function_input_get_vector(
    b_lean_obj_arg input_obj, uint64_t col_idx, lean_obj_arg world
) {
    linen_duckdb_borrowed_data_chunk_t *iw =
        (linen_duckdb_borrowed_data_chunk_t *)lean_get_external_data(input_obj);
    duckdb_vector vec = duckdb_data_chunk_get_vector(iw->chunk, col_idx);
    lean_obj_res obj = mk_duckdb_vector(vec);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/* ================================================================
 * VALIDITY
 * ================================================================ */

/*
 * @[extern "linen_duckdb_validity_row_is_valid"]
 * opaque rowIsValid : @& ValidityMask -> Idx -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_validity_row_is_valid(
    b_lean_obj_arg validity_obj, uint64_t row, lean_obj_arg world
) {
    linen_duckdb_validity_mask_t *mw = (linen_duckdb_validity_mask_t *)lean_get_external_data(validity_obj);
    bool valid = duckdb_validity_row_is_valid(mw->mask, row);
    return lean_io_result_mk_ok(lean_box(valid ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_validity_set_row_validity"]
 * opaque setRowValidity : @& ValidityMask -> Idx -> Bool -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_validity_set_row_validity(
    b_lean_obj_arg validity_obj, uint64_t row, uint8_t valid, lean_obj_arg world
) {
    linen_duckdb_validity_mask_t *mw = (linen_duckdb_validity_mask_t *)lean_get_external_data(validity_obj);
    duckdb_validity_set_row_validity(mw->mask, row, valid != 0);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_validity_set_row_invalid"]
 * opaque setRowInvalid : @& ValidityMask -> Idx -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_validity_set_row_invalid(
    b_lean_obj_arg validity_obj, uint64_t row, lean_obj_arg world
) {
    linen_duckdb_validity_mask_t *mw = (linen_duckdb_validity_mask_t *)lean_get_external_data(validity_obj);
    duckdb_validity_set_row_invalid(mw->mask, row);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_validity_set_row_valid"]
 * opaque setRowValid : @& ValidityMask -> Idx -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_validity_set_row_valid(
    b_lean_obj_arg validity_obj, uint64_t row, lean_obj_arg world
) {
    linen_duckdb_validity_mask_t *mw = (linen_duckdb_validity_mask_t *)lean_get_external_data(validity_obj);
    duckdb_validity_set_row_valid(mw->mask, row);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * VECTOR
 * ================================================================ */

/*
 * @[extern "linen_duckdb_create_vector"]
 * opaque createVector : @& LogicalType -> Idx -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_create_vector(
    b_lean_obj_arg ty_obj, uint64_t capacity, lean_obj_arg world
) {
    linen_duckdb_logical_type_t *tw = (linen_duckdb_logical_type_t *)lean_get_external_data(ty_obj);
    duckdb_vector vec = duckdb_create_vector(tw->type, capacity);
    lean_obj_res obj = mk_duckdb_vector(vec);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_destroy_vector"]
 * opaque destroy : Vector -> IO Unit
 *
 * Only valid for a `Vector` obtained from `createVector` — see
 * `Vector.lean`'s module doc comment.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_destroy_vector(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    if (vw->vec) {
        duckdb_destroy_vector(&vw->vec);
        vw->vec = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_column_type"]
 * opaque getColumnType : @& Vector -> IO LogicalType
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_column_type(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_logical_type ty = duckdb_vector_get_column_type(vw->vec);
    lean_obj_res obj = mk_duckdb_logical_type(ty);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_logical_type wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_vector_get_data_bytes"]
 * opaque getDataBytes : @& Vector -> UInt64 -> UInt64 -> IO ByteArray
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_data_bytes(
    b_lean_obj_arg vec_obj, uint64_t byte_offset, uint64_t length, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    uint8_t *data = (uint8_t *)duckdb_vector_get_data(vw->vec);
    lean_obj_res bytesObj = lean_alloc_sarray(1, length, length);
    if (data && length > 0) memcpy(lean_sarray_cptr(bytesObj), data + byte_offset, length);
    return lean_io_result_mk_ok(bytesObj);
}

/*
 * @[extern "linen_duckdb_vector_set_data_bytes"]
 * opaque setDataBytes : @& Vector -> UInt64 -> @& ByteArray -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_set_data_bytes(
    b_lean_obj_arg vec_obj, uint64_t byte_offset, b_lean_obj_arg bytes_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    uint8_t *data = (uint8_t *)duckdb_vector_get_data(vw->vec);
    size_t len = lean_sarray_size(bytes_obj);
    if (data && len > 0) memcpy(data + byte_offset, lean_sarray_cptr(bytes_obj), len);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_int32"]
 * opaque getInt32 : @& Vector -> Idx -> IO Int32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_int32(
    b_lean_obj_arg vec_obj, uint64_t idx, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    int32_t *data = (int32_t *)duckdb_vector_get_data(vw->vec);
    int32_t value = data ? data[idx] : 0;
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)value));
}

/*
 * @[extern "linen_duckdb_vector_set_int32"]
 * opaque setInt32 : @& Vector -> Idx -> Int32 -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_set_int32(
    b_lean_obj_arg vec_obj, uint64_t idx, uint32_t value, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    int32_t *data = (int32_t *)duckdb_vector_get_data(vw->vec);
    if (data) data[idx] = (int32_t)value;
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_int64"]
 * opaque getInt64 : @& Vector -> Idx -> IO Int64
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_int64(
    b_lean_obj_arg vec_obj, uint64_t idx, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    int64_t *data = (int64_t *)duckdb_vector_get_data(vw->vec);
    int64_t value = data ? data[idx] : 0;
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)value));
}

/*
 * @[extern "linen_duckdb_vector_set_int64"]
 * opaque setInt64 : @& Vector -> Idx -> Int64 -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_set_int64(
    b_lean_obj_arg vec_obj, uint64_t idx, uint64_t value, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    int64_t *data = (int64_t *)duckdb_vector_get_data(vw->vec);
    if (data) data[idx] = (int64_t)value;
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_double"]
 * opaque getDouble : @& Vector -> Idx -> IO Float
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_double(
    b_lean_obj_arg vec_obj, uint64_t idx, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    double *data = (double *)duckdb_vector_get_data(vw->vec);
    double value = data ? data[idx] : 0.0;
    return lean_io_result_mk_ok(lean_box_float(value));
}

/*
 * @[extern "linen_duckdb_vector_set_double"]
 * opaque setDouble : @& Vector -> Idx -> Float -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_set_double(
    b_lean_obj_arg vec_obj, uint64_t idx, double value, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    double *data = (double *)duckdb_vector_get_data(vw->vec);
    if (data) data[idx] = value;
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_bool"]
 * opaque getBool : @& Vector -> Idx -> IO Bool
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_bool(
    b_lean_obj_arg vec_obj, uint64_t idx, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    bool *data = (bool *)duckdb_vector_get_data(vw->vec);
    bool value = data ? data[idx] : false;
    return lean_io_result_mk_ok(lean_box(value ? 1 : 0));
}

/*
 * @[extern "linen_duckdb_vector_set_bool"]
 * opaque setBool : @& Vector -> Idx -> Bool -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_set_bool(
    b_lean_obj_arg vec_obj, uint64_t idx, uint8_t value, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    bool *data = (bool *)duckdb_vector_get_data(vw->vec);
    if (data) data[idx] = value != 0;
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_get_validity"]
 * opaque getValidity : @& Vector -> IO (Option ValidityMask)
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_get_validity(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    uint64_t *mask = duckdb_vector_get_validity(vw->vec);
    if (!mask) return lean_io_result_mk_ok(mk_option_none());
    lean_obj_res obj = mk_duckdb_validity_mask(mask);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb validity mask wrapper")));
    }
    return lean_io_result_mk_ok(mk_option_some(obj));
}

/*
 * @[extern "linen_duckdb_vector_ensure_validity_writable"]
 * opaque ensureValidityWritable : @& Vector -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_ensure_validity_writable(
    b_lean_obj_arg vec_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_vector_ensure_validity_writable(vw->vec);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_assign_string_element"]
 * opaque assignStringElement : @& Vector -> Idx -> @& String -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_assign_string_element(
    b_lean_obj_arg vec_obj, uint64_t index, b_lean_obj_arg str_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_vector_assign_string_element(vw->vec, index, lean_string_cstr(str_obj));
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_assign_string_element_len"]
 * opaque assignStringElementLen : @& Vector -> Idx -> @& ByteArray -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_assign_string_element_len(
    b_lean_obj_arg vec_obj, uint64_t index, b_lean_obj_arg bytes_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    const char *data = (const char *)lean_sarray_cptr(bytes_obj);
    size_t len = lean_sarray_size(bytes_obj);
    duckdb_vector_assign_string_element_len(vw->vec, index, data, (idx_t)len);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_unsafe_vector_assign_string_element_len"]
 * opaque unsafeAssignStringElementLen : @& Vector -> Idx -> @& ByteArray -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_unsafe_vector_assign_string_element_len(
    b_lean_obj_arg vec_obj, uint64_t index, b_lean_obj_arg bytes_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    const char *data = (const char *)lean_sarray_cptr(bytes_obj);
    size_t len = lean_sarray_size(bytes_obj);
    duckdb_unsafe_vector_assign_string_element_len(vw->vec, index, data, (idx_t)len);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_list_vector_get_child"]
 * opaque listVectorGetChild : @& Vector -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_list_vector_get_child(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_vector child = duckdb_list_vector_get_child(vw->vec);
    lean_obj_res obj = mk_duckdb_vector(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_list_vector_get_size"]
 * opaque listVectorGetSize : @& Vector -> IO Idx
 */
LEAN_EXPORT lean_obj_res linen_duckdb_list_vector_get_size(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    idx_t size = duckdb_list_vector_get_size(vw->vec);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)size));
}

/*
 * @[extern "linen_duckdb_list_vector_set_size_raw"]
 * opaque listVectorSetSizeRaw : @& Vector -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_list_vector_set_size_raw(
    b_lean_obj_arg vec_obj, uint64_t size, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_state rc = duckdb_list_vector_set_size(vw->vec, size);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_list_vector_reserve_raw"]
 * opaque listVectorReserveRaw : @& Vector -> Idx -> IO UInt32
 */
LEAN_EXPORT lean_obj_res linen_duckdb_list_vector_reserve_raw(
    b_lean_obj_arg vec_obj, uint64_t required_capacity, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_state rc = duckdb_list_vector_reserve(vw->vec, required_capacity);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * @[extern "linen_duckdb_struct_vector_get_child"]
 * opaque structVectorGetChild : @& Vector -> Idx -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_struct_vector_get_child(
    b_lean_obj_arg vec_obj, uint64_t index, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_vector child = duckdb_struct_vector_get_child(vw->vec, index);
    lean_obj_res obj = mk_duckdb_vector(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_array_vector_get_child"]
 * opaque arrayVectorGetChild : @& Vector -> IO Vector
 */
LEAN_EXPORT lean_obj_res linen_duckdb_array_vector_get_child(b_lean_obj_arg vec_obj, lean_obj_arg world) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    duckdb_vector child = duckdb_array_vector_get_child(vw->vec);
    lean_obj_res obj = mk_duckdb_vector(child);
    if (!obj) {
        return lean_io_result_mk_error(
            lean_mk_io_user_error(lean_mk_string("malloc failed for duckdb_vector wrapper")));
    }
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_duckdb_vector_reference_value"]
 * opaque referenceValue : @& Vector -> @& Value -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_reference_value(
    b_lean_obj_arg vec_obj, b_lean_obj_arg value_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *vw = (linen_duckdb_vector_t *)lean_get_external_data(vec_obj);
    linen_duckdb_value_t *valw = (linen_duckdb_value_t *)lean_get_external_data(value_obj);
    duckdb_vector_reference_value(vw->vec, valw->val);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_duckdb_vector_reference_vector"]
 * opaque referenceVector : @& Vector -> @& Vector -> IO Unit
 */
LEAN_EXPORT lean_obj_res linen_duckdb_vector_reference_vector(
    b_lean_obj_arg to_obj, b_lean_obj_arg from_obj, lean_obj_arg world
) {
    linen_duckdb_vector_t *tow = (linen_duckdb_vector_t *)lean_get_external_data(to_obj);
    linen_duckdb_vector_t *fromw = (linen_duckdb_vector_t *)lean_get_external_data(from_obj);
    duckdb_vector_reference_vector(tow->vec, fromw->vec);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * TEST SUPPORT
 *
 * Small helpers used only by this batch's own `Tests/` (see the file
 * header comment for why): `duckdb_query` (for DDL/DML setup, e.g.
 * `CREATE TABLE`) and `duckdb_prepare`/`duckdb_destroy_prepare` (to obtain
 * a real `PreparedStatement` to bind against). None of these back a
 * `Linen/` module — they exist purely so `Tests/` can exercise real
 * DuckDB behavior end-to-end without prematurely porting
 * `Database.DuckDB.FFI.QueryExecution`/`PreparedStatements`.
 * ================================================================ */

/*
 * Test-only: run `query` on `connection` for side effects (DDL/DML),
 * returning just its `duckdb_state`. Any result data is immediately
 * discarded.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_test_query(
    b_lean_obj_arg conn_obj, b_lean_obj_arg query_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    duckdb_result result;
    duckdb_state rc = duckdb_query(cw->conn, query, &result);
    duckdb_destroy_result(&result);
    return lean_io_result_mk_ok(lean_box_uint32((uint32_t)rc));
}

/*
 * Test-only: prepare `query` on `connection`, returning `(state,
 * prepared_statement?)`.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_test_prepare(
    b_lean_obj_arg conn_obj, b_lean_obj_arg query_obj, lean_obj_arg world
) {
    linen_duckdb_connection_t *cw = (linen_duckdb_connection_t *)lean_get_external_data(conn_obj);
    const char *query = lean_string_cstr(query_obj);
    duckdb_prepared_statement stmt = NULL;
    duckdb_state rc = duckdb_prepare(cw->conn, query, &stmt);
    lean_obj_res stmtOpt = stmt ? mk_option_some(mk_duckdb_prepared_statement(stmt)) : mk_option_none();
    return lean_io_result_mk_ok(mk_pair(lean_box_uint32((uint32_t)rc), stmtOpt));
}

/*
 * Test-only: destroy a `PreparedStatement` obtained from
 * `linen_duckdb_test_prepare`. Idempotent.
 */
LEAN_EXPORT lean_obj_res linen_duckdb_test_destroy_prepare(b_lean_obj_arg stmt_obj, lean_obj_arg world) {
    linen_duckdb_prepared_statement_t *sw = (linen_duckdb_prepared_statement_t *)lean_get_external_data(stmt_obj);
    if (sw->stmt) {
        duckdb_destroy_prepare(&sw->stmt);
        sw->stmt = NULL;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * Test-only: the row count of a `duckdb_result` obtained from
 * `Database.DuckDB.FFI.ExecutePrepared.execute`, used only by
 * `Tests/Linen/Database/DuckDB/FFI/ExecutePreparedTest.lean` to check that
 * executing a bound prepared statement really produced the expected rows
 * (as opposed to merely reporting `duckdb_state.success`).
 */
LEAN_EXPORT lean_obj_res linen_duckdb_test_result_row_count(b_lean_obj_arg result_obj, lean_obj_arg world) {
    linen_duckdb_result_t *rw = (linen_duckdb_result_t *)lean_get_external_data(result_obj);
    idx_t n = duckdb_row_count(&rw->result);
    return lean_io_result_mk_ok(lean_box_uint64((uint64_t)n));
}
