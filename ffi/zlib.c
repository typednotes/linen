/*
 * ffi/zlib.c — zlib inflate (decompress) FFI for Lean 4
 *
 * Wraps a persistent `z_stream *` for the raw zlib/RFC 1950 inflate
 * direction only (WindowBits 15). Follows the same `lean_alloc_external`
 * pattern as `ffi/tls.c`.
 *
 * Scope (see docs/imports/Zlib/dependencies.md): only
 * `initInflate`/`feedInflate`/`finishInflate` — no deflate/compress, no
 * gzip (WindowBits 31).
 *
 * Platform: macOS and Linux. Requires the system zlib (`libz`).
 */

#include <lean/lean.h>
#include <zlib.h>
#include <string.h>
#include <stdlib.h>

/* ────────────────────────────────────────────────────────────
 * External class for the inflate stream handle
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_zlib_inflate_class = NULL;

typedef struct {
    z_stream strm;
    int initialized;  /* whether inflateInit succeeded (needs inflateEnd) */
    int finished;     /* whether Z_STREAM_END/inflateEnd already happened */
} linen_inflate_t;

static void linen_zlib_inflate_finalizer(void *ptr) {
    linen_inflate_t *s = (linen_inflate_t *)ptr;
    if (s) {
        if (s->initialized && !s->finished) {
            inflateEnd(&s->strm);
        }
        free(s);
    }
}

static void linen_zlib_noop_foreach(void *mod, b_lean_obj_arg fn) {
    /* no sub-objects to traverse */
}

static void ensure_zlib_class(void) {
    if (!g_linen_zlib_inflate_class) {
        g_linen_zlib_inflate_class = lean_register_external_class(
            linen_zlib_inflate_finalizer, linen_zlib_noop_foreach);
    }
}

static lean_obj_res linen_zlib_mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

/* Grow-and-retry output buffer size for a single `inflate` driving loop. */
#define LINEN_ZLIB_CHUNK 16384

/*
 * Run `inflate` in a loop, feeding no more input, growing the output
 * buffer as needed, until the input's available bytes are exhausted (and,
 * if `flush == Z_FINISH`, until Z_STREAM_END or an error). Appends produced
 * bytes into `*out_buf`/`*out_len`/`*out_cap` (a simple growable buffer).
 *
 * Returns 0 on success, -1 on a zlib error (message left in `err_msg`).
 */
static int linen_zlib_drive(
    z_stream *strm,
    int flush,
    uint8_t **out_buf,
    size_t *out_len,
    size_t *out_cap,
    int *stream_ended,
    const char **err_msg
) {
    uint8_t chunk[LINEN_ZLIB_CHUNK];
    for (;;) {
        strm->next_out = chunk;
        strm->avail_out = LINEN_ZLIB_CHUNK;

        int ret = inflate(strm, flush);

        size_t produced = LINEN_ZLIB_CHUNK - strm->avail_out;
        if (produced > 0) {
            if (*out_len + produced > *out_cap) {
                size_t new_cap = (*out_cap == 0) ? LINEN_ZLIB_CHUNK : *out_cap * 2;
                while (new_cap < *out_len + produced) new_cap *= 2;
                uint8_t *grown = realloc(*out_buf, new_cap);
                if (!grown) {
                    *err_msg = "out of memory growing inflate output buffer";
                    return -1;
                }
                *out_buf = grown;
                *out_cap = new_cap;
            }
            memcpy(*out_buf + *out_len, chunk, produced);
            *out_len += produced;
        }

        if (ret == Z_STREAM_END) {
            *stream_ended = 1;
            return 0;
        }
        if (ret == Z_OK) {
            /* Keep going as long as there is remaining input, or the output
               buffer was full (more output may still be pending). */
            if (strm->avail_in == 0 && produced < LINEN_ZLIB_CHUNK) {
                return 0;
            }
            continue;
        }
        if (ret == Z_BUF_ERROR) {
            /* No progress possible right now: out of input and output
               buffer wasn't full — nothing more to produce this call. */
            if (strm->avail_in == 0) {
                return 0;
            }
            continue;
        }
        *err_msg = strm->msg ? strm->msg : "inflate failed";
        return -1;
    }
}

/* ────────────────────────────────────────────────────────────
 * initInflate
 *
 * @[extern "linen_zlib_inflate_init"]
 * opaque initInflateImpl : IO Inflate.type
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_zlib_inflate_init(lean_obj_arg world) {
    ensure_zlib_class();

    linen_inflate_t *s = malloc(sizeof(linen_inflate_t));
    if (!s) {
        return linen_zlib_mk_io_error("malloc failed");
    }
    memset(&s->strm, 0, sizeof(s->strm));
    s->initialized = 0;
    s->finished = 0;

    /* WindowBits 15: raw zlib/RFC 1950 format (not gzip). */
    int ret = inflateInit2(&s->strm, 15);
    if (ret != Z_OK) {
        const char *msg = s->strm.msg ? s->strm.msg : "inflateInit failed";
        char buf[256];
        strncpy(buf, msg, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
        free(s);
        return linen_zlib_mk_io_error(buf);
    }
    s->initialized = 1;

    lean_obj_res obj = lean_alloc_external(g_linen_zlib_inflate_class, s);
    return lean_io_result_mk_ok(obj);
}

/* ────────────────────────────────────────────────────────────
 * feedInflate
 *
 * @[extern "linen_zlib_inflate_feed"]
 * opaque feedInflateImpl : @& Inflate.type → @& ByteArray → IO ByteArray
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_zlib_inflate_feed(
    b_lean_obj_arg handle_obj,
    b_lean_obj_arg data_obj,
    lean_obj_arg world
) {
    linen_inflate_t *s = lean_get_external_data(handle_obj);

    if (s->finished) {
        return lean_io_result_mk_ok(lean_mk_empty_byte_array(lean_box(0)));
    }

    size_t len = lean_sarray_size(data_obj);
    const uint8_t *buf = lean_sarray_cptr(data_obj);

    s->strm.next_in = (uint8_t *)buf;
    s->strm.avail_in = (uInt)len;

    uint8_t *out_buf = NULL;
    size_t out_len = 0;
    size_t out_cap = 0;
    int stream_ended = 0;
    const char *err_msg = NULL;

    int rc = linen_zlib_drive(&s->strm, Z_NO_FLUSH, &out_buf, &out_len, &out_cap,
                               &stream_ended, &err_msg);
    if (rc != 0) {
        char errbuf[256];
        strncpy(errbuf, err_msg, sizeof(errbuf) - 1);
        errbuf[sizeof(errbuf) - 1] = '\0';
        free(out_buf);
        return linen_zlib_mk_io_error(errbuf);
    }
    if (stream_ended) {
        s->finished = 1;
    }

    lean_obj_res arr = lean_alloc_sarray(1, out_len, out_len);
    if (out_len > 0) {
        memcpy(lean_sarray_cptr(arr), out_buf, out_len);
    }
    free(out_buf);
    return lean_io_result_mk_ok(arr);
}

/* ────────────────────────────────────────────────────────────
 * finishInflate
 *
 * @[extern "linen_zlib_inflate_finish"]
 * opaque finishInflateImpl : @& Inflate.type → IO ByteArray
 *
 * Flushes any remaining buffered output (Z_FINISH) and calls `inflateEnd`.
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_zlib_inflate_finish(
    b_lean_obj_arg handle_obj,
    lean_obj_arg world
) {
    linen_inflate_t *s = lean_get_external_data(handle_obj);

    if (s->finished) {
        return lean_io_result_mk_ok(lean_mk_empty_byte_array(lean_box(0)));
    }

    /* No new input — just flush whatever `inflate` can still produce. */
    uint8_t empty;
    s->strm.next_in = &empty;
    s->strm.avail_in = 0;

    uint8_t *out_buf = NULL;
    size_t out_len = 0;
    size_t out_cap = 0;
    int stream_ended = 0;
    const char *err_msg = NULL;

    int rc = linen_zlib_drive(&s->strm, Z_FINISH, &out_buf, &out_len, &out_cap,
                               &stream_ended, &err_msg);

    if (s->initialized) {
        inflateEnd(&s->strm);
        s->initialized = 0;
    }
    s->finished = 1;

    if (rc != 0) {
        char errbuf[256];
        strncpy(errbuf, err_msg, sizeof(errbuf) - 1);
        errbuf[sizeof(errbuf) - 1] = '\0';
        free(out_buf);
        return linen_zlib_mk_io_error(errbuf);
    }

    lean_obj_res arr = lean_alloc_sarray(1, out_len, out_len);
    if (out_len > 0) {
        memcpy(lean_sarray_cptr(arr), out_buf, out_len);
    }
    free(out_buf);
    return lean_io_result_mk_ok(arr);
}
