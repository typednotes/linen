/*
 * ffi/tls.c — OpenSSL/LibreSSL TLS FFI for Lean 4
 *
 * Wraps OpenSSL's SSL_CTX, SSL objects for TLS server and client support.
 * Follows the same lean_alloc_external pattern as ffi/network.c.
 *
 * Features:
 * - TLS 1.2 / 1.3 support
 * - ALPN negotiation (for HTTP/2)
 * - Client certificate retrieval
 * - Client-side TLS with system CA trust and SNI
 * - Proper resource cleanup via GC finalizer
 *
 * Platform: macOS and Linux. Requires OpenSSL or LibreSSL.
 */

#include <lean/lean.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* ────────────────────────────────────────────────────────────
 * External classes for SSL_CTX and SSL
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_ssl_ctx_class = NULL;
static lean_external_class *g_linen_ssl_class = NULL;

typedef struct {
    SSL_CTX *ctx;
    unsigned char *alpn;      /* server ALPN preference list, wire format, or NULL */
    unsigned int alpn_len;
} linen_ssl_ctx_t;

typedef struct {
    SSL *ssl;
    int fd;      /* borrowed — not owned, closed by socket layer */
    int failed;  /* a fatal SSL/SYSCALL error happened: never SSL_shutdown */
} linen_ssl_t;

static void linen_ssl_ctx_finalizer(void *ptr) {
    linen_ssl_ctx_t *c = (linen_ssl_ctx_t *)ptr;
    if (c) {
        if (c->ctx) SSL_CTX_free(c->ctx);
        free(c->alpn);
        free(c);
    }
}

/* The finalizer frees and never shuts down: it runs whenever the GC gets
 * to it, by which time the fd may be closed and its number reused by an
 * unrelated connection, which a close_notify would then be written to.
 * `linen_tls_close` is the orderly shutdown. */
static void linen_ssl_finalizer(void *ptr) {
    linen_ssl_t *s = (linen_ssl_t *)ptr;
    if (s) {
        if (s->ssl) SSL_free(s->ssl);
        free(s);
    }
}

static void linen_noop_foreach_tls(void *mod, b_lean_obj_arg fn) {
    /* no sub-objects to traverse */
}

static void ensure_classes(void) {
    if (!g_linen_ssl_ctx_class) {
        g_linen_ssl_ctx_class = lean_register_external_class(
            linen_ssl_ctx_finalizer, linen_noop_foreach_tls);
    }
    if (!g_linen_ssl_class) {
        g_linen_ssl_class = lean_register_external_class(
            linen_ssl_finalizer, linen_noop_foreach_tls);
    }
}

static lean_obj_res mk_io_error(const char *msg) {
    unsigned long err = ERR_get_error();
    char buf[256];
    if (err) {
        ERR_error_string_n(err, buf, sizeof(buf));
    } else {
        strncpy(buf, msg, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
    }
    return lean_mk_io_user_error(lean_mk_string(buf));
}

/* Modes every context gets. ACCEPT_MOVING_WRITE_BUFFER: a retried SSL_write
 * may be handed the same bytes at a different address (a Lean ByteArray can
 * move), which OpenSSL otherwise rejects as "bad write retry". AUTO_RETRY is
 * OpenSSL's default, stated for clarity. Partial writes stay off, so a write
 * is all-or-nothing and the non-blocking retry contract is "repeat the same
 * write" (as HsOpenSSL). */
static void linen_tls_set_modes(SSL_CTX *ctx) {
    SSL_CTX_set_mode(ctx, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER | SSL_MODE_AUTO_RETRY);
}

/* ────────────────────────────────────────────────────────────
 * SSL_CTX creation and configuration
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_tls_ctx_create"]
 * opaque tlsCtxCreateImpl : @& String → @& String → IO TLSContextHandle.type
 *
 * Creates an SSL_CTX configured for TLS server mode with the given
 * certificate and key files.
 */
LEAN_EXPORT lean_obj_res linen_tls_ctx_create(
    b_lean_obj_arg cert_path_obj,
    b_lean_obj_arg key_path_obj,
    lean_obj_arg world
) {
    ensure_classes();

    const char *cert_path = lean_string_cstr(cert_path_obj);
    const char *key_path = lean_string_cstr(key_path_obj);

    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) {
        return lean_io_result_mk_error(mk_io_error("SSL_CTX_new failed"));
    }

    /* Set minimum TLS version to 1.2 */
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    linen_tls_set_modes(ctx);

    /* TLS 1.2 cipher suites: ephemeral key exchange and AEAD only — the
     * Mozilla "intermediate" set, and what RFC 9113 §9.2.2 requires of a
     * connection carrying HTTP/2 (its blocklist is every other suite).
     * TLS 1.3 suites all qualify already. Renegotiation is off (§9.2.1). */
    if (SSL_CTX_set_cipher_list(ctx,
            "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:"
            "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:"
            "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:"
            "DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384") != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("SSL_CTX_set_cipher_list failed"));
    }
#ifdef SSL_OP_NO_RENEGOTIATION
    SSL_CTX_set_options(ctx, SSL_OP_NO_RENEGOTIATION);
#endif

    /* Load certificate and private key */
    if (SSL_CTX_use_certificate_chain_file(ctx, cert_path) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Failed to load certificate"));
    }

    if (SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Failed to load private key"));
    }

    if (SSL_CTX_check_private_key(ctx) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Private key does not match certificate"));
    }

    linen_ssl_ctx_t *wrapper = malloc(sizeof(linen_ssl_ctx_t));
    if (!wrapper) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ctx = ctx;
    wrapper->alpn = NULL;
    wrapper->alpn_len = 0;

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_ctx_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

/* ────────────────────────────────────────────────────────────
 * ALPN configuration (for HTTP/2 negotiation)
 * ──────────────────────────────────────────────────────────── */

static int alpn_select_cb(SSL *ssl, const unsigned char **out, unsigned char *outlen,
                          const unsigned char *in, unsigned int inlen, void *arg) {
    /* The context's list, in the server's order of preference: the first of
     * ours the client also offers. No overlap: no ALPN (the client decides
     * whether it can live without it). */
    linen_ssl_ctx_t *c = (linen_ssl_ctx_t *)arg;
    if (!c || !c->alpn) return SSL_TLSEXT_ERR_NOACK;
    if (SSL_select_next_proto((unsigned char **)out, outlen, c->alpn, c->alpn_len,
                              in, inlen) == OPENSSL_NPN_NEGOTIATED) {
        return SSL_TLSEXT_ERR_OK;
    }
    return SSL_TLSEXT_ERR_NOACK;
}

/*
 * @[extern "linen_tls_ctx_set_alpn_protocols"]
 * opaque setServerAlpnWire : @& TLSContext → @& ByteArray → IO Unit
 *
 * Answer ALPN from this list (wire format: length-prefixed names), preferring
 * earlier entries. An empty list stops answering ALPN.
 */
LEAN_EXPORT lean_obj_res linen_tls_ctx_set_alpn_protocols(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg wire_obj, lean_obj_arg world) {
    linen_ssl_ctx_t *wrapper = lean_get_external_data(ctx_obj);
    size_t len = lean_sarray_size(wire_obj);
    unsigned char *copy = NULL;
    if (len > 0) {
        copy = malloc(len);
        if (!copy) return lean_io_result_mk_error(mk_io_error("malloc failed"));
        memcpy(copy, lean_sarray_cptr(wire_obj), len);
    }
    free(wrapper->alpn);
    wrapper->alpn = copy;
    wrapper->alpn_len = (unsigned int)len;
    SSL_CTX_set_alpn_select_cb(wrapper->ctx, copy ? alpn_select_cb : NULL, copy ? wrapper : NULL);
    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_tls_ctx_set_alpn"]
 * opaque tlsCtxSetAlpnImpl : @& TLSContextHandle.type → IO Unit
 *
 * The historical entry point: prefer h2, then http/1.1.
 */
LEAN_EXPORT lean_obj_res linen_tls_ctx_set_alpn(
    b_lean_obj_arg ctx_obj,
    lean_obj_arg world
) {
    static const unsigned char h2_http11[] = "\x02h2\x08http/1.1";
    lean_obj_res wire = lean_alloc_sarray(1, sizeof(h2_http11) - 1, sizeof(h2_http11) - 1);
    memcpy(lean_sarray_cptr(wire), h2_http11, sizeof(h2_http11) - 1);
    lean_obj_res r = linen_tls_ctx_set_alpn_protocols(ctx_obj, wire, world);
    lean_dec(wire);
    return r;
}

/*
 * @[extern "linen_tls_ctx_set_alpn_offer"]
 * opaque setClientAlpnWire : @& TLSContext → @& ByteArray → IO Unit
 *
 * What a client context offers in its ClientHello (wire format).
 */
LEAN_EXPORT lean_obj_res linen_tls_ctx_set_alpn_offer(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg wire_obj, lean_obj_arg world) {
    linen_ssl_ctx_t *wrapper = lean_get_external_data(ctx_obj);
    /* SSL_CTX_set_alpn_protos returns 0 on success (unlike most of OpenSSL). */
    if (SSL_CTX_set_alpn_protos(wrapper->ctx, lean_sarray_cptr(wire_obj),
                                (unsigned int)lean_sarray_size(wire_obj)) != 0) {
        return lean_io_result_mk_error(mk_io_error("SSL_CTX_set_alpn_protos failed"));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ────────────────────────────────────────────────────────────
 * Sessions and the resumable handshake
 *
 * A session is created once — SSL_new, SSL_set_fd, and the role — and the
 * handshake is then *stepped*: `linen_tls_handshake_nb` calls
 * SSL_do_handshake on the same SSL object each time, returning wantRead /
 * wantWrite until it completes. The caller waits for the socket in the
 * direction asked and steps again. This is the design of rust-openssl
 * (MidHandshakeSslStream::handshake), HsOpenSSL (sslBlock over one SSL) and
 * tokio-openssl. The previous *_nb entry points created a fresh SSL per call
 * and freed it on WANT_*, so a handshake needing more than one read could
 * never complete.
 * ──────────────────────────────────────────────────────────── */

static lean_obj_res linen_tls_new_session(b_lean_obj_arg ctx_obj, b_lean_obj_arg sock_obj,
                                          const char *hostname) {
    int fd = (int)(intptr_t)lean_get_external_data(sock_obj);
    ensure_classes();
    linen_ssl_ctx_t *ctx_wrapper = lean_get_external_data(ctx_obj);
    ERR_clear_error();
    SSL *ssl = SSL_new(ctx_wrapper->ctx);
    if (!ssl) return lean_io_result_mk_error(mk_io_error("SSL_new failed"));
    if (SSL_set_fd(ssl, fd) != 1) {
        SSL_free(ssl);
        return lean_io_result_mk_error(mk_io_error("SSL_set_fd failed"));
    }
    if (hostname) {
        /* SNI for virtual hosting, and the name the certificate must match. */
        SSL_set_tlsext_host_name(ssl, hostname);
        SSL_set1_host(ssl, hostname);
        SSL_set_connect_state(ssl);
    } else {
        SSL_set_accept_state(ssl);
    }
    linen_ssl_t *wrapper = malloc(sizeof(linen_ssl_t));
    if (!wrapper) {
        SSL_free(ssl);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ssl = ssl;
    wrapper->fd = fd;
    wrapper->failed = 0;
    return lean_io_result_mk_ok(lean_alloc_external(g_linen_ssl_class, wrapper));
}

/*
 * @[extern "linen_tls_session_new_server"]
 * opaque newServerSession : @& TLSContext → @& RawSocket → IO TLSSession
 */
LEAN_EXPORT lean_obj_res linen_tls_session_new_server(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg sock_obj, lean_obj_arg world) {
    return linen_tls_new_session(ctx_obj, sock_obj, NULL);
}

/*
 * @[extern "linen_tls_session_new_client"]
 * opaque newClientSession : @& TLSContext → @& RawSocket → @& String → IO TLSSession
 */
LEAN_EXPORT lean_obj_res linen_tls_session_new_client(
    b_lean_obj_arg ctx_obj, b_lean_obj_arg sock_obj, b_lean_obj_arg hostname_obj,
    lean_obj_arg world) {
    return linen_tls_new_session(ctx_obj, sock_obj, lean_string_cstr(hostname_obj));
}

/* ────────────────────────────────────────────────────────────
 * TLS read / write / close
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_tls_read"]
 * opaque tlsReadImpl : @& TLSSessionHandle.type → USize → IO ByteArray
 */
LEAN_EXPORT lean_obj_res linen_tls_read(
    b_lean_obj_arg ssl_obj,
    size_t maxlen,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl) {
        /* Return empty on closed session */
        lean_obj_res arr = lean_mk_empty_byte_array(lean_box(0));
        return lean_io_result_mk_ok(arr);
    }

    if (maxlen == 0) return lean_io_result_mk_ok(lean_mk_empty_byte_array(lean_box(0)));
    int cap = maxlen > (size_t)INT32_MAX ? INT32_MAX : (int)maxlen;
    lean_obj_res arr = lean_alloc_sarray(1, 0, (size_t)cap);

    ERR_clear_error();
    int n = SSL_read(wrapper->ssl, lean_sarray_cptr(arr), cap);
    if (n <= 0) {
        /* EOF or error — return empty array (and free the unused one,
         * which used to leak on every end of stream). */
        int err = SSL_get_error(wrapper->ssl, n);
        if (err != SSL_ERROR_ZERO_RETURN) wrapper->failed = 1;
        lean_dec(arr);
        return lean_io_result_mk_ok(lean_mk_empty_byte_array(lean_box(0)));
    }

    lean_sarray_set_size(arr, n);
    return lean_io_result_mk_ok(arr);
}

/*
 * @[extern "linen_tls_write"]
 * opaque tlsWriteImpl : @& TLSSessionHandle.type → @& ByteArray → IO Unit
 */
LEAN_EXPORT lean_obj_res linen_tls_write(
    b_lean_obj_arg ssl_obj,
    b_lean_obj_arg data_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("TLS write on closed session")));
    }

    size_t len = lean_sarray_size(data_obj);
    const uint8_t *buf = lean_sarray_cptr(data_obj);
    size_t written = 0;

    while (written < len) {
        size_t rest = len - written;
        ERR_clear_error();
        int n = SSL_write(wrapper->ssl, buf + written,
                          rest > (size_t)INT32_MAX ? INT32_MAX : (int)rest);
        if (n <= 0) {
            wrapper->failed = 1;
            return lean_io_result_mk_error(mk_io_error("SSL_write failed"));
        }
        written += n;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

/*
 * @[extern "linen_tls_close"]
 * opaque tlsCloseImpl : @& TLSSessionHandle.type → IO Unit
 */
LEAN_EXPORT lean_obj_res linen_tls_close(
    b_lean_obj_arg ssl_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (wrapper->ssl) {
        /* A fast, one-call shutdown: send close_notify without waiting for
         * the peer's. Never after a fatal error (OpenSSL forbids it). On a
         * non-blocking socket a WANT_* result is simply dropped — the
         * connection is closed next anyway. */
        if (!wrapper->failed && SSL_is_init_finished(wrapper->ssl)) {
            ERR_clear_error();
            SSL_shutdown(wrapper->ssl);
        }
        SSL_free(wrapper->ssl);
        wrapper->ssl = NULL;
    }
    ERR_clear_error();
    return lean_io_result_mk_ok(lean_box(0));
}

/* ────────────────────────────────────────────────────────────
 * Non-blocking TLS operations
 *
 * Return tagged TLSOutcome instead of throwing on WANT_READ/WRITE.
 * Tag encoding matches Lean inductive:
 *   tag 0 = .ok α           — ctor(0, 1, 0)[value]
 *   tag 1 = .wantRead       — ctor(1, 0, 0)
 *   tag 2 = .wantWrite      — ctor(2, 0, 0)
 *   tag 3 = .error IO.Error — ctor(3, 1, 0)[err]
 * ──────────────────────────────────────────────────────────── */

static lean_obj_res mk_tls_io_error(const char *msg) {
    return lean_mk_io_user_error(lean_mk_string(msg));
}

static lean_obj_res mk_tls_ssl_error(SSL *ssl, int ret) {
    int err = SSL_get_error(ssl, ret);
    char buf[256];
    unsigned long sslerr = ERR_get_error();
    if (sslerr) {
        ERR_error_string_n(sslerr, buf, sizeof(buf));
    } else {
        snprintf(buf, sizeof(buf), "SSL error %d", err);
    }
    return mk_tls_io_error(buf);
}

static lean_obj_res linen_tls_outcome(unsigned tag, lean_obj_res payload) {
    lean_obj_res r = lean_alloc_ctor(tag, payload ? 1 : 0, 0);
    if (payload) lean_ctor_set(r, 0, payload);
    return lean_io_result_mk_ok(r);
}

/* Whether a failed SSL_read is the peer going away without close_notify:
 * SYSCALL with an empty error queue and errno 0 before OpenSSL 3.0,
 * SSL_R_UNEXPECTED_EOF_WHILE_READING from 3.0. Treated as end of input,
 * like rust-openssl's `Read` impl: HTTP clients routinely close this way,
 * and HTTP framing (Content-Length, chunked) — not TLS — says whether a
 * body was complete. */
static int linen_tls_is_unexpected_eof(int err) {
    unsigned long e = ERR_peek_error();
    if (err == SSL_ERROR_SYSCALL && e == 0) return 1;
#ifdef SSL_R_UNEXPECTED_EOF_WHILE_READING
    if (err == SSL_ERROR_SSL && ERR_GET_REASON(e) == SSL_R_UNEXPECTED_EOF_WHILE_READING)
        return 1;
#endif
    return 0;
}

/*
 * @[extern "linen_tls_handshake_nb"]
 * opaque handshakeNB : @& TLSSession → IO (TLSOutcome Unit)
 *
 * One handshake step on the session's SSL object. Retry the *same session*
 * after waiting in the direction asked; never create a new one.
 */
LEAN_EXPORT lean_obj_res linen_tls_handshake_nb(b_lean_obj_arg ssl_obj, lean_obj_arg world) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl)
        return linen_tls_outcome(3, mk_tls_io_error("TLS handshake on closed session"));
    ERR_clear_error();
    int ret = SSL_do_handshake(wrapper->ssl);
    if (ret == 1) return linen_tls_outcome(0, lean_box(0));
    int err = SSL_get_error(wrapper->ssl, ret);
    if (err == SSL_ERROR_WANT_READ) return linen_tls_outcome(1, NULL);
    if (err == SSL_ERROR_WANT_WRITE) return linen_tls_outcome(2, NULL);
    wrapper->failed = 1;
    return linen_tls_outcome(3, mk_tls_ssl_error(wrapper->ssl, ret));
}

/*
 * @[extern "linen_tls_read_nb"]
 * opaque readNB : @& TLSSession → USize → IO (TLSOutcome ByteArray)
 *
 * `.ok` with data; `.ok` empty at end of input (close_notify, or the peer
 * closing without one); wantRead/wantWrite — a read can need to write — to
 * retry after waiting. Call this *before* waiting for readability: OpenSSL
 * may already hold decrypted bytes the socket will never signal.
 */
LEAN_EXPORT lean_obj_res linen_tls_read_nb(
    b_lean_obj_arg ssl_obj,
    size_t maxlen,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl)
        return linen_tls_outcome(3, mk_tls_io_error("TLS read on closed session"));
    if (maxlen == 0) return linen_tls_outcome(0, lean_mk_empty_byte_array(lean_box(0)));
    int cap = maxlen > (size_t)INT32_MAX ? INT32_MAX : (int)maxlen;
    lean_obj_res arr = lean_alloc_sarray(1, 0, (size_t)cap);
    ERR_clear_error();
    int n = SSL_read(wrapper->ssl, lean_sarray_cptr(arr), cap);
    if (n > 0) {
        lean_sarray_set_size(arr, (size_t)n);
        return linen_tls_outcome(0, arr);
    }
    lean_dec(arr);
    int err = SSL_get_error(wrapper->ssl, n);
    if (err == SSL_ERROR_WANT_READ) return linen_tls_outcome(1, NULL);
    if (err == SSL_ERROR_WANT_WRITE) return linen_tls_outcome(2, NULL);
    if (err == SSL_ERROR_ZERO_RETURN)
        return linen_tls_outcome(0, lean_mk_empty_byte_array(lean_box(0)));
    wrapper->failed = 1;
    if (linen_tls_is_unexpected_eof(err))
        return linen_tls_outcome(0, lean_mk_empty_byte_array(lean_box(0)));
    return linen_tls_outcome(3, mk_tls_ssl_error(wrapper->ssl, n));
}

/*
 * @[extern "linen_tls_write_nb"]
 * opaque writeNB : @& TLSSession → @& ByteArray → IO (TLSOutcome Unit)
 *
 * All-or-nothing (partial writes are off). On wantRead/wantWrite the caller
 * must repeat the *same* write — same bytes, same length — after waiting in
 * the direction asked, and do no other write on the session in between.
 */
LEAN_EXPORT lean_obj_res linen_tls_write_nb(
    b_lean_obj_arg ssl_obj,
    b_lean_obj_arg data_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl)
        return linen_tls_outcome(3, mk_tls_io_error("TLS write on closed session"));
    size_t len = lean_sarray_size(data_obj);
    if (len == 0) return linen_tls_outcome(0, lean_box(0));  /* SSL_write(…, 0) is an error */
    if (len > (size_t)INT32_MAX)
        return linen_tls_outcome(3, mk_tls_io_error("TLS write larger than 2 GiB"));
    ERR_clear_error();
    int n = SSL_write(wrapper->ssl, lean_sarray_cptr(data_obj), (int)len);
    if (n > 0) return linen_tls_outcome(0, lean_box(0));
    int err = SSL_get_error(wrapper->ssl, n);
    if (err == SSL_ERROR_WANT_READ) return linen_tls_outcome(1, NULL);
    if (err == SSL_ERROR_WANT_WRITE) return linen_tls_outcome(2, NULL);
    wrapper->failed = 1;
    return linen_tls_outcome(3, mk_tls_ssl_error(wrapper->ssl, n));
}

/* ────────────────────────────────────────────────────────────
 * TLS introspection
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_tls_get_version"]
 * opaque tlsGetVersionImpl : @& TLSSessionHandle.type → IO String
 */
LEAN_EXPORT lean_obj_res linen_tls_get_version(
    b_lean_obj_arg ssl_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    const char *ver = wrapper->ssl ? SSL_get_version(wrapper->ssl) : "unknown";
    return lean_io_result_mk_ok(lean_mk_string(ver));
}

/*
 * @[extern "linen_tls_get_alpn"]
 * opaque tlsGetAlpnImpl : @& TLSSessionHandle.type → IO (Option String)
 */
LEAN_EXPORT lean_obj_res linen_tls_get_alpn(
    b_lean_obj_arg ssl_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    const unsigned char *alpn = NULL;
    unsigned int alpn_len = 0;
    SSL_get0_alpn_selected(wrapper->ssl, &alpn, &alpn_len);

    if (alpn && alpn_len > 0) {
        lean_obj_res s = lean_mk_string_from_bytes((const char *)alpn, alpn_len);
        return lean_io_result_mk_ok(({lean_obj_res opt = lean_alloc_ctor(1, 1, 0); lean_ctor_set(opt, 0, s); opt;}));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ────────────────────────────────────────────────────────────
 * TLS client-side support
 *
 * For outgoing HTTPS connections: client context creation
 * (with system CA trust), and SSL_connect handshake with SNI.
 * ──────────────────────────────────────────────────────────── */

/*
 * CA bundles to fall back on when OpenSSL's compiled-in default file does not
 * exist — the usual case for the static OpenSSL a Lean toolchain links into
 * every executable, whose default paths are those of the machine it was built
 * on. Without this, every HTTPS call from a CI runner fails with "certificate
 * verify failed" until something exports SSL_CERT_FILE, and every consumer
 * ends up carrying its own copy of that step. Checked in order; the first
 * readable one is loaded *in addition to* the defaults.
 */
static const char *linen_ca_bundle_candidates[] = {
    "/etc/ssl/certs/ca-certificates.crt", /* Debian, Ubuntu, Alpine (ca-certificates) */
    "/etc/pki/tls/certs/ca-bundle.crt",   /* Fedora, RHEL, CentOS */
    "/etc/ssl/ca-bundle.pem",             /* openSUSE */
    "/etc/ssl/cert.pem",                  /* macOS, Alpine, the BSDs */
    NULL
};

/*
 * The bundle `linen_tls_add_fallback_ca` would load, or NULL when none is
 * needed or none exists: SSL_CERT_FILE set (non-empty) means the caller has
 * chosen, and an existing compiled-in default file means OpenSSL already has
 * one.
 */
static const char *linen_tls_fallback_ca_path(void) {
    const char *env = getenv(X509_get_default_cert_file_env());
    if (env && *env) return NULL;
    const char *def = X509_get_default_cert_file();
    if (def && access(def, R_OK) == 0) return NULL;
    for (int i = 0; linen_ca_bundle_candidates[i]; i++) {
        if (access(linen_ca_bundle_candidates[i], R_OK) == 0)
            return linen_ca_bundle_candidates[i];
    }
    return NULL;
}

/* Load the fallback bundle, if any. A failure to parse it is not fatal: the
 * defaults are still loaded, and verification reports what is missing. */
static void linen_tls_add_fallback_ca(SSL_CTX *ctx) {
    const char *path = linen_tls_fallback_ca_path();
    if (path) {
        if (SSL_CTX_load_verify_locations(ctx, path, NULL) != 1) ERR_clear_error();
    }
}

/*
 * @[extern "linen_tls_fallback_ca_bundle"]
 * opaque fallbackCaBundle : IO String
 *
 * The CA bundle client contexts load in addition to OpenSSL's defaults, or ""
 * when none is needed (SSL_CERT_FILE set, or the compiled-in default file
 * exists) or none of the known locations exists. For diagnostics and tests.
 */
LEAN_EXPORT lean_obj_res linen_tls_fallback_ca_bundle(lean_obj_arg world) {
    const char *path = linen_tls_fallback_ca_path();
    return lean_io_result_mk_ok(lean_mk_string(path ? path : ""));
}

/*
 * @[extern "linen_tls_client_ctx_create"]
 * opaque createClientContext : IO TLSContext
 *
 * Creates an SSL_CTX configured for TLS client mode.
 * Loads system default CA certificates for server verification, plus a
 * well-known system bundle when OpenSSL's compiled-in default file does not
 * exist (see linen_ca_bundle_candidates).
 * No client certificate is configured (mutual TLS not supported yet).
 */
LEAN_EXPORT lean_obj_res linen_tls_client_ctx_create(
    lean_obj_arg world
) {
    ensure_classes();

    SSL_CTX *ctx = SSL_CTX_new(TLS_method());
    if (!ctx) {
        return lean_io_result_mk_error(mk_io_error("SSL_CTX_new (client) failed"));
    }

    /* Set minimum TLS version to 1.2 */
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    linen_tls_set_modes(ctx);

    /* Load system default CA certificates for server verification */
    if (SSL_CTX_set_default_verify_paths(ctx) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Failed to load system CA certificates"));
    }
    /* ...and a system bundle, when the compiled-in default does not exist */
    linen_tls_add_fallback_ca(ctx);

    /* Enable server certificate verification */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

    linen_ssl_ctx_t *wrapper = malloc(sizeof(linen_ssl_ctx_t));
    if (!wrapper) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ctx = ctx;
    wrapper->alpn = NULL;
    wrapper->alpn_len = 0;

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_ctx_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_tls_client_ctx_create_with_ca"]
 * opaque createClientContextWithCA : @& String → IO TLSContext
 *
 * Creates an SSL_CTX configured for TLS client mode, trusting only the
 * CA certificate(s) found at `ca_path` instead of the system default
 * trust store. Used to connect to servers presenting a certificate
 * signed by a private or self-signed CA.
 */
LEAN_EXPORT lean_obj_res linen_tls_client_ctx_create_with_ca(
    b_lean_obj_arg ca_path_obj,
    lean_obj_arg world
) {
    const char *ca_path = lean_string_cstr(ca_path_obj);
    ensure_classes();

    SSL_CTX *ctx = SSL_CTX_new(TLS_method());
    if (!ctx) {
        return lean_io_result_mk_error(mk_io_error("SSL_CTX_new (client) failed"));
    }

    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    linen_tls_set_modes(ctx);

    if (SSL_CTX_load_verify_locations(ctx, ca_path, NULL) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Failed to load CA certificate"));
    }

    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

    linen_ssl_ctx_t *wrapper = malloc(sizeof(linen_ssl_ctx_t));
    if (!wrapper) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ctx = ctx;
    wrapper->alpn = NULL;
    wrapper->alpn_len = 0;

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_ctx_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

