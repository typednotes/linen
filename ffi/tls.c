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

/* ────────────────────────────────────────────────────────────
 * External classes for SSL_CTX and SSL
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_ssl_ctx_class = NULL;
static lean_external_class *g_linen_ssl_class = NULL;

typedef struct {
    SSL_CTX *ctx;
} linen_ssl_ctx_t;

typedef struct {
    SSL *ssl;
    int fd;  /* borrowed — not owned, closed by socket layer */
} linen_ssl_t;

static void linen_ssl_ctx_finalizer(void *ptr) {
    linen_ssl_ctx_t *c = (linen_ssl_ctx_t *)ptr;
    if (c) {
        if (c->ctx) SSL_CTX_free(c->ctx);
        free(c);
    }
}

static void linen_ssl_finalizer(void *ptr) {
    linen_ssl_t *s = (linen_ssl_t *)ptr;
    if (s) {
        if (s->ssl) {
            SSL_shutdown(s->ssl);
            SSL_free(s->ssl);
        }
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

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_ctx_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

/* ────────────────────────────────────────────────────────────
 * ALPN configuration (for HTTP/2 negotiation)
 * ──────────────────────────────────────────────────────────── */

static int alpn_select_cb(SSL *ssl, const unsigned char **out, unsigned char *outlen,
                          const unsigned char *in, unsigned int inlen, void *arg) {
    /* Prefer h2, fall back to http/1.1 */
    static const unsigned char h2[] = "\x02h2";
    static const unsigned char http11[] = "\x08http/1.1";

    if (SSL_select_next_proto((unsigned char **)out, outlen,
                              h2, sizeof(h2) - 1, in, inlen) == OPENSSL_NPN_NEGOTIATED) {
        return SSL_TLSEXT_ERR_OK;
    }
    if (SSL_select_next_proto((unsigned char **)out, outlen,
                              http11, sizeof(http11) - 1, in, inlen) == OPENSSL_NPN_NEGOTIATED) {
        return SSL_TLSEXT_ERR_OK;
    }
    return SSL_TLSEXT_ERR_NOACK;
}

/*
 * @[extern "linen_tls_ctx_set_alpn"]
 * opaque tlsCtxSetAlpnImpl : @& TLSContextHandle.type → IO Unit
 */
LEAN_EXPORT lean_obj_res linen_tls_ctx_set_alpn(
    b_lean_obj_arg ctx_obj,
    lean_obj_arg world
) {
    linen_ssl_ctx_t *wrapper = lean_get_external_data(ctx_obj);
    SSL_CTX_set_alpn_select_cb(wrapper->ctx, alpn_select_cb, NULL);
    return lean_io_result_mk_ok(lean_box(0));
}

/* ────────────────────────────────────────────────────────────
 * TLS handshake (accept)
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_tls_accept_socket"]
 * opaque tlsAcceptSocket : @& TLSContextHandle.type → @& RawSocket → IO TLSSessionHandle.type
 *
 * Performs a TLS server-side handshake on a Lean Socket external object.
 * Extracts the file descriptor from the external object (stored as (intptr_t)fd).
 */
LEAN_EXPORT lean_obj_res linen_tls_accept_socket(
    b_lean_obj_arg ctx_obj,
    b_lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    /* Extract fd from the Socket external object (same encoding as network.c) */
    int fd = (int)(intptr_t)lean_get_external_data(sock_obj);
    ensure_classes();

    linen_ssl_ctx_t *ctx_wrapper = lean_get_external_data(ctx_obj);
    SSL *ssl = SSL_new(ctx_wrapper->ctx);
    if (!ssl) {
        return lean_io_result_mk_error(mk_io_error("SSL_new failed"));
    }

    SSL_set_fd(ssl, (int)fd);

    int ret = SSL_accept(ssl);
    if (ret != 1) {
        int err = SSL_get_error(ssl, ret);
        SSL_free(ssl);
        char msg[128];
        snprintf(msg, sizeof(msg), "SSL_accept failed (error %d)", err);
        return lean_io_result_mk_error(mk_io_error(msg));
    }

    linen_ssl_t *wrapper = malloc(sizeof(linen_ssl_t));
    if (!wrapper) {
        SSL_free(ssl);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ssl = ssl;
    wrapper->fd = (int)fd;

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_class, wrapper);
    return lean_io_result_mk_ok(obj);
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

    lean_obj_res arr = lean_mk_empty_byte_array(lean_box(maxlen));
    uint8_t *buf = lean_sarray_cptr(arr);

    int n = SSL_read(wrapper->ssl, buf, (int)maxlen);
    if (n <= 0) {
        /* EOF or error — return empty array */
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
        int n = SSL_write(wrapper->ssl, buf + written, (int)(len - written));
        if (n <= 0) {
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
        SSL_shutdown(wrapper->ssl);
        SSL_free(wrapper->ssl);
        wrapper->ssl = NULL;
    }
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

/**
 * Non-blocking TLS handshake.
 * Returns TLSOutcome TLSSession.
 */
LEAN_EXPORT lean_obj_res linen_tls_accept_socket_nb(
    b_lean_obj_arg ctx_obj,
    b_lean_obj_arg sock_obj,
    lean_obj_arg world
) {
    int fd = (int)(intptr_t)lean_get_external_data(sock_obj);
    ensure_classes();

    linen_ssl_ctx_t *ctx_wrapper = lean_get_external_data(ctx_obj);
    SSL *ssl = SSL_new(ctx_wrapper->ctx);
    if (!ssl) {
        lean_obj_res err = mk_tls_io_error("SSL_new failed");
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }

    SSL_set_fd(ssl, fd);

    int ret = SSL_accept(ssl);
    if (ret == 1) {
        /* Handshake complete */
        linen_ssl_t *wrapper = malloc(sizeof(linen_ssl_t));
        if (!wrapper) {
            SSL_free(ssl);
            lean_obj_res err = mk_tls_io_error("malloc failed");
            lean_obj_res r = lean_alloc_ctor(3, 1, 0);
            lean_ctor_set(r, 0, err);
            return lean_io_result_mk_ok(r);
        }
        wrapper->ssl = ssl;
        wrapper->fd = fd;
        lean_obj_res session = lean_alloc_external(g_linen_ssl_class, wrapper);
        lean_obj_res r = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(r, 0, session);
        return lean_io_result_mk_ok(r);
    }

    int err = SSL_get_error(ssl, ret);
    if (err == SSL_ERROR_WANT_READ) {
        SSL_free(ssl);
        return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
    }
    if (err == SSL_ERROR_WANT_WRITE) {
        SSL_free(ssl);
        return lean_io_result_mk_ok(lean_alloc_ctor(2, 0, 0));
    }
    /* Real error */
    lean_obj_res e = mk_tls_ssl_error(ssl, ret);
    SSL_free(ssl);
    lean_obj_res r = lean_alloc_ctor(3, 1, 0);
    lean_ctor_set(r, 0, e);
    return lean_io_result_mk_ok(r);
}

/**
 * Non-blocking TLS read.
 * Returns TLSOutcome ByteArray.
 */
LEAN_EXPORT lean_obj_res linen_tls_read_nb(
    b_lean_obj_arg ssl_obj,
    size_t maxlen,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl) {
        lean_obj_res err = mk_tls_io_error("TLS read on closed session");
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }

    lean_obj_res arr = lean_mk_empty_byte_array(lean_box(maxlen));
    uint8_t *buf = lean_sarray_cptr(arr);

    int n = SSL_read(wrapper->ssl, buf, (int)maxlen);
    if (n > 0) {
        lean_sarray_set_size(arr, n);
        lean_obj_res r = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(r, 0, arr);
        return lean_io_result_mk_ok(r);
    }

    lean_dec(arr);
    int err = SSL_get_error(wrapper->ssl, n);
    if (err == SSL_ERROR_WANT_READ) {
        return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
    }
    if (err == SSL_ERROR_WANT_WRITE) {
        return lean_io_result_mk_ok(lean_alloc_ctor(2, 0, 0));
    }
    if (err == SSL_ERROR_ZERO_RETURN) {
        /* Peer closed — return empty ByteArray as .ok */
        lean_obj_res empty = lean_mk_empty_byte_array(lean_box(0));
        lean_obj_res r = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(r, 0, empty);
        return lean_io_result_mk_ok(r);
    }
    lean_obj_res e = mk_tls_ssl_error(wrapper->ssl, n);
    lean_obj_res r = lean_alloc_ctor(3, 1, 0);
    lean_ctor_set(r, 0, e);
    return lean_io_result_mk_ok(r);
}

/**
 * Non-blocking TLS write.
 * Returns TLSOutcome Unit.
 * Note: returns .ok with bytes written count for partial writes.
 */
LEAN_EXPORT lean_obj_res linen_tls_write_nb(
    b_lean_obj_arg ssl_obj,
    b_lean_obj_arg data_obj,
    lean_obj_arg world
) {
    linen_ssl_t *wrapper = lean_get_external_data(ssl_obj);
    if (!wrapper->ssl) {
        lean_obj_res err = mk_tls_io_error("TLS write on closed session");
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }

    size_t len = lean_sarray_size(data_obj);
    const uint8_t *buf = lean_sarray_cptr(data_obj);

    int n = SSL_write(wrapper->ssl, buf, (int)len);
    if (n > 0) {
        /* .ok Unit */
        lean_obj_res r = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(r, 0, lean_box(0));
        return lean_io_result_mk_ok(r);
    }

    int err = SSL_get_error(wrapper->ssl, n);
    if (err == SSL_ERROR_WANT_READ) {
        return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
    }
    if (err == SSL_ERROR_WANT_WRITE) {
        return lean_io_result_mk_ok(lean_alloc_ctor(2, 0, 0));
    }
    lean_obj_res e = mk_tls_ssl_error(wrapper->ssl, n);
    lean_obj_res r = lean_alloc_ctor(3, 1, 0);
    lean_ctor_set(r, 0, e);
    return lean_io_result_mk_ok(r);
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
 * @[extern "linen_tls_client_ctx_create"]
 * opaque createClientContext : IO TLSContext
 *
 * Creates an SSL_CTX configured for TLS client mode.
 * Loads system default CA certificates for server verification.
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

    /* Load system default CA certificates for server verification */
    if (SSL_CTX_set_default_verify_paths(ctx) != 1) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("Failed to load system CA certificates"));
    }

    /* Enable server certificate verification */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);

    linen_ssl_ctx_t *wrapper = malloc(sizeof(linen_ssl_ctx_t));
    if (!wrapper) {
        SSL_CTX_free(ctx);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ctx = ctx;

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

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_ctx_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_tls_connect_socket"]
 * opaque connectSocket : @& TLSContext → @& RawSocket → @& String → IO TLSSession
 *
 * Performs a blocking TLS client-side handshake on a connected socket.
 * Sets SNI (Server Name Indication) from the hostname parameter.
 */
LEAN_EXPORT lean_obj_res linen_tls_connect_socket(
    b_lean_obj_arg ctx_obj,
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg hostname_obj,
    lean_obj_arg world
) {
    int fd = (int)(intptr_t)lean_get_external_data(sock_obj);
    const char *hostname = lean_string_cstr(hostname_obj);
    ensure_classes();

    linen_ssl_ctx_t *ctx_wrapper = lean_get_external_data(ctx_obj);
    SSL *ssl = SSL_new(ctx_wrapper->ctx);
    if (!ssl) {
        return lean_io_result_mk_error(mk_io_error("SSL_new failed"));
    }

    SSL_set_fd(ssl, fd);

    /* Set SNI hostname for virtual hosting */
    SSL_set_tlsext_host_name(ssl, hostname);

    /* Set hostname for certificate verification (OpenSSL 1.1+) */
    SSL_set1_host(ssl, hostname);

    int ret = SSL_connect(ssl);
    if (ret != 1) {
        int err = SSL_get_error(ssl, ret);
        SSL_free(ssl);
        char msg[256];
        snprintf(msg, sizeof(msg), "SSL_connect failed (error %d)", err);
        return lean_io_result_mk_error(mk_io_error(msg));
    }

    linen_ssl_t *wrapper = malloc(sizeof(linen_ssl_t));
    if (!wrapper) {
        SSL_free(ssl);
        return lean_io_result_mk_error(mk_io_error("malloc failed"));
    }
    wrapper->ssl = ssl;
    wrapper->fd = fd;

    lean_obj_res obj = lean_alloc_external(g_linen_ssl_class, wrapper);
    return lean_io_result_mk_ok(obj);
}

/*
 * @[extern "linen_tls_connect_socket_nb"]
 * opaque connectSocketNB : @& TLSContext → @& RawSocket → @& String
 *                        → IO (TLSOutcome TLSSession)
 *
 * Non-blocking TLS client handshake. Returns TLSOutcome.
 */
LEAN_EXPORT lean_obj_res linen_tls_connect_socket_nb(
    b_lean_obj_arg ctx_obj,
    b_lean_obj_arg sock_obj,
    b_lean_obj_arg hostname_obj,
    lean_obj_arg world
) {
    int fd = (int)(intptr_t)lean_get_external_data(sock_obj);
    const char *hostname = lean_string_cstr(hostname_obj);
    ensure_classes();

    linen_ssl_ctx_t *ctx_wrapper = lean_get_external_data(ctx_obj);
    SSL *ssl = SSL_new(ctx_wrapper->ctx);
    if (!ssl) {
        lean_obj_res err = mk_tls_io_error("SSL_new failed");
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }

    SSL_set_fd(ssl, fd);
    SSL_set_tlsext_host_name(ssl, hostname);
    SSL_set1_host(ssl, hostname);

    int ret = SSL_connect(ssl);
    if (ret == 1) {
        /* Handshake complete */
        linen_ssl_t *wrapper = malloc(sizeof(linen_ssl_t));
        if (!wrapper) {
            SSL_free(ssl);
            lean_obj_res err = mk_tls_io_error("malloc failed");
            lean_obj_res r = lean_alloc_ctor(3, 1, 0);
            lean_ctor_set(r, 0, err);
            return lean_io_result_mk_ok(r);
        }
        wrapper->ssl = ssl;
        wrapper->fd = fd;
        lean_obj_res session = lean_alloc_external(g_linen_ssl_class, wrapper);
        lean_obj_res r = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(r, 0, session);
        return lean_io_result_mk_ok(r);
    }

    int err = SSL_get_error(ssl, ret);
    if (err == SSL_ERROR_WANT_READ) {
        SSL_free(ssl);
        return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
    }
    if (err == SSL_ERROR_WANT_WRITE) {
        SSL_free(ssl);
        return lean_io_result_mk_ok(lean_alloc_ctor(2, 0, 0));
    }
    /* Real error */
    lean_obj_res e = mk_tls_ssl_error(ssl, ret);
    SSL_free(ssl);
    lean_obj_res r = lean_alloc_ctor(3, 1, 0);
    lean_ctor_set(r, 0, e);
    return lean_io_result_mk_ok(r);
}
