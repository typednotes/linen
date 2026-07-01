/*
 * ffi/network.c — Cross-platform socket FFI for Lean 4
 *
 * Inspired by Haskell's `network` package. Supports:
 * - IPv4 and IPv6 (AF_INET, AF_INET6, AF_UNIX)
 * - TCP (SOCK_STREAM), UDP (SOCK_DGRAM), Raw (SOCK_RAW)
 * - Event multiplexing via kqueue (macOS) / epoll (Linux)
 * - Proper Lean external object pattern (lean_alloc_external)
 * - All errors surfaced as IO.Error (no crashes)
 *
 * Socket and EventLoop handles use lean_alloc_external with
 * lean_external_class, following the same pattern as IO.FS.Handle
 * in Lean's standard library. File descriptors are automatically
 * closed by the GC finalizer.
 *
 * Platform: macOS (Darwin) and Linux. No Windows support yet.
 */

#include <lean/lean.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/select.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdatomic.h>
#include <sched.h>

/* Platform-specific event multiplexing headers */
#ifdef __APPLE__
#include <sys/event.h>
#elif defined(__linux__)
#include <sys/epoll.h>
#endif

/* ────────────────────────────────────────────────────────────
 * External classes for Socket and EventLoop
 * ──────────────────────────────────────────────────────────── */

static lean_external_class *g_linen_socket_class = NULL;
static lean_external_class *g_linen_event_loop_class = NULL;

static void linen_socket_finalizer(void *ptr) {
    int fd = (int)(intptr_t)ptr;
    if (fd >= 0) close(fd);
}

static void linen_event_loop_finalizer(void *ptr) {
    int fd = (int)(intptr_t)ptr;
    if (fd >= 0) close(fd);
}

static void linen_noop_foreach(void *mod, b_lean_obj_arg fn) {
    /* nothing to traverse */
}

/* Ensure external classes are registered (lazy, thread-safe via atomic).
 * Uses a simple atomic flag to ensure registration happens exactly once.
 * The Lean runtime must be initialized before this is first called. */
static atomic_int g_linen_classes_initialized = 0;

static void linen_ensure_classes_initialized(void) {
    if (atomic_load_explicit(&g_linen_classes_initialized, memory_order_acquire)) return;
    g_linen_socket_class = lean_register_external_class(
        &linen_socket_finalizer, &linen_noop_foreach);
    g_linen_event_loop_class = lean_register_external_class(
        &linen_event_loop_finalizer, &linen_noop_foreach);
    atomic_store_explicit(&g_linen_classes_initialized, 1, memory_order_release);
}

/* Wrap a raw fd into a Lean external object */
static inline lean_obj_res mk_socket(int fd) {
    linen_ensure_classes_initialized();
    return lean_alloc_external(g_linen_socket_class, (void*)(intptr_t)fd);
}

/* Extract the fd from a Lean external object */
static inline int get_socket_fd(b_lean_obj_arg obj) {
    return (int)(intptr_t)lean_get_external_data(obj);
}

static inline lean_obj_res mk_event_loop(int fd) {
    linen_ensure_classes_initialized();
    return lean_alloc_external(g_linen_event_loop_class, (void*)(intptr_t)fd);
}

static inline int get_event_loop_fd(b_lean_obj_arg obj) {
    return (int)(intptr_t)lean_get_external_data(obj);
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean IO error from errno
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

static inline lean_obj_res mk_io_errno_error(void) {
    return mk_io_error(strerror(errno));
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean pair (Prod)
 *
 * The Lean compiler boxes USize/UInt values via lean_box()
 * when they appear in polymorphic positions (e.g. Prod fields).
 * All fields are passed as boxed lean_obj_arg values.
 *   Lean encodes (a, b) as ctor(0, 2, 0) with fields [a, b]
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_pair(lean_obj_arg fst, lean_obj_arg snd) {
    lean_object *p = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(p, 0, fst);
    lean_ctor_set(p, 1, snd);
    return p;
}

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean List cons / nil
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_list_nil(void) {
    return lean_box(0);
}

static inline lean_obj_res mk_list_cons(lean_obj_arg head, lean_obj_arg tail) {
    lean_object *c = lean_alloc_ctor(1, 2, 0);
    lean_ctor_set(c, 0, head);
    lean_ctor_set(c, 1, tail);
    return c;
}

/* ────────────────────────────────────────────────────────────
 * RecvBuffer: buffered reader for HTTP request parsing
 *
 * Reads socket data in 4KB chunks, scans for CRLF in C.
 * Eliminates per-byte syscall overhead (the #1 perf bottleneck).
 * Does NOT own the socket fd — Socket finalizer handles close.
 * ──────────────────────────────────────────────────────────── */

#define RECVBUF_SIZE 4096

typedef struct {
    int    fd;                  /* borrowed socket fd */
    size_t pos;                 /* next byte to consume */
    size_t len;                 /* bytes available in buf */
    uint8_t buf[RECVBUF_SIZE]; /* read buffer */
} linen_recvbuf_t;

static lean_external_class *g_linen_recvbuf_class = NULL;

static void linen_recvbuf_finalizer(void *ptr) {
    free(ptr); /* free the struct, NOT the fd */
}

static void linen_ensure_recvbuf_class(void) {
    if (g_linen_recvbuf_class) return;
    g_linen_recvbuf_class = lean_register_external_class(
        &linen_recvbuf_finalizer, &linen_noop_foreach);
}

static inline linen_recvbuf_t *get_recvbuf(b_lean_obj_arg obj) {
    return (linen_recvbuf_t *)lean_get_external_data(obj);
}

/* Refill the buffer from the socket. Returns bytes read, 0 on EOF, -1 on error.
 * Handles EAGAIN gracefully: retries up to 3 times with brief yields.
 * This allows the blocking RecvBuffer to work on non-blocking sockets
 * (after a readability event, EAGAIN is transient). */
static ssize_t recvbuf_fill(linen_recvbuf_t *rb) {
    /* Compact: move remaining bytes to front */
    if (rb->pos > 0 && rb->len > rb->pos) {
        size_t remaining = rb->len - rb->pos;
        memmove(rb->buf, rb->buf + rb->pos, remaining);
        rb->len = remaining;
        rb->pos = 0;
    } else if (rb->pos > 0) {
        rb->pos = 0;
        rb->len = 0;
    }
    size_t space = RECVBUF_SIZE - rb->len;
    if (space == 0) return 0;
    for (int retry = 0; retry < 4; retry++) {
        ssize_t n = recv(rb->fd, rb->buf + rb->len, space, 0);
        if (n >= 0) {
            if (n > 0) rb->len += (size_t)n;
            return n;
        }
        if (errno != EAGAIN && errno != EWOULDBLOCK) return -1;
        /* EAGAIN: brief yield then retry */
        if (retry < 3) sched_yield();
    }
    /* Still EAGAIN after retries — report as error */
    return -1;
}

/**
 * Create a RecvBuffer for a socket.
 */
LEAN_EXPORT lean_obj_res linen_recvbuf_create(b_lean_obj_arg sock) {
    linen_ensure_recvbuf_class();
    linen_recvbuf_t *rb = (linen_recvbuf_t *)calloc(1, sizeof(linen_recvbuf_t));
    if (!rb) return mk_io_error("recvbuf_create: out of memory");
    rb->fd = get_socket_fd(sock);
    return lean_io_result_mk_ok(
        lean_alloc_external(g_linen_recvbuf_class, rb));
}

/**
 * Read a CRLF-terminated line. Returns the line without CRLF.
 * Returns empty string on EOF. Scan+refill loop runs entirely in C.
 */
LEAN_EXPORT lean_obj_res linen_recvbuf_readline(b_lean_obj_arg buf) {
    linen_recvbuf_t *rb = get_recvbuf(buf);
    uint8_t line[8192];
    size_t line_len = 0;

    for (;;) {
        while (rb->pos < rb->len) {
            uint8_t c = rb->buf[rb->pos++];
            if (line_len > 0 && line[line_len - 1] == '\r' && c == '\n') {
                lean_object *s = lean_mk_string_from_bytes(
                    (const char *)line, line_len - 1);
                return lean_io_result_mk_ok(s);
            }
            if (line_len < sizeof(line) - 1) {
                line[line_len++] = c;
            } else {
                return mk_io_error("recvbuf_readline: line too long (>8KB)");
            }
        }
        ssize_t n = recvbuf_fill(rb);
        if (n < 0) return mk_io_errno_error();
        if (n == 0) {
            if (line_len == 0)
                return lean_io_result_mk_ok(lean_mk_string(""));
            lean_object *s = lean_mk_string_from_bytes(
                (const char *)line, line_len);
            return lean_io_result_mk_ok(s);
        }
    }
}

/**
 * Read exactly n bytes from the buffer. For request bodies.
 */
LEAN_EXPORT lean_obj_res linen_recvbuf_readn(b_lean_obj_arg buf, size_t n) {
    linen_recvbuf_t *rb = get_recvbuf(buf);
    lean_object *arr = lean_alloc_sarray(1, n, n);
    uint8_t *dst = lean_sarray_cptr(arr);
    size_t total = 0;

    while (total < n) {
        size_t avail = rb->len - rb->pos;
        if (avail > 0) {
            size_t to_copy = avail < (n - total) ? avail : (n - total);
            memcpy(dst + total, rb->buf + rb->pos, to_copy);
            rb->pos += to_copy;
            total += to_copy;
        }
        if (total >= n) break;
        ssize_t nr = recvbuf_fill(rb);
        if (nr < 0) { lean_dec(arr); return mk_io_errno_error(); }
        if (nr == 0) {
            lean_object *trimmed = lean_alloc_sarray(1, total, total);
            memcpy(lean_sarray_cptr(trimmed), dst, total);
            lean_dec(arr);
            return lean_io_result_mk_ok(trimmed);
        }
    }
    return lean_io_result_mk_ok(arr);
}

/* ────────────────────────────────────────────────────────────
 * Address family encoding: Family -> UInt8
 *   0 = AF_INET, 1 = AF_INET6, 2 = AF_UNIX
 * ──────────────────────────────────────────────────────────── */
static int family_to_af(uint8_t fam) {
    switch (fam) {
        case 0: return AF_INET;
        case 1: return AF_INET6;
        case 2: return AF_UNIX;
        default: return AF_INET;
    }
}

static uint8_t af_to_family(int af) {
    switch (af) {
        case AF_INET: return 0;
        case AF_INET6: return 1;
        case AF_UNIX: return 2;
        default: return 0;
    }
}

/* ────────────────────────────────────────────────────────────
 * Socket type encoding: SocketType -> UInt8
 *   0 = SOCK_STREAM, 1 = SOCK_DGRAM, 2 = SOCK_RAW
 * ──────────────────────────────────────────────────────────── */
static int socktype_to_st(uint8_t st) {
    switch (st) {
        case 0: return SOCK_STREAM;
        case 1: return SOCK_DGRAM;
        case 2: return SOCK_RAW;
        default: return SOCK_STREAM;
    }
}

/* ────────────────────────────────────────────────────────────
 * Helper: extract host string and port from a sockaddr
 * Fills `ip` buffer and sets `*out_port`.
 * ──────────────────────────────────────────────────────────── */
static void sockaddr_to_strings(struct sockaddr_storage *addr, char *ip, size_t ip_len, uint16_t *out_port) {
    *out_port = 0;

    if (addr->ss_family == AF_INET) {
        struct sockaddr_in *sin = (struct sockaddr_in *)addr;
        inet_ntop(AF_INET, &sin->sin_addr, ip, (socklen_t)ip_len);
        *out_port = ntohs(sin->sin_port);
    } else if (addr->ss_family == AF_INET6) {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)addr;
        inet_ntop(AF_INET6, &sin6->sin6_addr, ip, (socklen_t)ip_len);
        *out_port = ntohs(sin6->sin6_port);
    } else if (addr->ss_family == AF_UNIX) {
        struct sockaddr_un *sun = (struct sockaddr_un *)addr;
        strncpy(ip, sun->sun_path, ip_len - 1);
        ip[ip_len - 1] = '\0';
    } else {
        strcpy(ip, "unknown");
    }
}

/* ────────────────────────────────────────────────────────────
 * Helper: resolve host+port to sockaddr_storage
 * Tries getaddrinfo for both IPv4 and IPv6.
 * ──────────────────────────────────────────────────────────── */
static int resolve_addr(const char *host, uint16_t port, int family_hint,
                        struct sockaddr_storage *out, socklen_t *outlen) {
    /* Fast path: try inet_pton for numeric addresses */
    if (family_hint == AF_INET || family_hint == AF_UNSPEC) {
        struct sockaddr_in sin;
        memset(&sin, 0, sizeof(sin));
        sin.sin_family = AF_INET;
        sin.sin_port = htons(port);
        if (inet_pton(AF_INET, host, &sin.sin_addr) == 1) {
            memcpy(out, &sin, sizeof(sin));
            *outlen = sizeof(sin);
            return 0;
        }
    }
    if (family_hint == AF_INET6 || family_hint == AF_UNSPEC) {
        struct sockaddr_in6 sin6;
        memset(&sin6, 0, sizeof(sin6));
        sin6.sin6_family = AF_INET6;
        sin6.sin6_port = htons(port);
        if (inet_pton(AF_INET6, host, &sin6.sin6_addr) == 1) {
            memcpy(out, &sin6, sizeof(sin6));
            *outlen = sizeof(sin6);
            return 0;
        }
    }

    /* Fall back to getaddrinfo for hostnames */
    struct addrinfo hints, *res;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = family_hint;
    hints.ai_socktype = SOCK_STREAM;

    char portstr[16];
    snprintf(portstr, sizeof(portstr), "%u", port);

    int ret = getaddrinfo(host, portstr, &hints, &res);
    if (ret != 0) return -1;

    memcpy(out, res->ai_addr, res->ai_addrlen);
    *outlen = res->ai_addrlen;
    freeaddrinfo(res);
    return 0;
}

/* ================================================================
 * RESOURCE LIMITS
 * ================================================================ */

/**
 * Best-effort raise of the soft RLIMIT_NOFILE (open-file limit) toward
 * `target`, clamped to the hard limit. Returns the resulting soft limit so a
 * caller can scale its workload to fit. Never throws — on any failure it just
 * reports the current soft limit.
 */
LEAN_EXPORT lean_obj_res linen_set_fd_limit(size_t target) {
    struct rlimit rl;
    if (getrlimit(RLIMIT_NOFILE, &rl) != 0) {
        return lean_io_result_mk_ok(lean_box((size_t)0));
    }
    rlim_t want = (rlim_t)target;
    if (rl.rlim_max != RLIM_INFINITY && want > rl.rlim_max) want = rl.rlim_max;
    struct rlimit nrl = rl;
    nrl.rlim_cur = want;
    if (setrlimit(RLIMIT_NOFILE, &nrl) != 0) {
        /* macOS may reject values above kern.maxfilesperproc; try a cap */
        nrl.rlim_cur = (want > 10240) ? 10240 : want;
        setrlimit(RLIMIT_NOFILE, &nrl);
    }
    getrlimit(RLIMIT_NOFILE, &rl);
    return lean_io_result_mk_ok(lean_box((size_t)rl.rlim_cur));
}

/**
 * Number of online CPUs (≈ the size of Lean's default worker pool). Used to
 * report the green side's OS-thread budget. Returns at least 1.
 */
LEAN_EXPORT lean_obj_res linen_num_cpus(void) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 1) n = 1;
    return lean_io_result_mk_ok(lean_box((size_t)n));
}

/* ================================================================
 * SOCKET CREATION AND MANAGEMENT
 * ================================================================ */

/**
 * socket(domain, type, protocol) -> Socket
 * domain: 0=AF_INET, 1=AF_INET6, 2=AF_UNIX
 * type:   0=SOCK_STREAM, 1=SOCK_DGRAM, 2=SOCK_RAW
 */
LEAN_EXPORT lean_obj_res linen_socket_create(uint8_t domain, uint8_t socktype) {
    int af = family_to_af(domain);
    int st = socktype_to_st(socktype);
    int fd = socket(af, st, 0);
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(mk_socket(fd));
}

/**
 * close(sock) — the finalizer also closes, but explicit close is preferred.
 * We set the fd to -1 after closing to avoid double-close by the finalizer.
 */
LEAN_EXPORT lean_obj_res linen_socket_close(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    if (fd >= 0) {
        if (close(fd) < 0) {
            return mk_io_errno_error();
        }
        /* Prevent the finalizer from closing again.
         * Note: we use lean_get_external_data to get a pointer we can update.
         * For external objects storing data as (void*)(intptr_t)fd, we update
         * the data pointer directly. */
        ((lean_external_object *)sock)->m_data = (void*)(intptr_t)(-1);
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * bind(sock, host, port) -- supports IPv4, IPv6, and numeric addresses
 */
LEAN_EXPORT lean_obj_res linen_socket_bind(b_lean_obj_arg sock, lean_obj_arg host, uint16_t port) {
    int fd = get_socket_fd(sock);
    const char *h = lean_string_cstr(host);

    /* Determine socket family from the fd using getsockname */
    struct sockaddr_storage ss;
    socklen_t sslen;
    int sock_domain = AF_UNSPEC;
    {
        struct sockaddr_storage tmp;
        socklen_t tmplen = sizeof(tmp);
        if (getsockname(fd, (struct sockaddr *)&tmp, &tmplen) == 0) {
            sock_domain = tmp.ss_family;
        }
    }

    if (resolve_addr(h, port, sock_domain, &ss, &sslen) < 0) {
        return mk_io_error("bind: invalid address or hostname resolution failed");
    }

    if (bind(fd, (struct sockaddr *)&ss, sslen) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * listen(sock, backlog)
 */
LEAN_EXPORT lean_obj_res linen_socket_listen(b_lean_obj_arg sock, size_t backlog) {
    int fd = get_socket_fd(sock);
    if (listen(fd, (int)backlog) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * accept(sock) -> (Socket, (remote_host, remote_port))
 *
 * Returns nested pair (Socket x (String x USize))
 * encoded as ctor(0,2,0)[socket, ctor(0,2,0)[host, port]]
 *
 * Supports both IPv4 and IPv6 peers.
 */
LEAN_EXPORT lean_obj_res linen_socket_accept(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    int client = accept(fd, (struct sockaddr *)&addr, &addrlen);
    if (client < 0) {
        return mk_io_errno_error();
    }
    /* Set TCP_NODELAY on the accepted socket to disable Nagle's algorithm.
     * This eliminates 40ms delays for small HTTP responses. */
    int one = 1;
    setsockopt(client, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    /* Return just the client socket — use getpeername for address */
    return lean_io_result_mk_ok(mk_socket(client));
}

/**
 * connect(sock, host, port) -- supports IPv4 and IPv6
 */
LEAN_EXPORT lean_obj_res linen_socket_connect(b_lean_obj_arg sock, lean_obj_arg host, uint16_t port) {
    int fd = get_socket_fd(sock);
    const char *h = lean_string_cstr(host);

    struct sockaddr_storage ss;
    socklen_t sslen;
    int sock_domain = AF_UNSPEC;
    {
        struct sockaddr_storage tmp;
        socklen_t tmplen = sizeof(tmp);
        if (getsockname(fd, (struct sockaddr *)&tmp, &tmplen) == 0 && tmp.ss_family != 0) {
            sock_domain = tmp.ss_family;
        }
    }

    if (resolve_addr(h, port, sock_domain, &ss, &sslen) < 0) {
        return mk_io_error("connect: invalid address or hostname resolution failed");
    }

    if (connect(fd, (struct sockaddr *)&ss, sslen) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * SEND / RECV (TCP)
 * ================================================================ */

/**
 * send(sock, data) -> bytes_sent
 */
LEAN_EXPORT lean_obj_res linen_socket_send(b_lean_obj_arg sock, b_lean_obj_arg data) {
    int fd = get_socket_fd(sock);
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    ssize_t sent = send(fd, buf, len, 0);
    if (sent < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)sent));
}

/**
 * recv(sock, maxlen) -> ByteArray
 */
LEAN_EXPORT lean_obj_res linen_socket_recv(b_lean_obj_arg sock, size_t maxlen) {
    int fd = get_socket_fd(sock);
    uint8_t *buf = malloc(maxlen);
    if (!buf) {
        return mk_io_error("recv: malloc failed");
    }
    ssize_t n = recv(fd, buf, maxlen, 0);
    if (n < 0) {
        free(buf);
        return mk_io_errno_error();
    }
    lean_object *arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
    memcpy(lean_sarray_cptr(arr), buf, (size_t)n);
    free(buf);
    return lean_io_result_mk_ok(arr);
}

/**
 * sendall(sock, data) — loop until all bytes are sent.
 * Returns Unit on success, IO error on failure.
 */
LEAN_EXPORT lean_obj_res linen_socket_sendall(b_lean_obj_arg sock, b_lean_obj_arg data) {
    int fd = get_socket_fd(sock);
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    size_t total_sent = 0;
    while (total_sent < len) {
        ssize_t n = send(fd, buf + total_sent, len - total_sent, 0);
        if (n < 0) {
            return mk_io_errno_error();
        }
        if (n == 0) {
            return mk_io_error("sendall: connection closed");
        }
        total_sent += (size_t)n;
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/* ================================================================
 * UDP: sendto / recvfrom
 * ================================================================ */

/**
 * sendto(sock, data, host, port) -> bytes_sent
 */
LEAN_EXPORT lean_obj_res linen_socket_sendto(b_lean_obj_arg sock, b_lean_obj_arg data,
                                             lean_obj_arg host, uint16_t port) {
    int fd = get_socket_fd(sock);
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    const char *h = lean_string_cstr(host);

    struct sockaddr_storage ss;
    socklen_t sslen;
    if (resolve_addr(h, port, AF_UNSPEC, &ss, &sslen) < 0) {
        return mk_io_error("sendto: invalid address");
    }

    ssize_t sent = sendto(fd, buf, len, 0, (struct sockaddr *)&ss, sslen);
    if (sent < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box((size_t)sent));
}

/**
 * recvfrom(sock, maxlen) -> (ByteArray, (host_string, port))
 * Returns nested pair: (ByteArray x (String x Nat))
 */
LEAN_EXPORT lean_obj_res linen_socket_recvfrom(b_lean_obj_arg sock, size_t maxlen) {
    int fd = get_socket_fd(sock);
    uint8_t *buf = malloc(maxlen);
    if (!buf) {
        return mk_io_error("recvfrom: malloc failed");
    }
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    ssize_t n = recvfrom(fd, buf, maxlen, 0, (struct sockaddr *)&addr, &addrlen);
    if (n < 0) {
        free(buf);
        return mk_io_errno_error();
    }

    lean_object *arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
    memcpy(lean_sarray_cptr(arr), buf, (size_t)n);
    free(buf);

    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t rport = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &rport);
    lean_obj_res addr_pair = mk_pair(lean_mk_string(ip), lean_box((size_t)rport));
    lean_obj_res result = mk_pair(arr, addr_pair);
    return lean_io_result_mk_ok(result);
}

/* ================================================================
 * SOCKET OPTIONS
 * ================================================================ */

/**
 * setsockopt SO_REUSEADDR
 */
LEAN_EXPORT lean_obj_res linen_socket_set_reuseaddr(b_lean_obj_arg sock, uint8_t enable) {
    int fd = get_socket_fd(sock);
    int val = enable ? 1 : 0;
    if (setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt TCP_NODELAY
 */
LEAN_EXPORT lean_obj_res linen_socket_set_nodelay(b_lean_obj_arg sock, uint8_t enable) {
    int fd = get_socket_fd(sock);
    int val = enable ? 1 : 0;
    if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * Set non-blocking mode
 */
LEAN_EXPORT lean_obj_res linen_socket_set_nonblocking(b_lean_obj_arg sock, uint8_t enable) {
    int fd = get_socket_fd(sock);
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) {
        return mk_io_errno_error();
    }
    if (enable) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }
    if (fcntl(fd, F_SETFL, flags) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_KEEPALIVE
 */
LEAN_EXPORT lean_obj_res linen_socket_set_keepalive(b_lean_obj_arg sock, uint8_t enable) {
    int fd = get_socket_fd(sock);
    int val = enable ? 1 : 0;
    if (setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_LINGER
 */
LEAN_EXPORT lean_obj_res linen_socket_set_linger(b_lean_obj_arg sock, uint8_t enable, size_t seconds) {
    int fd = get_socket_fd(sock);
    struct linger lg;
    lg.l_onoff = enable ? 1 : 0;
    lg.l_linger = (int)seconds;
    if (setsockopt(fd, SOL_SOCKET, SO_LINGER, &lg, sizeof(lg)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_RCVBUF
 */
LEAN_EXPORT lean_obj_res linen_socket_set_recvbuf(b_lean_obj_arg sock, size_t sz) {
    int fd = get_socket_fd(sock);
    int val = (int)sz;
    if (setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * setsockopt SO_SNDBUF
 */
LEAN_EXPORT lean_obj_res linen_socket_set_sendbuf(b_lean_obj_arg sock, size_t sz) {
    int fd = get_socket_fd(sock);
    int val = (int)sz;
    if (setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &val, sizeof(val)) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * shutdown(sock, how)
 * how: 0=SHUT_RD, 1=SHUT_WR, 2=SHUT_RDWR
 */
LEAN_EXPORT lean_obj_res linen_socket_shutdown(b_lean_obj_arg sock, uint8_t how) {
    int fd = get_socket_fd(sock);
    int shuthow;
    switch (how) {
        case 0: shuthow = SHUT_RD; break;
        case 1: shuthow = SHUT_WR; break;
        case 2: shuthow = SHUT_RDWR; break;
        default: return mk_io_error("shutdown: invalid how value");
    }
    if (shutdown(fd, shuthow) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
}

/**
 * getpeername_host(sock) -> String
 */
LEAN_EXPORT lean_obj_res linen_socket_getpeername_host(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getpeername(fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &port);
    return lean_io_result_mk_ok(lean_mk_string(ip));
}

/**
 * getpeername_port(sock) -> UInt16
 */
LEAN_EXPORT lean_obj_res linen_socket_getpeername_port(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getpeername(fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &port);
    return lean_io_result_mk_ok(lean_box((uint32_t)port));
}

/**
 * getsockname_host(sock) -> String
 */
LEAN_EXPORT lean_obj_res linen_socket_getsockname_host(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getsockname(fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &port);
    return lean_io_result_mk_ok(lean_mk_string(ip));
}

/**
 * getsockname_port(sock) -> UInt16
 */
LEAN_EXPORT lean_obj_res linen_socket_getsockname_port(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    if (getsockname(fd, (struct sockaddr *)&addr, &addrlen) < 0) {
        return mk_io_errno_error();
    }
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &port);
    return lean_io_result_mk_ok(lean_box((uint32_t)port));
}

/* ================================================================
 * GETADDRINFO
 * ================================================================ */

/**
 * getaddrinfo(node, service) -> List (family x (host x port))
 *
 * Returns nested pairs, not flat 3-tuples.
 * Supports both IPv4 and IPv6 results.
 */
LEAN_EXPORT lean_obj_res linen_getaddrinfo(lean_obj_arg node, lean_obj_arg service) {
    struct addrinfo hints, *res, *p;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    const char *n = lean_string_cstr(node);
    const char *s = lean_string_cstr(service);

    int ret = getaddrinfo(n, s, &hints, &res);
    if (ret != 0) {
        return mk_io_error(gai_strerror(ret));
    }

    /* Build a Lean list in reverse order (prepend) */
    lean_object *list = mk_list_nil();
    for (p = res; p != NULL; p = p->ai_next) {
        char ip[INET6_ADDRSTRLEN];
        uint16_t port = 0;
        uint8_t family;

        if (p->ai_family == AF_INET) {
            struct sockaddr_in *sin = (struct sockaddr_in *)p->ai_addr;
            inet_ntop(AF_INET, &sin->sin_addr, ip, sizeof(ip));
            port = ntohs(sin->sin_port);
            family = 0;
        } else if (p->ai_family == AF_INET6) {
            struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *)p->ai_addr;
            inet_ntop(AF_INET6, &sin6->sin6_addr, ip, sizeof(ip));
            port = ntohs(sin6->sin6_port);
            family = 1;
        } else {
            continue;
        }

        /* Nested pair: (family, (host, port)) */
        lean_obj_res inner = mk_pair(lean_mk_string(ip), lean_box((size_t)port));
        lean_obj_res entry = mk_pair(lean_box((size_t)family), inner);
        list = mk_list_cons(entry, list);
    }

    freeaddrinfo(res);
    return lean_io_result_mk_ok(list);
}

/* ================================================================
 * NON-BLOCKING SOCKET OPERATIONS
 *
 * Return tagged Lean inductive values instead of throwing on EAGAIN.
 * Tag encoding matches the Lean inductive declaration order.
 * ================================================================ */

/* Helper: make a Lean IO.Error object from errno (without wrapping in IO result) */
static inline lean_obj_res mk_io_error_obj(const char *msg) {
    return lean_mk_io_user_error(lean_mk_string(msg));
}

static inline lean_obj_res mk_io_errno_error_obj(void) {
    return mk_io_error_obj(strerror(errno));
}

/**
 * Non-blocking accept.
 * Returns AcceptOutcome:
 *   tag 0 = .accepted (Socket .connected) SockAddr  — ctor(0, 2, 0)[socket, sockaddr]
 *   tag 1 = .wouldBlock                             — ctor(1, 0, 0)
 *   tag 2 = .error IO.Error                         — ctor(2, 1, 0)[err]
 *
 * SockAddr is a ctor(0, 2, sizeof(UInt16))[host, port]  (port is scalar)
 */
LEAN_EXPORT lean_obj_res linen_socket_accept_nb(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    int client = accept(fd, (struct sockaddr *)&addr, &addrlen);
    if (client < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            /* tag 1 = wouldBlock */
            return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
        }
        /* tag 2 = error */
        lean_obj_res err = mk_io_errno_error_obj();
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }
    /* Set TCP_NODELAY on accepted socket (eliminates 40ms Nagle delay).
     * Do NOT set O_NONBLOCK — the caller decides blocking mode. */
    int one = 1;
    setsockopt(client, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

    /* Build SockAddr */
    char ip[INET6_ADDRSTRLEN + 1];
    uint16_t port = 0;
    sockaddr_to_strings(&addr, ip, sizeof(ip), &port);
    lean_obj_res sa = lean_alloc_ctor(0, 1, 2);  /* SockAddr: 1 obj field (host), 2 scalar bytes (UInt16) */
    lean_ctor_set(sa, 0, lean_mk_string(ip));
    lean_ctor_set_uint16(sa, sizeof(void*), port);

    /* tag 0 = accepted socket sockaddr */
    lean_obj_res r = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(r, 0, mk_socket(client));
    lean_ctor_set(r, 1, sa);
    return lean_io_result_mk_ok(r);
}

/**
 * Non-blocking connect. Sets O_NONBLOCK on the socket before connecting.
 * Returns ConnectOutcome:
 *   tag 0 = .connected (Socket .connected)    — ctor(0, 1, 0)[socket]
 *   tag 1 = .inProgress (Socket .connecting)  — ctor(1, 1, 0)[socket]
 *   tag 2 = .refused IO.Error                 — ctor(2, 1, 0)[err]
 *
 * Note: the Lean Socket type is a struct with a single RawSocket field.
 * At the Lean level, the caller must reinterpret the raw socket with
 * the appropriate phantom state.  The C layer returns the raw socket.
 */
LEAN_EXPORT lean_obj_res linen_socket_connect_nb(b_lean_obj_arg sock, lean_obj_arg host, uint16_t port) {
    int fd = get_socket_fd(sock);
    const char *h = lean_string_cstr(host);

    /* Set non-blocking */
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags >= 0) fcntl(fd, F_SETFL, flags | O_NONBLOCK);

    struct sockaddr_storage ss;
    socklen_t sslen;
    int sock_domain = AF_UNSPEC;
    {
        struct sockaddr_storage tmp;
        socklen_t tmplen = sizeof(tmp);
        if (getsockname(fd, (struct sockaddr *)&tmp, &tmplen) == 0 && tmp.ss_family != 0) {
            sock_domain = tmp.ss_family;
        }
    }

    if (resolve_addr(h, port, sock_domain, &ss, &sslen) < 0) {
        lean_obj_res err = mk_io_error_obj("connect: invalid address or hostname resolution failed");
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }

    if (connect(fd, (struct sockaddr *)&ss, sslen) < 0) {
        if (errno == EINPROGRESS) {
            /* tag 1 = inProgress — return the raw socket handle */
            lean_inc(sock);
            lean_obj_res r = lean_alloc_ctor(1, 1, 0);
            lean_ctor_set(r, 0, sock);
            return lean_io_result_mk_ok(r);
        }
        lean_obj_res err = mk_io_errno_error_obj();
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }
    /* Connected immediately */
    lean_inc(sock);
    lean_obj_res r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, sock);
    return lean_io_result_mk_ok(r);
}

/**
 * Check whether a non-blocking connect completed.
 * Call after the event loop reports the socket is writable.
 * Returns ConnectOutcome (same encoding as connect_nb).
 */
LEAN_EXPORT lean_obj_res linen_socket_connect_finish(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    int err = 0;
    socklen_t len = sizeof(err);
    if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &len) < 0) {
        lean_obj_res e = mk_io_errno_error_obj();
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, e);
        return lean_io_result_mk_ok(r);
    }
    if (err != 0) {
        lean_obj_res e = mk_io_error_obj(strerror(err));
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, e);
        return lean_io_result_mk_ok(r);
    }
    /* Connected */
    lean_inc(sock);
    lean_obj_res r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, sock);
    return lean_io_result_mk_ok(r);
}

/**
 * Wait for a socket to become readable and/or writable using select().
 * `mode`: 0 = read, 1 = write, 2 = both.
 * `timeout_ms`: timeout in milliseconds.
 * Returns PollOutcome:
 *   tag 0 = .ready          — ctor(0, 0, 0)
 *   tag 1 = .timeout        — ctor(1, 0, 0)
 *   tag 2 = .error IO.Error — ctor(2, 1, 0)[err]
 */
LEAN_EXPORT lean_obj_res linen_socket_poll(b_lean_obj_arg sock, uint8_t mode, uint32_t timeout_ms) {
    int fd = get_socket_fd(sock);
    fd_set rfds, wfds;
    FD_ZERO(&rfds);
    FD_ZERO(&wfds);
    if (mode == 0 || mode == 2) FD_SET(fd, &rfds);
    if (mode == 1 || mode == 2) FD_SET(fd, &wfds);
    struct timeval tv;
    tv.tv_sec = timeout_ms / 1000;
    tv.tv_usec = (timeout_ms % 1000) * 1000;
    int sel = select(fd + 1,
                     (mode == 0 || mode == 2) ? &rfds : NULL,
                     (mode == 1 || mode == 2) ? &wfds : NULL,
                     NULL, &tv);
    if (sel > 0) {
        return lean_io_result_mk_ok(lean_alloc_ctor(0, 0, 0));  /* .ready */
    }
    if (sel == 0) {
        return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));  /* .timeout */
    }
    lean_obj_res e = mk_io_errno_error_obj();
    lean_obj_res r = lean_alloc_ctor(2, 1, 0);
    lean_ctor_set(r, 0, e);
    return lean_io_result_mk_ok(r);  /* .error */
}

/**
 * Non-blocking send.
 * Returns SendOutcome:
 *   tag 0 = .sent Nat              — ctor(0, 1, 0)[n]
 *   tag 1 = .wouldBlock            — ctor(1, 0, 0)
 *   tag 2 = .error IO.Error        — ctor(2, 1, 0)[err]
 */
LEAN_EXPORT lean_obj_res linen_socket_send_nb(b_lean_obj_arg sock, b_lean_obj_arg data) {
    int fd = get_socket_fd(sock);
    size_t len = lean_sarray_size(data);
    const uint8_t *buf = lean_sarray_cptr(data);
    ssize_t sent = send(fd, buf, len, MSG_DONTWAIT);
    if (sent < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
        }
        lean_obj_res err = mk_io_errno_error_obj();
        lean_obj_res r = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }
    /* tag 0 = sent n */
    lean_obj_res r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, lean_box((size_t)sent));
    return lean_io_result_mk_ok(r);
}

/**
 * Non-blocking recv.
 * Returns RecvOutcome:
 *   tag 0 = .data ByteArray        — ctor(0, 1, 0)[arr]
 *   tag 1 = .wouldBlock            — ctor(1, 0, 0)
 *   tag 2 = .eof                   — ctor(2, 0, 0)
 *   tag 3 = .error IO.Error        — ctor(3, 1, 0)[err]
 */
LEAN_EXPORT lean_obj_res linen_socket_recv_nb(b_lean_obj_arg sock, size_t maxlen) {
    int fd = get_socket_fd(sock);
    uint8_t *buf = malloc(maxlen);
    if (!buf) {
        lean_obj_res err = mk_io_error_obj("recv: malloc failed");
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }
    ssize_t n = recv(fd, buf, maxlen, MSG_DONTWAIT);
    if (n < 0) {
        free(buf);
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return lean_io_result_mk_ok(lean_alloc_ctor(1, 0, 0));
        }
        lean_obj_res err = mk_io_errno_error_obj();
        lean_obj_res r = lean_alloc_ctor(3, 1, 0);
        lean_ctor_set(r, 0, err);
        return lean_io_result_mk_ok(r);
    }
    if (n == 0) {
        free(buf);
        /* tag 2 = eof */
        return lean_io_result_mk_ok(lean_alloc_ctor(2, 0, 0));
    }
    /* tag 0 = data */
    lean_object *arr = lean_alloc_sarray(1, (size_t)n, (size_t)n);
    memcpy(lean_sarray_cptr(arr), buf, (size_t)n);
    free(buf);
    lean_obj_res r = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(r, 0, arr);
    return lean_io_result_mk_ok(r);
}

/**
 * Extract raw fd from a socket external object. For EventLoop correlation.
 */
LEAN_EXPORT lean_obj_res linen_socket_get_fd(b_lean_obj_arg sock) {
    int fd = get_socket_fd(sock);
    return lean_io_result_mk_ok(lean_box((size_t)(unsigned)fd));
}

/* ================================================================
 * NON-BLOCKING RECVBUFFER
 * ================================================================ */

/**
 * Non-blocking refill. Returns:
 *   > 0 : bytes read
 *     0 : EOF
 *    -1 : EAGAIN (no data available)
 *    -2 : real error
 */
static ssize_t recvbuf_fill_nb(linen_recvbuf_t *rb) {
    /* Compact: move remaining bytes to front */
    if (rb->pos > 0 && rb->len > rb->pos) {
        size_t remaining = rb->len - rb->pos;
        memmove(rb->buf, rb->buf + rb->pos, remaining);
        rb->len = remaining;
        rb->pos = 0;
    } else if (rb->pos > 0) {
        rb->pos = 0;
        rb->len = 0;
    }
    size_t space = RECVBUF_SIZE - rb->len;
    if (space == 0) return 0;
    ssize_t n = recv(rb->fd, rb->buf + rb->len, space, MSG_DONTWAIT);
    if (n > 0) rb->len += (size_t)n;
    if (n < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) return -1;
        return -2;
    }
    return n;
}

/**
 * Non-blocking readline. Returns Option String:
 *   some line  — complete CRLF-terminated line (without CRLF)
 *   none       — need more data (EAGAIN on underlying socket)
 *
 * Encoding: Option α = none (box 0) | some a (ctor(1,1,0)[a])
 */
LEAN_EXPORT lean_obj_res linen_recvbuf_readline_nb(b_lean_obj_arg buf) {
    linen_recvbuf_t *rb = get_recvbuf(buf);
    uint8_t line[8192];
    size_t line_len = 0;

    for (;;) {
        while (rb->pos < rb->len) {
            uint8_t c = rb->buf[rb->pos++];
            if (line_len > 0 && line[line_len - 1] == '\r' && c == '\n') {
                lean_object *s = lean_mk_string_from_bytes(
                    (const char *)line, line_len - 1);
                lean_obj_res opt = lean_alloc_ctor(1, 1, 0);
                lean_ctor_set(opt, 0, s);
                return lean_io_result_mk_ok(opt);
            }
            if (line_len < sizeof(line) - 1) {
                line[line_len++] = c;
            } else {
                return mk_io_error("recvbuf_readline_nb: line too long (>8KB)");
            }
        }
        ssize_t n = recvbuf_fill_nb(rb);
        if (n == -1) {
            /* EAGAIN — rewind partial line back into buffer for next call */
            /* The partial data is already consumed from buf, so we need to
             * save it. Push partial line bytes back by adjusting pos. */
            /* Actually, the bytes are already consumed. We need to save
             * partial state. For simplicity, push them back into buf. */
            if (line_len > 0) {
                /* Compact buffer first */
                if (rb->pos > 0 && rb->len > rb->pos) {
                    size_t remaining = rb->len - rb->pos;
                    memmove(rb->buf, rb->buf + rb->pos, remaining);
                    rb->len = remaining;
                    rb->pos = 0;
                } else if (rb->pos > 0) {
                    rb->pos = 0;
                    rb->len = 0;
                }
                /* Prepend partial line back into buffer */
                if (line_len + rb->len <= RECVBUF_SIZE) {
                    memmove(rb->buf + line_len, rb->buf, rb->len);
                    memcpy(rb->buf, line, line_len);
                    rb->len += line_len;
                    rb->pos = 0;
                }
                /* else: buffer overflow, data lost — shouldn't happen with 4KB buf + 8KB line */
            }
            return lean_io_result_mk_ok(lean_box(0)); /* none */
        }
        if (n == -2) return mk_io_errno_error();
        if (n == 0) {
            /* EOF */
            if (line_len == 0)
                return lean_io_result_mk_ok(lean_alloc_ctor(1, 1, 0)); /* some "" */
            lean_object *s = lean_mk_string_from_bytes(
                (const char *)line, line_len);
            lean_obj_res opt = lean_alloc_ctor(1, 1, 0);
            lean_ctor_set(opt, 0, s);
            return lean_io_result_mk_ok(opt);
        }
    }
}

/**
 * Non-blocking readn. Returns (ByteArray × Bool) where Bool = all bytes read.
 * Encoding: (ByteArray × Bool) = ctor(0,2,0)[arr, box(0 or 1)]
 */
LEAN_EXPORT lean_obj_res linen_recvbuf_readn_nb(b_lean_obj_arg buf, size_t n) {
    linen_recvbuf_t *rb = get_recvbuf(buf);
    lean_object *arr = lean_alloc_sarray(1, n, n);
    uint8_t *dst = lean_sarray_cptr(arr);
    size_t total = 0;

    while (total < n) {
        size_t avail = rb->len - rb->pos;
        if (avail > 0) {
            size_t to_copy = avail < (n - total) ? avail : (n - total);
            memcpy(dst + total, rb->buf + rb->pos, to_copy);
            rb->pos += to_copy;
            total += to_copy;
        }
        if (total >= n) break;
        ssize_t nr = recvbuf_fill_nb(rb);
        if (nr == -1) {
            /* EAGAIN — return partial data with complete=false */
            lean_sarray_set_size(arr, total);
            lean_obj_res pair = mk_pair(arr, lean_box(0)); /* false */
            return lean_io_result_mk_ok(pair);
        }
        if (nr == -2) { lean_dec(arr); return mk_io_errno_error(); }
        if (nr == 0) {
            /* EOF — return what we have with complete=(total==n) */
            lean_sarray_set_size(arr, total);
            lean_obj_res pair = mk_pair(arr, lean_box(total >= n ? 1 : 0));
            return lean_io_result_mk_ok(pair);
        }
    }
    lean_obj_res pair = mk_pair(arr, lean_box(1)); /* true = complete */
    return lean_io_result_mk_ok(pair);
}

/* ================================================================
 * EVENT MULTIPLEXING: kqueue (macOS) / epoll (Linux)
 * ================================================================ */

/*
 * Event type flags (must match Lean EventType):
 *   bit 0 = readable  (1)
 *   bit 1 = writable  (2)
 *   bit 2 = error      (4)
 */
#define LINEN_EV_READABLE 1
#define LINEN_EV_WRITABLE 2
#define LINEN_EV_ERROR    4

/**
 * Create an event loop fd (kqueue on macOS, epoll on Linux)
 */
LEAN_EXPORT lean_obj_res linen_event_loop_create(void) {
#ifdef __APPLE__
    int fd = kqueue();
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(mk_event_loop(fd));
#elif defined(__linux__)
    int fd = epoll_create1(0);
    if (fd < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(mk_event_loop(fd));
#else
    return mk_io_error("event_loop_create: unsupported platform");
#endif
}

/**
 * Register interest in events for a socket
 * events: bitmask of LINEN_EV_READABLE | LINEN_EV_WRITABLE
 */
LEAN_EXPORT lean_obj_res linen_event_loop_add(b_lean_obj_arg loop, b_lean_obj_arg sock, size_t events) {
    int loop_fd = get_event_loop_fd(loop);
    int socket_fd = get_socket_fd(sock);
#ifdef __APPLE__
    struct kevent changes[2];
    int nchanges = 0;
    if (events & LINEN_EV_READABLE) {
        EV_SET(&changes[nchanges], (uintptr_t)socket_fd, EVFILT_READ, EV_ADD | EV_ENABLE, 0, 0, NULL);
        nchanges++;
    }
    if (events & LINEN_EV_WRITABLE) {
        EV_SET(&changes[nchanges], (uintptr_t)socket_fd, EVFILT_WRITE, EV_ADD | EV_ENABLE, 0, 0, NULL);
        nchanges++;
    }
    if (nchanges == 0) {
        return mk_io_error("event_loop_add: no events specified");
    }
    if (kevent(loop_fd, changes, nchanges, NULL, 0, NULL) < 0) {
        return mk_io_errno_error();
    }
    return lean_io_result_mk_ok(lean_box(0));
#elif defined(__linux__)
    struct epoll_event ev;
    memset(&ev, 0, sizeof(ev));
    ev.data.fd = socket_fd;
    if (events & LINEN_EV_READABLE) ev.events |= EPOLLIN;
    if (events & LINEN_EV_WRITABLE) ev.events |= EPOLLOUT;
    if (epoll_ctl(loop_fd, EPOLL_CTL_ADD, socket_fd, &ev) < 0) {
        /* If already registered, try MOD */
        if (errno == EEXIST) {
            if (epoll_ctl(loop_fd, EPOLL_CTL_MOD, socket_fd, &ev) < 0) {
                return mk_io_errno_error();
            }
        } else {
            return mk_io_errno_error();
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
#else
    return mk_io_error("event_loop_add: unsupported platform");
#endif
}

/**
 * Unregister a socket from the event loop
 */
LEAN_EXPORT lean_obj_res linen_event_loop_del(b_lean_obj_arg loop, b_lean_obj_arg sock) {
    int loop_fd = get_event_loop_fd(loop);
    int socket_fd = get_socket_fd(sock);
#ifdef __APPLE__
    /* Remove both read and write filters; ignore errors if not registered */
    struct kevent changes[2];
    EV_SET(&changes[0], (uintptr_t)socket_fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
    EV_SET(&changes[1], (uintptr_t)socket_fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
    /* Best effort: kevent may fail for filters not registered */
    kevent(loop_fd, &changes[0], 1, NULL, 0, NULL);
    kevent(loop_fd, &changes[1], 1, NULL, 0, NULL);
    return lean_io_result_mk_ok(lean_box(0));
#elif defined(__linux__)
    if (epoll_ctl(loop_fd, EPOLL_CTL_DEL, socket_fd, NULL) < 0) {
        if (errno != ENOENT) {
            return mk_io_errno_error();
        }
    }
    return lean_io_result_mk_ok(lean_box(0));
#else
    return mk_io_error("event_loop_del: unsupported platform");
#endif
}

/**
 * Wait for events. Returns List (fd x events) where events is a bitmask.
 * timeout_ms: timeout in milliseconds (-1 = block indefinitely)
 */
LEAN_EXPORT lean_obj_res linen_event_loop_wait(b_lean_obj_arg loop, size_t timeout_ms) {
    int loop_fd = get_event_loop_fd(loop);
#ifdef __APPLE__
    #define MAX_EVENTS 64
    struct kevent kevents[MAX_EVENTS];
    struct timespec ts;
    struct timespec *tsp = NULL;

    if ((int64_t)timeout_ms >= 0) {
        ts.tv_sec = (time_t)(timeout_ms / 1000);
        ts.tv_nsec = (long)((timeout_ms % 1000) * 1000000);
        tsp = &ts;
    }

    int n = kevent(loop_fd, NULL, 0, kevents, MAX_EVENTS, tsp);
    if (n < 0) {
        if (errno == EINTR) {
            return lean_io_result_mk_ok(mk_list_nil());
        }
        return mk_io_errno_error();
    }

    lean_object *list = mk_list_nil();
    for (int i = n - 1; i >= 0; i--) {
        size_t ev_fd = (size_t)kevents[i].ident;
        size_t ev_flags = 0;
        if (kevents[i].filter == EVFILT_READ) ev_flags |= LINEN_EV_READABLE;
        if (kevents[i].filter == EVFILT_WRITE) ev_flags |= LINEN_EV_WRITABLE;
        if (kevents[i].flags & EV_ERROR) ev_flags |= LINEN_EV_ERROR;
        if (kevents[i].flags & EV_EOF) ev_flags |= LINEN_EV_ERROR;

        lean_obj_res pair = mk_pair(lean_box(ev_fd), lean_box(ev_flags));
        list = mk_list_cons(pair, list);
    }
    return lean_io_result_mk_ok(list);
    #undef MAX_EVENTS

#elif defined(__linux__)
    #define MAX_EVENTS 64
    struct epoll_event epevents[MAX_EVENTS];

    int n = epoll_wait(loop_fd, epevents, MAX_EVENTS, (int)timeout_ms);
    if (n < 0) {
        if (errno == EINTR) {
            return lean_io_result_mk_ok(mk_list_nil());
        }
        return mk_io_errno_error();
    }

    lean_object *list = mk_list_nil();
    for (int i = n - 1; i >= 0; i--) {
        size_t ev_fd = (size_t)epevents[i].data.fd;
        size_t ev_flags = 0;
        if (epevents[i].events & EPOLLIN) ev_flags |= LINEN_EV_READABLE;
        if (epevents[i].events & EPOLLOUT) ev_flags |= LINEN_EV_WRITABLE;
        if (epevents[i].events & (EPOLLERR | EPOLLHUP)) ev_flags |= LINEN_EV_ERROR;

        lean_obj_res pair = mk_pair(lean_box(ev_fd), lean_box(ev_flags));
        list = mk_list_cons(pair, list);
    }
    return lean_io_result_mk_ok(list);
    #undef MAX_EVENTS

#else
    return mk_io_error("event_loop_wait: unsupported platform");
#endif
}

/**
 * Close the event loop — the finalizer also closes, but explicit close is preferred.
 */
LEAN_EXPORT lean_obj_res linen_event_loop_close(b_lean_obj_arg loop) {
    int fd = get_event_loop_fd(loop);
    if (fd >= 0) {
        if (close(fd) < 0) {
            return mk_io_errno_error();
        }
        ((lean_external_object *)loop)->m_data = (void*)(intptr_t)(-1);
    }
    return lean_io_result_mk_ok(lean_box(0));
}
