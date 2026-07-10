/*
 * ffi/keychain.c — Cross-platform OS credential-store FFI for Lean 4
 *
 * Ported from the Rust `keyring` crate (`keyring-rs`), collapsing its three
 * per-OS backend crates (`keyring-macos`, `keyring-secret-service`,
 * `keyring-windows`) into one portable shim, following the same `#ifdef
 * __APPLE__` / `#ifdef __linux__` (extended here with `#ifdef _WIN32`)
 * pattern as `ffi/network.c`.
 *
 * Exposes three functions, each keyed on a (service, account) pair:
 *   - linen_keychain_set    : store/replace a secret.
 *   - linen_keychain_get    : retrieve a previously stored secret.
 *   - linen_keychain_delete : remove a stored secret.
 *
 * All errors are surfaced as `IO.Error` (never a crash), following the
 * convention established by `ffi/tls.c`/`ffi/network.c`.
 *
 * Platform backends:
 *   - macOS   : Security.framework Keychain (`kSecClassGenericPassword`).
 *   - Linux   : the D-Bus Secret Service, via libsecret's synchronous
 *               convenience API.
 *   - Windows : Win32 Credential Manager (`wincred.h`).
 *
 * NOTE: only the macOS branch can be compiled and tested in this
 * environment. The Linux and Windows branches are written against the real
 * libsecret / wincred APIs but are unverified — see the module's Lean-side
 * doc-comment (`Linen/System/Keychain.lean`) for the same caveat.
 */

#include <lean/lean.h>
#include <string.h>
#include <stdlib.h>

#ifdef __APPLE__
#include <Security/Security.h>
#include <CoreFoundation/CoreFoundation.h>
#elif defined(__linux__)
#include <libsecret/secret.h>
#elif defined(_WIN32)
#include <windows.h>
#include <wincred.h>
#endif

/* ────────────────────────────────────────────────────────────
 * Helper: make a Lean IO error from a message
 * ──────────────────────────────────────────────────────────── */
static inline lean_obj_res mk_io_error(const char *msg) {
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(msg)));
}

#ifdef __APPLE__
/* Turn a `CFStringRef`/`OSStatus` failure into a human-readable message. */
static lean_obj_res mk_osstatus_error(const char *prefix, OSStatus status) {
    char buf[256];
    CFStringRef msg = SecCopyErrorMessageString(status, NULL);
    if (msg) {
        char cbuf[192];
        if (!CFStringGetCString(msg, cbuf, sizeof(cbuf), kCFStringEncodingUTF8)) {
            cbuf[0] = '\0';
        }
        CFRelease(msg);
        snprintf(buf, sizeof(buf), "%s: %s (status %d)", prefix, cbuf, (int)status);
    } else {
        snprintf(buf, sizeof(buf), "%s (status %d)", prefix, (int)status);
    }
    return mk_io_error(buf);
}

/* Build the base `kSecClassGenericPassword` query dictionary keyed on
   service+account. The caller may add further keys before use. */
static CFMutableDictionaryRef mk_query(const char *service, const char *account) {
    CFStringRef serviceStr = CFStringCreateWithCString(kCFAllocatorDefault, service, kCFStringEncodingUTF8);
    CFStringRef accountStr = CFStringCreateWithCString(kCFAllocatorDefault, account, kCFStringEncodingUTF8);

    CFMutableDictionaryRef query = CFDictionaryCreateMutable(
        kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    CFDictionarySetValue(query, kSecClass, kSecClassGenericPassword);
    CFDictionarySetValue(query, kSecAttrService, serviceStr);
    CFDictionarySetValue(query, kSecAttrAccount, accountStr);

    CFRelease(serviceStr);
    CFRelease(accountStr);
    return query;
}
#endif

#ifdef __linux__
/* The Secret Service schema used to key stored secrets on service+account,
   mirroring `keyring-secret-service`'s own schema attributes. */
static const SecretSchema linen_keychain_schema = {
    "org.linen.Keychain", SECRET_SCHEMA_NONE,
    {
        { "service", SECRET_SCHEMA_ATTRIBUTE_STRING },
        { "account", SECRET_SCHEMA_ATTRIBUTE_STRING },
        { NULL, 0 },
    },
    0, 0, 0, 0, 0, 0, 0
};
#endif

#ifdef _WIN32
/* Win32 Credential Manager keys credentials by a single `TargetName`, so we
   combine service+account into one string (mirroring `keyring-windows`,
   which also folds the pair into one target name). */
static LPWSTR mk_target_name(const char *service, const char *account) {
    size_t slen = strlen(service), alen = strlen(account);
    char *combined = malloc(slen + alen + 2);
    if (!combined) return NULL;
    memcpy(combined, service, slen);
    combined[slen] = ':';
    memcpy(combined + slen + 1, account, alen + 1);

    int wlen = MultiByteToWideChar(CP_UTF8, 0, combined, -1, NULL, 0);
    LPWSTR wide = malloc((size_t)wlen * sizeof(WCHAR));
    if (wide) MultiByteToWideChar(CP_UTF8, 0, combined, -1, wide, wlen);
    free(combined);
    return wide;
}
#endif

/* ────────────────────────────────────────────────────────────
 * linen_keychain_set : store or replace a secret
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_keychain_set"]
 * opaque setImpl : @& String → @& String → @& ByteArray → IO Unit
 */
LEAN_EXPORT lean_obj_res linen_keychain_set(
    b_lean_obj_arg service_obj,
    b_lean_obj_arg account_obj,
    b_lean_obj_arg secret_obj
) {
    const char *service = lean_string_cstr(service_obj);
    const char *account = lean_string_cstr(account_obj);
    size_t secret_len = lean_sarray_size(secret_obj);
    const uint8_t *secret = lean_sarray_cptr(secret_obj);

#ifdef __APPLE__
    CFMutableDictionaryRef query = mk_query(service, account);
    CFDataRef secretData = CFDataCreate(kCFAllocatorDefault, secret, (CFIndex)secret_len);

    /* Try to add; if an item already exists, update it instead. */
    CFMutableDictionaryRef addQuery = CFDictionaryCreateMutableCopy(kCFAllocatorDefault, 0, query);
    CFDictionarySetValue(addQuery, kSecValueData, secretData);
    OSStatus status = SecItemAdd(addQuery, NULL);
    CFRelease(addQuery);

    if (status == errSecDuplicateItem) {
        CFMutableDictionaryRef attrsToUpdate = CFDictionaryCreateMutable(
            kCFAllocatorDefault, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
        CFDictionarySetValue(attrsToUpdate, kSecValueData, secretData);
        status = SecItemUpdate(query, attrsToUpdate);
        CFRelease(attrsToUpdate);
    }

    CFRelease(secretData);
    CFRelease(query);

    if (status != errSecSuccess) {
        return mk_osstatus_error("keychain: failed to store secret", status);
    }
    return lean_io_result_mk_ok(lean_box(0));

#elif defined(__linux__)
    /* `secret_password_store_sync` takes a NUL-terminated string, so the
       secret must not contain an embedded NUL byte — a documented
       limitation of this convenience API (unlike the macOS/Windows
       backends, which store the raw bytes verbatim). */
    char *password = malloc(secret_len + 1);
    if (!password) return mk_io_error("keychain: out of memory");
    memcpy(password, secret, secret_len);
    password[secret_len] = '\0';

    GError *error = NULL;
    gboolean ok = secret_password_store_sync(
        &linen_keychain_schema, SECRET_COLLECTION_DEFAULT, service, password,
        NULL, &error, "service", service, "account", account, NULL);
    free(password);

    if (!ok) {
        char buf[256];
        snprintf(buf, sizeof(buf), "keychain: failed to store secret: %s",
                  error && error->message ? error->message : "unknown error");
        if (error) g_error_free(error);
        return mk_io_error(buf);
    }
    return lean_io_result_mk_ok(lean_box(0));

#elif defined(_WIN32)
    LPWSTR target = mk_target_name(service, account);
    if (!target) return mk_io_error("keychain: out of memory");

    int walen = MultiByteToWideChar(CP_UTF8, 0, account, -1, NULL, 0);
    LPWSTR wuser = malloc((size_t)walen * sizeof(WCHAR));
    if (wuser) MultiByteToWideChar(CP_UTF8, 0, account, -1, wuser, walen);

    CREDENTIALW cred;
    memset(&cred, 0, sizeof(cred));
    cred.Type = CRED_TYPE_GENERIC;
    cred.TargetName = target;
    cred.CredentialBlobSize = (DWORD)secret_len;
    cred.CredentialBlob = (LPBYTE)secret;
    cred.Persist = CRED_PERSIST_LOCAL_MACHINE;
    cred.UserName = wuser;

    BOOL ok = CredWriteW(&cred, 0);
    DWORD err = ok ? 0 : GetLastError();
    free(target);
    free(wuser);

    if (!ok) {
        char buf[128];
        snprintf(buf, sizeof(buf), "keychain: CredWriteW failed (error %lu)", (unsigned long)err);
        return mk_io_error(buf);
    }
    return lean_io_result_mk_ok(lean_box(0));

#else
    return mk_io_error("keychain: unsupported platform");
#endif
}

/* ────────────────────────────────────────────────────────────
 * linen_keychain_get : retrieve a stored secret
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_keychain_get"]
 * opaque getImpl : @& String → @& String → IO ByteArray
 */
LEAN_EXPORT lean_obj_res linen_keychain_get(
    b_lean_obj_arg service_obj,
    b_lean_obj_arg account_obj
) {
    const char *service = lean_string_cstr(service_obj);
    const char *account = lean_string_cstr(account_obj);

#ifdef __APPLE__
    CFMutableDictionaryRef query = mk_query(service, account);
    CFDictionarySetValue(query, kSecReturnData, kCFBooleanTrue);
    CFDictionarySetValue(query, kSecMatchLimit, kSecMatchLimitOne);

    CFTypeRef result = NULL;
    OSStatus status = SecItemCopyMatching(query, &result);
    CFRelease(query);

    if (status == errSecItemNotFound) {
        return mk_io_error("keychain: no matching entry found");
    }
    if (status != errSecSuccess) {
        return mk_osstatus_error("keychain: failed to retrieve secret", status);
    }

    CFDataRef data = (CFDataRef)result;
    CFIndex len = CFDataGetLength(data);
    lean_obj_res arr = lean_alloc_sarray(1, (size_t)len, (size_t)len);
    memcpy(lean_sarray_cptr(arr), CFDataGetBytePtr(data), (size_t)len);
    CFRelease(result);
    return lean_io_result_mk_ok(arr);

#elif defined(__linux__)
    GError *error = NULL;
    gchar *password = secret_password_lookup_sync(
        &linen_keychain_schema, NULL, &error, "service", service, "account", account, NULL);

    if (error) {
        char buf[256];
        snprintf(buf, sizeof(buf), "keychain: failed to retrieve secret: %s", error->message);
        g_error_free(error);
        if (password) secret_password_free(password);
        return mk_io_error(buf);
    }
    if (!password) {
        return mk_io_error("keychain: no matching entry found");
    }

    size_t len = strlen(password);
    lean_obj_res arr = lean_alloc_sarray(1, len, len);
    memcpy(lean_sarray_cptr(arr), password, len);
    secret_password_free(password);
    return lean_io_result_mk_ok(arr);

#elif defined(_WIN32)
    LPWSTR target = mk_target_name(service, account);
    if (!target) return mk_io_error("keychain: out of memory");

    PCREDENTIALW cred = NULL;
    BOOL ok = CredReadW(target, CRED_TYPE_GENERIC, 0, &cred);
    DWORD err = ok ? 0 : GetLastError();
    free(target);

    if (!ok) {
        char buf[128];
        if (err == ERROR_NOT_FOUND) {
            return mk_io_error("keychain: no matching entry found");
        }
        snprintf(buf, sizeof(buf), "keychain: CredReadW failed (error %lu)", (unsigned long)err);
        return mk_io_error(buf);
    }

    size_t len = cred->CredentialBlobSize;
    lean_obj_res arr = lean_alloc_sarray(1, len, len);
    memcpy(lean_sarray_cptr(arr), cred->CredentialBlob, len);
    CredFree(cred);
    return lean_io_result_mk_ok(arr);

#else
    return mk_io_error("keychain: unsupported platform");
#endif
}

/* ────────────────────────────────────────────────────────────
 * linen_keychain_delete : remove a stored secret
 * ──────────────────────────────────────────────────────────── */

/*
 * @[extern "linen_keychain_delete"]
 * opaque deleteImpl : @& String → @& String → IO Unit
 */
LEAN_EXPORT lean_obj_res linen_keychain_delete(
    b_lean_obj_arg service_obj,
    b_lean_obj_arg account_obj
) {
    const char *service = lean_string_cstr(service_obj);
    const char *account = lean_string_cstr(account_obj);

#ifdef __APPLE__
    CFMutableDictionaryRef query = mk_query(service, account);
    OSStatus status = SecItemDelete(query);
    CFRelease(query);

    if (status == errSecItemNotFound) {
        return mk_io_error("keychain: no matching entry found");
    }
    if (status != errSecSuccess) {
        return mk_osstatus_error("keychain: failed to delete secret", status);
    }
    return lean_io_result_mk_ok(lean_box(0));

#elif defined(__linux__)
    GError *error = NULL;
    gboolean removed = secret_password_clear_sync(
        &linen_keychain_schema, NULL, &error, "service", service, "account", account, NULL);

    if (error) {
        char buf[256];
        snprintf(buf, sizeof(buf), "keychain: failed to delete secret: %s", error->message);
        g_error_free(error);
        return mk_io_error(buf);
    }
    if (!removed) {
        return mk_io_error("keychain: no matching entry found");
    }
    return lean_io_result_mk_ok(lean_box(0));

#elif defined(_WIN32)
    LPWSTR target = mk_target_name(service, account);
    if (!target) return mk_io_error("keychain: out of memory");

    BOOL ok = CredDeleteW(target, CRED_TYPE_GENERIC, 0);
    DWORD err = ok ? 0 : GetLastError();
    free(target);

    if (!ok) {
        if (err == ERROR_NOT_FOUND) {
            return mk_io_error("keychain: no matching entry found");
        }
        char buf[128];
        snprintf(buf, sizeof(buf), "keychain: CredDeleteW failed (error %lu)", (unsigned long)err);
        return mk_io_error(buf);
    }
    return lean_io_result_mk_ok(lean_box(0));

#else
    return mk_io_error("keychain: unsupported platform");
#endif
}
