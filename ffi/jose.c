/*
 * ffi/jose.c -- JOSE/JWT cryptographic primitives FFI for Lean 4
 *
 * Provides the low-level cryptographic operations needed for JWT
 * signature verification and JWK key handling, using OpenSSL's EVP API.
 *
 * Features:
 * - HMAC computation (HS256/HS384/HS512)
 * - RSA signature verification (RS256/RS384/RS512, PS256/PS384/PS512)
 * - EC signature verification (ES256/ES384/ES512)
 * - JWK-to-DER public key construction (RSA and EC)
 * - Base64url encode/decode
 *
 * Platform: macOS and Linux. Requires OpenSSL or LibreSSL.
 */

#include <lean/lean.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/rsa.h>
#include <openssl/bn.h>
#include <openssl/err.h>
#include <openssl/ecdsa.h>
#include <openssl/x509.h>
#include <openssl/objects.h>
#include <openssl/param_build.h>
#include <openssl/core_names.h>
#include <string.h>
#include <stdlib.h>

/* ────────────────────────────────────────────────────────────
 * Helpers
 * ──────────────────────────────────────────────────────────── */

static lean_obj_res jose_mk_io_error(const char *msg) {
    unsigned long err = ERR_get_error();
    char buf[256];
    if (err) {
        ERR_error_string_n(err, buf, sizeof(buf));
    } else {
        strncpy(buf, msg, sizeof(buf) - 1);
        buf[sizeof(buf) - 1] = '\0';
    }
    return lean_io_result_mk_error(lean_mk_io_user_error(lean_mk_string(buf)));
}

/* Select EVP_MD from algorithm index: 0=SHA256, 1=SHA384, 2=SHA512 */
static const EVP_MD *jose_select_md(uint8_t alg) {
    switch (alg) {
        case 0:  return EVP_sha256();
        case 1:  return EVP_sha384();
        case 2:  return EVP_sha512();
        default: return NULL;
    }
}

/* Allocate a Lean ByteArray of a given size with data copied in */
static lean_obj_res jose_mk_byte_array(const uint8_t *data, size_t len) {
    lean_obj_res arr = lean_alloc_sarray(1, len, len);
    if (data && len > 0) {
        memcpy(lean_sarray_cptr(arr), data, len);
    }
    return arr;
}

/* ────────────────────────────────────────────────────────────
 * HMAC (HS256 / HS384 / HS512)
 *
 * @[extern "linen_jose_hmac"]
 * opaque haleJoseHmac : @& ByteArray -> @& ByteArray -> UInt8 -> IO ByteArray
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_jose_hmac(
    b_lean_obj_arg key_obj,
    b_lean_obj_arg data_obj,
    uint8_t algorithm,
    lean_obj_arg world
) {
    const EVP_MD *md = jose_select_md(algorithm);
    if (!md) {
        return jose_mk_io_error("HMAC: unsupported algorithm");
    }

    const uint8_t *key  = lean_sarray_cptr(key_obj);
    size_t key_len      = lean_sarray_size(key_obj);
    const uint8_t *data = lean_sarray_cptr(data_obj);
    size_t data_len     = lean_sarray_size(data_obj);

    unsigned char result[EVP_MAX_MD_SIZE];
    unsigned int result_len = 0;

    if (!HMAC(md, key, (int)key_len, data, data_len, result, &result_len)) {
        return jose_mk_io_error("HMAC computation failed");
    }

    return lean_io_result_mk_ok(jose_mk_byte_array(result, result_len));
}

/* ────────────────────────────────────────────────────────────
 * RSA signature verification (RS256/RS384/RS512, PS256/PS384/PS512)
 *
 * @[extern "linen_jose_rsa_verify"]
 * opaque haleJoseRsaVerify : @& ByteArray -> @& ByteArray -> @& ByteArray
 *                          -> UInt8 -> UInt8 -> IO UInt8
 *
 * pubkey_der : DER-encoded SubjectPublicKeyInfo
 * data       : signed payload (header.payload)
 * signature  : raw signature bytes
 * algorithm  : 0=SHA256, 1=SHA384, 2=SHA512
 * use_pss    : 0=PKCS1v15 (RS*), 1=PSS (PS*)
 * Returns    : 1 if valid, 0 if invalid
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_jose_rsa_verify(
    b_lean_obj_arg pubkey_der_obj,
    b_lean_obj_arg data_obj,
    b_lean_obj_arg sig_obj,
    uint8_t algorithm,
    uint8_t use_pss,
    lean_obj_arg world
) {
    const EVP_MD *md = jose_select_md(algorithm);
    if (!md) {
        return jose_mk_io_error("RSA verify: unsupported algorithm");
    }

    const uint8_t *der     = lean_sarray_cptr(pubkey_der_obj);
    size_t der_len         = lean_sarray_size(pubkey_der_obj);
    const uint8_t *data    = lean_sarray_cptr(data_obj);
    size_t data_len        = lean_sarray_size(data_obj);
    const uint8_t *sig     = lean_sarray_cptr(sig_obj);
    size_t sig_len         = lean_sarray_size(sig_obj);

    /* Parse DER-encoded public key */
    const uint8_t *p = der;
    EVP_PKEY *pkey = d2i_PUBKEY(NULL, &p, (long)der_len);
    if (!pkey) {
        return jose_mk_io_error("RSA verify: failed to parse DER public key");
    }

    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (!ctx) {
        EVP_PKEY_free(pkey);
        return jose_mk_io_error("RSA verify: EVP_MD_CTX_new failed");
    }

    EVP_PKEY_CTX *pctx = NULL;
    int rc = EVP_DigestVerifyInit(ctx, &pctx, md, NULL, pkey);
    if (rc != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(pkey);
        return jose_mk_io_error("RSA verify: EVP_DigestVerifyInit failed");
    }

    /* Set PSS padding if requested */
    if (use_pss) {
        if (EVP_PKEY_CTX_set_rsa_padding(pctx, RSA_PKCS1_PSS_PADDING) <= 0) {
            EVP_MD_CTX_free(ctx);
            EVP_PKEY_free(pkey);
            return jose_mk_io_error("RSA verify: failed to set PSS padding");
        }
        /* PSS salt length = digest length (RFC 7518 default) */
        if (EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, RSA_PSS_SALTLEN_DIGEST) <= 0) {
            EVP_MD_CTX_free(ctx);
            EVP_PKEY_free(pkey);
            return jose_mk_io_error("RSA verify: failed to set PSS salt length");
        }
    }

    rc = EVP_DigestVerify(ctx, sig, sig_len, data, data_len);

    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);

    /* rc == 1 means valid, anything else means invalid (not necessarily error) */
    return lean_io_result_mk_ok(lean_box((unsigned)(rc == 1)));
}

/* ────────────────────────────────────────────────────────────
 * EC signature verification (ES256 / ES384 / ES512)
 *
 * @[extern "linen_jose_ec_verify"]
 * opaque haleJoseEcVerify : @& ByteArray -> @& ByteArray -> @& ByteArray
 *                         -> UInt8 -> IO UInt8
 *
 * JWS encodes ECDSA signatures as raw r||s (fixed-width concatenation).
 * OpenSSL expects DER-encoded ECDSA_SIG. We convert before verifying.
 *
 * algorithm : 0=ES256(P-256/SHA-256), 1=ES384(P-384/SHA-384), 2=ES512(P-521/SHA-512)
 * Returns   : 1 if valid, 0 if invalid
 * ──────────────────────────────────────────────────────────── */

/* Component size for each ES algorithm (r and s are each this many bytes) */
static size_t jose_ec_component_size(uint8_t alg) {
    switch (alg) {
        case 0:  return 32;  /* P-256 */
        case 1:  return 48;  /* P-384 */
        case 2:  return 66;  /* P-521 */
        default: return 0;
    }
}

/* Convert JWS raw r||s to DER-encoded ECDSA_SIG.
 * Returns a malloc'd buffer; caller must free. Sets *out_len. */
static uint8_t *jose_jws_sig_to_der(const uint8_t *raw, size_t raw_len,
                                     uint8_t alg, size_t *out_len)
{
    size_t comp_size = jose_ec_component_size(alg);
    if (comp_size == 0 || raw_len != comp_size * 2) {
        return NULL;
    }

    BIGNUM *r = BN_bin2bn(raw, (int)comp_size, NULL);
    BIGNUM *s = BN_bin2bn(raw + comp_size, (int)comp_size, NULL);
    if (!r || !s) {
        BN_free(r);
        BN_free(s);
        return NULL;
    }

    ECDSA_SIG *esig = ECDSA_SIG_new();
    if (!esig) {
        BN_free(r);
        BN_free(s);
        return NULL;
    }

    /* ECDSA_SIG_set0 takes ownership of r and s */
    if (!ECDSA_SIG_set0(esig, r, s)) {
        ECDSA_SIG_free(esig);
        return NULL;
    }

    int der_len = i2d_ECDSA_SIG(esig, NULL);
    if (der_len <= 0) {
        ECDSA_SIG_free(esig);
        return NULL;
    }

    uint8_t *der = malloc((size_t)der_len);
    if (!der) {
        ECDSA_SIG_free(esig);
        return NULL;
    }

    uint8_t *p = der;
    i2d_ECDSA_SIG(esig, &p);
    ECDSA_SIG_free(esig);

    *out_len = (size_t)der_len;
    return der;
}

LEAN_EXPORT lean_obj_res linen_jose_ec_verify(
    b_lean_obj_arg pubkey_der_obj,
    b_lean_obj_arg data_obj,
    b_lean_obj_arg sig_obj,
    uint8_t algorithm,
    lean_obj_arg world
) {
    const EVP_MD *md = jose_select_md(algorithm);
    if (!md) {
        return jose_mk_io_error("EC verify: unsupported algorithm");
    }

    const uint8_t *der     = lean_sarray_cptr(pubkey_der_obj);
    size_t der_len         = lean_sarray_size(pubkey_der_obj);
    const uint8_t *data    = lean_sarray_cptr(data_obj);
    size_t data_len        = lean_sarray_size(data_obj);
    const uint8_t *sig_raw = lean_sarray_cptr(sig_obj);
    size_t sig_raw_len     = lean_sarray_size(sig_obj);

    /* Convert JWS raw r||s to DER */
    size_t sig_der_len = 0;
    uint8_t *sig_der = jose_jws_sig_to_der(sig_raw, sig_raw_len, algorithm, &sig_der_len);
    if (!sig_der) {
        return jose_mk_io_error("EC verify: invalid JWS signature format");
    }

    /* Parse DER-encoded public key */
    const uint8_t *p = der;
    EVP_PKEY *pkey = d2i_PUBKEY(NULL, &p, (long)der_len);
    if (!pkey) {
        free(sig_der);
        return jose_mk_io_error("EC verify: failed to parse DER public key");
    }

    EVP_MD_CTX *ctx = EVP_MD_CTX_new();
    if (!ctx) {
        free(sig_der);
        EVP_PKEY_free(pkey);
        return jose_mk_io_error("EC verify: EVP_MD_CTX_new failed");
    }

    int rc = EVP_DigestVerifyInit(ctx, NULL, md, NULL, pkey);
    if (rc != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(pkey);
        free(sig_der);
        return jose_mk_io_error("EC verify: EVP_DigestVerifyInit failed");
    }

    rc = EVP_DigestVerify(ctx, sig_der, sig_der_len, data, data_len);

    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pkey);
    free(sig_der);

    return lean_io_result_mk_ok(lean_box((unsigned)(rc == 1)));
}

/* ────────────────────────────────────────────────────────────
 * JWK-to-DER: RSA public key from components
 *
 * @[extern "linen_jose_rsa_pubkey_from_components"]
 * opaque haleJoseRsaPubkeyFromComponents : @& ByteArray -> @& ByteArray
 *                                        -> IO ByteArray
 *
 * Builds an RSA public key from modulus (n) and exponent (e),
 * returns DER-encoded SubjectPublicKeyInfo.
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_jose_rsa_pubkey_from_components(
    b_lean_obj_arg n_obj,
    b_lean_obj_arg e_obj,
    lean_obj_arg world
) {
    const uint8_t *n_data = lean_sarray_cptr(n_obj);
    size_t n_len          = lean_sarray_size(n_obj);
    const uint8_t *e_data = lean_sarray_cptr(e_obj);
    size_t e_len          = lean_sarray_size(e_obj);

    BIGNUM *bn_n = BN_bin2bn(n_data, (int)n_len, NULL);
    BIGNUM *bn_e = BN_bin2bn(e_data, (int)e_len, NULL);
    if (!bn_n || !bn_e) {
        BN_free(bn_n);
        BN_free(bn_e);
        return jose_mk_io_error("RSA pubkey: BN_bin2bn failed");
    }

    /* Build EVP_PKEY via OSSL_PARAM (OpenSSL 3.0+ API) */
    OSSL_PARAM_BLD *bld = OSSL_PARAM_BLD_new();
    if (!bld) {
        BN_free(bn_n);
        BN_free(bn_e);
        return jose_mk_io_error("RSA pubkey: OSSL_PARAM_BLD_new failed");
    }

    OSSL_PARAM_BLD_push_BN(bld, OSSL_PKEY_PARAM_RSA_N, bn_n);
    OSSL_PARAM_BLD_push_BN(bld, OSSL_PKEY_PARAM_RSA_E, bn_e);

    OSSL_PARAM *params = OSSL_PARAM_BLD_to_param(bld);
    OSSL_PARAM_BLD_free(bld);
    BN_free(bn_n);
    BN_free(bn_e);

    if (!params) {
        return jose_mk_io_error("RSA pubkey: OSSL_PARAM_BLD_to_param failed");
    }

    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, "RSA", NULL);
    if (!ctx) {
        OSSL_PARAM_free(params);
        return jose_mk_io_error("RSA pubkey: EVP_PKEY_CTX_new_from_name failed");
    }

    EVP_PKEY *pkey = NULL;
    if (EVP_PKEY_fromdata_init(ctx) <= 0 ||
        EVP_PKEY_fromdata(ctx, &pkey, EVP_PKEY_PUBLIC_KEY, params) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        OSSL_PARAM_free(params);
        return jose_mk_io_error("RSA pubkey: EVP_PKEY_fromdata failed");
    }
    EVP_PKEY_CTX_free(ctx);
    OSSL_PARAM_free(params);

    /* Encode as SubjectPublicKeyInfo DER */
    int der_len = i2d_PUBKEY(pkey, NULL);
    if (der_len <= 0) {
        EVP_PKEY_free(pkey);
        return jose_mk_io_error("RSA pubkey: i2d_PUBKEY failed");
    }

    lean_obj_res arr = lean_alloc_sarray(1, (size_t)der_len, (size_t)der_len);
    uint8_t *out = lean_sarray_cptr(arr);
    uint8_t *p = out;
    i2d_PUBKEY(pkey, &p);
    EVP_PKEY_free(pkey);

    return lean_io_result_mk_ok(arr);
}

/* ────────────────────────────────────────────────────────────
 * JWK-to-DER: EC public key from components
 *
 * @[extern "linen_jose_ec_pubkey_from_components"]
 * opaque haleJoseEcPubkeyFromComponents : UInt8 -> @& ByteArray -> @& ByteArray
 *                                       -> IO ByteArray
 *
 * crv: 0=P-256, 1=P-384, 2=P-521
 * x, y: affine coordinates (big-endian unsigned)
 * Returns DER-encoded SubjectPublicKeyInfo.
 * ──────────────────────────────────────────────────────────── */

static const char *jose_curve_name(uint8_t crv) {
    switch (crv) {
        case 0:  return SN_X9_62_prime256v1;  /* P-256 */
        case 1:  return SN_secp384r1;          /* P-384 */
        case 2:  return SN_secp521r1;          /* P-521 */
        default: return NULL;
    }
}

LEAN_EXPORT lean_obj_res linen_jose_ec_pubkey_from_components(
    uint8_t crv,
    b_lean_obj_arg x_obj,
    b_lean_obj_arg y_obj,
    lean_obj_arg world
) {
    const char *curve_name = jose_curve_name(crv);
    if (!curve_name) {
        return jose_mk_io_error("EC pubkey: unsupported curve");
    }

    const uint8_t *x_data = lean_sarray_cptr(x_obj);
    size_t x_len          = lean_sarray_size(x_obj);
    const uint8_t *y_data = lean_sarray_cptr(y_obj);
    size_t y_len          = lean_sarray_size(y_obj);

    /* Build uncompressed point: 0x04 || x || y */
    size_t pt_len = 1 + x_len + y_len;
    uint8_t *pt_buf = (uint8_t *)malloc(pt_len);
    if (!pt_buf) {
        return jose_mk_io_error("EC pubkey: malloc failed");
    }
    pt_buf[0] = 0x04;
    memcpy(pt_buf + 1, x_data, x_len);
    memcpy(pt_buf + 1 + x_len, y_data, y_len);

    /* Build EVP_PKEY via OSSL_PARAM (OpenSSL 3.0+ API) */
    OSSL_PARAM_BLD *bld = OSSL_PARAM_BLD_new();
    if (!bld) {
        free(pt_buf);
        return jose_mk_io_error("EC pubkey: OSSL_PARAM_BLD_new failed");
    }

    OSSL_PARAM_BLD_push_utf8_string(bld, OSSL_PKEY_PARAM_GROUP_NAME, curve_name, 0);
    OSSL_PARAM_BLD_push_octet_string(bld, OSSL_PKEY_PARAM_PUB_KEY, pt_buf, pt_len);

    OSSL_PARAM *params = OSSL_PARAM_BLD_to_param(bld);
    OSSL_PARAM_BLD_free(bld);
    free(pt_buf);

    if (!params) {
        return jose_mk_io_error("EC pubkey: OSSL_PARAM_BLD_to_param failed");
    }

    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);
    if (!ctx) {
        OSSL_PARAM_free(params);
        return jose_mk_io_error("EC pubkey: EVP_PKEY_CTX_new_from_name failed");
    }

    EVP_PKEY *pkey = NULL;
    if (EVP_PKEY_fromdata_init(ctx) <= 0 ||
        EVP_PKEY_fromdata(ctx, &pkey, EVP_PKEY_PUBLIC_KEY, params) <= 0) {
        EVP_PKEY_CTX_free(ctx);
        OSSL_PARAM_free(params);
        return jose_mk_io_error("EC pubkey: EVP_PKEY_fromdata failed");
    }
    EVP_PKEY_CTX_free(ctx);
    OSSL_PARAM_free(params);

    int der_len = i2d_PUBKEY(pkey, NULL);
    if (der_len <= 0) {
        EVP_PKEY_free(pkey);
        return jose_mk_io_error("EC pubkey: i2d_PUBKEY failed");
    }

    lean_obj_res arr = lean_alloc_sarray(1, (size_t)der_len, (size_t)der_len);
    uint8_t *out = lean_sarray_cptr(arr);
    uint8_t *p = out;
    i2d_PUBKEY(pkey, &p);
    EVP_PKEY_free(pkey);

    return lean_io_result_mk_ok(arr);
}

/* ────────────────────────────────────────────────────────────
 * Base64url decode
 *
 * @[extern "linen_jose_base64url_decode"]
 * opaque haleJoseBase64urlDecode : @& String -> IO ByteArray
 *
 * Base64url uses '-' and '_' instead of '+' and '/', with no padding.
 * We convert to standard base64, add padding, then use EVP_DecodeBlock.
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_jose_base64url_decode(
    b_lean_obj_arg input_obj,
    lean_obj_arg world
) {
    const char *input = lean_string_cstr(input_obj);
    size_t input_len  = lean_string_size(input_obj) - 1; /* exclude NUL */

    if (input_len == 0) {
        return lean_io_result_mk_ok(jose_mk_byte_array(NULL, 0));
    }

    /* Calculate padded length: round up to multiple of 4 */
    size_t padded_len = input_len;
    size_t pad = (4 - (input_len % 4)) % 4;
    padded_len += pad;

    char *b64 = malloc(padded_len + 1);
    if (!b64) {
        return jose_mk_io_error("base64url decode: malloc failed");
    }

    /* Convert base64url to standard base64 */
    for (size_t i = 0; i < input_len; i++) {
        char c = input[i];
        if (c == '-')      b64[i] = '+';
        else if (c == '_') b64[i] = '/';
        else               b64[i] = c;
    }
    /* Add padding */
    for (size_t i = 0; i < pad; i++) {
        b64[input_len + i] = '=';
    }
    b64[padded_len] = '\0';

    /* Decode using OpenSSL */
    size_t max_out = (padded_len / 4) * 3;
    uint8_t *decoded = malloc(max_out);
    if (!decoded) {
        free(b64);
        return jose_mk_io_error("base64url decode: malloc failed");
    }

    EVP_ENCODE_CTX *ectx = EVP_ENCODE_CTX_new();
    if (!ectx) {
        free(b64);
        free(decoded);
        return jose_mk_io_error("base64url decode: EVP_ENCODE_CTX_new failed");
    }

    EVP_DecodeInit(ectx);

    int out_len = 0;
    int rc = EVP_DecodeUpdate(ectx, decoded, &out_len,
                              (const unsigned char *)b64, (int)padded_len);
    if (rc < 0) {
        EVP_ENCODE_CTX_free(ectx);
        free(b64);
        free(decoded);
        return jose_mk_io_error("base64url decode: EVP_DecodeUpdate failed");
    }

    int final_len = 0;
    rc = EVP_DecodeFinal(ectx, decoded + out_len, &final_len);
    EVP_ENCODE_CTX_free(ectx);
    free(b64);

    if (rc < 0) {
        free(decoded);
        return jose_mk_io_error("base64url decode: EVP_DecodeFinal failed");
    }

    size_t total = (size_t)out_len + (size_t)final_len;
    lean_obj_res arr = jose_mk_byte_array(decoded, total);
    free(decoded);

    return lean_io_result_mk_ok(arr);
}

/* ────────────────────────────────────────────────────────────
 * Base64url encode
 *
 * @[extern "linen_jose_base64url_encode"]
 * opaque haleJoseBase64urlEncode : @& ByteArray -> IO String
 *
 * Encodes bytes to base64url (no padding, '-' and '_' alphabet).
 * ──────────────────────────────────────────────────────────── */

LEAN_EXPORT lean_obj_res linen_jose_base64url_encode(
    b_lean_obj_arg input_obj,
    lean_obj_arg world
) {
    const uint8_t *data = lean_sarray_cptr(input_obj);
    size_t data_len     = lean_sarray_size(input_obj);

    if (data_len == 0) {
        return lean_io_result_mk_ok(lean_mk_string(""));
    }

    /* EVP_EncodeBlock output size: 4 * ceil(n/3) + 1 for NUL */
    size_t b64_max = ((data_len + 2) / 3) * 4 + 1;
    char *b64 = malloc(b64_max);
    if (!b64) {
        return jose_mk_io_error("base64url encode: malloc failed");
    }

    int b64_len = EVP_EncodeBlock((unsigned char *)b64, data, (int)data_len);
    if (b64_len < 0) {
        free(b64);
        return jose_mk_io_error("base64url encode: EVP_EncodeBlock failed");
    }

    /* Convert standard base64 to base64url and strip padding */
    size_t out_len = (size_t)b64_len;
    for (size_t i = 0; i < out_len; i++) {
        if (b64[i] == '+')      b64[i] = '-';
        else if (b64[i] == '/') b64[i] = '_';
    }

    /* Remove trailing '=' padding */
    while (out_len > 0 && b64[out_len - 1] == '=') {
        out_len--;
    }
    b64[out_len] = '\0';

    lean_obj_res result = lean_mk_string_from_bytes(b64, out_len);
    free(b64);

    return lean_io_result_mk_ok(result);
}
