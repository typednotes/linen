# `cryptohash` module dependencies

Topological order of every module of the [`cryptohash`](https://hackage.haskell.org/package/cryptohash)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md), which uses it for the
PDF Standard Security Handler's key-derivation hashing (ISO 32000 §7.6.3.3).

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`cryptohash` exposes ~16 hash algorithms (`Crypto.Hash.{MD2,MD4,MD5,SHA1,
SHA224,SHA256,SHA384,SHA512,RIPEMD160,Skein*,Tiger,Whirlpool}` plus a generic
`Crypto.Hash` dispatcher) built on a shared internal C implementation. Only
`Crypto.Hash.MD5.hash` is ever called anywhere in `pdf-toolbox-core`
(`Pdf.Core.Encryption.mkKey`/`mkDecryptor`, per Algorithm 2 of the PDF spec's
encryption key derivation and the per-object key salt) — verified directly
against the real `Pdf.Core.Encryption` source. This follows the same
full-but-real-and-narrow scoping the user chose for the crypto port overall
(real MD5, not a stub — but only the one algorithm this consumer needs, not
the other 15).

A second module, `Crypto.Hash.SHA1`, was later brought in for a different
consumer: RFC 6455's WebSocket handshake fixes SHA-1 as the function behind
`Sec-WebSocket-Accept` (`Linen.Network.WebSockets.Handshake`), which until
then carried a placeholder. It is independent of `Crypto.Hash.MD5` and has the
same (already-covered) dependencies.

`Crypto.Hash.MD5`'s own `build-depends` (`byteable`, `bytestring`, `base`) are
either already covered by `linen` or are an internal typeclass
(`Crypto.Classes`, from the separate `crypto-api` package) not needed for a
direct `ByteArray → ByteArray` hash function.

## Topologically sorted modules

<!-- 1. `Crypto.Hash.MD5` — ported as `Linen/Crypto/MD5.lean` (namespace
   `Crypto.MD5`): a pure, structurally-recursive implementation of RFC 1321's
   MD5 (fixed 64-round compression function per 512-bit block, no `partial`
   needed — the block count is `ByteArray.size`-derived and strictly
   decreasing). -->

<!-- 2. `Crypto.Hash.SHA1` — ported as `Linen/Crypto/SHA1.lean` (namespace
   `Crypto.SHA1`): FIPS 180-4 SHA-1 in the same style — an 80-word message
   schedule and 80 rounds, each a fold over a fixed range; `hash_size`
   proves the 20-byte output. -->
