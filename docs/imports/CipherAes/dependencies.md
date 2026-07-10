# `cipher-aes` module dependencies

Topological order of every module of the [`cipher-aes`](https://hackage.haskell.org/package/cipher-aes)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md)'s `AESV2`-algorithm
decryptor.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`cipher-aes` exposes `Crypto.Cipher.AES` (ECB/CBC/CTR/XTS/GCM modes, `initAES`,
key sizes 128/192/256). `Pdf.Core.Encryption.mkDecryptor`'s `AESV2` branch
calls exactly `initAES` (128-bit key, per the PDF spec's `AESV2` crypt filter)
and `decryptCBC` — one-shot decrypt of the whole (already `Streams.toList`-
slurped) ciphertext, using the stream's leading 16 bytes as the IV. No other
mode (ECB/CTR/XTS/GCM) or encrypt-direction function is called anywhere in
`pdf-toolbox-*` — those are not ported, matching the "real but narrow" crypto
scope: a genuine, working AES-128 block cipher and CBC chaining, not the
whole package's mode/key-size matrix.

`cipher-aes`'s own `build-depends` (`crypto-cipher-types`, `securemem`,
`bytestring`, `base`) — as with [`cipher-rc4`](../CipherRc4/dependencies.md),
`crypto-cipher-types`' generic typeclass surface isn't needed for a direct,
concrete `decryptCBC` call, and `securemem` (constant-time/zeroing memory) is
an upstream side-channel hardening detail with no equivalent call site in
`pdf-toolbox-core` (it never wipes key material) — not ported.

## Topologically sorted modules

<!-- 1. `Crypto.Cipher.AES` — ported as `Linen/Crypto/AES.lean` (namespace
   `Crypto.AES`): a real AES-128 block cipher (`initAES : ByteArray → AESKey`,
   expanding the 128-bit key into 11 round keys via the standard key
   schedule) and `decryptCBC : AESKey → ByteArray → ByteArray → ByteArray`
   (IV, ciphertext → plaintext; each 16-byte block's `InvSubBytes`/
   `InvShiftRows`/`InvMixColumns`/`AddRoundKey` decrypt round, chained via
   CBC XOR-with-previous-ciphertext-block — structurally recursive over the
   block count). -->

