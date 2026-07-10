# `crypto-api` module dependencies

Topological order of every module of the [`crypto-api`](https://hackage.haskell.org/package/crypto-api)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md)'s `AESV2` decryptor,
which needs PKCS5 unpadding after the CBC decrypt.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`crypto-api` exposes a generic cryptography typeclass surface (`Crypto.Classes`,
`Crypto.HMAC`, `Crypto.Modes`, `Crypto.Random`, `Crypto.Types`, `Crypto.Padding`).
`Pdf.Core.Encryption.mkDecryptor`'s `AESV2` branch calls exactly one function
from the whole package, `Crypto.Padding.unpadPKCS5`, after the AES-CBC
decrypt. None of the generic `Cipher`/`Hash` typeclasses, HMAC, or random-
number generation are used anywhere in `pdf-toolbox-*` — not ported, per the
same "concrete functions actually called, not the whole generic surface"
scoping already applied to [`cryptohash`](../Cryptohash/dependencies.md) and
[`cipher-rc4`](../CipherRc4/dependencies.md)/[`cipher-aes`](../CipherAes/dependencies.md).

Given it is exactly one small, self-contained function with a single caller,
it is folded directly into the AES port (`Linen/Crypto/AES.lean`) rather than
given its own module/namespace, per AGENTS.md's "place modules the way the
Lean stdlib would" rule (a one-function generic-crypto package doesn't
warrant a standalone `Linen.Crypto.*` module distinct from its only caller).

## Topologically sorted modules

<!-- 1. `Crypto.Padding` — ported as `Linen/Crypto/AES.lean`'s `unpadPKCS5 :
   ByteArray → Option ByteArray` (reads the last byte as the pad length `n`,
   validates `1 ≤ n ≤ 16` and that the trailing `n` bytes all equal `n`,
   returning `none` on malformed padding rather than upstream's `error`). -->

