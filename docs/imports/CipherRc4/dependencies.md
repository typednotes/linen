# `cipher-rc4` module dependencies

Topological order of every module of the [`cipher-rc4`](https://hackage.haskell.org/package/cipher-rc4)
Hackage package imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
Hackage-import convention. A prerequisite of
[`pdf-toolbox-core`](../PdfToolboxCore/dependencies.md)'s `V2`-algorithm
(RC4) decryptor.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

`cipher-rc4` exposes one module, `Crypto.Cipher.RC4`, providing `initCtx` (key
setup / KSA) and `combine` (the keystream-generation-and-XOR step, PRGA).
`Pdf.Core.Encryption.mkDecryptor` calls exactly these two functions (streamed
one chunk at a time via a per-object, per-stream RC4 context held in an
`IORef`) — the entire upstream module surface is used, nothing to exclude.

`cipher-rc4`'s own `build-depends` (`crypto-cipher-types`, `bytestring`,
`base`) — `crypto-cipher-types`' generic `Cipher`/`StreamCipher` typeclasses
are not needed for a direct, concrete RC4 implementation (this consumer never
calls RC4 through the generic interface, only the two named functions above),
so `crypto-cipher-types` itself is not separately ported.

## Topologically sorted modules

<!-- 1. `Crypto.Cipher.RC4` — ported as `Linen/Crypto/RC4.lean` (namespace
   `Crypto.RC4`): `initCtx : ByteArray → RC4Ctx` (256-byte S-box + two index
   counters, structurally-recursive 256-round KSA) and
   `combine : RC4Ctx → ByteArray → RC4Ctx × ByteArray` (PRGA, structurally
   recursive over the input length). -->

