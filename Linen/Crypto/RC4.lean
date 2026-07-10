/-
  Linen.Crypto.RC4 — the RC4 stream cipher

  Ports Hackage's `cipher-rc4` (`Crypto.Cipher.RC4`), per
  `docs/imports/CipherRc4/dependencies.md`. RC4 is a symmetric stream cipher:
  a key-scheduling algorithm (KSA, `initCtx`) builds a 256-byte S-box
  permutation from the key, and a pseudo-random generation algorithm (PRGA,
  `combine`) streams that permutation into a keystream which is XORed with the
  input — the same operation for both encryption and decryption.

  This module is a prerequisite of the PDF Standard Security Handler's
  V2/RC4 decryptor (`Pdf.Core.Encryption.mkDecryptor`), which drives `initCtx`
  once per stream and then feeds it through `combine` a chunk at a time.

  Both `initCtx`'s 256-round mixing and `combine`'s per-byte keystream
  generation are **structural recursions** — the KSA folds over the fixed
  list `List.range 256`, and the PRGA recurses over the input `ByteArray`'s
  `List UInt8` representation — so no `partial def` and no fuel parameter is
  needed anywhere in this module.
-/

namespace Crypto.RC4

/-! ── Context ── -/

/-- RC4 running state: the 256-byte S-box permutation (`s`, one byte per
    entry, itself holding a value in `0–255`) plus the two indices `i`/`j`
    used by the pseudo-random generation algorithm. `s.size = 256` is an
    invariant maintained by `initCtx` and `combine`, not enforced in the
    type. -/
structure Ctx where
  /-- The 256-entry S-box permutation. -/
  s : ByteArray
  /-- The first running index, advanced once per output byte. -/
  i : Nat
  /-- The second running index, updated from the S-box contents at `i`. -/
  j : Nat

/-! ── Key-scheduling algorithm (KSA) ── -/

/-- Swap the bytes at positions `a` and `b` of an S-box. -/
private def swap (s : ByteArray) (a b : Nat) : ByteArray :=
  let sa := s.get! a
  let sb := s.get! b
  (s.set! a sb).set! b sa

/-- The identity permutation `#[0, 1, …, 255]`, the KSA's starting S-box. -/
private def identitySBox : ByteArray :=
  (List.range 256).foldl (fun acc n => acc.push n.toUInt8) ByteArray.empty

/-- One round of the KSA mixing loop, advancing `j` and swapping `S[n] ↔
    S[j]` using one byte of the (cyclically-indexed) key. `keyLen` is passed
    separately (rather than recomputed from `key`) since it is loop-invariant. -/
private def ksaRound (key : ByteArray) (keyLen : Nat) (acc : ByteArray × Nat) (n : Nat) :
    ByteArray × Nat :=
  let (s, j) := acc
  let j' := (j + (s.get! n).toNat + (key.get! (n % keyLen)).toNat) % 256
  (swap s n j', j')

/-- Key-scheduling algorithm: build the initial RC4 context from a key.
    Starts from the identity S-box and runs 256 mixing rounds (a structural
    fold over `List.range 256`, not a `partial def`), each swapping two S-box
    entries using successive (cyclically-repeated) key bytes. Both running
    indices start at `0`, per the classic PRGA initialization.

    An empty key has no well-defined RC4 schedule (there is no key byte to
    mix in); `initCtx` leaves the S-box as the identity permutation in that
    degenerate case rather than dividing by a zero key length. -/
def initCtx (key : ByteArray) : Ctx :=
  let keyLen := key.size
  if keyLen = 0 then
    { s := identitySBox, i := 0, j := 0 }
  else
    let (s, _) := (List.range 256).foldl (ksaRound key keyLen) (identitySBox, 0)
    { s := s, i := 0, j := 0 }

/-! ── Pseudo-random generation algorithm (PRGA) ── -/

/-- Advance the PRGA state by one step: bump `i`, update `j` from the S-box
    entry at the new `i`, swap those two entries, and return the resulting
    context together with the next keystream byte. -/
private def step (ctx : Ctx) : Ctx × UInt8 :=
  let i' := (ctx.i + 1) % 256
  let si := ctx.s.get! i'
  let j' := (ctx.j + si.toNat) % 256
  let sj := ctx.s.get! j'
  let s' := swap ctx.s i' j'
  let k := s'.get! ((si.toNat + sj.toNat) % 256)
  ({ s := s', i := i', j := j' }, k)

/-- Combine a context with a list of input bytes: advance the PRGA once per
    byte and XOR each input byte with the resulting keystream byte.
    Structurally recursive over the input list. -/
private def combineList : Ctx → List UInt8 → Ctx × List UInt8
  | ctx, [] => (ctx, [])
  | ctx, b :: bs =>
    let (ctx', k) := step ctx
    let (ctxFinal, rest) := combineList ctx' bs
    (ctxFinal, (b ^^^ k) :: rest)

/-- Pseudo-random generation algorithm: stream `input` through the RC4
    keystream derived from `ctx`, XORing byte-for-byte, and return the
    advanced context alongside the result. Since XOR is self-inverse,
    `combine` is used for both encryption and decryption — decrypting a
    ciphertext produced by `combine` (from the matching starting context)
    recovers the original plaintext.
    $$\text{combine}(\mathit{ctx}, m)_k = m_k \oplus \mathrm{keystream}(\mathit{ctx})_k$$ -/
def combine (ctx : Ctx) (input : ByteArray) : Ctx × ByteArray :=
  let (ctx', outList) := combineList ctx input.toList
  (ctx', outList.toByteArray)

end Crypto.RC4
