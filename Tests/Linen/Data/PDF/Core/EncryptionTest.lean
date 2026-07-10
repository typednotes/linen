/-
  Tests for `Linen.Data.PDF.Core.Encryption`.

  Pure helpers (`uint32LEBytes`, `takeBytes`, `defaultUserPassword`) are
  checked with `#guard`; everything touching key derivation/verification or
  the `IO`-returning decryptors is checked with `#eval` (a thrown error, or
  a mismatched result reaching the final `throw`, fails the build) —
  following `Tests/Linen/Crypto/RC4Test.lean`/`AESTest.lean` and
  `Tests/Linen/Data/PDF/Core/UtilTest.lean`'s established mixed pattern.

  Since `Crypto.AES` (deliberately) has no encryption half — only the
  decryption this port needs — the `.v2`/RC4 half of `mkDecryptor` is
  exercised via RC4's own self-inverse property (applying the same
  per-object keystream twice returns the original bytes), while the
  `.aesv2` half is exercised only for its "malformed padding surfaces as a
  real `corrupted` error, not a panic" behaviour (see that def's
  doc-comment) rather than for round-tripping real ciphertext.
-/
import Linen.Data.PDF.Core.Encryption

open Data.PDF.Core.Object Data.PDF.Core.Object.Util Data.PDF.Core.Encryption

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

private def str (s : String) : Object := .string (Data.ByteString.pack s.toUTF8.toList)

/-- Local mirror of `Encryption`'s own private `toByteArray`/`ofByteArray`
    helpers, needed here since they are private to that module. -/
private def toByteArray (bs : Data.ByteString) : ByteArray :=
  ByteArray.mk (Data.ByteString.unpack bs).toArray

private def repeatedStr (n : Nat) (c : Char) : String := String.ofList (List.replicate n c)

namespace Tests.Data.PDF.Core.Encryption

/-! ### Little-endian byte encoding -/

-- A small, non-negative value round-trips as its ordinary little-endian
-- byte pattern.
#guard uint32LEBytes 1 == ByteArray.mk #[1, 0, 0, 0]
#guard uint32LEBytes 256 == ByteArray.mk #[0, 1, 0, 0]

-- A negative `Int` wraps into the unsigned 32-bit range before being
-- encoded (e.g. a typical PDF `/P` permissions value).
#guard uint32LEBytes (-1) == ByteArray.mk #[255, 255, 255, 255]
#guard uint32LEBytes (-3904) == uint32LEBytes (4294967296 - 3904)

/-! ### Clip-safe `take` -/

#guard takeBytes 3 (ByteArray.mk #[1, 2, 3, 4, 5]) == ByteArray.mk #[1, 2, 3]
#guard takeBytes 10 (ByteArray.mk #[1, 2, 3]) == ByteArray.mk #[1, 2, 3]
#guard takeBytes 0 (ByteArray.mk #[1, 2, 3]) == ByteArray.empty

/-! ### The default user password -/

#guard defaultUserPassword.size == 32

/-! ### Key derivation and verification (self-consistent oracle) -/

-- Build a self-consistent trailer/encryption dictionary pair for a
-- classic (R = 2) handler: pick arbitrary `/O`/`/P`/`/ID`, derive the
-- encryption key from them via `mkKey`, then compute the matching `/U`
-- entry the same way the Standard Security Handler itself would (a
-- single RC4 pass over the default user password) — this exercises the
-- real MD5/RC4 pipeline without needing a second, independent oracle.
#eval show IO Unit from do
  let tr : Dict := Std.HashMap.ofList [(mkName "ID", .array #[str "0123456789abcdef"])]
  let encBase : Dict := Std.HashMap.ofList
    [ (mkName "O", str (repeatedStr 32 'x'))
    , (mkName "P", .number (-3904))
    , (mkName "R", .number 2) ]
  match mkKey tr encBase defaultUserPassword 5 with
  | .error e => throw (IO.userError s!"mkKey failed: {e}")
  | .ok ekey =>
    let uVal := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey) defaultUserPassword).2
    let enc := encBase.insert (mkName "U") (.string (Data.ByteString.pack uVal.toList))
    match verifyKey tr enc ekey with
    | .error e => throw (IO.userError s!"verifyKey failed: {e}")
    | .ok true => pure ()
    | .ok false => throw (IO.userError "expected verifyKey to accept the self-consistent key")

-- The same oracle, but with a wrong key, must be rejected.
#eval show IO Unit from do
  let tr : Dict := Std.HashMap.ofList [(mkName "ID", .array #[str "0123456789abcdef"])]
  let encBase : Dict := Std.HashMap.ofList
    [ (mkName "O", str (repeatedStr 32 'x'))
    , (mkName "P", .number (-3904))
    , (mkName "R", .number 2) ]
  match mkKey tr encBase defaultUserPassword 5 with
  | .error e => throw (IO.userError s!"mkKey failed: {e}")
  | .ok ekey =>
    let uVal := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey) defaultUserPassword).2
    let enc := encBase.insert (mkName "U") (.string (Data.ByteString.pack uVal.toList))
    let wrongKey := ByteArray.mk (ekey.data.map (· + 1))
    match verifyKey tr enc wrongKey with
    | .error e => throw (IO.userError s!"verifyKey failed: {e}")
    | .ok false => pure ()
    | .ok true => throw (IO.userError "expected verifyKey to reject a wrong key")

-- Same oracle for a revision-≥3 handler (exercises `rc4XorRounds`'s 19
-- extra RC4-with-XORed-key passes).
#eval show IO Unit from do
  let tr : Dict := Std.HashMap.ofList [(mkName "ID", .array #[str "0123456789abcdef"])]
  let encBase : Dict := Std.HashMap.ofList
    [ (mkName "O", str (repeatedStr 32 'y'))
    , (mkName "P", .number (-3904))
    , (mkName "R", .number 3) ]
  match mkKey tr encBase defaultUserPassword 16 with
  | .error e => throw (IO.userError s!"mkKey failed: {e}")
  | .ok ekey =>
    let pass1 := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey)
      (Data.PDF.Core.Encryption.takeBytes 16
        (Crypto.MD5.hash (defaultUserPassword ++ toByteArray (Data.ByteString.pack "0123456789abcdef".toUTF8.toList))))).2
    let uVal := (List.range 19).foldl
      (fun input i =>
        let ekey' := ByteArray.mk (ekey.data.map (· ^^^ UInt8.ofNat (i + 1)))
        (Crypto.RC4.combine (Crypto.RC4.initCtx ekey') input).2)
      pass1
    let enc := encBase.insert (mkName "U") (.string (Data.ByteString.pack uVal.toList))
    match verifyKey tr enc ekey with
    | .error e => throw (IO.userError s!"verifyKey failed: {e}")
    | .ok true => pure ()
    | .ok false => throw (IO.userError "expected verifyKey to accept the self-consistent R3 key")

/-! ### Decrypting objects, via a trivial RC4 decryptor -/

-- `mkDecryptor .v2`'s per-object keystream is deterministic in
-- `(ekey, n, ref)`, so applying it twice to the same `ref` is RC4's own
-- self-inverse: encrypt-then-decrypt with the same keystream returns the
-- original bytes.
#eval show IO Unit from do
  let ekey := ByteArray.mk #[1, 2, 3, 4, 5]
  let ref : Ref := ⟨7, 0⟩
  let plain := ByteArray.mk "hello, pdf!".toUTF8.data
  let is0 ← Data.PDF.Stream.fromByteString plain
  let is1 ← mkDecryptor .v2 ekey 5 ref is0
  let scrambled ← (Data.PDF.Stream.toList is1).map (·.foldl (· ++ ·) ByteArray.empty)
  let is2 ← Data.PDF.Stream.fromByteString scrambled
  let is3 ← mkDecryptor .v2 ekey 5 ref is2
  let roundTripped ← (Data.PDF.Stream.toList is3).map (·.foldl (· ++ ·) ByteArray.empty)
  unless roundTripped == plain do
    throw (IO.userError s!"RC4 round-trip mismatch: {roundTripped.toList} vs {plain.toList}")

-- `decryptObject`/`decryptStr` recurse into a dictionary's/array's nested
-- strings and leave every other object shape untouched, using the same
-- self-inverse RC4 decryptor as above wired through the `Decryptor`
-- abbreviation.
#eval show IO Unit from do
  let ekey := ByteArray.mk #[9, 9, 9]
  let ref : Ref := ⟨1, 0⟩
  let decryptor : Decryptor := fun r _scope is => mkDecryptor .v2 ekey 5 r is
  let plain := ByteArray.mk "secret".toUTF8.data
  -- Scramble once "at rest", the way an encrypted file would store it.
  let scrambled ← do
    let is0 ← Data.PDF.Stream.fromByteString plain
    let is1 ← decryptor ref .string is0
    (Data.PDF.Stream.toList is1).map (·.foldl (· ++ ·) ByteArray.empty)
  let obj := Object.dictRaw #[
    (mkName "S", .string (Data.ByteString.pack scrambled.toList)),
    (mkName "N", .number 42)]
  let obj' ← decryptObject decryptor ref obj
  match obj' with
  | .dictRaw entries =>
    match entries.toList.lookup (mkName "S") with
    | some (.string decrypted) =>
      unless toByteArray decrypted == plain do
        throw (IO.userError "decryptObject did not recover the plaintext string")
    | _ => throw (IO.userError "missing/wrong-shaped /S entry after decryption")
  | _ => throw (IO.userError "decryptObject changed the object's shape")

/-! ### AESV2 malformed-padding failure surfaces as a real error -/

-- Feeding `mkDecryptor .aesv2` a 16-byte block that (overwhelmingly, for
-- an arbitrary key) does not decrypt to valid PKCS5 padding must raise a
-- `corrupted` error rather than panic — this is what
-- `Crypto.AES.unpadPKCS5`'s totality buys the port (see the module
-- doc-comment).
#eval show IO Unit from do
  let ekey := ByteArray.mk #[1, 2, 3, 4, 5]
  let ref : Ref := ⟨3, 0⟩
  -- 32 bytes: one IV-sized block plus one ciphertext block of arbitrary
  -- data, deliberately not a valid encryption of anything.
  let garbage := ByteArray.mk (Array.range 32 |>.map (fun i => UInt8.ofNat (i * 37 % 256)))
  let is0 ← Data.PDF.Stream.fromByteString garbage
  MonadExcept.tryCatch
    (do
      let _ ← mkDecryptor .aesv2 ekey 16 ref is0
      throw (IO.userError "expected mkDecryptor .aesv2 to reject malformed padding"))
    (fun e => match e with
      | .userError msg =>
        if msg.startsWith "corrupted" ∨ (msg.splitOn "AESV2").length > 1 then pure ()
        else throw (IO.userError s!"unexpected error message: {msg}")
      | other => throw other)

end Tests.Data.PDF.Core.Encryption
