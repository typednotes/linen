/-
  Data.PDF.Core.Encryption — basic support for encrypted PDF files

  Ports `Pdf.Core.Encryption` from Hackage's `pdf-toolbox-core`
  (https://github.com/Yuras/pdf-toolbox, `core/lib/Pdf/Core/Encryption.hs`,
  fetched from
  `https://raw.githubusercontent.com/Yuras/pdf-toolbox/master/core/lib/Pdf/Core/Encryption.hs`),
  module 15 of the `pdf-toolbox-core` import documented in
  `docs/imports/PdfToolboxCore/dependencies.md`.

  Implements the PDF Standard Security Handler (PDF32000-1:2008 §7.6/§7.6.3.3,
  "Encryption Key Algorithm"/Algorithm 2), for revisions 2–4, using RC4 and
  AES-128-CBC. This is **real, working cryptography**, not a stub — per this
  project's standing decision to port genuine RC4/AES support rather than a
  placeholder — built on the already-ported `Crypto.RC4`, `Crypto.MD5` and
  `Crypto.AES` primitives.

  ## Design

  - Upstream's `DecryptorScope = DecryptString | DecryptStream` is renamed to
    the Lean-idiomatic `DecryptorScope.string`/`.stream` (dropping the
    redundant `Decrypt` prefix each constructor already carries via its
    namespace).

  - Upstream's `Decryptor = Ref -> DecryptorScope -> InputStream ByteString ->
    IO (InputStream ByteString)` is ported as the `abbrev Decryptor` below,
    over `Data.PDF.Stream.InputStream` (this project's pull-based stream
    type, itself over raw `ByteArray` chunks — see `Linen/Data/PDF/Stream.lean`).

  - `decryptObject`/`decryptDict`(`Entries`)/`decryptArray`(`Items`) mutually
    recurse over `Object`'s `dictRaw`/`array` cases by direct structural
    `List` pattern matching (one cons-cell at a time), exactly the pattern
    already established by `Data.PDF.Core.Object.Builder`'s
    `buildObject`/`buildDictEntries`/`buildArrayItems` mutual recursion (see
    that module's doc-comment): this is what lets Lean's structural-recursion
    checker see the decreasing measure through a recursive type nested inside
    `Array`, with no generic fold combinator obscuring it and no hand-written
    `termination_by` needed.

  - `Data.ByteString ↔ ByteArray` conversion (needed because `Object.string`
    works in terms of the project's slice-based `Data.ByteString`, while
    `Crypto.RC4`/`Crypto.MD5`/`Crypto.AES` all operate on raw `ByteArray`) is
    two small local helpers, `toByteArray`/`ofByteArray`, built from
    `Data.ByteString.copy` (materialises a slice into a fresh, exactly-sized
    `ByteArray`) and `Data.ByteString.pack`/`.unpack` respectively — no
    dedicated conversion function exists in `Data.ByteString`, and these
    round-trip through the smallest number of already-existing primitives.

  - Upstream's `mkKey`/`verifyKey`/`mkStandardDecryptor` return
    `Either String a`; ported as `Except String α`, using the already-ported
    `Data.PDF.Core.Util.notice`/dictionary accessors from `Object.Util`.

  - Upstream's `Data.Bits.xor`/`Data.ByteString.Builder`'s `word32LE`/
    `int32LE` (a `Builder` round-tripped through a lazy `ByteString`, to get
    exactly 4 little-endian bytes of a 32-bit wrapped integer, whether the
    source `Int` is meant as signed or unsigned makes no difference to the
    bit pattern) are both replaced by a single local helper,
    `uint32LEBytes`, computing the same 4-byte little-endian wrapped
    representation directly via `Int.emod`.

  - Upstream's `Algorithm = V2 | AESV2` is ported as `Algorithm.v2`/`.aesv2`
    (Lean-idiomatic constructor naming, dropping the shouty-caps).

  - `mkDecryptor`'s `AESV2` branch calls `Padding.unpadPKCS5`, a *partial*
    function upstream (raises on malformed padding). This project's own
    `Crypto.AES.unpadPKCS5` is already total (`Option`-returning — see its
    doc-comment), so the `none` case here is surfaced as a genuine
    `Data.PDF.Core.Exception.corrupted` error instead of being unreachable:
    a real totality treatment, not a panic.

  - `verifyKey`'s revision-≥3 loop (`loop 1 pass1` down to `loop 20 input =
    input`, i.e. exactly 19 successive RC4-with-XORed-key rounds — an
    ordinary bounded loop with a *fixed* iteration count intrinsic to the
    Standard Security Handler algorithm, not an unbounded search) is ported
    as `rc4XorRounds`, structurally recursive on an explicit remaining-count
    `Nat`: this is plain structural recursion on a literal, algorithm-defined
    bound, not the "fuel to dodge an unbounded loop's termination proof"
    pattern flagged elsewhere in this codebase (e.g. `Data.PDF.Core.XRef`'s
    genuinely unbounded IO loops) — `AGENTS.md`'s ban on fuel-as-a-dodge does
    not apply to a loop whose exact trip count is part of the spec itself.
-/
import Linen.Data.PDF.Core.Object
import Linen.Data.PDF.Core.Object.Util
import Linen.Data.PDF.Core.Util
import Linen.Data.PDF.Core.Exception
import Linen.Data.PDF.Stream
import Linen.Crypto.RC4
import Linen.Crypto.MD5
import Linen.Crypto.AES

namespace Data.PDF.Core.Encryption

open Data.PDF.Core.Object
open Data.PDF.Core.Object.Util
open Data.PDF.Core.Exception (corrupted)
open Data.PDF.Core.Util (notice)

private def mkName (s : String) : Data.PDF.Core.Name.Name :=
  (Data.PDF.Core.Name.Name.make (Data.ByteString.pack s.toUTF8.toList)).toOption.getD
    Data.PDF.Core.Name.Name.empty

/-! ── `Data.ByteString` ↔ `ByteArray` conversion ── -/

/-- Materialise a `Data.ByteString` slice into a fresh, exactly-sized
    `ByteArray` (see the module doc-comment: no dedicated conversion
    function exists, so this goes through `Data.ByteString.copy`). -/
private def toByteArray (bs : Data.ByteString) : ByteArray :=
  (Data.ByteString.copy bs).data

/-- Wrap a `ByteArray` back into a `Data.ByteString`. -/
private def ofByteArray (arr : ByteArray) : Data.ByteString :=
  Data.ByteString.pack arr.toList

/-! ── Decryptor type ── -/

/-- Encryption handler may specify different encryption keys for strings and
    streams. Mirrors upstream's `DecryptorScope = DecryptString |
    DecryptStream` (see the module doc-comment for the renaming). -/
inductive DecryptorScope where
  /-- Decrypt a PDF string object. -/
  | string
  /-- Decrypt stream content. -/
  | stream
deriving BEq, Repr

/-- Decrypt an input stream given the object it belongs to and which scope
    (string vs. stream) it's being decrypted in. Mirrors upstream's
    `Decryptor`. -/
abbrev Decryptor :=
  Ref → DecryptorScope → Data.PDF.Stream.InputStream → IO Data.PDF.Stream.InputStream

/-! ── Decrypting objects ── -/

/-- Decrypt a single PDF string with `decryptor`, mirroring upstream's
    `decryptStr`: wrap the bytes as a one-chunk stream, decrypt it, then
    concatenate whatever chunks come back out. -/
def decryptStr (decryptor : Decryptor) (ref : Ref) (str : Data.ByteString) :
    IO Data.ByteString := do
  let is ← Data.PDF.Stream.fromList [toByteArray str]
  let is' ← decryptor ref .string is
  let chunks ← Data.PDF.Stream.toList is'
  pure (ofByteArray (chunks.foldl (· ++ ·) ByteArray.empty))

mutual
  /-- Decrypt every value in a dictionary's entry list, recursing into
      nested strings/dictionaries/arrays. Mirrors upstream's `decryptDict`
      (`Traversable.forM`), written as direct structural `List` recursion —
      see the module doc-comment for why (same pattern as
      `Object.Builder.buildDictEntries`). -/
  def decryptDictEntries (decryptor : Decryptor) (ref : Ref) :
      List (Name × Object) → IO (List (Name × Object))
    | [] => pure []
    | (k, v) :: rest => do
        let v' ← decryptObject decryptor ref v
        let rest' ← decryptDictEntries decryptor ref rest
        pure ((k, v') :: rest')

  /-- Decrypt every item of an array. Mirrors upstream's `decryptArray`
      (`Vector.forM`). -/
  def decryptArrayItems (decryptor : Decryptor) (ref : Ref) :
      List Object → IO (List Object)
    | [] => pure []
    | x :: xs => do
        let x' ← decryptObject decryptor ref x
        let xs' ← decryptArrayItems decryptor ref xs
        pure (x' :: xs')

  /-- Decrypt an object with `decryptor`: strings are decrypted directly,
      dictionaries/arrays recurse into their contents, everything else is
      returned unchanged. Mirrors upstream's `decryptObject`. -/
  def decryptObject (decryptor : Decryptor) (ref : Ref) : Object → IO Object
    | .string s => .string <$> decryptStr decryptor ref s
    | .dictRaw entries => do
        let entries' ← decryptDictEntries decryptor ref entries.toList
        pure (.dictRaw entries'.toArray)
    | .array items => do
        let items' ← decryptArrayItems decryptor ref items.toList
        pure (.array items'.toArray)
    | other => pure other
end

/-! ── The default user password ── -/

/-- The default user password, PDF32000-1:2008 §7.6.3.3, used when no
    password is supplied. -/
def defaultUserPassword : ByteArray :=
  ByteArray.mk #[
    0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41, 0x64, 0x00, 0x4E,
    0x56, 0xFF, 0xFA, 0x01, 0x08, 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68,
    0x3E, 0x80, 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A]

/-! ── Little-endian byte encoding ── -/

/-- Encode `i` as 4 little-endian bytes of `i` wrapped into the unsigned
    32-bit range (`i.emod 2^32`) — the same bit pattern whether the value at
    hand is meant to be read back as `Word32` or `Int32`. Replaces upstream's
    `Data.ByteString.Builder`-via-`toLazyByteString` round trip through
    `word32LE`/`int32LE` (see the module doc-comment). -/
def uint32LEBytes (i : Int) : ByteArray :=
  let n : Nat := (((i % 4294967296) + 4294967296) % 4294967296).toNat
  ByteArray.mk #[
    UInt8.ofNat (n % 256),
    UInt8.ofNat ((n / 256) % 256),
    UInt8.ofNat ((n / 65536) % 256),
    UInt8.ofNat ((n / 16777216) % 256)]

/-- Clip-safe `take n` for a `ByteArray`: never runs past the array's end
    (mirrors `Data.ByteString.take`'s clipping semantics, which the plain
    `ByteArray.extract` alone doesn't provide when `n` exceeds the size). -/
def takeBytes (n : Nat) (bs : ByteArray) : ByteArray :=
  bs.extract 0 (min n bs.size)

/-! ── Key derivation and verification (PDF32000-1:2008 §7.6.3.3) ── -/

/-- The trailer's `/ID`'s first element, as raw bytes. Shared by `mkKey` and
    `verifyKey`, both of which need it. -/
private def firstIdBytes (tr : Dict) : Except String ByteArray := do
  let idsObj ← notice (tr.get? (mkName "ID")) "ID should be an array"
  let ids ← notice (arrayValue idsObj) "ID should be an array"
  match ids.toList with
  | [] => .error "ID array is empty"
  | x :: _ =>
      let s ← notice (stringValue x) "The first element if ID should be a string"
      pure (toByteArray s)

/-- Derive the encryption key from the trailer/encryption dictionary and a
    (32-byte) password. Mirrors upstream's `mkKey` (PDF32000-1:2008
    Algorithm 2). -/
def mkKey (tr enc : Dict) (pass : ByteArray) (n : Nat) : Except String ByteArray := do
  let oVal ← do
    let o ← notice (enc.get? (mkName "O")) "O is missing"
    let s ← notice (stringValue o) "o should be a string"
    pure (toByteArray s)
  let pVal ← do
    let o ← notice (enc.get? (mkName "P")) "P is missing"
    let i ← notice (intValue o) "P should be an integer"
    pure (uint32LEBytes i)
  let idVal ← firstIdBytes tr
  let rVal ← notice (enc.get? (mkName "R") >>= intValue) "R should be an integer"
  let encMD ←
    match enc.get? (mkName "EncryptMetadata") with
    | none => pure true
    | some o => notice (boolValue o) "EncryptMetadata should be a bool"
  let pad : ByteArray := if rVal < 4 || encMD then ByteArray.empty else ByteArray.mk #[255, 255, 255, 255]
  let ekey' := takeBytes n (Crypto.MD5.hash (pass ++ oVal ++ pVal ++ idVal ++ pad))
  let ekey :=
    if rVal < 3 then ekey'
    else (List.range 50).foldl (fun bs _ => takeBytes n (Crypto.MD5.hash bs)) ekey'
  pure ekey

/-- Exactly 19 successive rounds of "RC4 with the key XORed by the round
    number" — see the module doc-comment for why this is plain structural
    recursion on a fixed, algorithm-defined bound, not a fuel dodge. Mirrors
    upstream's `loop 1 pass1` through `loop 19 ...` (`loop 20 input = input`
    is the base case, never itself transforming `input`). -/
private def rc4XorRounds (ekey : ByteArray) : Nat → Nat → ByteArray → ByteArray
  | 0, _, input => input
  | remaining + 1, i, input =>
      let ekey' := ByteArray.mk (ekey.data.map (· ^^^ UInt8.ofNat i))
      let out := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey') input).2
      rc4XorRounds ekey remaining (i + 1) out

/-- Verify a candidate encryption key against the encryption dictionary's
    `/U` entry. Mirrors upstream's `verifyKey`. -/
def verifyKey (tr enc : Dict) (ekey : ByteArray) : Except String Bool := do
  let rVal ← notice (enc.get? (mkName "R") >>= intValue) "R should be an integer"
  let idVal ← firstIdBytes tr
  let uVal ← do
    let s ← notice (enc.get? (mkName "U") >>= stringValue) "U should be a string"
    pure (toByteArray s)
  if rVal == 2 then
    let uVal' := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey) defaultUserPassword).2
    pure (uVal == uVal')
  else
    let pass1 := (Crypto.RC4.combine (Crypto.RC4.initCtx ekey)
      (takeBytes 16 (Crypto.MD5.hash (defaultUserPassword ++ idVal)))).2
    let uVal' := rc4XorRounds ekey 19 1 pass1
    pure (takeBytes 16 uVal == takeBytes 16 uVal')

/-! ── Building a decryptor for a stream/string ── -/

/-- Which cryptographic filter method a crypt filter dictionary specifies.
    Mirrors upstream's `Algorithm = V2 | AESV2` (see the module doc-comment
    for the renaming). -/
inductive Algorithm where
  /-- RC4. -/
  | v2
  /-- AES-128-CBC. -/
  | aesv2
deriving BEq, Repr

/-- Build a `Decryptor` for one particular `Ref`, deriving the per-object key
    from the file-level encryption key `ekey` (PDF32000-1:2008 Algorithm 1).
    Mirrors upstream's `mkDecryptor`. -/
def mkDecryptor (alg : Algorithm) (ekey : ByteArray) (n : Nat) (ref : Ref)
    (is : Data.PDF.Stream.InputStream) : IO Data.PDF.Stream.InputStream := do
  let salt : ByteArray := match alg with
    | .v2 => ByteArray.empty
    | .aesv2 => ByteArray.mk #[0x73, 0x41, 0x6C, 0x54]  -- "sAlT"
  let key := takeBytes (min 16 n + 5) (Crypto.MD5.hash
    (ekey ++ (uint32LEBytes ref.index).extract 0 3 ++ (uint32LEBytes ref.generation).extract 0 2 ++ salt))
  match alg with
  | .v2 => do
      let ctxRef ← IO.mkRef (Crypto.RC4.initCtx key)
      Data.PDF.Stream.makeInputStream do
        match ← Data.PDF.Stream.read is with
        | none => pure none
        | some chunk => do
            let ctx ← ctxRef.get
            let (ctx', res) := Crypto.RC4.combine ctx chunk
            ctxRef.set ctx'
            pure (some res)
  | .aesv2 => do
      let chunks ← Data.PDF.Stream.toList is
      let content := chunks.foldl (· ++ ·) ByteArray.empty
      let initV := content.extract 0 (min 16 content.size)
      let aesKey := Crypto.AES.initAES key
      let decrypted := Crypto.AES.decryptCBC aesKey initV (content.extract (min 16 content.size) content.size)
      match Crypto.AES.unpadPKCS5 decrypted with
      | some unpadded => Data.PDF.Stream.fromByteString unpadded
      | none => throw (corrupted "AESV2: malformed PKCS5 padding")

/-! ── The Standard Security Handler ── -/

/-- Build a decryptor from the trailer/encryption dictionaries and user
    password for the "V4" crypt-filter-dictionary scheme (PDF32000-1:2008
    §7.6.5), dispatching per-scope to the filters named `/StrF`/`/StmF`.
    Mirrors upstream's `mk4`. -/
private def mkStandardDecryptorV4 (tr enc : Dict) (pass : ByteArray) :
    Except String (Option Decryptor) := do
  let cfDict ← do
    let o ← notice (enc.get? (mkName "CF")) "CF is missing in crypt handler V4"
    notice (dictValue o) "CF should be a dictionary"
  let keysMap ← cfDict.toList.mapM fun (name, obj) => do
    let dict ← notice (dictValue obj) "Crypto filter should be a dictionary"
    let n ← notice (dict.get? (mkName "Length") >>= intValue) "Crypto filter length should be int"
    let algName ← notice (dict.get? (mkName "CFM") >>= nameValue) "CFM should be a name"
    let alg ←
      if algName == mkName "V2" then pure Algorithm.v2
      else if algName == mkName "AESV2" then pure Algorithm.aesv2
      else .error s!"Unknown crypto method: {reprStr algName}"
    let ekey ← mkKey tr enc pass n.toNat
    pure (name, (ekey, n.toNat, alg))
  let keysHM := Std.HashMap.ofList keysMap
  let (stdCfKey, _, _) ← notice (keysHM.get? (mkName "StdCF")) "StdCF is missing"
  let ok ← verifyKey tr enc stdCfKey
  if !ok then pure none
  else
    let strFName ← notice (enc.get? (mkName "StrF") >>= nameValue) "StrF is missing"
    let (strFKey, strFN, strFAlg) ← notice (keysHM.get? strFName)
      s!"Crypto filter not found: {reprStr strFName}"
    let stmFName ← notice (enc.get? (mkName "StmF") >>= nameValue) "StmF is missing"
    let (stmFKey, stmFN, stmFAlg) ← notice (keysHM.get? stmFName)
      s!"Crypto filter not found: {reprStr stmFName}"
    pure (some (fun ref scope is =>
      match scope with
      | .string => mkDecryptor strFAlg strFKey strFN ref is
      | .stream => mkDecryptor stmFAlg stmFKey stmFN ref is))

/-- Build a decryptor for the classic (V1/V2, RC4-only) crypt scheme.
    Mirrors upstream's `mk12`. -/
private def mkStandardDecryptorLegacy (tr enc : Dict) (pass : ByteArray) (v : Int) :
    Except String (Option Decryptor) := do
  let n ←
    match v with
    | 1 => pure 5
    | 2 =>
      match enc.get? (mkName "Length") with
      | some o => (notice (intValue o) "Length should be an integer").map (fun l => l.toNat / 8)
      | none => .error "Length is missing"
    | _ => .error s!"Unsuported encryption handler version: {v}"
  let ekey ← mkKey tr enc pass n
  let ok ← verifyKey tr enc ekey
  pure (if !ok then none else some (fun ref (_ : DecryptorScope) is => mkDecryptor .v2 ekey n ref is))

/-- Build a decryptor for the PDF Standard Security Handler
    (PDF32000-1:2008 §7.6), given the document trailer, the encryption
    dictionary, and a 32-byte user password (see `defaultUserPassword`).
    Returns `none` if the password doesn't verify (i.e. it's the wrong
    password), or an `Except`-carried error string if the encryption
    dictionary itself is malformed/unsupported. Mirrors upstream's
    `mkStandardDecryptor`. -/
def mkStandardDecryptor (tr enc : Dict) (pass : ByteArray) :
    Except String (Option Decryptor) := do
  let filterType ← notice (enc.get? (mkName "Filter") >>= nameValue) "Filter should be a name"
  unless filterType == mkName "Standard" do
    .error s!"Unsupported encryption handler: {reprStr filterType}"
  let v ← notice (enc.get? (mkName "V") >>= intValue) "V should be an integer"
  if v == 4 then mkStandardDecryptorV4 tr enc pass
  else mkStandardDecryptorLegacy tr enc pass v

end Data.PDF.Core.Encryption
