/- Versioned native Lean cells. A compiler attests a reusable worker and a
Lean caller binds its wire codecs before calling the already compiled code. -/
import Linen.Control.Monad.Effect
import Linen.Crypto.SHA256
import Linen.Data.Hex
import Linen.System.Git.Remote
import Linen.System.GitFn.Worker
import Linen.System.Worker

namespace System.Cell

open Lean (Json Name FromJson ToJson fromJson? toJson)
open Control.Resource (Owned Program)

/-- Canonical SHA-256 digest, serialized as lowercase hexadecimal. -/
structure Digest where
  private mk ::
  /-- The 64 hexadecimal digits. -/
  hex : String
  deriving DecidableEq, Repr

/-- Validate a digest supplied as text. -/
def Digest.check (text : String) : Except String Digest :=
  if text.length == 64 && text.all (fun c => c.isDigit || ('a' ≤ c && c ≤ 'f'))
  then .ok ⟨text⟩ else .error "expected a lowercase SHA-256 digest"

instance : ToJson Digest := ⟨fun d => .str d.hex⟩
instance : FromJson Digest := ⟨fun j => j.getStr? >>= Digest.check⟩

/-- Hash exact bytes. -/
def Digest.ofBytes (bytes : ByteArray) : IO Digest :=
  return ⟨Data.Hex.encode (← Crypto.SHA256.digest bytes)⟩

/-- Domain of a content-addressed cell reference. -/
inductive RefKind where
  | intent | policy | contract | codec | build | manifest | artifact

/-- A digest named by its use in the cell format. -/
structure Ref (_kind : RefKind) where
  /-- Content digest. -/
  digest : Digest
  deriving DecidableEq, Repr

instance : ToJson (Ref k) := ⟨fun r => toJson r.digest⟩
instance : FromJson (Ref k) := ⟨fun j => return ⟨← fromJson? j⟩⟩

/-- Immutable Git coordinates for a Lean implementation. -/
structure GitSource where
  private mk ::
  /-- Repository URL. -/
  repository : String
  /-- Exact Git commit. -/
  commit : GitFn.CommitSha
  /-- Relative project path, or `.` for the root. -/
  project : String
  /-- Module containing the implementation. -/
  moduleName : Name
  /-- Fully qualified Lean declaration. -/
  declaration : Name
  deriving DecidableEq, Repr

/-- Check producer coordinates before writing or accepting a manifest. -/
def GitSource.check (repository commit project moduleName declaration : String) :
    Except String GitSource := do
  discard (Git.Repository.parse repository true)
  let some sha := GitFn.CommitSha.ofString? commit | throw "cell source requires a full Git commit"
  let project := if project.isEmpty then "." else project
  unless project.length ≤ 512 && (project == "." || (project.splitOn "/").all Git.Repository.isSegment) do
    throw "invalid cell project path"
  unless !moduleName.isEmpty && moduleName.length ≤ 128 &&
      !declaration.isEmpty && declaration.length ≤ 128 do
    throw "invalid cell module or declaration length"
  return ⟨repository, sha, project, ← Data.Name.parse moduleName, ← Data.Name.parse declaration⟩

instance : ToJson GitSource := ⟨fun s => Json.mkObj [
  ("repository", toJson s.repository), ("commit", toJson s.commit.hex), ("project", toJson s.project),
  ("module", toJson s.moduleName.toString), ("declaration", toJson s.declaration.toString)]⟩

instance : FromJson GitSource := ⟨fun j => do
  let project ← match j.getObjVal? "project" with
    | .error _ | .ok .null => pure "."
    | .ok value => value.getStr?
  GitSource.check (← j.getObjValAs? String "repository") (← j.getObjValAs? String "commit")
    project (← j.getObjValAs? String "module") (← j.getObjValAs? String "declaration")⟩

/-- The authoring records and source coordinates that identify a cell revision. -/
structure Revision where
  /-- Exact user intent and acceptance examples. -/
  intentSha256 : Ref .intent
  /-- Independently approved platform ceiling. -/
  constraintsSha256 : Ref .policy
  /-- Lean signature declaration and contract source. -/
  contractSha256 : Ref .contract
  /-- Implementation to which the contract applies. -/
  source : GitSource
  deriving DecidableEq, Repr, ToJson, FromJson

/-- Compatibility profile independently selected by the consumer. -/
structure Runtime where
  /-- Exact Lean compiler/toolchain identity. -/
  leanToolchain : String
  /-- Target triple. -/
  target : String
  /-- Native execution ABI identity. -/
  runtimeAbi : String
  /-- Version of the compiler's admission and audit checks. -/
  auditVersion : String
  /-- Versioned ordered input and dependent result codecs. -/
  codecSha256 : Ref .codec
  deriving DecidableEq, Repr, ToJson, FromJson

private def Runtime.validate (p : Runtime) : Except String Unit := do
  unless [p.leanToolchain, p.target, p.runtimeAbi, p.auditVersion].all
      (fun s => !s.isEmpty && s.length ≤ 128 && s.all (fun c => c.toNat > 32 && c.toNat < 127)) do
    throw "invalid cell runtime profile"

/-- Digests of the actual compiler inputs. The builder resolves imports and
lock files before computing these digests. -/
structure BuildInputs where
  /-- Admitted source, including the contract and compiled capability arguments. -/
  sourceSha256 : Digest
  /-- Generated worker wrapper and codecs. -/
  wrapperSha256 : Digest
  /-- Transitive dependency sources and their lock files. -/
  dependenciesSha256 : Digest
  /-- Native link inputs and compiler/linker options. -/
  linkSha256 : Digest
  deriving ToJson, FromJson

/-- Compute a build key from compilation inputs and the runtime profile. The
authoring record is intentionally independent of native cache identity. -/
def BuildInputs.key (inputs : BuildInputs) (runtime : Runtime) : IO (Ref .build) := do
  let record := Json.arr #[.str "linen.cell.build.v1", toJson inputs, toJson runtime]
  return ⟨← Digest.ofBytes record.compress.toUTF8⟩

/-- A compiler attestation for a single native worker executable. -/
structure Manifest where
  private mk ::
  /-- Immutable authoring records. -/
  revision : Revision
  /-- Digest of the actual compiler inputs. -/
  buildKey : Ref .build
  /-- Required execution profile. -/
  runtime : Runtime
  /-- Digest of the executable bytes. -/
  artifactSha256 : Ref .artifact
  /-- Length of the executable bytes. -/
  artifactBytes : Nat
  deriving DecidableEq, Repr

/-- The full compiler-attested format begins at version 2. -/
def manifestVersion : Nat := 2

/-- Byte limit for one native worker executable. -/
def maxArtifactBytes : Nat := 128 * 1024 * 1024

private def Manifest.checked (revision : Revision) (buildKey : Ref .build)
    (runtime : Runtime) (artifactSha256 : Ref .artifact) (artifactBytes : Nat) : Except String Manifest := do
  runtime.validate
  unless 0 < artifactBytes && artifactBytes ≤ maxArtifactBytes do throw "invalid cell artifact size"
  return ⟨revision, buildKey, runtime, artifactSha256, artifactBytes⟩

instance : ToJson Manifest := ⟨fun m => Json.mkObj [
  ("version", toJson manifestVersion), ("revision", toJson m.revision), ("buildKey", toJson m.buildKey),
  ("runtime", toJson m.runtime), ("artifactSha256", toJson m.artifactSha256),
  ("artifactBytes", toJson m.artifactBytes)]⟩

instance : FromJson Manifest := ⟨fun j => do
  unless (← j.getObjValAs? Nat "version") == manifestVersion do throw "unsupported cell manifest version"
  Manifest.checked (← j.getObjValAs? Revision "revision") (← j.getObjValAs? (Ref .build) "buildKey")
    (← j.getObjValAs? Runtime "runtime") (← j.getObjValAs? (Ref .artifact) "artifactSha256")
    (← j.getObjValAs? Nat "artifactBytes")⟩

/-- Make a manifest for an already built and checked executable. Only the
trusted compiler can attest that these are the outputs of those inputs. -/
def Manifest.make (revision : Revision) (inputs : BuildInputs) (runtime : Runtime)
    (artifact : ByteArray) : IO Manifest := do
  unless 0 < artifact.size && artifact.size ≤ maxArtifactBytes do
    throw (IO.userError "invalid cell artifact size")
  let key ← inputs.key runtime
  let digest ← Digest.ofBytes artifact
  IO.ofExcept ((Manifest.checked revision key runtime ⟨digest⟩ artifact.size).mapError IO.userError)

/-- Return the exact UTF-8 text that a producer can sign with detached OpenPGP. -/
def Manifest.render (m : Manifest) : String := (toJson m).compress

private def signText (text fingerprint : String) : IO String := do
  unless [40, 64].contains fingerprint.length && (Data.Hex.decode fingerprint).isSome do
    throw (IO.userError "provide a complete compiler signing-key fingerprint")
  let result ← Process.run "gpg"
    #["--no-options", "--batch", "--no-tty", "--pinentry-mode", "cancel", "--armor", "--detach-sign",
      "--local-user", fingerprint ++ "!", "--output", "-"] 30000 (input := some text)
  unless result.ok && result.stdout.startsWith "-----BEGIN PGP SIGNATURE-----" do
    throw (IO.userError "cell compiler signing failed")
  return result.stdout

/-- Sign the rendered manifest with a compiler's explicitly selected OpenPGP
key. Store the returned armor beside the exact text from `render`. -/
def Manifest.sign (m : Manifest) (fingerprint : String) : IO String :=
  signText m.render fingerprint

/-- Pin the exact signed manifest bytes, including their artifact digest. -/
def manifestPin (text : String) : IO (Ref .manifest) :=
  return ⟨← Digest.ofBytes text.toUTF8⟩

/-- A configured HTTPS base URL. Its paths use plain segments. -/
structure HttpsBase where
  private mk ::
  /-- Base URL without a trailing slash. -/
  url : String

/-- Validate an HTTPS base for artifacts or a trusted worker. There is no
userinfo, query, fragment, percent escaping, or relative path component. -/
def HttpsBase.check (url : String) : Except String HttpsBase := do
  unless url.startsWith "https://" && url.utf8ByteSize ≤ 2048 do throw "a cell URL must use HTTPS"
  let root := if url.endsWith "/" then (url.dropEnd 1).toString else url
  let authority :: path := ((root.drop 8).toString).splitOn "/" | throw "invalid cell URL"
  let host ← match authority.splitOn ":" with
    | [host] => pure host
    | [host, port] =>
      match port.toNat? with
      | some n => if 0 < n && n ≤ 65535 then pure host else throw "invalid cell port"
      | none => throw "invalid cell port"
    | _ => throw "invalid cell authority"
  let validDns := (host.splitOn ".").all fun label =>
    !label.isEmpty && label.all (fun c => c.isAlphanum || c == '-')
  unless validDns && path.all Git.Repository.isSegment do throw "invalid cell URL path or host"
  return ⟨root⟩

/-- Where a package's fixed manifest, signature, and worker names are resolved. -/
inductive Location where
  | local (directory : FilePath)
  | https (base : HttpsBase)

private def readFile (path : FilePath) (limit : Nat) : IO ByteArray := do
  let info ← path.metadata
  unless info.type == .file && info.byteSize.toNat ≤ limit do
    throw (IO.userError "cell input is not a regular file within the byte limit")
  Process.readBounded (← IO.FS.Handle.mk path .read) limit

private def download (base : HttpsBase) (file : String) (limit : Nat)
    (body : Option FilePath := none) : IO ByteArray := do
  let args := #["--disable", "--proto", "=https", "--fail", "--silent", "--show-error",
    "--globoff", "--max-filesize", toString limit, "--connect-timeout", "10", "--max-time", "30"]
  let args := match body with
    | none => args
    | some file => args ++ #["--header", "Content-Type: application/json", "--data-binary", "@" ++ file.toString]
  Process.runBytesBounded "curl" (args ++ #["--", base.url ++ "/" ++ file]) 31000 limit

private def Location.read (location : Location) (file : String) (limit : Nat) : IO ByteArray :=
  match location with
  | .local dir => readFile (dir / file) limit
  | .https url => download url file limit

/-- Admission data from the host, independently of the package being opened. -/
structure Admission where
  /-- Digest from the selected graph placement. -/
  pin : Ref .manifest
  /-- Local runtime, audit, and codec requirements. -/
  runtime : Runtime
  /-- Compiler public keys from trusted configuration. -/
  keyring : FilePath

/-- A signed package with matching runtime and checked executable bytes.
Only the verifier constructs this handle. -/
structure Verified where
  private mk ::
  /-- Authenticated build record. -/
  manifest : Manifest
  /-- Actual digest of the authenticated manifest text. -/
  pin : Ref .manifest
  private artifact : ByteArray

private def requireUtf8 (bytes : ByteArray) : IO String :=
  match String.fromUTF8? bytes with
  | some text => pure text
  | none => throw (IO.userError "cell text is not UTF-8")

private def parseManifest (text : String) : IO Manifest :=
  IO.ofExcept ((Json.parse text >>= fromJson?).mapError IO.userError)

private def checkArtifact (manifest : Manifest) (bytes : ByteArray) : IO Unit := do
  unless bytes.size == manifest.artifactBytes && (← Digest.ofBytes bytes) == manifest.artifactSha256.digest do
    throw (IO.userError "cell worker does not match its manifest")

private def verifySignature (text : String) (signature : ByteArray) (keyring : FilePath) : IO Unit := do
  let keyring ← IO.FS.realPath keyring
  IO.FS.withTempDir fun dir => do
    let signaturePath := dir / "manifest.asc"
    IO.FS.writeBinFile signaturePath signature
    let result ← Process.run "gpgv"
      #["--homedir", dir.toString, "--keyring", keyring.toString, "--", signaturePath.toString, "-"]
      30000 (input := some text)
    unless result.ok do throw (IO.userError "cell compiler signature verification failed")

private def checkFiles (record artifact : FilePath) : IO String := do
  let text ← requireUtf8 (← readFile record 65536)
  let manifest ← parseManifest text
  checkArtifact manifest (← readFile artifact manifest.artifactBytes)
  return text

/-- Check offline compiler output against its manifest. With a signature and
keyring, also check who signed the exact file bytes. Use `openPackage` before
execution, which also requires the caller's manifest pin and runtime. -/
def Manifest.verifyFiles (record artifact : FilePath)
    (signature : Option (FilePath × FilePath) := none) : IO Unit := do
  let text ← checkFiles record artifact
  if let some (file, keyring) := signature then
    verifySignature text (← readFile file 65536) keyring

/-- Sign the exact bytes of a manifest file after checking its worker's
digest. The compiler selecting the key attests the supplied provenance. -/
def Manifest.signFiles (record artifact : FilePath) (fingerprint : String) : IO String := do
  signText (← checkFiles record artifact) fingerprint

/-- Create a manifest for an already checked worker file. -/
def Manifest.makeFromFile (revision : Revision) (inputs : BuildInputs) (runtime : Runtime)
    (worker : FilePath) : IO Manifest := do
  Manifest.make revision inputs runtime (← readFile worker maxArtifactBytes)

/-- Admit a package with fixed files `manifest.json`, `manifest.asc` and
`worker`. Verify the exact signed text, runtime and payload before returning
a handle. Local reads and HTTPS downloads have the same byte limits. -/
def openPackage (admission : Admission) (location : Location) : IO Verified := do
  let text ← requireUtf8 (← location.read "manifest.json" 65536)
  let pin ← manifestPin text
  unless pin == admission.pin do throw (IO.userError "cell manifest differs from the selected pin")
  let manifest ← parseManifest text
  unless manifest.runtime == admission.runtime do throw (IO.userError "incompatible cell runtime or codecs")
  verifySignature text (← location.read "manifest.asc" 65536) admission.keyring
  let bytes ← location.read "worker" manifest.artifactBytes
  checkArtifact manifest bytes
  return ⟨manifest, pin, bytes⟩

/-- A Lean signature, including an input-indexed result and the permitted effect row. -/
structure Signature where
  /-- Ordered call arguments, represented by a single Lean type. -/
  Input : Type
  /-- Result type at the given input. -/
  Output : Input → Type
  /-- Effects a compiled implementation may request. -/
  effects : List (Type → Type)

/-- Type of the implementation the producer must check. -/
def Signature.Run (s : Signature) : Type 1 :=
  (input : s.Input) → Control.Monad.Effect.Eff s.effects (s.Output input)

/-- The caller and compiler agree on a versioned wire adapter. The result
decoder checks a result against the same input sent to the worker. -/
structure Codecs (sig : Signature) where
  /-- Pinned formal signature and effect contract. -/
  contract : Ref .contract
  /-- Pinned codec definition identity. -/
  id : Ref .codec
  /-- Encode call arguments in declaration order. -/
  encodeInput : sig.Input → List Json
  /-- Decode arguments at the execution boundary. -/
  decodeInput : List Json → Except String sig.Input
  /-- Encode the typed result on the worker. -/
  encodeOutput : (input : sig.Input) → sig.Output input → Json
  /-- Decode a result while reconstructing any input-dependent witness. -/
  decodeOutput : (input : sig.Input) → Json → Except String (sig.Output input)

/-- Turn an already elaborated implementation into a wire handler. An
executor supplies the effect interpreter separately for each invocation. -/
def Codecs.handler (codecs : Codecs sig) (implementation : sig.Run)
    (interpret : {α : Type} → Control.Monad.Effect.Eff sig.effects α → IO (Except String α))
    (args : List Json) : IO (Except String Json) := do
  match codecs.decodeInput args with
  | .error err => return .error err
  | .ok input => return (← interpret (implementation input)).map (codecs.encodeOutput input)

private opaque localAccess : {p : Prop // p} := ⟨True, trivial⟩

private structure LocalState (sig : Signature) where
  worker : System.Worker.Worker
  codecs : Codecs sig
  nextId : IO.Ref Nat

/-- A program over a private local worker with a bound Lean signature. -/
abbrev Local (sig : Signature) := Owned localAccess.val (LocalState sig)

/-- Call an open precompiled local worker, decoding its result at `input`. -/
def Local.call (input : sig.Input) (timeoutMillis : Nat := 30000) :
    Local sig .open_ .open_ (Except String (sig.Output input)) :=
  Owned.use localAccess.property fun state => do
    let id ← state.nextId.modifyGet fun n => (n, n + 1)
    let request := GitFn.request id (state.codecs.encodeInput input)
    let line ← IO.ofExcept ((System.Worker.Line.check request).mapError IO.userError)
    return GitFn.reply id (← state.worker.call line timeoutMillis) >>= state.codecs.decodeOutput input

private def Verified.checkCodecs (worker : Verified) (codecs : Codecs sig) : IO Unit :=
  unless codecs.contract == worker.manifest.revision.contractSha256 &&
      codecs.id == worker.manifest.runtime.codecSha256 do
    throw (IO.userError "the Lean contract or codecs do not match the cell")

/-- Run several typed calls on one worker process. Stage the verified bytes in
the host's private temporary directory. Each run owns and reaps its process. -/
def Verified.runLocal (package : Verified) (codecs : Codecs sig) (action : Local sig .open_ post α) : IO α := do
  package.checkCodecs codecs
  IO.FS.withTempDir fun dir => do
    let executable := dir / "worker"
    IO.FS.writeBinFile executable package.artifact
    let mode ← Process.run "chmod" #["0700", executable.toString] 5000
    unless mode.ok do throw (IO.userError "could not set cell worker permissions")
    Owned.run localAccess.property
      (do return ⟨← System.Worker.spawn "/usr/bin/env" #["-i", executable.toString],
          codecs, ← IO.mkRef 0⟩)
      (fun state => state.worker.stop) action

/-- Version of the pinned remote worker envelope. -/
def wireVersion : Nat := 1

private def wirePin (pin : Ref .manifest) : Json :=
  Json.mkObj [("version", toJson wireVersion), ("pin", toJson pin)]

/-- Serve a pinned call using the actual loaded cell's pin. The supplied
executor obtains its own invocation context and interprets effects. -/
def dispatch (loaded : Ref .manifest) (args : List Json)
    (call : List Json → IO (Except String Json)) : IO (Except String Json) := do
  match args with
  | header :: rest =>
    if header == wirePin loaded then
      return (← call rest).map fun value => Json.mkObj [("cell", header), ("value", value)]
    else return .error "cell pin or wire version mismatch"
  | [] => return .error "missing cell pin"

/-- Call a trusted HTTPS worker's `/call` endpoint. The response must identify
the same cell and wire version before dependent result decoding. -/
def Verified.callHttp (package : Verified) (url : HttpsBase) (codecs : Codecs sig)
    (input : sig.Input) : IO (Except String (sig.Output input)) := do
  package.checkCodecs codecs
  let header := wirePin package.pin
  let request := GitFn.request 0 (header :: codecs.encodeInput input)
  unless request.utf8ByteSize ≤ 64 * 1024 * 1024 do
    throw (IO.userError "cell request exceeds 64 MiB")
  IO.FS.withTempDir fun dir => do
    let body := dir / "request.json"
    IO.FS.writeFile body request
    let response ← requireUtf8 (← download url "call" (64 * 1024 * 1024) (some body))
    return (GitFn.reply 0 response) >>= fun j => do
      unless (← j.getObjVal? "cell") == header do throw "cell response pin or wire version mismatch"
      codecs.decodeOutput input (← j.getObjVal? "value")

end System.Cell
