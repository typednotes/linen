/- Tests for versioned packages and input-indexed wire contracts. -/
import Linen.System.Cell

namespace Tests.System.Cell
open _root_.System.Cell
open Lean (Json fromJson? toJson)
open Control.Monad.Effect

private def a := String.ofList (List.replicate 64 'a')
private def b := String.ofList (List.replicate 64 'b')

-- S1: source coordinates and reference domains are checked at the boundary.
#guard (Digest.check a).isOk
#guard !(Digest.check a.toUpper).isOk
#guard !(Digest.check "short").isOk
#guard (GitSource.check "https://github.com/typednotes/lode" (a.take 40).toString
  "." "Cell.Addition" "Cell.add").isOk
#guard !(GitSource.check "https://github.com/typednotes/lode" "main" "." "Cell.Addition" "Cell.add").isOk
#guard !(GitSource.check "https://github.com/typednotes/lode" (a.take 40).toString
  "../escape" "Cell.Addition" "Cell.add").isOk
#guard !(fromJson? (α := Manifest) (Json.mkObj [("version", toJson (1 : Nat))])).isOk

example (_pin : Ref .manifest) : True := by
  fail_if_success
    have _ : Ref .artifact := _pin
  trivial

example (_package : Attested) (_manifest : Manifest) : True := by
  fail_if_success
    have _ : Attested := { _package with manifest := _manifest }
  trivial

-- S6: Preserve the original v2 rendering and native build key. The signed
-- optional release changes the manifest pin, and malformed release fields fail.
#eval show IO Unit from do
  let io {α : Type} (result : Except String α) : IO α := IO.ofExcept (result.mapError IO.userError)
  let hash ← io (Digest.check a)
  let source ← io (GitSource.check "https://github.com/typednotes/lode" (a.take 40).toString
    "." "Cell.Addition" "Cell.add")
  let revision : Revision := ⟨⟨hash⟩, ⟨hash⟩, ⟨hash⟩, source⟩
  let inputs : BuildInputs := ⟨hash, hash, hash, hash⟩
  let runtime : Runtime := ⟨"lean4", "arm64", "abi", "audit", ⟨hash⟩⟩
  let legacy ← Manifest.make revision inputs runtime "worker".toUTF8
  let oldJson := Json.mkObj [("version", toJson (2 : Nat)), ("revision", toJson revision),
    ("buildKey", toJson legacy.buildKey), ("runtime", toJson runtime),
    ("artifactSha256", toJson legacy.artifactSha256), ("artifactBytes", toJson (6 : Nat))]
  unless legacy.render == oldJson.compress && (← io (fromJson? oldJson)) == legacy do
    throw (IO.userError "an unversioned package must retain its original v2 format")
  let release : Release := ⟨← io (ReleaseName.parse "math.addition"), ← io (Version.parse "1.2.3")⟩
  let published ← io (legacy.withRelease release)
  let publishedJson := toJson published
  unless (← io (fromJson? publishedJson)) == published && published.buildKey == legacy.buildKey &&
      (← manifestPin published.render) != (← manifestPin legacy.render) do
    throw (IO.userError "a release must round-trip and change only the signed manifest identity")
  for invalid in [Json.null, Json.mkObj [("name", toJson release.name), ("version", .str "01.2.3")]] do
    unless !(fromJson? (α := Manifest) (oldJson.setObjVal! "release" invalid)).isOk do
      throw (IO.userError "malformed release metadata was accepted")
  -- S8: Dependency declarations require the new format and unique names.
  let dep : Dependency := {
    name := ← io (ReleaseName.parse "math.units"), version := .exact release.version,
    contract := ⟨hash⟩, policy := ⟨hash⟩, codec := ⟨hash⟩,
    runtimeAbi := runtime.runtimeAbi, auditVersion := runtime.auditVersion }
  let parent ← io (legacy.withRelease release [dep])
  let json := toJson parent
  unless (json.getObjValAs? Nat "version").toOption == some dependencyManifestVersion do
    throw (IO.userError "a dependency manifest must require a dependency-aware reader")
  unless (← io (fromJson? json)) == parent && parent.buildKey == legacy.buildKey do
    throw (IO.userError "dependency declarations changed or affected the native key")
  unless !(fromJson? (α := Manifest) (json.setObjVal! "version" (toJson (2 : Nat)))).isOk &&
      !(legacy.withRelease release [dep, dep]).isOk &&
      !(legacy.withRelease release [{ dep with name := release.name }]).isOk do
    throw (IO.userError "invalid dependency declaration was accepted")

-- S2: configured HTTPS origins and package filenames cannot be redirected by a manifest.
#guard (HttpsBase.check "https://cells.example.test:443/builds/example/").isOk
#guard !(HttpsBase.check "http://cells.example.test/builds/example").isOk
#guard !(HttpsBase.check "https://cells.example.test/builds/../worker").isOk
#guard !(HttpsBase.check "https://user@cells.example.test").isOk
#guard !(HttpsBase.check "https://cells.example.test/builds?query=value").isOk

private abbrev quoteSignature : Signature where
  Input := Array Nat
  Output quantities := Vector Nat quantities.size
  effects := []

private def quote : quoteSignature.Run :=
  fun quantities => pure (Vector.ofFn fun i => quantities[i] * 200)

private def quoteCodecs (contract : Ref .contract) (id : Ref .codec) : Codecs quoteSignature where
  contract
  id
  encodeInput quantities := [toJson quantities]
  decodeInput
    | [value] => fromJson? value
    | _ => .error "expected one array"
  encodeOutput _ result := toJson result.toArray
  decodeOutput quantities value := do
    let data ← fromJson? (α := Array Nat) value
    if same : data.size = quantities.size then return ⟨data, same⟩
    else throw "wrong subtotal count"

-- S3: a result is decoded using the original input, including its length witness.
#eval show IO Unit from do
  let hash ← IO.ofExcept ((Digest.check a).mapError IO.userError)
  let codecs := quoteCodecs ⟨hash⟩ ⟨hash⟩
  let quantities := #[2, 3]
  let output ← codecs.handler quote (fun action => pure (.ok action.run)) (codecs.encodeInput quantities)
  let native := output >>= codecs.decodeOutput quantities
  unless (native.toOption.map (·.toArray)) == some #[400, 600] do
    throw (IO.userError "dependent cell produced the wrong result")
  unless (output >>= codecs.decodeOutput #[2]).isOk == false do
    throw (IO.userError "dependent cell accepted an output for another input")

-- S4: a mismatched remote pin never reaches execution.
#eval show IO Unit from do
  let first ← IO.ofExcept ((Digest.check a).mapError IO.userError)
  let second ← IO.ofExcept ((Digest.check b).mapError IO.userError)
  let header := Json.mkObj [("version", toJson wireVersion), ("pin", toJson first)]
  let calls ← IO.mkRef 0
  let answer (_ : List Json) : IO (Except String Json) := do
    calls.modify (· + 1)
    pure (.ok (toJson (5 : Nat)))
  unless !(← dispatch ⟨second⟩ [header] answer).isOk && (← calls.get) == 0 do
    throw (IO.userError "mismatched pin ran code")
  unless (← dispatch ⟨first⟩ [header] answer).isOk && (← calls.get) == 1 do
    throw (IO.userError "the selected pin did not run exactly once")

end Tests.System.Cell
