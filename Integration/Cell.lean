/- Compiler attestation fixture and Lean consumer, built against the contract alone. -/
import Integration.CellContract

open System.Cell
open Integration.CellContract

private def io (result : Except String α) : IO α :=
  IO.ofExcept (result.mapError IO.userError)

private def additionRelease (version : String) : IO Release := do
  return ⟨← io (ReleaseName.parse "math.addition"), ← io (Version.parse version)⟩

private def fixtureInputs : IO BuildInputs := do
  return {
    sourceSha256 := ← hash "Integration.Cell"
    wrapperSha256 := ← hash "addition JSON-RPC"
    dependenciesSha256 := ← hash "integration fixtures"
    linkSha256 := ← hash "integration native dependencies" }

private def attestFixture (dir keyring : System.FilePath) (fingerprint : String)
    (manifest : Manifest) : IO Attested := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "manifest.json") manifest.render
  IO.FS.writeFile (dir / "manifest.asc") (← manifest.sign fingerprint)
  inspectPackage (← manifestPin manifest.render) keyring (.local dir)

private def publish (dir worker : System.FilePath) (fingerprint : String) : IO Unit := do
  let source ← io (GitSource.check "https://github.com/typednotes/linen" (String.ofList (List.replicate 40 'a'))
    "." "Integration.CellWorker" "add")
  let revision : Revision := {
    source
    intentSha256 := ⟨← hash "add two numbers"⟩
    constraintsSha256 := ⟨← hash "pure"⟩
    contractSha256 := ← contract }
  let artifact ← System.Process.readBounded (← IO.FS.Handle.mk worker .read) maxArtifactBytes
  IO.FS.writeBinFile (dir / "worker") artifact
  let manifest ← Manifest.makeFromFile revision (← fixtureInputs) (← runtime) (dir / "worker")
  let manifest ← io (manifest.withRelease (← additionRelease "1.2.0"))
  let text := manifest.render ++ "\n"
  IO.FS.writeFile (dir / "manifest.json") text
  let signature ← Manifest.signFiles (dir / "manifest.json") (dir / "worker") fingerprint
  IO.FS.writeFile (dir / "manifest.asc") signature
  IO.println (← manifestPin text).digest.hex

private def checkCatalog (dir keyring : System.FilePath) (pin : Ref .manifest) (fingerprint : String) :
    IO Unit := do
  let original ← inspectPackage pin keyring (.local dir)
  let m := original.manifest
  let host ← runtime
  let signed (folder : String) := attestFixture (dir / folder) keyring fingerprint
  let named (folder version : String) := do signed folder (← io (m.withRelease (← additionRelease version)))
  let candidate ← named "higher-stable" "1.10.0"
  let prerelease ← named "higher-prerelease" "1.11.0-rc.1"
  let higherMajor ← named "higher-major" "2.0.0"
  let otherRuntime := { host with target := "fixture-other-target" }
  let otherNative ← Manifest.make m.revision (← fixtureInputs) otherRuntime "other architecture".toUTF8
  let other ← signed "alternate-target" (← io (otherNative.withRelease (← additionRelease "1.10.0")))
  let catalog ← io ([original, candidate, prerelease, higherMajor, other, candidate].foldlM
    (init := Catalog.empty) Catalog.insert)
  let query : ReleaseQuery := {
    name := (← additionRelease "1.2.0").name, version := ← io (VersionRequirement.parse "^1.2.0")
    runtime := host, contract := ← contract, policy := ⟨← hash "pure"⟩ }
  let selected (query : ReleaseQuery) := (catalog.resolve query).map (·.pin)
  unless selected query == some candidate.pin &&
      selected { query with runtime := otherRuntime } == some other.pin &&
      selected { query with version := .exact (← io (Version.parse "1.11.0-rc.1")) } == some prerelease.pin &&
      selected { query with contract := ⟨← hash "different contract"⟩ } == none &&
      selected { query with policy := ⟨← hash "different policy"⟩ } == none do
    throw (IO.userError "catalog failed semantic version or compatibility selection")
  let pinnedVersion ← io (VersionRequirement.parse "1.2.0")
  let some pinned := catalog.resolve { query with version := pinnedVersion }
    | throw (IO.userError "the exact version stopped resolving after later releases")
  let admitted ← openPackage { pin := pinned.pin, runtime := host, keyring } pinned.location
  unless admitted.pin == original.pin do
    throw (IO.userError "the selected exact version changed the worker pin")
  let metadata ← named "build-metadata-conflict" "1.10.0+other"
  let rebuilt ← Manifest.make m.revision (← fixtureInputs) host "rebuilt worker".toUTF8
  let rebuilt ← signed "rebuilt" (← io (rebuilt.withRelease (← additionRelease "1.10.0")))
  let differentRevision := { m.revision with intentSha256 := ⟨← hash "changed intent"⟩ }
  let changed ← Manifest.make differentRevision (← fixtureInputs) otherRuntime "changed worker".toUTF8
  let changed ← signed "changed-revision" (← io (changed.withRelease (← additionRelease "1.10.0")))
  let legacy ← signed "legacy" otherNative
  unless [metadata, rebuilt, changed, legacy].all (fun variant => !(catalog.insert variant).isOk) do
    throw (IO.userError "catalog accepted a conflicting or unversioned release")

private def checkDependencies (dir keyring : System.FilePath) (pin : Ref .manifest)
    (fingerprint : String) : IO Unit := do
  let original ← inspectPackage pin keyring (.local dir)
  let manifest := original.manifest
  let host ← runtime
  let contract ← contract
  let policy : Ref .policy := ⟨← hash "pure"⟩
  let dependency (name version : String) : IO Dependency := do
    return {
      name := ← io (ReleaseName.parse name), version := ← io (VersionRequirement.parse version)
      runtimeAbi := host.runtimeAbi, auditVersion := host.auditVersion, codec := host.codecSha256,
      contract, policy }
  let sign (name version : String) (dependencies : List Dependency := []) : IO Attested := do
    let release : Release := ⟨← io (ReleaseName.parse name), ← io (Version.parse version)⟩
    let out := dir / name / version
    if name == "math.addition" then
      attestFixture out keyring fingerprint (← io (manifest.withRelease release dependencies))
    else
      let bytes := s!"fixture worker for {name}".toUTF8
      let leaf ← Manifest.make manifest.revision (← fixtureInputs) host bytes
      let m ← io (leaf.withRelease release dependencies)
      let signed ← attestFixture out keyring fingerprint m
      IO.FS.writeBinFile (out / "worker") bytes
      return signed
  let fold (packages : List Attested) := io (packages.foldlM (init := Catalog.empty) Catalog.insert)
  let common10 ← dependency "common" "1.0.0"
  let common11 ← dependency "common" "1.1.0"
  let first ← sign "common" "1.0.0"
  let second ← sign "common" "1.1.0"
  let older ← sign "normalizer" "1.0.0" [common10]
  let newer ← sign "normalizer" "1.1.0" [common11]
  let units ← sign "units" "1.0.0" [common10]
  let rootDeps ← [("normalizer", "^1.0.0"), ("units", "^1.0.0")].mapM fun (name, version) =>
    dependency name version
  let app ← sign "math.addition" "1.3.0" rootDeps
  let appDir := dir / "math.addition" / "1.3.0"
  IO.FS.writeBinFile (appDir / "worker") (← IO.FS.readBinFile (dir / "worker"))
  let query : ReleaseQuery := {
    name := (← additionRelease "1.3.0").name, version := ← io (VersionRequirement.parse "^1.3.0")
    runtime := host, contract, policy }
  let packages := [first, second, older, newer, units, app]
  let catalog ← fold packages
  let roots := [query]
  let lock ← io (catalog.lock roots)
  unless lock.packages.map (·.pin) == [first.pin, older.pin, units.pin, app.pin] do
    throw (IO.userError "the resolver did not backtrack to the shared compatible dependency")
  let catalogWithoutSolution ← fold [first, second, newer, units, app]
  unless !(catalogWithoutSolution.lock roots).isOk &&
      !(catalog.lock [{ query with runtime := { host with target := "other-target" } }]).isOk do
    throw (IO.userError "the resolver accepted incompatible requirements")
  let otherTarget := { host with target := "other-target" }
  let otherManifest ← Manifest.make manifest.revision (← fixtureInputs) otherTarget "other worker".toUTF8
  let commonRelease : Release := ⟨common10.name, ← io (Version.parse "1.0.0")⟩
  let otherCommon ← attestFixture (dir / "other-target-common") keyring fingerprint
    (← io (otherManifest.withRelease commonRelease))
  let wrongTargetCatalog ← fold [otherCommon, second, older, newer, units, app]
  unless !(wrongTargetCatalog.lock roots).isOk do
    throw (IO.userError "a dependency selected a different target from its parent")
  let differentDeps ← attestFixture (dir / "other-target-app") keyring fingerprint
    (← io (otherManifest.withRelease (← additionRelease "1.3.0") [common10]))
  unless !(catalog.insert differentDeps).isOk do
    throw (IO.userError "one release declared different dependencies across targets")
  let reverseInput ← fold packages.reverse
  unless (reverseInput.lock roots).toOption == some lock do
    throw (IO.userError "catalog insertion order affected dependency resolution")
  let fresh ← sign "normalizer" "1.2.0" [common10]
  let updated ← io (catalog.insert fresh)
  let newLock ← io (updated.lock roots)
  unless (← newLock.pin) != (← lock.pin) && newLock.packages.any (·.pin == fresh.pin) do
    throw (IO.userError "a new compatible version did not affect fresh resolution")
  let locate (pin : Ref .manifest) : IO Location := do
    let some package := packages.find? (·.pin == pin) | throw (IO.userError "unknown package pin")
    return package.location
  let lockPin ← lock.pin
  let readBack : Lockfile ← io (Lean.Json.parse (Lean.toJson lock).compress >>= Lean.fromJson?)
  unless !(← (readBack.admit lockPin roots keyring locate (maxWorkerBytes := 1)).toBaseIO).isOk do
    throw (IO.userError "an oversized closure was admitted")
  let admitted ← readBack.admit lockPin roots keyring locate
  let worker ← admitted.openPackage query.name
  let sum ← worker.runLocal (codecs contract host) (Local.call (sig := addition) (10, 20))
  unless worker.pin == app.pin && sum.toOption == some 30 do
    throw (IO.userError "replayed dependency lock did not execute its original worker")
  let unlocked ← (openPackage { pin := app.pin, runtime := host, keyring } app.location).toBaseIO
  unless !unlocked.isOk do throw (IO.userError "a dependency package opened outside a lock")
  let incomplete : Lockfile ← io (Lean.fromJson?
    ((Lean.toJson lock).setObjVal! "packages" (Lean.toJson (lock.packages.drop 1))))
  unless !(← (incomplete.admit lockPin roots keyring locate).toBaseIO).isOk &&
      !(← (incomplete.admit (← incomplete.pin) roots keyring locate).toBaseIO).isOk do
    throw (IO.userError "an incomplete closure was admitted")
  IO.FS.removeFile (dir / "common" / "1.0.0" / "worker")
  unless !(← (readBack.admit lockPin roots keyring locate).toBaseIO).isOk do
    throw (IO.userError "a closure with a missing worker was admitted")
  let cycleA ← dependency "cycle-a" "1.0.0"
  let cycleB ← dependency "cycle-b" "1.0.0"
  let a ← sign "cycle-a" "1.0.0" [cycleB]
  let b ← sign "cycle-b" "1.0.0" [cycleA]
  let cycle ← fold [a, b]
  unless !(cycle.lock [{ query with name := cycleA.name, version := cycleA.version }]).isOk do
    throw (IO.userError "a cyclic cell closure was accepted")

private def verify (location : Location) (keyring : System.FilePath) (pin : Ref .manifest)
    (url : Option HttpsBase := none) : IO Unit := do
  let runtime ← runtime
  let package ← openPackage { pin, keyring, runtime } location
  let adapter := codecs (← contract) runtime
  let args : List (Nat × Nat) := [(2, 3), (0, 8), (123456789, 987654321)]
  match url with
  | none =>
    for _ in [:2] do
      package.runLocal adapter do
        for input in args do
          let result ← Local.call (sig := addition) input
          unless result.toOption == some (input.1 + input.2) do
            Control.Resource.Program.lift (throw (IO.userError "local cell returned the wrong sum"))
  | some url =>
    for input in args do
      let result ← package.callHttp url adapter input
      unless result.toOption == some (input.1 + input.2) do
        throw (IO.userError "remote cell returned the wrong sum")

def main (args : List String) : IO Unit := do
  match args with
  | [] => IO.println "Usage: cell-integration publish|verify|catalog|dependencies|local|https"
  | ["publish", dir, worker, key] => publish ⟨dir⟩ ⟨worker⟩ key
  | ["verify", dir, key] =>
    let dir : System.FilePath := ⟨dir⟩
    Manifest.verifyFiles (dir / "manifest.json") (dir / "worker")
      (some (dir / "manifest.asc", ⟨key⟩))
  | ["catalog", dir, keyring, pin, fingerprint] =>
    checkCatalog ⟨dir⟩ ⟨keyring⟩ ⟨← io (Digest.check pin)⟩ fingerprint
  | ["dependencies", dir, keyring, pin, fingerprint] =>
    checkDependencies ⟨dir⟩ ⟨keyring⟩ ⟨← io (Digest.check pin)⟩ fingerprint
  | ["local", dir, key, pin] => verify (.local ⟨dir⟩) ⟨key⟩ ⟨← io (Digest.check pin)⟩
  | ["https", base, key, pin] =>
    let base ← io (HttpsBase.check base)
    verify (.https base) ⟨key⟩ ⟨← io (Digest.check pin)⟩ (some base)
  | _ => throw (IO.userError "invalid cell integration command")
