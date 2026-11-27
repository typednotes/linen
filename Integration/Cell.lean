/- Compiler attestation fixture and Lean consumer, built against the contract alone. -/
import Integration.CellContract

open System.Cell
open Integration.CellContract

private def io (result : Except String α) : IO α :=
  IO.ofExcept (result.mapError IO.userError)

private def publish (dir worker : System.FilePath) (fingerprint : String) : IO Unit := do
  let source ← io (GitSource.check "https://github.com/typednotes/linen" (String.ofList (List.replicate 40 'a'))
    "." "Integration.CellWorker" "add")
  let revision : Revision := {
    source
    intentSha256 := ⟨← hash "add two numbers"⟩
    constraintsSha256 := ⟨← hash "pure"⟩
    contractSha256 := ← contract }
  let artifact ← System.Process.readBounded (← IO.FS.Handle.mk worker .read) maxArtifactBytes
  let inputs : BuildInputs := {
    sourceSha256 := ← hash "Integration.Cell"
    wrapperSha256 := ← hash "addition JSON-RPC"
    dependenciesSha256 := ← hash "integration fixtures"
    linkSha256 := ← hash "integration native dependencies" }
  IO.FS.writeBinFile (dir / "worker") artifact
  let manifest ← Manifest.makeFromFile revision inputs (← runtime) (dir / "worker")
  let text := manifest.render ++ "\n"
  IO.FS.writeFile (dir / "manifest.json") text
  let signature ← Manifest.signFiles (dir / "manifest.json") (dir / "worker") fingerprint
  IO.FS.writeFile (dir / "manifest.asc") signature
  IO.println (← manifestPin text).digest.hex

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
  | [] => IO.println "Usage: cell-integration publish|verify|local|https"
  | ["publish", dir, worker, key] => publish ⟨dir⟩ ⟨worker⟩ key
  | ["verify", dir, key] =>
    let dir : System.FilePath := ⟨dir⟩
    Manifest.verifyFiles (dir / "manifest.json") (dir / "worker")
      (some (dir / "manifest.asc", ⟨key⟩))
  | ["local", dir, key, pin] => verify (.local ⟨dir⟩) ⟨key⟩ ⟨← io (Digest.check pin)⟩
  | ["https", base, key, pin] =>
    let base ← io (HttpsBase.check base)
    verify (.https base) ⟨key⟩ ⟨← io (Digest.check pin)⟩ (some base)
  | _ => throw (IO.userError "invalid cell integration command")
