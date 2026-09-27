/-
  Tests for `Linen.System.GitFn`: the facade re-exports the four parts; each
  is tested with its module, and the pipeline end to end by
  `lake exe gitfn-integration`.
-/
import Linen.System.GitFn

open System.GitFn Lean

namespace Tests.System.GitFn

-- The secure policy is the default, and it is strict.
#guard ({} : Policy).imports == [`Init, `Std, `Lean.Data.Json]
#guard forbiddenAtoms.contains "unsafe" && forbiddenAtoms.contains "partial"
#guard forbiddenIdents.contains "unsafeBaseIO"
#guard !allowedCommands.contains ``Lean.Parser.Command.eval
-- The facade reaches every part: a descriptor, its binding for a vendored
-- import, and the JSON-RPC call a worker answers.
def sha : CommitSha := (CommitSha.ofString? "0123456789abcdef0123456789abcdef01234567").get!
#guard ({ repo := "r", commit := sha, name := `A.f, type := "Nat → Nat" } : GitFn).definition `f ==
  "def f : (Nat → Nat) := @A.f"
#guard (Json.parse (request 1 [(2 : Nat)]) >>= (·.getObjValAs? String "method")) == .ok "call"

end Tests.System.GitFn
