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
-- The two transports.
#guard Transport.stdio != Transport.http

end Tests.System.GitFn
