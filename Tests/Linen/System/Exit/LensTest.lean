/-
  Tests for `Linen.System.Exit.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Prism
import Linen.Control.Lens.Review
import Linen.System.Exit.Lens

open Control.Lens
open System (ExitCode)

namespace Tests.Linen.System.Exit.Lens

-- (run directly via `withPrism`, matching `Tests.Linen.Control.Lens.
-- ConsTest`/`EmptyTest`'s own precedent for exercising a `Prism` without a
-- bare-arrow `Getting`/`Setter` bridge.)

#guard withPrism _ExitSuccess (fun _ seta =>
  match seta ExitCode.success with | .inr () => true | .inl _ => false)
#guard withPrism _ExitSuccess (fun _ seta =>
  match seta (ExitCode.failure 1) with | .inr () => false | .inl _ => true)
#guard review _ExitSuccess () == ExitCode.success

#guard withPrism _ExitFailure (fun _ seta =>
  match seta (ExitCode.failure 2) with | .inr n => n == 2 | .inl _ => false)
#guard withPrism _ExitFailure (fun _ seta =>
  match seta ExitCode.success with | .inr _ => false | .inl _ => true)
#guard review _ExitFailure 2 == ExitCode.failure 2

end Tests.Linen.System.Exit.Lens
