/-
  Tests for `Linen.PostgREST.TimeIt`.

  `timeIt`/`timeIt_` are IO-effectful (they read the monotonic clock), so
  they are exercised with `#eval show IO Unit from do ...` — a thrown
  error fails the build.
-/
import Linen.PostgREST.TimeIt

open PostgREST.TimeIt

namespace Tests.PostgREST.TimeIt

#eval show IO Unit from do
  let (result, _elapsed) ← timeIt (pure 42)
  unless result == 42 do throw (IO.userError s!"expected timeIt to preserve the action's result, got {result}")

  let _elapsed_ ← timeIt_ (pure ())

end Tests.PostgREST.TimeIt
