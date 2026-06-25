/-
  Tests for `Linen.Control.Exception`.

  `bracket` / `onException` are IO actions, so behaviour is checked with `#eval`
  (a thrown error fails the build). We thread an `IO.Ref` log to observe both
  ordering and whether cleanup ran.
-/
import Linen.Control.Exception

open Control.Exception

namespace Tests.Control.Exception

-- bracket: acquire → use → release, in order, returning use's result.
#eval show IO Unit from do
  let log ← IO.mkRef ""
  let r ← bracket
    (do log.modify (· ++ "acq;"); pure 7)
    (fun _ => log.modify (· ++ "rel;"))
    (fun a => do log.modify (· ++ "use;"); pure (a + 1))
  unless r == 8 do throw (IO.userError s!"bracket result expected 8, got {r}")
  unless (← log.get) == "acq;use;rel;" do throw (IO.userError s!"bad order: {← log.get}")

-- bracket: release still runs when `use` throws, and the error is re-thrown.
#eval show IO Unit from do
  let log ← IO.mkRef ""
  let threw ← try
      let _ ← bracket (pure 1) (fun _ => log.modify (· ++ "rel;"))
                (fun _ => (throw (IO.userError "boom") : IO Nat))
      pure false
    catch _ => pure true
  unless threw do throw (IO.userError "bracket should re-throw the use error")
  unless (← log.get) == "rel;" do throw (IO.userError s!"release must run on throw: {← log.get}")

-- onException: cleanup is skipped on success.
#eval show IO Unit from do
  let log ← IO.mkRef ""
  let _ ← onException (pure 1) (log.modify (· ++ "clean;"))
  unless (← log.get) == "" do throw (IO.userError "cleanup must be skipped on success")

-- onException: cleanup runs on failure, then the error is re-thrown.
#eval show IO Unit from do
  let log ← IO.mkRef ""
  let threw ← try
      let _ ← onException (throw (IO.userError "boom") : IO Nat) (log.modify (· ++ "clean;"))
      pure false
    catch _ => pure true
  unless threw do throw (IO.userError "onException should re-throw")
  unless (← log.get) == "clean;" do throw (IO.userError "cleanup must run on failure")

end Tests.Control.Exception
