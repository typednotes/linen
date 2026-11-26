import Linen.Control.Resource

open Control.Resource

private def ExampleWorld : Bool → Type
  | false => Nat
  | true => String

-- S1: a value selects the concrete world available to its continuation.
#eval show IO Unit from do
  let start (promote : Bool) : Program True ExampleWorld false Bool id :=
    Program.step trivial fun n => do
      if promote then
        return ⟨true, toString ((show Nat from n) + 1)⟩
      return ⟨false, n⟩
  let next (b : Bool) : Action True ExampleWorld (id b) false String :=
    match b with
    | true => Program.step trivial fun text => return ⟨text, (0 : Nat)⟩
    | false => Program.step trivial fun n => return ⟨toString (show Nat from n), (0 : Nat)⟩
  for (promote, expected) in [(false, "41"), (true, "42")] do
    let result ← Program.run trivial (Program.bind (start promote) next) (41 : Nat)
    unless result.1 == expected do
      throw (IO.userError s!"wrong dependent world after outcome {promote}")

-- S2 and S3: ordinary return, early close, an aborted body, and failed
-- cleanup each invoke release exactly once. Cleanup preserves the body error.
#eval show IO Unit from do
  let releases ← IO.mkRef 0
  let release (fails : Bool) (_ : Unit) : IO Unit := do
    releases.modify (· + 1)
    if fails then throw (IO.userError "cleanup failure")
  let implicitClose : Owned True Unit .open_ .open_ Nat := pure 42
  let result ← Owned.run trivial (pure ()) (release false) implicitClose
  unless result == 42 do throw (IO.userError "result lost at scope exit")
  let explicitClose := Owned.close trivial (release false)
  Owned.run trivial (pure ()) (release false) explicitClose
  let bodyFailure : Owned True Unit .open_ .open_ Unit :=
    Program.lift (throw (IO.userError "body failure"))
  let aborted ← (Owned.run trivial (pure ()) (release true) bodyFailure).toBaseIO
  match aborted with
  | .error (.userError "body failure") => pure ()
  | _ => throw (IO.userError "cleanup replaced the original exception")
  let failedClose ← (Owned.run trivial (pure ()) (release true) (Owned.close trivial (release true))).toBaseIO
  match failedClose with
  | .error (.userError "cleanup failure") => pure ()
  | _ => throw (IO.userError "cleanup failure was lost")
  unless (← releases.get) == 4 do throw (IO.userError "release count differs from acquisitions")
