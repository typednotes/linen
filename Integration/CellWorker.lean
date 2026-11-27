/- The separately compiled native implementation used by the integration test. -/
import Integration.CellContract

open Lean (Json fromJson? toJson)
open Lean.JsonRpc (Message)
open Control.Monad.Effect
open System.Cell
open Integration.CellContract

-- Same ordinary effect-free addition contract as Lode's cell-addition example.
private def add : addition.Run := fun (left, right) => pure (left + right)

private def io (result : Except String α) : IO α :=
  IO.ofExcept (result.mapError IO.userError)

private def respond (line : String) (pin : Option (Ref .manifest)) : IO String := do
  let runtime ← runtime
  let call := (codecs (← contract) runtime).handler add (fun computation => pure (.ok computation.run))
  let incoming : Message ← io (Json.parse line >>= fromJson?)
  let .request id "call" (some (.arr args)) := incoming | throw (IO.userError "expected call")
  let answer ← if let some pin := pin then dispatch pin args.toList call else call args.toList
  let message := match answer with
    | .ok value => Message.response id value
    | .error e => Message.responseError id .invalidParams e none
  return (toJson message).compress

private def runWorker : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  repeat
    let line ← stdin.getLine
    if line.isEmpty then break
    stdout.putStrLn (← respond line none)
    stdout.flush

def main (args : List String) : IO Unit := do
  match args with
  | [] => runWorker
  | ["respond", pin] =>
    IO.println (← respond (← (← IO.getStdin).getLine) (some ⟨← io (Digest.check pin)⟩))
  | _ => throw (IO.userError "expected worker stdin or respond MANIFEST_PIN")
