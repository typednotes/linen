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
