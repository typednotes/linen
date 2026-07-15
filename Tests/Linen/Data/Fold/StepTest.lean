import Linen.Data.Fold.Step

open Data.Fold Data.Fold.Step

-- `mapFst` maps the state; `mapSnd` maps the result.
#guard (match Step.mapFst (· + 1) (Step.Partial 4 : Step Nat String) with
          | .Partial s => s == 5 | _ => false)
#guard (match Step.mapSnd (· ++ "!") (Step.Done "hi" : Step Nat String) with
          | .Done b => b == "hi!" | _ => false)

-- `mapFst` leaves `Done` and `mapSnd` leaves `Partial` unchanged.
#guard (match Step.mapFst (· + 1) (Step.Done "x" : Step Nat String) with
          | .Done b => b == "x" | _ => false)
#guard (match Step.mapSnd (· ++ "!") (Step.Partial 7 : Step Nat String) with
          | .Partial s => s == 7 | _ => false)

-- `bimap` maps both sides.
#guard (match Step.bimap (· + 1) (· ++ "!") (Step.Partial 1 : Step Nat String) with
          | .Partial s => s == 2 | _ => false)

-- The `Functor` instance maps the result.
#guard (match (· + 1) <$> (Step.Done 4 : Step String Nat) with
          | .Done b => b == 5 | _ => false)

-- `mapMStep` maps the result monadically.
#guard (match Step.mapMStep (m := Id) (fun b => pure (b + 1)) (Step.Done 4 : Step String Nat) with
          | .Done b => b == 5 | _ => false)

-- `chainStepM` maps the state on `Partial`.
#guard (match Step.chainStepM (m := Id) (fun s => pure (s + 1))
              (fun (_ : Nat) => pure (Step.Done "" : Step Nat String))
              (Step.Partial 4 : Step Nat Nat) with
          | .Partial s => s == 5 | _ => false)

-- `chainStepM` runs the continuation on `Done`.
#guard (match Step.chainStepM (m := Id) (fun (s : Nat) => pure s)
              (fun b => pure (Step.Done (b + 1) : Step Nat Nat))
              (Step.Done 4 : Step Nat Nat) with
          | .Done b => b == 5 | _ => false)
