/-
  PostgREST.TimeIt — Timing utility

  ## Haskell source
  - `PostgREST.TimeIt` (postgrest package)
-/

namespace PostgREST.TimeIt

/-- Time an IO action and return the result with elapsed milliseconds.
    $$\text{timeIt} : \text{IO}\ \alpha \to \text{IO}\ (\alpha \times \text{Nat})$$ -/
def timeIt (action : IO α) : IO (α × Nat) := do
  let start ← IO.monoMsNow
  let result ← action
  let elapsed ← IO.monoMsNow
  return (result, elapsed - start)

/-- Time an IO action, discarding the result. -/
def timeIt_ (action : IO Unit) : IO Nat := do
  let ((), elapsed) ← timeIt action
  return elapsed

end PostgREST.TimeIt
