/-
  Alternative combinators

  Utility combinators for `Alternative` that are not in the Lean standard
  library.
-/

namespace Control.Applicative

/-- Fold a list of alternatives with `<|>`, starting from `failure`.

    $$\text{asum}\;[a_1, \ldots, a_n] = a_1 \mathbin{<|>} \cdots \mathbin{<|>} a_n \mathbin{<|>} \text{failure}$$ -/
def asum [Alternative f] : List (f α) → f α
  | [] => failure
  | x :: xs => x <|> asum xs

end Control.Applicative
