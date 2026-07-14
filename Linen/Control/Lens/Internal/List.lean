/-
  Linen.Control.Lens.Internal.List — `ordinalNub`, `stripSuffix`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.List` (fetched and
  read via Hackage's rendered source, which for this module could only be
  retrieved as a detailed structural description rather than a verbatim
  quote — the summary below was cross-checked for the exact type signatures
  it does give). Upstream exports exactly two functions:

  ```
  ordinalNub   :: Int -> [Int] -> [Int]
  stripSuffix  :: Eq a => [a] -> [a] -> Maybe [a]
  ```

  `ordinalNub l xs` keeps the first occurrence of every element of `xs`
  that is a valid *ordinal* below the bound `l` (i.e. `0 <= x < l`),
  dropping out-of-range and repeated elements while preserving the order of
  first appearance — e.g. `ordinalNub 3 [-1,2,1,4,2,3] = [2,1]` (`-1`, `4`,
  and `3` are out of bounds for a bound of `3`; the second `2` is a repeat).
  It is used by `lens`'s indexed-traversal composition to de-duplicate a
  list of derived integer indices. `stripSuffix suffix xs` removes `suffix`
  from the end of `xs`, returning `none` if `xs` does not end with it.

  **Scope note (`ordinalNub`'s `IntSet`).** Upstream tracks the "already
  seen" ordinals with `Data.IntSet`, an asymptotically efficient integer set
  `linen` has no port of. This is a pure implementation-detail substitution:
  `ordinalNub` below tracks the same set of seen values as a plain `List
  Int` accumulator, giving the identical observable de-duplication behaviour
  (just without `IntSet`'s $O(\log n)$ membership test) — not a behaviour
  change.

  **Scope note (`stripSuffix`'s two-pointer algorithm).** Upstream computes
  the length difference between `xs` and `suffix` with a single lock-step
  walk (`drp`) to run in $O(\min(m, n))$ rather than computing both lengths
  separately, then confirms the suffix with `zipWith const`/`guard`. The
  port below computes `xs.length` and `suffix.length` directly and slices
  with `List.take`/`List.drop`, which is asymptotically slightly worse
  ($O(m + n)$ instead of $O(\min(m, n))$) but observably identical: the same
  `some`/`none` result and the same stripped prefix on `some`.
-/

namespace Control.Lens.Internal

-- ── ordinalNub ─────────────────────────────────

/-- Keep the first occurrence of every element of `xs` that is a valid
    ordinal below the bound `l` (`0 <= x < l`), dropping out-of-range and
    repeated elements while preserving first-appearance order.

    ```
    #eval ordinalNub 3 [-1, 2, 1, 4, 2, 3] -- [2, 1]
    ```

    See the module's scope note on the `List`-based "seen" accumulator
    standing in for upstream's `IntSet`. -/
def ordinalNub (l : Int) (xs : List Int) : List Int :=
  go [] xs
where
  /-- `go seen ys`: filter `ys`, skipping out-of-range or already-`seen`
      elements, recording every kept element into `seen` as it goes. -/
  go (seen : List Int) : List Int → List Int
    | [] => []
    | x :: ys =>
      if x < 0 ∨ l ≤ x then go seen ys
      else if seen.contains x then go seen ys
      else x :: go (x :: seen) ys

-- ── stripSuffix ────────────────────────────────

/-- Remove `suffix` from the end of `xs`, returning the remaining prefix, or
    `none` if `xs` does not end with `suffix`.

    ```
    #eval stripSuffix "bar".toList "foobar".toList -- some "foo".toList
    ```

    See the module's scope note on the length-based slicing standing in for
    upstream's single-pass two-pointer algorithm. -/
def stripSuffix [BEq A] (suffix xs : List A) : Option (List A) :=
  let n := xs.length
  let k := suffix.length
  if k ≤ n ∧ xs.drop (n - k) == suffix then
    some (xs.take (n - k))
  else
    none

end Control.Lens.Internal
