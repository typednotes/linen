/-
  Linen.Numeric.Natural.Lens — `_Pair`, `_Sum`, `_Naturals`

  Port of Hackage's `lens-5.3.6`'s `Numeric.Natural.Lens` (fetched and read
  via the real source, not recalled from memory) — "useful tools for Gödel
  numbering", three isomorphisms exhibiting `Natural`'s self-similar
  structure ($\mathbb{N} \cong \mathbb{N} \times \mathbb{N}$, $\mathbb{N}
  \cong \mathbb{N} + \mathbb{N}$, $\mathbb{N} \cong [\mathbb{N}]$). Upstream's
  real source:

  ```
  _Pair :: Iso' Natural (Natural, Natural)
  _Pair = iso hither (uncurry yon) where
    yon 0 0 = 0
    yon m n = case quotRem m 2 of (q,r) -> r + 2 * yon n q -- rotation
    hither 0 = (0,0)
    hither n = case quotRem n 2 of (p,r) -> case hither p of (x,y) -> (r+2*y,x)

  _Sum :: Iso' Natural (Either Natural Natural)
  _Sum = iso hither yon where
    hither p = case quotRem p 2 of (q,0) -> Left q; (q,1) -> Right q
    yon (Left q)  = 2*q
    yon (Right q) = 2*q+1

  _Naturals :: Iso' Natural [Natural]
  _Naturals = iso hither yon where
    hither 0 = []
    hither n | (h, t) <- (n-1)^._Pair = h : hither t
    yon [] = 0
    yon (x:xs) = 1 + review _Pair (x, yon xs)
  ```

  **Substitution (`Natural` → `Nat`).** `Linen`'s native `Nat` is already
  Haskell's `Natural` (an arbitrary-precision, non-negative integer) — no
  further substitution needed beyond the type name.

  **Task-note (discrepancy with the shorthand plan entry).** The batch plan
  this module was assigned from (`docs/imports/lens/dependencies.md`)
  describes this module in shorthand as "one `Prism' Int Nat`-style
  instance". The real upstream `Numeric.Natural.Lens` module (quoted above,
  fetched directly, not from memory) has no such prism at all — its actual
  content is the three self-similarity isomorphisms above. Per `AGENTS.md`'s
  explicit "fetch real source, port faithfully" mandate (which takes
  precedence over a plan's shorthand description), this module ports that
  real content. The literal `Prism' Int Nat`-shaped combinator the plan's
  shorthand asks for already exists verbatim as `Numeric.Lens.integral`
  (`Linen.Numeric.Lens`, this batch's sibling module, ported from the real
  upstream `Numeric.Lens`'s `integral :: Prism Integer Integer a b`
  specialized to `Nat`) — see that module rather than duplicating it here.

  **Deviation (`_Pair`'s `hither`/`yon` reformulated as bit-interleaving).**
  Upstream's own `yon` alternates its two arguments on every recursive call
  (`yon m n` recurses into `yon n q` with `q = m \`div\` 2`), which has *no*
  measure that strictly decreases on every single step: starting from `yon m
  0` with `m` odd, the very next call is `yon 0 (m \`div\` 2)`, whose sum of
  arguments is unchanged (`0 + m/2` vs `m + 0`, and `m/2 < m` only fires on
  the *following* step). A hand-verified trace confirms `m + n` merely fails
  to strictly decrease every step, only every second step — no single-step
  well-founded measure over `(m, n)` accepts upstream's literal recursion
  as-is, and `AGENTS.md` forbids dodging this with a fuel parameter.
  Reformulating the *same* isomorphism ($\mathbb{N} \cong \mathbb{N} \times
  \mathbb{N}$ via interleaving bits) as an explicit bit-list interleave/
  deinterleave (`bitsList`/`ofBits`/`interleaveBits`/`deinterleaveBits`
  below) preserves the intended bijection while every recursive call is
  structural (each step consumes one constructor of a `List`/strictly
  smaller `Nat`), matching this codebase's genuine-termination-proof
  standard without weakening `_Pair`'s type. -/

import Linen.Control.Lens.Iso

open Control.Lens

namespace Numeric.Natural.Lens

-- ── bit-list encoding/decoding ───────────────────

/-- The little-endian (least-significant-bit-first) binary digits of `n` —
    `0` encodes as the empty list; every nonzero `n`'s list ends (most
    significant) with `true`. The structural bridge `_Pair`'s
    bit-interleaving reformulation is built from (see the module doc
    comment's deviation note). -/
def bitsList (n : Nat) : List Bool :=
  if h : n = 0 then []
  else (n % 2 == 1) :: bitsList (n / 2)
termination_by n
decreasing_by
  have : n ≠ 0 := h
  exact Nat.div_lt_self (by omega) (by omega)

/-- Recover a `Nat` from its little-endian binary digits — the left inverse
    of `bitsList`, `ofBits (bitsList n) = n` (`ofBits_bitsList` below). -/
def ofBits : List Bool → Nat
  | [] => 0
  | b :: bs => (if b then 1 else 0) + 2 * ofBits bs

/-- `ofBits (bitsList n) = n`: `bitsList`/`ofBits` really are inverse to one
    another (Lemma A behind `_Pair`'s termination proof). -/
theorem ofBits_bitsList (n : Nat) : ofBits (bitsList n) = n := by
  induction n using Nat.strongRecOn with
  | ind n ih =>
    unfold bitsList
    split
    · next h => simp [h, ofBits]
    · next h =>
      have hlt : n / 2 < n := Nat.div_lt_self (by omega) (by omega)
      have hih := ih (n / 2) hlt
      simp only [ofBits]
      rcases Nat.mod_two_eq_zero_or_one n with hm | hm <;> simp [hm] <;> omega

/-- Merge two little-endian bit lists by alternating their digits, padding
    whichever list runs out first with `false` (i.e. with more significant
    zero bits) until both are exhausted — the "interleave" half of
    `_Pair`'s bit-interleaving reformulation, and `deinterleaveBits`'s
    inverse. (Padding with `false` — rather than simply appending the
    longer list's remaining digits — is what keeps every digit at its
    correct, doubled position; e.g. interleaving `[]` with `[true]` must
    yield `[false, true]`, i.e. `2`, not `[true]`, i.e. `1`.) -/
def interleaveBits : List Bool → List Bool → List Bool
  | [], [] => []
  | [], y :: ys => false :: y :: interleaveBits [] ys
  | x :: xs, [] => x :: false :: interleaveBits xs []
  | x :: xs, y :: ys => x :: y :: interleaveBits xs ys

/-- Split a little-endian bit list into its even-indexed and odd-indexed
    digits — the "deinterleave" half of `_Pair`'s reformulation, and
    `interleaveBits`'s inverse. -/
def deinterleaveBits : List Bool → List Bool × List Bool
  | [] => ([], [])
  | [x] => ([x], [])
  | x :: y :: rest =>
    let (a, b) := deinterleaveBits rest
    (x :: a, y :: b)

/-- `ofBits (deinterleaveBits l).2 ≤ ofBits l`: the odd-indexed half of a
    bit list never encodes a larger value than the whole list (Lemma B
    behind `_Naturals`'s termination proof — every digit `deinterleaveBits`
    keeps for the second component was already present in `l`, just
    reindexed to a *smaller or equal* power of two). -/
theorem ofBits_deinterleave_snd_le : ∀ l : List Bool, ofBits (deinterleaveBits l).2 ≤ ofBits l
  | [] => by simp [deinterleaveBits, ofBits]
  | [_] => by simp [deinterleaveBits, ofBits]
  | x :: y :: rest => by
    have ih := ofBits_deinterleave_snd_le rest
    simp only [deinterleaveBits, ofBits]
    omega

-- ── _Pair ────────────────────────────────────────

/-- `pairHither n = (a, b)` where `a`/`b` are recovered from `n`'s
    even/odd-indexed bits — the "build" direction of `_Pair`. -/
def pairHither (n : Nat) : Nat × Nat :=
  let bs := deinterleaveBits (bitsList n)
  (ofBits bs.1, ofBits bs.2)

/-- `pairYon (m, n) = ` the `Nat` whose bits interleave `m`'s and `n`'s —
    the "match" direction of `_Pair`. -/
def pairYon (p : Nat × Nat) : Nat :=
  ofBits (interleaveBits (bitsList p.1) (bitsList p.2))

/-- `snd (pairHither n) ≤ n`: combines Lemmas A and B above — needed by
    `_Naturals`'s `natsHither` to show its recursive call's argument
    strictly decreases. -/
theorem pairHither_snd_le (n : Nat) : (pairHither n).2 ≤ n := by
  have hB := ofBits_deinterleave_snd_le (bitsList n)
  have hA := ofBits_bitsList n
  simpa [pairHither, hA] using hB

/-- `_Pair :: Iso' Natural (Natural, Natural)`: the natural numbers are
    isomorphic to the product of the natural numbers with itself ($\mathbb
    N \cong \mathbb N \times \mathbb N$), via bit-interleaving (see the
    module doc comment's deviation note on why this reformulates, rather
    than transliterates, upstream's `hither`/`yon`). -/
def _Pair : Iso' Nat (Nat × Nat) :=
  iso pairHither pairYon

-- ── _Sum ─────────────────────────────────────────

/-- `_Sum :: Iso' Natural (Either Natural Natural)`: the natural numbers are
    isomorphic to disjoint sums of natural numbers embedded as evens or odds
    ($\mathbb N \cong 2 \mathbb N$) — `p`'s quotient by `2` tagged `.inl`
    when `p` is even, `.inr` when odd; the reverse direction rebuilds `2q`/
    `2q+1`. Ports directly with no reformulation needed (no recursion at
    all, unlike `_Pair`/`_Naturals`). -/
def _Sum : Iso' Nat (Nat ⊕ Nat) :=
  iso (fun p => if p % 2 = 0 then Sum.inl (p / 2) else Sum.inr (p / 2))
    (fun s => match s with
      | Sum.inl q => 2 * q
      | Sum.inr q => 2 * q + 1)

-- ── _Naturals ────────────────────────────────────

/-- `natsHither n = ` the list `h :: natsHither t` where `(h, t) = pairHither
    (n - 1)` — the "build" direction of `_Naturals`, terminating since
    `pairHither_snd_le` gives `t ≤ n - 1 < n`. -/
def natsHither (n : Nat) : List Nat :=
  if h : n = 0 then []
  else
    let p := pairHither (n - 1)
    p.1 :: natsHither p.2
termination_by n
decreasing_by
  have : (pairHither (n - 1)).2 ≤ n - 1 := pairHither_snd_le (n - 1)
  omega

/-- `natsYon [] = 0`, `natsYon (x :: xs) = 1 + pairYon (x, natsYon xs)` — the
    "match" direction of `_Naturals`, structural on the list. -/
def natsYon : List Nat → Nat
  | [] => 0
  | x :: xs => 1 + pairYon (x, natsYon xs)

/-- `_Naturals :: Iso' Natural [Natural]`: the natural numbers are
    isomorphic to lists of natural numbers ($\mathbb N \cong \mathbb N^*$).
    -/
def _Naturals : Iso' Nat (List Nat) :=
  iso natsHither natsYon

end Numeric.Natural.Lens
