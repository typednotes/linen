/-
  Linen.Data.Vector — the handful of `Data.Vector` combinators `Array` lacks

  ## Haskell equivalent
  `Data.Vector` (https://hackage.haskell.org/package/vector/docs/Data-Vector.html)

  ## Design
  Haskell's `Data.Vector` is a boxed, immutable, dynamically-sized array —
  exactly what Lean's built-in `Array` already is. Per the stdlib-substitution
  rule, this module does **not** introduce a wrapper/alias type (that would
  also collide with Lean core's own fixed-size `Vector α n`); instead it adds,
  as plain `Array` extensions, only the combinators `Array` is missing:
  `generate`, `ifilter`, `foldl1'`/`foldr1`, `ifoldl'`/`ifoldr`, `and`/`or`,
  `product`, `notElem`, `backpermute`, and `slice`.

  Everything else Haskell's `Data.Vector` offers — `map`, `filter`, `foldl`,
  `zip`/`zipWith`/`unzip`, `reverse`, `sum`, `elem`, `any`/`all`, `take`/`drop`,
  `mapIdx`/`flatMap`, `max?`/`min?`, `push`/`pop` — already exists verbatim on
  `Array`.
-/

namespace Array

-- ── Construction ─────────────────────────────────

/-- Build an array of length `n` by applying `f` to each index.
    $$\text{generate}(n, f) = [f(0), f(1), \ldots, f(n-1)]$$ -/
def generate (n : Nat) (f : Nat → α) : Array α :=
  Array.ofFn (n := n) fun i => f i.val

-- ── Filtering ────────────────────────────────────

/-- Keep elements whose index and value satisfy a predicate.
    $$\text{ifilter}(p, v) = [v_i \mid p(i, v_i)]$$ -/
def ifilter (p : Nat → α → Bool) (v : Array α) : Array α :=
  (v.zipIdx.filter fun (x, i) => p i x).map Prod.fst

-- ── Folding ──────────────────────────────────────

/-- Strict left fold over a (possibly empty) array, using the first element
    as the seed. Returns `none` on an empty array.
    $$\text{foldl1'}(f, [x_1, \ldots, x_n]) = f(\ldots f(x_1, x_2) \ldots, x_n)$$ -/
def foldl1' (f : α → α → α) (v : Array α) : Option α :=
  v.foldl (fun acc x =>
    match acc with
    | none => some x
    | some a => some (f a x)) none

/-- Right fold over a (possibly empty) array, using the last element as the
    seed. Returns `none` on an empty array.
    $$\text{foldr1}(f, [x_1, \ldots, x_n]) = f(x_1, f(x_2, \ldots f(x_{n-1}, x_n)))$$ -/
def foldr1 (f : α → α → α) (v : Array α) : Option α :=
  foldl1' (fun acc x => f x acc) v.reverse

/-- Strict left fold with each element's index.
    $$\text{ifoldl'}(f, z, v) = f(\ldots f(f(z, 0, v_0), 1, v_1) \ldots, n-1, v_{n-1})$$ -/
def ifoldl' (f : β → Nat → α → β) (z : β) (v : Array α) : β :=
  v.zipIdx.foldl (fun acc (x, i) => f acc i x) z

/-- Right fold with each element's index.
    $$\text{ifoldr}(f, z, v) = f(0, v_0, f(1, v_1, \ldots f(n-1, v_{n-1}, z)))$$ -/
def ifoldr (f : Nat → α → β → β) (z : β) (v : Array α) : β :=
  v.zipIdx.foldr (fun (x, i) acc => f i x acc) z

-- ── Boolean / numeric reductions ─────────────────

/-- Are all elements `true`?
    $$\text{and}(v) = \bigwedge v$$ -/
def and (v : Array Bool) : Bool := v.all id

/-- Is any element `true`?
    $$\text{or}(v) = \bigvee v$$ -/
def or (v : Array Bool) : Bool := v.any id

/-- Product of all elements.
    $$\text{product}(v) = \prod v$$ -/
def product [Mul α] [OfNat α 1] (v : Array α) : α := v.foldl (· * ·) 1

-- ── Search ───────────────────────────────────────

/-- Does the element NOT occur in the array?
    $$\text{notElem}(x, v) = \neg\text{elem}(x, v)$$ -/
def notElem [BEq α] (x : α) (v : Array α) : Bool := !(v.contains x)

-- ── Reordering / slicing ─────────────────────────

/-- Permute elements according to an index array.
    $$\text{backpermute}(v, is) = [v_{is_0}, v_{is_1}, \ldots]$$
    Out-of-bounds indices produce a default value. -/
def backpermute [Inhabited α] (v : Array α) (is : Array Nat) : Array α :=
  is.map fun i => v.getD i default

/-- Extract `n` elements starting at index `i`.
    $$\text{slice}(i, n, v) = [v_i, v_{i+1}, \ldots, v_{i+n-1}]$$ -/
def slice (i n : Nat) (v : Array α) : Array α := v.extract i (i + n)

-- ── Proofs ───────────────────────────────────────

/-- `generate` produces an array of the requested length.
    $$\text{length}(\text{generate}(n, f)) = n$$ -/
theorem size_generate (n : Nat) (f : Nat → α) : (generate n f).size = n := by
  simp [generate, Array.size_ofFn]

/-- `backpermute` produces an array of the same length as the index array.
    $$\text{length}(\text{backpermute}(v, is)) = \text{length}(is)$$ -/
theorem size_backpermute [Inhabited α] (v : Array α) (is : Array Nat) :
    (backpermute v is).size = is.size := by
  simp [backpermute, Array.size_map]

/-- `and` agrees with `all id`.
    $$\text{and}(v) = \text{all}(\text{id}, v)$$ -/
theorem and_eq_all_id (v : Array Bool) : and v = v.all id := rfl

/-- `or` agrees with `any id`.
    $$\text{or}(v) = \text{any}(\text{id}, v)$$ -/
theorem or_eq_any_id (v : Array Bool) : or v = v.any id := rfl

end Array
