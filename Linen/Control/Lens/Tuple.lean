/-
  Linen.Control.Lens.Tuple — `Field1`..`Field9`, `_1`..`_9`

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Tuple` (fetched and read via
  Hackage's rendered Haddock and source). Upstream declares one class per
  tuple position, `Field1`..`Field19`, each with a single method (`_1`..
  `_19`) that is itself the `Lens` used to read or write that position —
  e.g. `class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where _1 ::
  Lens s t a b`, with one instance per GHC tuple arity from the pair up to
  the 19-tuple (`instance Field1 (a,b) (a',b) a a'`, `instance Field1
  (a,b,c) (a',b,c) a a'`, …). `_1`..`_19` are exactly those class methods —
  there is no separate top-level function wrapping them, matching upstream.

  **Representation.** GHC's tuples `(a,b)`, `(a,b,c)`, … are genuinely flat,
  so upstream needs one syntactically distinct instance per arity even
  though every instance for a given position ultimately does the same thing
  (peel off leading fields until the target one is reached). `linen` has no
  flat n-ary tuple; the natural analogue, as the Lean standard library
  itself uses, is `Prod`, with `α × β × γ` parsed (like Haskell's `(,,)` is
  *not*, but like every recursive positional-access scheme in a
  right-associated encoding *is*) as `α × (β × γ)`. That nesting turns
  "one instance per arity" into "one instance per position, defined
  recursively on the shape of the tuple's tail":

  * `Field1` needs only ever look at the first component of a pair — a
    single instance `Field1 (α × β) α' α (α' × β)` (matching upstream's
    `instance Field1 (a,b) (a',b) a a'` verbatim) already covers every
    arity ≥ 2, since `β` is free to itself be `α₂ × α₃ × …`.
  * `Field2` must instead choose, from the *type* of a pair `α × β` alone,
    between "`β` is the whole 2nd field" (a bare pair) and "`β` is itself a
    tuple, and the 2nd field is `β`'s own 1st field" (a 3-or-more-tuple) —
    these are genuinely two different instances, tried in that recursive
    order first (`Field1 β …` is required to exist) and falling back to the
    direct case only when it does not. `Field3`..`Field9` need only ever
    recurse into `Field2`..`Field8` of the tail (the base case is already
    supplied once, by `Field2`), so each needs exactly one instance.

  This mirrors upstream's own instances field-for-field (the pair-only
  `instance Field1 (a,b) (a',b) a a'`, `instance Field2 (a,b) (a,b') b b'`
  each correspond exactly to this port's base cases; every longer-tuple
  instance upstream writes out by hand is, here, a single instance derived
  once by recursion instead of restated at every arity from 2 to 9).

  **Scope note (arity cap, `Field10`..`Field19`).** Upstream continues this
  same scheme up to 19-tuples. This port stops at `Field9` (9-tuples), the
  cap requested for this batch; `Field10`..`Field19` would be produced by
  nothing more than continuing the same recursive-instance pattern one
  step further for each.

  **Scope note (`GHC.Generics` `default`, `Data.Strict.Pair`,
  `Data.Functor.Product`, `(:*:)`).** Upstream also gives every `Field`
  class a `Generic`-derived `default` implementation, plus hand-written
  instances for `Data.Functor.Product`, `GHC.Generics`'s `(:*:)`, and
  `Data.Strict`'s strict `Pair`. `linen` has ported none of `GHC.Generics`,
  `Data.Functor.Product` (only the un-related `Linen.Data.Functor.Product`
  functor-product, not the same type), or `Data.Strict`, so none of these
  have a corresponding type to instantiate against here; only the `Prod`
  instances (upstream's tuple instances) are ported.

  **Scope note (`_1'`..`_9'`, upstream's "strict variations").** Upstream's
  primed variants are literally `_1' f !x = _1 f x` — a `BangPatterns`
  forcing of the input to weak-head-normal-form before running `_1`,
  otherwise behaviourally identical to `_1`. Lean has no notion of
  laziness/WHNF for `_1` to force past (every `Prod` here is already a
  plain eagerly-evaluated structure), so a `_1'` distinct from `_1` would
  have no observable difference in behaviour; they are skipped. -/

import Linen.Control.Lens.Lens

open Data.Functor

namespace Control.Lens

-- ── Field1 / _1 ─────────────────────────────────

/-- `class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where _1 ::
    Lens s t a b`: provides access to the 1st field of a tuple. `S`/`B` are
    the class's real inputs (the tuple type, and the type to overwrite the
    field with); `A`/`T` are `outParam`s computed from them, modeling
    upstream's `s -> a`/`s b -> t` functional dependencies. -/
class Field1 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _1 : Lens S T A B

export Field1 (_1)

/-- `instance Field1 (a,b) (a',b) a a' where _1 k ~(a,b) = (\a' -> (a',b))
    <$> k a`: for any pair (in particular, since `α × β × γ := α × (β × γ)`,
    for every longer right-nested tuple as well), `_1` reads/writes the
    first component, leaving the rest (`β`, whatever shape it has)
    untouched. -/
instance instField1Prod {A B A' : Type u} : Field1 (A × B) A' A (A' × B) where
  _1 := fun {F} [Functor F] (afb : A → F A') (p : A × B) =>
    Functor.map (fun a' => (a', p.2)) (afb p.1)

-- ── Field2 / _2 ─────────────────────────────────

/-- `class Field2 s t a b | … where _2 :: Lens s t a b`: provides access to
    the 2nd field of a tuple. -/
class Field2 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _2 : Lens S T A B

export Field2 (_2)

/-- The recursive case, tried first: if the tail `Rest` of `Head × Rest`
    itself has a `Field1` instance (i.e. `Rest` is at least a pair, so the
    whole tuple has at least 3 fields), the 2nd field of the whole tuple is
    the 1st field of `Rest`. -/
instance (priority := high) instField2Rec {Head Rest B A' RestT : Type u}
    [Field1 Rest B A' RestT] : Field2 (Head × Rest) B A' (Head × RestT) where
  _2 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field1._1 afb p.2)

/-- `instance Field2 (a,b) (a,b') b b' where _2 k ~(a,b) = (\b' -> (a,b'))
    <$> k b`: the base case, tried only once the recursive case above fails
    to find a `Field1 Rest …` instance — i.e. exactly when `Head × Rest` is
    a bare pair (no third field to recurse into), so the 2nd field *is*
    `Rest` in full. -/
instance (priority := low) instField2Base {Head B' Rest : Type u} :
    Field2 (Head × Rest) B' Rest (Head × B') where
  _2 := fun {F} [Functor F] (afb : Rest → F B') (p : Head × Rest) =>
    Functor.map (fun b' => (p.1, b')) (afb p.2)

-- ── Field3 / _3 ─────────────────────────────────

/-- `class Field3 s t a b | … where _3 :: Lens s t a b`: provides access to
    the 3rd field of a tuple. -/
class Field3 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _3 : Lens S T A B

export Field3 (_3)

/-- The 3rd field of `Head × Rest` is the 2nd field of `Rest` — `Field2`'s
    own two instances (recursive/base) decide, from `Rest`'s shape, how far
    to descend from there. -/
instance instField3Rec {Head Rest B A' RestT : Type u}
    [Field2 Rest B A' RestT] : Field3 (Head × Rest) B A' (Head × RestT) where
  _3 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field2._2 afb p.2)

-- ── Field4 / _4 ─────────────────────────────────

/-- `class Field4 s t a b | … where _4 :: Lens s t a b`: provides access to
    the 4th field of a tuple. -/
class Field4 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _4 : Lens S T A B

export Field4 (_4)

/-- The 4th field of `Head × Rest` is the 3rd field of `Rest`. -/
instance instField4Rec {Head Rest B A' RestT : Type u}
    [Field3 Rest B A' RestT] : Field4 (Head × Rest) B A' (Head × RestT) where
  _4 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field3._3 afb p.2)

-- ── Field5 / _5 ─────────────────────────────────

/-- `class Field5 s t a b | … where _5 :: Lens s t a b`: provides access to
    the 5th field of a tuple. -/
class Field5 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _5 : Lens S T A B

export Field5 (_5)

/-- The 5th field of `Head × Rest` is the 4th field of `Rest`. -/
instance instField5Rec {Head Rest B A' RestT : Type u}
    [Field4 Rest B A' RestT] : Field5 (Head × Rest) B A' (Head × RestT) where
  _5 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field4._4 afb p.2)

-- ── Field6 / _6 ─────────────────────────────────

/-- `class Field6 s t a b | … where _6 :: Lens s t a b`: provides access to
    the 6th field of a tuple. -/
class Field6 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _6 : Lens S T A B

export Field6 (_6)

/-- The 6th field of `Head × Rest` is the 5th field of `Rest`. -/
instance instField6Rec {Head Rest B A' RestT : Type u}
    [Field5 Rest B A' RestT] : Field6 (Head × Rest) B A' (Head × RestT) where
  _6 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field5._5 afb p.2)

-- ── Field7 / _7 ─────────────────────────────────

/-- `class Field7 s t a b | … where _7 :: Lens s t a b`: provides access to
    the 7th field of a tuple. -/
class Field7 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _7 : Lens S T A B

export Field7 (_7)

/-- The 7th field of `Head × Rest` is the 6th field of `Rest`. -/
instance instField7Rec {Head Rest B A' RestT : Type u}
    [Field6 Rest B A' RestT] : Field7 (Head × Rest) B A' (Head × RestT) where
  _7 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field6._6 afb p.2)

-- ── Field8 / _8 ─────────────────────────────────

/-- `class Field8 s t a b | … where _8 :: Lens s t a b`: provides access to
    the 8th field of a tuple. -/
class Field8 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _8 : Lens S T A B

export Field8 (_8)

/-- The 8th field of `Head × Rest` is the 7th field of `Rest`. -/
instance instField8Rec {Head Rest B A' RestT : Type u}
    [Field7 Rest B A' RestT] : Field8 (Head × Rest) B A' (Head × RestT) where
  _8 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field7._7 afb p.2)

-- ── Field9 / _9 ─────────────────────────────────

/-- `class Field9 s t a b | … where _9 :: Lens s t a b`: provides access to
    the 9th field of a tuple. -/
class Field9 (S B : Type u) (A : outParam (Type u)) (T : outParam (Type u)) where
  _9 : Lens S T A B

export Field9 (_9)

/-- The 9th field of `Head × Rest` is the 8th field of `Rest`. -/
instance instField9Rec {Head Rest B A' RestT : Type u}
    [Field8 Rest B A' RestT] : Field9 (Head × Rest) B A' (Head × RestT) where
  _9 := fun {F} [Functor F] (afb : A' → F B) (p : Head × Rest) =>
    Functor.map (fun rest' => (p.1, rest')) (Field8._8 afb p.2)

end Control.Lens
