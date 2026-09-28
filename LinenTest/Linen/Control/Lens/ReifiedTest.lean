/-
  Tests for `Linen.Control.Lens.Reified`.
-/
import Linen.Control.Lens.Reified
import Linen.Control.Lens.Tuple

open Control.Lens

namespace Tests.Linen.Control.Lens.Reified

-- ── ReifiedLens / ReifiedLens' ──────────────────

-- A `Lens` reified into ordinary data, storable in a `List`, and still
-- usable exactly as the underlying `Lens` once unwrapped.
def fstLens : ReifiedLens' (Nat × Nat) Nat := ⟨_1⟩
def sndLens : ReifiedLens' (Nat × Nat) Nat := ⟨_2⟩

#guard [fstLens, sndLens].map (fun rl => (1, 2) ^. rl.runLens) = [1, 2]
#guard set fstLens.runLens 9 (1, 2) = (9, 2)

-- ── ReifiedIso / ReifiedIso' ────────────────────

def notIso : ReifiedIso' Bool Bool := ⟨iso not not⟩

#guard withIso notIso.runIso (fun f _ => f true) = false

-- ── ReifiedGetter ───────────────────────────────

def fstGetter : ReifiedGetter (Nat × Nat) Nat := ⟨to Prod.fst⟩
def sndGetter : ReifiedGetter (Nat × Nat) Nat := ⟨to Prod.snd⟩

#guard view fstGetter.runGetter (3, 4) = 3

-- `Functor`: `fmap` post-processes the focused value.
#guard view ((· + 1) <$> fstGetter).runGetter (3, 4) = 4

-- `Applicative`/`Monad`: combine two getters focused on the same input.
#guard view ((Prod.mk <$> fstGetter <*> sndGetter)).runGetter (3, 4) = (3, 4)
#guard view (pure 42 : ReifiedGetter (Nat × Nat) Nat).runGetter (3, 4) = 42
#guard view (fstGetter >>= fun a => pure (a * 10)).runGetter (3, 4) = 30

-- ── ReifiedFold ─────────────────────────────────

def evens : ReifiedFold (List Nat) Nat := ⟨folding (fun s => s.filter (· % 2 == 0))⟩
def odds : ReifiedFold (List Nat) Nat := ⟨folding (fun s => s.filter (· % 2 == 1))⟩

#guard toListOf evens.runFold [1, 2, 3, 4] = [2, 4]

-- `Append`: concatenate the targets of both folds.
#guard toListOf (evens ++ odds).runFold [1, 2, 3, 4] = [2, 4, 1, 3]

-- `Inhabited`: the fold with no targets at all.
#guard toListOf (default : ReifiedFold (List Nat) Nat).runFold [1, 2, 3, 4] = []

-- `Functor`/`Applicative`/`Monad`.
#guard toListOf ((· * 10) <$> evens).runFold [1, 2, 3, 4] = [20, 40]
#guard toListOf (pure 7 : ReifiedFold (List Nat) Nat).runFold [1, 2, 3, 4] = [7]
#guard toListOf (evens >>= fun a => (a + ·) <$> odds).runFold [1, 2, 3, 4]
  = [3, 5, 5, 7]

-- ── ReifiedSetter / ReifiedSetter' ──────────────

def fstSetter : ReifiedSetter' (Nat × Nat) Nat := ⟨sets (fun f p => (f p.1, p.2))⟩

#guard over fstSetter.runSetter (· + 1) (1, 2) = (2, 2)

-- ── ReifiedTraversal / ReifiedTraversal' ────────

def bothTraversal : ReifiedTraversal' (Nat × Nat) Nat :=
  ⟨fun {F} [Applicative F] afb p => Prod.mk <$> afb p.1 <*> afb p.2⟩

#guard over bothTraversal.runTraversal (· + 1) (1, 2) = (2, 3)

-- ── ReifiedPrism / ReifiedPrism' ────────────────

def justPrism : ReifiedPrism' (Option Nat) Nat := ⟨prism' some id⟩

#guard withPrism justPrism.runPrism (fun _ seta => seta (some 5)) = Sum.inr 5
#guard withPrism justPrism.runPrism (fun _ seta => seta none) = Sum.inl none
#guard withPrism justPrism.runPrism (fun bt _ => bt 5) = some 5

end Tests.Linen.Control.Lens.Reified
