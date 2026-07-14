/-
  Linen.Control.Lens.Internal.Instances — misc `Traversable` instances `lens`
  needs that `linen`'s own `Data.Traversable` doesn't yet provide

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal.Instances`. The
  real upstream module (fetched and read via Hackage's rendered source) is
  itself essentially empty:

  ```
  module Control.Lens.Internal.Instances () where
  import Data.Orphans ()
  import Data.Traversable.Instances ()
  ```

  — it exists only to pull in orphan `Foldable`/`Traversable` instances for
  `(,) a`/`Either a`/`Const a` that, at the time this module was written,
  GHC's own `base` didn't yet ship (upstream's own comment: "these instances
  have moved to `semigroupoids` as of 4.2"). `linen` has no such historical
  gap to paper over, but it does need a couple of `Data.Traversable`
  instances of its own that no other module has ported yet and that later
  modules in this batch (`Control.Lens.Internal.Setter`'s `Settable`) do
  need: `Id` (Lean's substitute for `Identity`) and
  `Data.Functor.Const`.
-/

import Linen.Data.Traversable
import Linen.Data.Functor

namespace Control.Lens.Internal

/-- `Id` is `Traversable`: `traverse f (Id a) = Id <$> f a`. -/
instance : Data.Traversable Id where
  traverse f a := (id : _ → _) <$> f a

/-- `Data.Functor.Const α` is `Traversable`: there is no element to visit, so
    `traverse` never invokes `f` and just repackages the constant. -/
instance : Data.Traversable (Data.Functor.Const α) where
  traverse _ c := pure ⟨c.getConst⟩

end Control.Lens.Internal
