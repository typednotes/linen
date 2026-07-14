/-
  Linen.Control.Lens.Internal — facade re-exporting every `Control.Lens.
  Internal.*` module

  Port of Hackage's `lens-5.3.6`'s `Control.Lens.Internal` module, which
  upstream is itself nothing but a re-export list:

  ```
  module Control.Lens.Internal
    ( module Control.Lens.Internal.Bazaar
    , module Control.Lens.Internal.Context
    , module Control.Lens.Internal.Fold
    , module Control.Lens.Internal.Getter
    , module Control.Lens.Internal.Indexed
    , module Control.Lens.Internal.Iso
    , module Control.Lens.Internal.Level
    , module Control.Lens.Internal.Magma
    , module Control.Lens.Internal.Prism
    , module Control.Lens.Internal.Review
    , module Control.Lens.Internal.Setter
    , module Control.Lens.Internal.Zoom
    ) where
  ```

  Lean has no re-export statement, so this module's entire content is the
  `import` list below: every internal-machinery module ported in this batch
  (`Profunctor`, `Indexed`, `Instances`, `Context`, `Magma`, `Bazaar`, `Iso`,
  `Prism`, `Review`, `Getter`, `Setter`, `Fold`, `Level`, `Deque`, `List`,
  `Zoom`) becomes visible to anyone who imports `Linen.Control.Lens.Internal`
  alone, exactly mirroring what importing upstream's facade module gives you.
  `Instances`/`Deque`/`List` have no upstream `Control.Lens.Internal` mention
  in the re-export list above (upstream's `Deque`/`List` are consumed inside
  `Control.Lens.Fold`/`.Traversal` directly rather than re-exported, and
  `Instances` is upstream's orphan-instance shell, never re-exported either),
  but are included here too since every module in this batch belongs under
  `Control.Lens.Internal.*` and this facade is the natural single entry point
  for the whole batch.
-/

import Linen.Control.Lens.Internal.Profunctor
import Linen.Control.Lens.Internal.Indexed
import Linen.Control.Lens.Internal.Instances
import Linen.Control.Lens.Internal.Context
import Linen.Control.Lens.Internal.Magma
import Linen.Control.Lens.Internal.Bazaar
import Linen.Control.Lens.Internal.Iso
import Linen.Control.Lens.Internal.Prism
import Linen.Control.Lens.Internal.Review
import Linen.Control.Lens.Internal.Getter
import Linen.Control.Lens.Internal.Setter
import Linen.Control.Lens.Internal.Fold
import Linen.Control.Lens.Internal.Level
import Linen.Control.Lens.Internal.Deque
import Linen.Control.Lens.Internal.List
import Linen.Control.Lens.Internal.Zoom
