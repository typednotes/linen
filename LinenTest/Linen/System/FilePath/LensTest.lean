/-
  Tests for `Linen.System.FilePath.Lens`.
-/
import Linen.Control.Lens.Fold
import Linen.Control.Lens.Setter
import Linen.System.FilePath.Lens

open Control.Lens

namespace Tests.Linen.System.FilePath.Lens

#guard view directory (System.FilePath.mk "a/b/c.txt") = System.FilePath.mk "a/b"
#guard view filename (System.FilePath.mk "a/b/c.txt") = System.FilePath.mk "c.txt"
#guard view basename (System.FilePath.mk "a/b/c.txt") = System.FilePath.mk "c"
#guard view extension (System.FilePath.mk "a/b/c.txt") = System.FilePath.mk "txt"

#guard set directory (System.FilePath.mk "x/y") (System.FilePath.mk "a/b/c.txt")
  = System.FilePath.mk "x/y/c.txt"

end Tests.Linen.System.FilePath.Lens
