# `colour` module dependencies

Topological order of every module of the
[`colour`](https://hackage.haskell.org/package/colour) Hackage package to
import into `linen`, per [AGENTS.md](../../../AGENTS.md)'s Hackage-import
convention. A prerequisite of `hip`.

An edge **A → B** means *module A imports module B*, so **B must be built
before A**.

## Topologically sorted modules

1. `Data.Colour.Chan`
2. `Data.Colour.Matrix`
3. `Data.Colour.CIE.Chromaticity`
4. `Data.Colour.Internal` → 1
5. `Data.Colour.RGB` → 2, 3
6. `Data.Colour.CIE.Illuminant` → 3
7. `Data.Colour.SRGB.Linear` → 1, 3, 4, 5, 6
8. `Data.Colour.RGBSpace` → 2, 3, 5, 7
9. `Data.Colour.RGBSpace.HSL` → 5
10. `Data.Colour.RGBSpace.HSV` → 5
11. `Data.Colour` → 3, 4, 7
12. `Data.Colour.SRGB` → 4, 7, 8
13. `Data.Colour.CIE` → 2, 3, 5, 7, 11
14. `Data.Colour.Names` → 11, 12
