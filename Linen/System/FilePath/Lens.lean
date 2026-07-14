/-
  Linen.System.FilePath.Lens — `directory`, `filename`, `basename`,
  `extension`

  Port of Hackage's `lens-5.3.6`'s `System.FilePath.Lens` (fetched and read
  via Hackage's rendered source), which ports the `filepath` package's
  `takeDirectory`/`takeFileName`/`takeBaseName`/`takeExtension` and their
  `</>`/`<.>`-based rebuilding into four not-fully-lawful lenses:

  ```
  basename :: Lens' FilePath FilePath
  basename f p = (<.> takeExtension p) . (takeDirectory p </>) <$> f (takeBaseName p)

  directory :: Lens' FilePath FilePath
  directory f p = (</> takeFileName p) <$> f (takeDirectory p)

  extension :: Lens' FilePath FilePath
  extension f p = (n <.>) <$> f e where (n, e) = splitExtension p

  filename :: Lens' FilePath FilePath
  filename f p = (takeDirectory p </>) <$> f (takeFileName p)
  ```

  Upstream itself documents these as convenient but "not fully law-abiding"
  lenses (e.g. `basename`'s setter re-derives the directory/extension from
  the *original* path rather than the just-written value, so `view l (set l
  x s) ≠ x` in general when `x` itself contains path separators). This port
  keeps that same non-strict character, translated against Lean core's own
  `System.FilePath` (`Init/System/FilePath.lean`) — which, unlike the
  `filepath` package, already returns `Option` from `parent`/`fileName`/
  `fileStem`/`extension` rather than a total (if sometimes degenerate)
  `String`. Every lens below totalizes with the same default `filepath`'s
  own total functions themselves use for a value with no such component:
  `.` for a missing directory (`takeDirectory "foo" == "."` upstream), and
  `""` for a missing file name/base name/extension.

  **Scope note (`</>~`/`<<`/>~`/`<.>~`/`=`-suffixed operators).** Upstream
  additionally defines six infix combinators (`</>~`, `<</>~`, `<<</>~`,
  `<.>~`, `<<.>~`, `<<<.>~`) and their `MonadState`-based `=`-suffixed
  variants — all thin thin `over`/`<%~`/`%%=` sugar wrapping `(</>)`/
  `(<.>)` directly, carrying no optics content beyond what `directory`/
  `extension` above plus `Linen.System.FilePath`'s own `join`/
  `addExtension` already provide when composed with `Linen.Control.Lens.
  Setter.over`/`Linen.Control.Lens.Lens.overF` at the call site. Skipped,
  matching this batch's identical `(<|)`/`(|>)` scope note in `Linen.
  Control.Lens.Cons`. -/

import Linen.Control.Lens.Lens

namespace Control.Lens

open System (FilePath)

/-- `directory :: Lens' FilePath FilePath`: the directory component —
    `directory f p = (</> takeFileName p) <$> f (takeDirectory p)`, with
    Lean's `Option`-returning `FilePath.parent` totalized to `.` (matching
    `filepath`'s own `takeDirectory` on a path with no parent). -/
@[inline] def directory : Lens' FilePath FilePath :=
  lens
    (fun p => p.parent.getD (FilePath.mk "."))
    (fun p d =>
      match p.fileName with
      | some fn => d.join (FilePath.mk fn)
      | none => d)

/-- `filename :: Lens' FilePath FilePath`: the final path component —
    `filename f p = (takeDirectory p </>) <$> f (takeFileName p)`. -/
@[inline] def filename : Lens' FilePath FilePath :=
  lens
    (fun p => FilePath.mk (p.fileName.getD ""))
    (fun p fn => p.withFileName fn.toString)

/-- `basename :: Lens' FilePath FilePath`: the final path component with its
    extension stripped — `basename f p = (<.> takeExtension p) .
    (takeDirectory p </>) <$> f (takeBaseName p)`. -/
@[inline] def basename : Lens' FilePath FilePath :=
  lens
    (fun p => FilePath.mk (p.fileStem.getD ""))
    (fun p b =>
      match p.extension with
      | some ext => p.withFileName (b.toString ++ "." ++ ext)
      | none => p.withFileName b.toString)

/-- `extension :: Lens' FilePath FilePath`: the final path component's
    extension (without the leading `.`) — `extension f p = (n <.>) <$> f e
    where (n, e) = splitExtension p`, with a missing extension totalized to
    `""` (matching `filepath`'s own `takeExtension` on an extension-less
    path). Writing `""` removes the extension entirely, matching
    `FilePath.withExtension`'s own behaviour on an empty extension string. -/
@[inline] def extension : Lens' FilePath FilePath :=
  lens
    (fun p => FilePath.mk (p.extension.getD ""))
    (fun p e => if e.toString.isEmpty then p else p.withExtension e.toString)

end Control.Lens
