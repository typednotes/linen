/-
  `Linen.Text.Pandoc.Class.CommonState` — shared `PandocMonad` state.

  ## Haskell source

  Ported from `Text.Pandoc.Class.CommonState` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Class/CommonState.hs`).

  Provides the `CommonState` record — the state shared by every `PandocMonad`
  instance (log, media bag, resource path, verbosity, request headers, …) —
  and its default value.

  ### Deviations from upstream

  * `Text` → `String`; `FilePath` → `String`.
  * `stManager` (an `http-client` `Manager`, behind the `PANDOC_HTTP_SUPPORT`
    CPP flag upstream) is omitted — the HTTP layer is deferred.
-/

import Linen.Text.Pandoc.Logging
import Linen.Text.Pandoc.MediaBag
import Linen.Text.Pandoc.Translations
import Linen.Data.Default

namespace Linen.Text.Pandoc

open Data (Default)
open Verbosity

/-- State shared by all instances of `PandocMonad`. Ordinary users should use
    helper functions (`setVerbosity`, `withMediaBag`, …) rather than touching
    the fields directly. -/
structure CommonState where
  /-- List of log messages accumulated during conversion (in reverse order). -/
  stLog : List LogMessage := []
  /-- Directory to search for data files. -/
  stUserDataDir : Option String := none
  /-- Base URL for resolving relative resource paths. -/
  stSourceURL : Option String := none
  /-- HTTP request headers to add when fetching resources. -/
  stRequestHeaders : List (String × String) := []
  /-- Whether to skip TLS certificate validation. -/
  stNoCheckCertificate : Bool := false
  /-- Media bag of embedded/collected resources. -/
  stMediaBag : MediaBag := MediaBag.empty
  /-- The active language and (once loaded) its translation table. -/
  stTranslations : Option (Lang × Option Translations) := none
  /-- The list of input file paths. -/
  stInputFiles : List String := []
  /-- The output file path, if any. -/
  stOutputFile : Option String := none
  /-- The resource search path (defaults to the current directory). -/
  stResourcePath : List String := ["."]
  /-- The current verbosity level. -/
  stVerbosity : Verbosity := WARNING
  /-- Whether tracing is enabled. -/
  stTrace : Bool := false
  deriving Inhabited

/-- The default `CommonState`: empty log/media bag, `["."]` resource path,
    `WARNING` verbosity, tracing off. -/
def defaultCommonState : CommonState := {}

instance : Default CommonState where default := defaultCommonState

end Linen.Text.Pandoc
