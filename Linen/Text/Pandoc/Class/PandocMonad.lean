/-
  `Linen.Text.Pandoc.Class.PandocMonad` — the `PandocMonad` typeclass.

  ## Haskell source

  Ported from `Text.Pandoc.Class.PandocMonad` in the `pandoc` package
  (v3.10, `src/Text/Pandoc/Class/PandocMonad.hs`).

  Provides the `PandocMonad` typeclass — the abstract interface to the
  (potentially IO-related) capabilities pandoc's readers and writers need
  (reading files, fetching URLs, time/randomness, common-state access) — and
  the derived pure helpers built on it (`report`, `getVerbosity`,
  `getMediaBag`, `insertMedia`, `fetchItem`, `getTimestamp`, `toLang`,
  `readFileFromDirs`, `fillMediaBag`, …).  Also hosts the `PandocMonad`-using
  translation helpers `setTranslations` and `translateTerm`.

  ### Deviations from upstream

  * `Text` → `String`; lazy/strict `ByteString` → `ByteArray`.
  * Upstream's `MonadError PandocError m` superclass is realised by making the
    error operations (`throwError`/`catchError`) methods of the class, so the
    class needs no `MonadExcept` machinery from callers.
  * `UTCTime`/`TimeZone`/`POSIXTime`/`StdGen` are lightweight placeholders
    (`Int` POSIX seconds / `Int` minutes / a small LCG) rather than the full
    `time`/`random` types (deferred dependencies); the pure instance only
    needs deterministic stand-ins, and the readers that use real timestamps
    are themselves deferred.
  * `downloadOrRead`/`fetchItem` handle base64 `data:` URIs (via
    `Text.Pandoc.URI.pBase64DataURI`), `openURL` for other URIs, and local
    files (via `readFileStrict`); the full URL-encoded-data-URI and
    resource-path-search logic is simplified.
  * IO-heavy helpers tied to the deferred App/CLI layer
    (`readMetadataFile`, `findFileWithDataFallback`, `withPaths`,
    `checkUserDataDir`, `runSilently`) are omitted; `getsCommonState`/
    `modifyCommonState` are provided as derived helpers rather than class
    methods.
-/

import Linen.Text.Pandoc.Error
import Linen.Text.Pandoc.Logging
import Linen.Text.Pandoc.Class.CommonState
import Linen.Text.Pandoc.MediaBag
import Linen.Text.Pandoc.MIME
import Linen.Text.Pandoc.URI
import Linen.Text.Pandoc.Shared
import Linen.Text.Pandoc.Walk
import Linen.Text.Pandoc.Definition
import Linen.Text.Pandoc.UTF8
import Linen.Text.Pandoc.Translations

namespace Linen.Text.Pandoc

open Verbosity

-- ── Time / randomness placeholders ────────────────────────────────────

/-- A point in time, as POSIX seconds since the epoch (placeholder for the
    deferred `time` `UTCTime`). -/
abbrev UTCTime := Int
/-- A time zone, as minutes east of UTC (placeholder for `time`'s `TimeZone`). -/
abbrev TimeZone := Int
/-- POSIX time, as seconds since the epoch (placeholder for `POSIXTime`). -/
abbrev POSIXTime := Int

/-- A tiny deterministic pseudo-random generator (placeholder for the deferred
    `random` `StdGen`). -/
structure StdGen where
  /-- The generator's current seed. -/
  seed : Nat := 1848
  deriving Repr, Inhabited

namespace StdGen
/-- Advance the generator, returning a value and the next generator. -/
def next (g : StdGen) : Nat × StdGen :=
  let s := (g.seed * 1103515245 + 12345) % 2147483648
  (s, ⟨s⟩)
/-- Split the generator into two independent-ish generators. -/
def split (g : StdGen) : StdGen × StdGen :=
  (⟨(g.seed * 2654435761 + 1) % 2147483648⟩, ⟨(g.seed * 40503 + 7) % 2147483648⟩)
end StdGen

/-- Build a generator from a seed. -/
def mkStdGen (n : Nat) : StdGen := ⟨n⟩

-- ── The PandocMonad class ─────────────────────────────────────────────

/-- The abstract capabilities pandoc's readers and writers depend on. -/
class PandocMonad (m : Type → Type) [Monad m] where
  /-- Raise a `PandocError`. -/
  throwError : {α : Type} → PandocError → m α
  /-- Handle a raised `PandocError`. -/
  catchError : {α : Type} → m α → (PandocError → m α) → m α
  /-- Look up an environment variable. -/
  lookupEnv : String → m (Option String)
  /-- The current time. -/
  getCurrentTime : m UTCTime
  /-- The current time zone. -/
  getCurrentTimeZone : m TimeZone
  /-- A fresh pseudo-random generator. -/
  newStdGen : m StdGen
  /-- A fresh unique hash (integer). -/
  newUniqueHash : m Int
  /-- Fetch a URL, returning its bytes and MIME type. -/
  openURL : String → m (ByteArray × Option MIME.MimeType)
  /-- Read a file's contents (lazily). -/
  readFileLazy : String → m ByteArray
  /-- Read a file's contents (strictly). -/
  readFileStrict : String → m ByteArray
  /-- Read standard input. -/
  readStdinStrict : m ByteArray
  /-- Expand a glob pattern to matching paths. -/
  glob : String → m (List String)
  /-- Does a file exist? -/
  fileExists : String → m Bool
  /-- Resolve a bundled data-file name to a path. -/
  getDataFileName : String → m String
  /-- The modification time of a file. -/
  getModificationTime : String → m UTCTime
  /-- Get the shared common state. -/
  getCommonState : m CommonState
  /-- Replace the shared common state. -/
  putCommonState : CommonState → m Unit
  /-- Emit a log message. -/
  logOutput : LogMessage → m Unit
  /-- Emit a trace message (for `--trace`). -/
  trace : String → m Unit

namespace PandocMonad

variable {m : Type → Type} [Monad m] [PandocMonad m]

/-- Apply a projection to the common state. -/
def getsCommonState {α : Type} (f : CommonState → α) : m α :=
  f <$> getCommonState

/-- Modify the common state. -/
def modifyCommonState (f : CommonState → CommonState) : m Unit := do
  putCommonState (f (← getCommonState))

-- ── Verbosity / logging ───────────────────────────────────────────────

/-- Set the verbosity level. -/
def setVerbosity (v : Verbosity) : m Unit :=
  modifyCommonState (fun st => { st with stVerbosity := v })

/-- Get the verbosity level. -/
def getVerbosity : m Verbosity := getsCommonState (·.stVerbosity)

/-- Get the accumulated log messages, in temporal order. -/
def getLog : m (List LogMessage) := getsCommonState (fun st => st.stLog.reverse)

/-- Log a message (emitting it if verbosity permits) and record it. -/
def report (msg : LogMessage) : m Unit := do
  let verbosity ← getVerbosity
  if (compare (messageVerbosity msg) verbosity) != Ordering.gt then
    logOutput msg
  modifyCommonState (fun st => { st with stLog := msg :: st.stLog })

/-- Enable or disable tracing. -/
def setTrace (b : Bool) : m Unit :=
  modifyCommonState (fun st => { st with stTrace := b })

/-- Is tracing enabled? -/
def getTrace : m Bool := getsCommonState (·.stTrace)

-- ── Media bag ─────────────────────────────────────────────────────────

/-- Replace the media bag. -/
def setMediaBag (mb : MediaBag) : m Unit :=
  modifyCommonState (fun st => { st with stMediaBag := mb })

/-- Get the media bag. -/
def getMediaBag : m MediaBag := getsCommonState (·.stMediaBag)

/-- Insert a resource into the media bag. -/
def insertMedia (fp : String) (mime : Option MIME.MimeType) (contents : ByteArray) : m Unit :=
  modifyCommonState (fun st =>
    { st with stMediaBag := st.stMediaBag.insertMedia fp mime contents })

-- ── Input / output / resource path ────────────────────────────────────

/-- Get the input file paths. -/
def getInputFiles : m (List String) := getsCommonState (·.stInputFiles)

/-- Set the input file paths (and derive a source URL from the first). -/
def setInputFiles (files : List String) : m Unit := do
  let sourceURL := match files.head? with
    | some f => if URI.isURI f then some ((f.takeWhile (· != '#')).toString) else none
    | none => none
  modifyCommonState (fun st => { st with stInputFiles := files, stSourceURL := sourceURL })

/-- Get the output file path. -/
def getOutputFile : m (Option String) := getsCommonState (·.stOutputFile)

/-- Set the output file path. -/
def setOutputFile (mf : Option String) : m Unit :=
  modifyCommonState (fun st => { st with stOutputFile := mf })

/-- Get the resource search path. -/
def getResourcePath : m (List String) := getsCommonState (·.stResourcePath)

/-- Set the resource search path. -/
def setResourcePath (ps : List String) : m Unit :=
  modifyCommonState (fun st => { st with stResourcePath := ps })

/-- Get the HTTP request headers. -/
def getRequestHeaders : m (List (String × String)) := getsCommonState (·.stRequestHeaders)

/-- Set the HTTP request headers. -/
def setRequestHeaders (hs : List (String × String)) : m Unit :=
  modifyCommonState (fun st => { st with stRequestHeaders := hs })

/-- Add a single HTTP request header. -/
def setRequestHeader (name val : String) : m Unit :=
  modifyCommonState (fun st => { st with stRequestHeaders := (name, val) :: st.stRequestHeaders })

/-- Toggle certificate checking. -/
def setNoCheckCertificate (b : Bool) : m Unit :=
  modifyCommonState (fun st => { st with stNoCheckCertificate := b })

/-- Get the base source URL. -/
def getSourceURL : m (Option String) := getsCommonState (·.stSourceURL)

/-- Set the user data directory. -/
def setUserDataDir (mf : Option String) : m Unit :=
  modifyCommonState (fun st => { st with stUserDataDir := mf })

/-- Get the user data directory. -/
def getUserDataDir : m (Option String) := getsCommonState (·.stUserDataDir)

-- ── Time ──────────────────────────────────────────────────────────────

/-- The current timestamp, honouring `SOURCE_DATE_EPOCH` if set. -/
def getTimestamp : m UTCTime := do
  match ← lookupEnv "SOURCE_DATE_EPOCH" with
  | some s => match (Shared.trim s).toInt? with
              | some n => pure n
              | none => getCurrentTime
  | none => getCurrentTime

/-- The current timestamp as POSIX time. -/
def getPOSIXTime : m POSIXTime := getTimestamp

-- ── URIs / data URIs ──────────────────────────────────────────────────

/-- Decode a base64 `data:` URI to its bytes and MIME type. -/
def extractURIData (s : String) : ByteArray × Option MIME.MimeType :=
  match URI.pBase64DataURI s with
  | some (bytes, mime) => (bytes, some mime)
  | none => (ByteArray.empty, none)

/-- Fetch content from a URI, `data:` URI, or local filesystem path. -/
def downloadOrRead (s : String) : m (ByteArray × Option MIME.MimeType) := do
  if "data:".isPrefixOf s then
    pure (extractURIData s)
  else if URI.isURI s then
    openURL s
  else do
    let bytes ← readFileStrict s
    pure (bytes, MIME.getMimeType s)

/-- Fetch a resource, checking the media bag first. -/
def fetchItem (s : String) : m (ByteArray × Option MIME.MimeType) := do
  let mb ← getMediaBag
  match mb.lookupMedia s with
  | some item => pure (item.mediaContents, some item.mediaMimeType)
  | none => downloadOrRead s

-- ── Text decoding ─────────────────────────────────────────────────────

/-- Decode UTF-8 bytes to text. -/
def toTextM (_fp : String) (bs : ByteArray) : m String :=
  pure (UTF8.toString bs)

/-- Read a file from the first of the given directories that contains it,
    returning its (UTF-8-decoded) text, or `none` if not found in any. -/
def readFileFromDirs (dirs : List String) (fp : String) : m (Option String) := do
  match dirs with
  | [] => pure none
  | d :: ds => do
      let path := if d == "" then fp else d ++ "/" ++ fp
      if ← fileExists path then
        let bytes ← readFileStrict path
        pure (some (UTF8.toString bytes))
      else
        readFileFromDirs ds fp

-- ── Language tags ─────────────────────────────────────────────────────

/-- Parse a BCP 47 tag into a `Lang`, reporting a warning on failure. -/
def toLang (mbs : Option String) : m (Option Lang) := do
  match mbs with
  | none => pure none
  | some "" => pure none
  | some s =>
      match parseLang s with
      | .ok l => pure (some l)
      | .error _ => do
          report (.InvalidLang s)
          pure none

-- ── Media collection ──────────────────────────────────────────────────

/-- Walk the document, fetching and caching any images not already in the
    media bag (leaving the AST unchanged; errors are reported and skipped). -/
def fillMediaBag (doc : Pandoc) : m Pandoc :=
  walkM (b := Pandoc) (fun (i : Inline) =>
    match i with
    | .Image _ _ (src, _) => do
        let mb ← getMediaBag
        match mb.lookupMedia src with
        | some _ => pure i
        | none =>
            catchError
              (do
                let (bytes, mime) ← fetchItem src
                insertMedia src mime bytes
                pure i)
              (fun _ => do
                report (.CouldNotFetchResource src "")
                pure i)
    | x => pure x) doc

-- ── Translations (PandocMonad-using; from Text.Pandoc.Translations) ────

/-- Set the active language for translation lookups (does not load a file). -/
def setTranslations (lang : Lang) : m Unit :=
  modifyCommonState (fun st => { st with stTranslations := some (lang, none) })

/-- Look up a term's translation for the current language.  If no language is
    set (or the term is absent) returns `""`, reporting `NoTranslation`.

    (Upstream additionally loads `translations/<lang>.yaml` data files on
    demand; that data-file loading is part of the deferred data subtree, so
    here the lookup uses whatever `Translations` were cached in the state.) -/
def translateTerm (term : Term) : m String := do
  match ← getsCommonState (·.stTranslations) with
  | none => pure ""
  | some (_, none) => do
      report (.NoTranslation (toString term))
      pure ""
  | some (_, some tr) =>
      match tr.lookupTerm term with
      | some t => pure t
      | none => do
          report (.NoTranslation (toString term))
          pure ""

end PandocMonad
end Linen.Text.Pandoc
