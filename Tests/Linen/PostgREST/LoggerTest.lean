/-
  Tests for `Linen.PostgREST.Logger`.

  `LogLevel`'s `Ord`/`ToString` are pure and checked with `#guard`. The
  `log`/`logCrit`/... family is IO-effectful (it prints to stderr), so it is
  merely exercised with `#eval show IO Unit from do ...` — a thrown error
  fails the build.
-/
import Linen.PostgREST.Logger

open PostgREST.Logger

namespace Tests.PostgREST.Logger

/-! ### `ToString` -/

#guard toString LogLevel.crit == "CRT"
#guard toString LogLevel.error == "ERR"
#guard toString LogLevel.warn == "WRN"
#guard toString LogLevel.info == "INF"
#guard toString LogLevel.debug == "DBG"

/-! ### `Ord` -/

#guard compare LogLevel.crit LogLevel.error == .lt
#guard compare LogLevel.debug LogLevel.info == .gt
#guard compare LogLevel.warn LogLevel.warn == .eq
#guard compare LogLevel.error LogLevel.debug == .lt

/-! ### `log` and friends -/

#eval show IO Unit from do
  -- at configLevel = info, crit/error/warn/info all fire, debug is suppressed
  logCrit .info "critical message"
  logError .info "error message"
  logWarn .info "warning message"
  logInfo .info "info message"
  logDebug .info "this should be suppressed"
  -- at configLevel = crit, only crit fires
  logCrit .crit "still fires"
  logError .crit "suppressed"

end Tests.PostgREST.Logger
