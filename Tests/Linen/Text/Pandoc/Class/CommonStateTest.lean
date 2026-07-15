/-
  Tests for `Linen.Text.Pandoc.Class.CommonState`.
-/
import Linen.Text.Pandoc.Class.CommonState

namespace Tests.Linen.Text.Pandoc.Class.CommonState

open _root_.Linen.Text.Pandoc

-- ── defaultCommonState ────────────────────────────────────────────────

#guard defaultCommonState.stVerbosity == Verbosity.WARNING
#guard defaultCommonState.stResourcePath == ["."]
#guard defaultCommonState.stTrace == false
#guard defaultCommonState.stLog.length == 0
#guard defaultCommonState.stInputFiles == []
#guard defaultCommonState.stNoCheckCertificate == false

-- ── record update ─────────────────────────────────────────────────────

#guard ({ defaultCommonState with stVerbosity := Verbosity.INFO }).stVerbosity == Verbosity.INFO

end Tests.Linen.Text.Pandoc.Class.CommonState
