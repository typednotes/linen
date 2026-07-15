/-
  Tests for `Linen.Text.Pandoc.Class.PandocPure`.
-/
import Linen.Text.Pandoc.Class.PandocPure

namespace Tests.Linen.Text.Pandoc.Class.PandocPure

open _root_.Linen.Text.Pandoc
open _root_.Linen.Text.Pandoc.PandocMonad

-- ── runPure over the pure environment ─────────────────────────────────

#guard (runPure (lookupEnv "USER")).toOption == some (some "pandoc-user")
#guard (runPure (lookupEnv "NOPE")).toOption == some (none : Option String)

-- newUniqueHash yields fresh consecutive values
#guard (runPure (do let a ← newUniqueHash; let b ← newUniqueHash; pure (a, b))).toOption == some ((1 : Int), (2 : Int))

-- fileExists is false on the empty tree; openURL / readFileStrict fail
#guard (runPure (fileExists "foo.txt")).toOption == some false
#guard (runPure (openURL "http://example.com")).toOption == (none : Option (ByteArray × Option MIME.MimeType))
#guard (runPure (readFileStrict "missing")).toOption.isNone

-- getDataFileName is a stub prefix
#guard (runPure (getDataFileName "foo")).toOption == some "data/foo"

-- ── common-state round-trip through the pure monad ────────────────────

#guard (runPure (do setVerbosity Verbosity.INFO; getVerbosity)).toOption == some Verbosity.INFO
#guard (runPure (do setResourcePath ["a", "b"]; getResourcePath)).toOption == some ["a", "b"]

end Tests.Linen.Text.Pandoc.Class.PandocPure
