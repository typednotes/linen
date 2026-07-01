/-
  Tests for `Linen.PostgREST.CLI`.
-/
import Linen.PostgREST.CLI

open PostgREST.CLI

namespace Tests.PostgREST.CLI

/-! ### `parseArgs`
    `Command` doesn't derive `BEq`, so tests pattern-match directly. -/

#guard match parseArgs [] with | .serve none => true | _ => false
#guard match parseArgs ["--version"] with | .version => true | _ => false
#guard match parseArgs ["-v"] with | .version => true | _ => false
#guard match parseArgs ["--help"] with | .help => true | _ => false
#guard match parseArgs ["-h"] with | .help => true | _ => false
#guard match parseArgs ["--dump-config"] with | .dumpConfig => true | _ => false
#guard match parseArgs ["--dump-schema"] with | .dumpSchema => true | _ => false
#guard match parseArgs ["config.conf"] with | .serve (some "config.conf") => true | _ => false
#guard match parseArgs ["a", "b"] with | .help => true | _ => false

/-! ### `printUsage` -/

#eval printUsage

end Tests.PostgREST.CLI
