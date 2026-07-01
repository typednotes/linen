/-
  PostgREST.CLI — Command-line interface

  Parses command-line arguments and dispatches to the appropriate
  PostgREST mode (serve, version, dump-config, dump-schema).

  ## Haskell source
  - `PostgREST.CLI` (postgrest package)
-/

import Linen.PostgREST.Version

namespace PostgREST.CLI

/-- CLI command modes. -/
inductive Command where
  | serve (configPath : Option String)
  | version
  | dumpConfig
  | dumpSchema
  | help
  deriving Repr

/-- Parse command-line arguments into a command. -/
def parseArgs (args : List String) : Command :=
  match args with
  | [] => .serve none
  | ["--version"] | ["-v"] => .version
  | ["--help"] | ["-h"] => .help
  | ["--dump-config"] => .dumpConfig
  | ["--dump-schema"] => .dumpSchema
  | [configPath] => .serve (some configPath)
  | _ => .help

/-- Print usage information. -/
def printUsage : IO Unit := do
  IO.println "Usage: postgrest [OPTION | CONFIG_FILE]"
  IO.println ""
  IO.println "PostgREST — REST API for any PostgreSQL database"
  IO.println ""
  IO.println "Options:"
  IO.println "  CONFIG_FILE          Path to configuration file"
  IO.println "  --version, -v        Show version"
  IO.println "  --dump-config        Dump parsed configuration"
  IO.println "  --dump-schema        Dump schema cache as JSON"
  IO.println "  --help, -h           Show this help"
  IO.println ""
  IO.println s!"{Version.prettyVersion}"

end PostgREST.CLI
