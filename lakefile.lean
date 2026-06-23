import Lake
open System Lake DSL

package linen where
  version := v!"0.1.0"

-- ── Native FFI (POSIX sockets + kqueue/epoll) ──
-- The C shim in `ffi/network.c` is portable across macOS and Linux; it selects
-- kqueue vs epoll via `#ifdef __APPLE__ / __linux__`. Compiled to an object
-- file and bundled into a static library that Lake links into the `Linen` lib.

/-- Compile `ffi/network.c` into an object file. -/
target network.o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "network.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "network.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs (traceArgs := #["-O2", "-fPIC"]) (extraDepTrace := getLeanTrace)

/-- Bundle the FFI object(s) into a static lib that Lake links automatically. -/
extern_lib linenffi pkg := do
  let networkObj ← network.o.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "linenffi") #[networkObj]

@[default_target]
lean_lib Linen where
  -- Link the native socket FFI, and precompile so the test suite's `#eval`
  -- checks can call the `@[extern]` bindings through the interpreter.
  needs := #[linenffi]
  precompileModules := true

lean_lib Tests where
  needs := #[linenffi]

lean_exe linen where
  root := `Main

lean_exe bench where
  root := `Bench
