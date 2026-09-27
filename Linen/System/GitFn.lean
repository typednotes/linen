/-
  `System.GitFn` — Lean functions defined by their git location, run securely

  A `GitFn` names a function universally: a repository, a commit (SHA), the
  directory of the Lean project in it, the function's fully qualified name,
  and its declared type.

  ```lean
  let fn : GitFn := {
    repo := "https://github.com/acme/geometry", commit := sha
    project := ".", name := `Geometry.shift, type := "Geometry.Point → Nat → Geometry.Point" }
  match ← build { cache := ".lake/gitfn", libraries := [.linen "/path/to/linen"] } fn with
  | .ok built =>
    let w ← built.spawn                          -- or `built.serve` for HTTP
    let r ← w.call [toJson p, toJson 3]
  | .error why => IO.eprintln why
  ```

  **Secure mode is the only mode.** Nothing from the repository runs — not
  its lakefile, build scripts or toolchain. Its sources are parsed and
  checked (`Policy`) before anything is compiled, admitting only plain,
  kernel-checked Lean with no side effects outside a monad in its type; they
  are compiled with the host's own toolchain, linking only the libraries you
  select; and the compiled result is checked again (type, safety, axioms,
  initializers). Arguments and results travel as JSON (Lean core's
  `ToJson`/`FromJson`), to a worker over stdio or HTTP (`Worker`); or the
  checked sources are vendored into a package you `require` (`vendor`).

  **In a reactive graph, a GitFn is compiled in or remote.** Vendored, its
  functions are ordinary Lean definitions of your program — `fn` registers
  them like any other (see `Examples/GitFnGraph.lean`). Built into a worker,
  `Reactive.remote` registers it as an `FnRef` of the same signature
  (`System.GitFn.Reactive`), for a function to isolate in its own process or
  to deploy apart. The graph is written the same way either way.

  ## Prior art

  - **Naming code by (repository, commit)** is how Cargo (`git = …, rev = …`),
    Go modules and Nix flakes pin dependencies; `GitFn` adds the function's
    name and declared type, which the build checks — closer to Unison, where
    a definition is addressed by its content and type, than to a package
    manager. `vendor` is Cargo's `cargo vendor`, with the check in front.
  - **The policy** plays the role of Safe Haskell (`{-# LANGUAGE Safe #-}`:
    no `unsafePerformIO`, no FFI, only safe imports) and of Rust's
    `#![forbid(unsafe_code)]` with a `cargo-geiger`-style audit of what a
    function reaches — decided here on the sources before anything is built,
    and again on the compiled constants.
  - **The worker protocol is JSON-RPC 2.0**, with Lean core's
    `Lean.JsonRpc` types (those of its language server) rather than a bespoke
    format — the protocol of Haskell's `json-rpc` and Rust's `jsonrpsee` — so
    any client can call a worker.

  Modules: `System.GitFn.Descriptor` (the descriptor, `resolve`),
  `System.GitFn.Policy` (the source check), `System.GitFn.Build` (fetch,
  compile, vendor), `System.GitFn.Worker` (calling a worker),
  `System.GitFn.Reactive` (workers as graph nodes).
-/
import Linen.System.GitFn.Descriptor
import Linen.System.GitFn.Policy
import Linen.System.GitFn.Build
import Linen.System.GitFn.Worker
import Linen.System.GitFn.Reactive
