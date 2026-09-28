/-
  Linen.System.Process — run a command to completion, with a deadline

  Core's `IO.Process.output` waits for as long as the child runs, and when it
  is killed only the child goes: a `lake build`'s `lean` workers, or a
  shell's children, carry on. A service that runs commands on behalf of
  others needs three more things, which `run` adds:

  - a **deadline**, after which the command is killed;
  - an **abort flag** (an `IO.Ref Bool` someone else sets), polled while it
    runs, so a cancelled request stops its build promptly;
  - the **whole process group** killed, not just the child: the child is
    started in its own session (`setsid`), so its descendants share its
    process group and go with it (signalled explicitly: see `await` for the
    runtime bug that makes `Child.kill` miss them).

  stdout and stderr are read concurrently (a child that fills one pipe while
  the parent waits on the other would otherwise deadlock), and stdin carries
  an optional input, then is closed.

  ## Provenance
  Moved from the sibling services `lode` and `lun` (`Process.lean`), which
  carried the same runner (lun's a subset of lode's). `System.GitFn.Build`
  runs its `git` and `lake` steps through it too, under `Config.timeoutMs`.

  ## Not provided
  Resource limits (memory, CPU, files) and sandboxing: a deadline bounds how
  long a command runs, not what it does.
-/

namespace System.Process

-- ── Results ─────────────────────────────────────────────────────────────────

/-- How a command ended. -/
structure Result where
  /-- The exit code, or `none` if it was killed (deadline or abort). -/
  exitCode : Option UInt32
  /-- Everything it wrote on stdout. -/
  stdout : String
  /-- Everything it wrote on stderr. -/
  stderr : String
  deriving Repr, Inhabited

/-- It exited with `0`. -/
def Result.ok (r : Result) : Bool := r.exitCode == some 0

/-- At most `maxChars` characters of `s`, marked with `…` when cut. -/
private def clip (s : String) (maxChars : Nat) : String :=
  if s.length > maxChars then (s.take maxChars).toString ++ "…" else s

/-- A one-line account of a command that did not succeed, for error messages
    and logs: `{what} exited with 2: {stderr}` (stdout when stderr is empty),
    the detail cut at `maxDetail` characters. -/
def Result.describe (r : Result) (what : String) (maxDetail : Nat := 2000) : String :=
  let reason := match r.exitCode with
    | none => "was stopped (timeout or abort)"
    | some c => s!"exited with {c}"
  let detail := (if r.stderr.trimAscii.isEmpty then r.stdout else r.stderr).trimAscii.toString
  s!"{what} {reason}" ++ (if detail.isEmpty then "" else s!": {clip detail maxDetail}")

-- ── Running ─────────────────────────────────────────────────────────────────

/-- Write the input, then drop the handle, which closes the child's stdin. -/
private def feed (h : IO.FS.Handle) (input : Option String) : IO Unit := do
  if let some s := input then
    -- A child that exits without reading closes the pipe; that is its answer.
    try h.putStr s; h.flush catch _ => pure ()

/-- `SIGKILL` to every process of the group `pgid`, through the shell's
    `kill` builtin (present wherever `sh` is; core has no signal API).
    Failures — a group already gone — are ignored. -/
def killGroup (pgid : UInt32) : IO Unit := do
  let _ ← (IO.Process.output
    { cmd := "sh", args := #["-c", "kill -s KILL -- \"-$1\" 2>/dev/null", "sh", toString pgid]
      stdin := .null }).toBaseIO

/-- Wait for `child`, killing its process group at `deadline` (on
    `IO.monoMsNow`'s clock) or once `abort` is set. `none` if it was killed.

    The child was spawned with `setsid`, so it leads a process group holding
    its descendants. `Child.kill` alone is not enough to reach them: Lean's
    runtime (4.34) forgets the `setsid` flag in the `Child` that
    `Child.takeStdin` returns, and `kill` on it signals the leader only —
    which leaves the descendants running *and* holding the stdout/stderr
    pipes, so reading them to the end would wait for the descendants past
    the deadline. The group is therefore signalled explicitly
    (`killGroup`), while the leader is known to be alive (so its pid is
    still its group's id). -/
private def await (child : IO.Process.Child cfg) (deadline : Nat) (abort : Option (IO.Ref Bool)) :
    IO (Option UInt32) := do
  let mut code : Option UInt32 := none
  repeat
    match ← child.tryWait with
    | some c => code := some c; break
    | none =>
      let aborted ← match abort with
        | some r => r.get
        | none => pure false
      if aborted || (← IO.monoMsNow) ≥ deadline then
        killGroup child.pid
        child.kill
        let _ ← child.wait
        break
      IO.sleep 20
  return code

/-- Run `cmd args` in `cwd`, feeding `input` on stdin, and wait for it —
    killing it and its process group after `timeoutMs` milliseconds, or once
    `abort` is set. `env` entries set (`some`) or unset (`none`) variables of
    the inherited environment.

    A command that runs and fails is a `Result` with its exit code. One that
    cannot be started (not found, not executable) either throws or — where
    the runtime forks before it `exec`s, as on macOS — is a `Result` with a
    non-zero exit code (`255`) and the reason on stderr: treat both as
    failures. -/
def run (cmd : String) (args : Array String) (timeoutMs : Nat)
    (cwd : Option System.FilePath := none) (env : Array (String × Option String) := #[])
    (input : Option String := none) (abort : Option (IO.Ref Bool) := none) : IO Result := do
  let child ← IO.Process.spawn
    { cmd, args, cwd, env, setsid := true
      stdin := .piped, stdout := .piped, stderr := .piped }
  let out ← IO.asTask child.stdout.readToEnd .dedicated
  let err ← IO.asTask child.stderr.readToEnd .dedicated
  let (stdin, child) ← child.takeStdin
  feed stdin input
  let code ← await child ((← IO.monoMsNow) + timeoutMs) abort
  let stdout ← IO.ofExcept (← IO.wait out)
  let stderr ← IO.ofExcept (← IO.wait err)
  return { exitCode := code, stdout, stderr }

/-- Run a command whose stdout is binary (`git cat-file blob …`, an archive),
    with no stdin, under the same deadline and abort flag as `run`. Throws
    unless it exits with `0`, with its stderr. -/
def runBytes (cmd : String) (args : Array String) (timeoutMs : Nat)
    (cwd : Option System.FilePath := none) (env : Array (String × Option String) := #[])
    (abort : Option (IO.Ref Bool) := none) : IO ByteArray := do
  let child ← IO.Process.spawn
    { cmd, args, cwd, env, setsid := true
      stdin := .null, stdout := .piped, stderr := .piped }
  let out ← IO.asTask child.stdout.readBinToEnd .dedicated
  let err ← IO.asTask child.stderr.readToEnd .dedicated
  let code ← await child ((← IO.monoMsNow) + timeoutMs) abort
  let stdout ← IO.ofExcept (← IO.wait out)
  let stderr ← IO.ofExcept (← IO.wait err)
  unless code == some 0 do
    let what := s!"{cmd} {" ".intercalate args.toList}"
    throw (IO.userError ({ exitCode := code, stdout := "", stderr : Result }.describe what))
  return stdout

-- ── Environments ────────────────────────────────────────────────────────────

/-- An environment for `git` (and `lake`, which runs git) that ignores the
    host's global and system git configuration — a `url.insteadOf` rewrite, a
    credential helper or commit signing there would change what is fetched or
    make a command hang — and never prompts. Add a committer identity
    (`GIT_AUTHOR_NAME`, …) to it if the command commits. -/
def hermeticGit : Array (String × Option String) :=
  #[("GIT_CONFIG_GLOBAL", some "/dev/null"), ("GIT_CONFIG_NOSYSTEM", some "1"),
    ("GIT_TERMINAL_PROMPT", some "0"), ("GIT_ASKPASS", some "true")]

end System.Process
