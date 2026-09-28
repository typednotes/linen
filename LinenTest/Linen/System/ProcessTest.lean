/-
  Tests for `Linen.System.Process`.

  `describe` is pure (`#guard`). The runner is exercised with `#eval` against
  real processes (`sh`, `printf`, `sleep`, `kill`), so a failing check throws
  and fails the build: output capture on both pipes, stdin, `cwd`, `env`
  (set and unset), the deadline, the abort flag, the process group going with
  the child, binary output, and a command that cannot be started.
-/
import Linen.System.Process

open System.Process

namespace Tests.System.Process

private def check (b : Bool) (msg : String) : IO Unit :=
  unless b do throw (IO.userError msg)

/-! ### `Result` -/

#guard ({ exitCode := some 0, stdout := "", stderr := "" } : Result).ok
#guard !({ exitCode := some 1, stdout := "", stderr := "" } : Result).ok
#guard !({ exitCode := none, stdout := "", stderr := "" } : Result).ok

#guard ({ exitCode := some 2, stdout := "out", stderr := " bad \n" } : Result).describe "git fetch"
  == "git fetch exited with 2: bad"
-- stdout when stderr is empty; nothing when both are.
#guard ({ exitCode := some 1, stdout := "out\n", stderr := "  " } : Result).describe "lake"
  == "lake exited with 1: out"
#guard ({ exitCode := none, stdout := "", stderr := "" } : Result).describe "lake build"
  == "lake build was stopped (timeout or abort)"
-- The detail is cut.
#guard ({ exitCode := some 1, stdout := "", stderr := "abcdef" } : Result).describe "x" 3
  == "x exited with 1: abc…"

/-! ### Running -/

-- stdout and stderr are both captured, and the exit code is reported.
#eval show IO Unit from do
  let r ← run "sh" #["-c", "echo out; echo err >&2; exit 3"] 10000
  check (r.exitCode == some 3) s!"exit code: {repr r.exitCode}"
  check (r.stdout == "out\n" && r.stderr == "err\n") s!"output: {repr r}"

-- stdin carries the input, then is closed (`cat` would otherwise never end).
#eval show IO Unit from do
  let r ← run "cat" #[] 10000 (input := some "hello\nworld")
  check (r.ok && r.stdout == "hello\nworld") s!"cat: {repr r}"

-- No input: stdin is closed at once.
#eval show IO Unit from do
  let r ← run "cat" #[] 10000
  check (r.ok && r.stdout == "") s!"cat without input: {repr r}"

-- A child that exits without reading its input is not an error.
#eval show IO Unit from do
  let r ← run "true" #[] 10000 (input := some (String.ofList (List.replicate 200000 'x')))
  check r.ok s!"true: {repr r}"

-- `cwd` and `env`: a variable set, another unset.
#eval show IO Unit from do
  let r ← run "sh" #["-c", "pwd; echo \"${LINEN_A-unset} ${HOME-unset}\""] 10000
    (cwd := some "/") (env := #[("LINEN_A", some "set"), ("HOME", none)])
  check (r.stdout == "/\nset unset\n") s!"cwd/env: {repr r.stdout}"

-- A large output on both pipes at once does not deadlock.
#eval show IO Unit from do
  let r ← run "sh" #["-c", "i=0; while [ $i -lt 5000 ]; do echo line; echo err >&2; i=$((i+1)); done"] 30000
  check (r.ok && r.stdout.length == 25000 && r.stderr.length == 20000)
    s!"large output: {r.stdout.length} / {r.stderr.length}"

-- The deadline: the command is killed, promptly, and reported as such.
#eval show IO Unit from do
  let t0 ← IO.monoMsNow
  let r ← run "sleep" #["30"] 200
  let dt := (← IO.monoMsNow) - t0
  check (r.exitCode == none) s!"sleep past the deadline: {repr r.exitCode}"
  check (dt < 5000) s!"the deadline took {dt} ms"

-- The whole process group goes: a background grandchild is killed too, and
-- `run` returns at the deadline although the grandchild held the pipes (it
-- waited the full 30 s when only the leader was killed — see `await`).
#eval show IO Unit from do
  let pidFile ← IO.FS.createTempFile
  let (h, path) := pidFile
  h.flush
  let t0 ← IO.monoMsNow
  let r ← run "sh" #["-c", s!"sleep 30 & echo $! > {path}; wait"] 300
  let dt := (← IO.monoMsNow) - t0
  check (r.exitCode == none) "the shell should have been killed"
  check (dt < 5000) s!"run returned {dt} ms after starting: the grandchild outlived the deadline"
  let pid := (← IO.FS.readFile path).trimAscii.toString
  check (pid.toNat?.isSome) s!"no pid recorded: {pid}"
  IO.sleep 100
  -- `kill -0` fails once the process is gone.
  let alive ← run "kill" #["-0", pid] 5000
  IO.FS.removeFile path
  check (!alive.ok) s!"the grandchild {pid} survived its process group's kill"

-- The abort flag, set from another task, stops the command.
#eval show IO Unit from do
  let abort ← IO.mkRef false
  let _ ← IO.asTask (do IO.sleep 200; abort.set true)
  let t0 ← IO.monoMsNow
  let r ← run "sleep" #["30"] 60000 (abort := some abort)
  let dt := (← IO.monoMsNow) - t0
  check (r.exitCode == none && dt < 5000) s!"abort: {repr r.exitCode} after {dt} ms"

-- A command that cannot be started fails: depending on the platform the
-- spawn throws, or the forked child reports it and exits non-zero.
#eval show IO Unit from do
  match ← (run "linen-no-such-command" #[] 5000).toBaseIO with
  | .error _ => pure ()
  | .ok r => check (!r.ok && r.exitCode.isSome) s!"a missing command succeeded: {repr r}"

/-! ### Binary output -/

#eval show IO Unit from do
  let b ← runBytes "printf" #["\\000\\377A"] 10000
  check (b.toList == [0, 255, 65]) s!"bytes: {b.toList}"

#eval show IO Unit from do
  match ← (runBytes "sh" #["-c", "echo nope >&2; exit 4"] 10000).toBaseIO with
  | .ok _ => throw (IO.userError "a failing command should throw")
  | .error e => check (toString e == "sh -c echo nope >&2; exit 4 exited with 4: nope") s!"error: {e}"

/-! ### `hermeticGit` -/

#guard hermeticGit.contains ("GIT_CONFIG_GLOBAL", some "/dev/null")
#guard hermeticGit.contains ("GIT_TERMINAL_PROMPT", some "0")

-- git really ignores the global configuration under it.
#eval show IO Unit from do
  let r ← run "git" #["config", "--global", "--list"] 10000 (env := hermeticGit)
  check (r.stdout.trimAscii.isEmpty) s!"global config visible: {r.stdout}"

end Tests.System.Process
