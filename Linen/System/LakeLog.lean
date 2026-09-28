/-
  Linen.System.LakeLog — `lake build`'s output, as a list of diagnostics

  `lake build` prints each compiler message as `error: FILE:LINE:COL: text`
  (or `warning: …`), the text possibly continuing on the following lines,
  interleaved with progress lines (`✔ [3/5] Built …`, `✖ …`, `trace: …`) and
  a closing summary (`Some required targets logged failures:`, `error: build
  failed`). `parse` recovers the messages, in order, each with its location
  when it has one — what a service building projects on someone's behalf
  hands back instead of the raw log.

  Lake has a JSON output (`lake --json`), but only for `lake query`; a build
  log is text, so it is read as text here.

  ## Provenance
  Moved from the sibling services `lode` and `lun` (`Diagnostics.lean`),
  whose `parse` was byte-identical. What each does with a diagnostic
  afterwards (lun's attribution to its generated modules, lode's rendering
  for a model) stays with them; `render` and `isSummary` are the generic
  parts.
-/
import Lean.Data.Json

namespace System.LakeLog

open Lean (Json ToJson toJson)

-- ── Diagnostics ─────────────────────────────────────────────────────────────

/-- One compiler message. -/
structure Diagnostic where
  /-- `error` or `warning`. -/
  severity : String
  /-- The file, as lake prints it (relative to the package, maybe `./…`). -/
  file : Option String
  /-- 1-based line. -/
  line : Option Nat
  /-- 0-based column, as Lean counts. -/
  column : Option Nat
  /-- The message, continuation lines included, trailing space removed. -/
  message : String
  deriving DecidableEq, Repr, Inhabited

/-- `{"severity", "message", "file"?, "line"?, "column"?}`, absent fields
    omitted. -/
instance : ToJson Diagnostic where
  toJson d := Json.mkObj <|
    [("severity", toJson d.severity), ("message", toJson d.message)] ++
    (d.file.map fun f => [("file", toJson f)]).getD [] ++
    (d.line.map fun l => [("line", toJson l)]).getD [] ++
    (d.column.map fun c => [("column", toJson c)]).getD []

-- ── Parsing ─────────────────────────────────────────────────────────────────

/-- Line prefixes that start something other than a continuation. -/
private def starters : List String :=
  ["error: ", "warning: ", "info: ", "trace: ", "✖ ", "✔ ", "⚠ ", "Some required targets", "Build completed"]

/-- Split `FILE:LINE:COL: text`; a text with no such location is returned
    whole. -/
def splitLocation (s : String) : Option String × Option Nat × Option Nat × String :=
  match s.splitOn ":" with
  | file :: line :: col :: rest =>
    match line.toNat?, col.trimAscii.toString.toNat? with
    | some l, some c =>
      (some file, some l, some c, (":".intercalate rest).trimAsciiStart.toString)
    | _, _ => (none, none, none, s)
  | _ => (none, none, none, s)

/-- The `error:` and `warning:` messages of a lake log, in order. A message
    runs until the next line that starts something else (another message, a
    progress line, the summary). -/
def parse (log : String) : List Diagnostic :=
  let lines := log.splitOn "\n"
  let (done, cur) := lines.foldl (init := (([] : List Diagnostic), (none : Option Diagnostic)))
    fun (done, cur) line =>
      let flush := match cur with | some d => d :: done | none => done
      let start (sev pfx : String) : Option Diagnostic :=
        if line.startsWith pfx then
          let (file, l, c, text) := splitLocation (line.drop pfx.length).toString
          some { severity := sev, file, line := l, column := c, message := text }
        else none
      match start "error" "error: " <|> start "warning" "warning: " with
      | some d => (flush, some d)
      | none =>
        if starters.any (fun (p : String) => line.startsWith p) then (flush, none)
        else match cur with
          | some d => (done, some { d with message := d.message ++ "\n" ++ line })
          | none => (done, none)
  let all := (match cur with | some d => d :: done | none => done).reverse
  all.map fun d => { d with message := d.message.trimAsciiEnd.toString }

-- ── Reading them ────────────────────────────────────────────────────────────

/-- `FILE:LINE:COL: severity: message` — the compiler's own shape, with the
    location first. -/
def Diagnostic.render (d : Diagnostic) : String :=
  let loc := match d.file, d.line, d.column with
    | some f, some l, some c => s!"{f}:{l}:{c}: "
    | some f, _, _ => s!"{f}: "
    | none, _, _ => ""
  s!"{loc}{d.severity}: {d.message}"

/-- Lake's closing summary of which targets failed (`error: build failed`,
    `… logged failures`): it repeats what the diagnostics already say. -/
def Diagnostic.isSummary (d : Diagnostic) : Bool :=
  d.file.isNone && (d.message.startsWith "build failed" ||
    (d.message.splitOn "logged failures").length > 1)

end System.LakeLog
