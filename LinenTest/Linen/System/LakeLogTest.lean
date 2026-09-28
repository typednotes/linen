/-
  Tests for `Linen.System.LakeLog`, on output in the shape `lake build`
  prints: progress lines, a multi-line message, a warning, a message with no
  location, and the closing summary.
-/
import Linen.System.LakeLog

open System.LakeLog Lean

namespace Tests.System.LakeLog

def log : String := "✔ [44/47] Built Demo.Math (1.1s)
✖ [45/47] Building Demo.Hello (2.2s)
trace: .> LEAN_PATH=… lean Demo/Hello.lean
error: Demo/Hello.lean:4:23: Type mismatch
  \"no\"
has type
  String
warning: ./Demo.lean:1:4: unused variable `x`

error: Lean exited with code 1
Some required targets logged failures:
- Demo.Hello
error: build failed"

def ds : List Diagnostic := parse log

#guard ds.map (fun d => (d.severity, d.file, d.line, d.column)) ==
  [("error", some "Demo/Hello.lean", some 4, some 23), ("warning", some "./Demo.lean", some 1, some 4),
   ("error", none, none, none), ("error", none, none, none)]
-- Continuation lines belong to the message; the next header ends it, and
-- trailing blank lines are dropped.
#guard ds[0]!.message == "Type mismatch\n  \"no\"\nhas type\n  String"
#guard ds[1]!.message == "unused variable `x`"
-- A `:` in the text is kept.
#guard (parse "error: a.lean:1:2: expected ':' here").head?.map (·.message) == some "expected ':' here"
-- Progress lines alone are not diagnostics; neither is an empty log.
#guard parse "✔ [1/1] Built A\nBuild completed successfully." == []
#guard parse "" == []

#guard splitLocation "a.lean:1:2: x: y" == (some "a.lean", some 1, some 2, "x: y")
#guard splitLocation "build failed" == (none, none, none, "build failed")
#guard splitLocation "C: drive" == (none, none, none, "C: drive")

#guard ds[0]!.render == "Demo/Hello.lean:4:23: error: Type mismatch\n  \"no\"\nhas type\n  String"
#guard ds[2]!.render == "error: Lean exited with code 1"
#guard ({ ds[1]! with line := none } : Diagnostic).render == "./Demo.lean: warning: unused variable `x`"

#guard ds.map (·.isSummary) == [false, false, false, true]
#guard ({ severity := "error", file := none, line := none, column := none,
          message := "Some required targets logged failures" } : Diagnostic).isSummary

#guard toJson ds[0]! == Json.mkObj [("severity", "error"), ("message", ds[0]!.message),
  ("file", "Demo/Hello.lean"), ("line", 4), ("column", 23)]
#guard toJson ds[2]! == Json.mkObj [("severity", "error"), ("message", "Lean exited with code 1")]

end Tests.System.LakeLog
