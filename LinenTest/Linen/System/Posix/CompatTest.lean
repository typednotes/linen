/-
  Tests for `Linen.System.Posix.Compat`.

  `getFileStatus`/`fileExist` touch the real filesystem, so they're checked
  with `#eval` against a scratch file created with `IO.FS.createTempFile`.
  `Fd`/`closeFd` are pure bookkeeping and get `#guard`-checked directly.
-/
import Linen.System.Posix.Compat

open System.Posix

namespace Tests.System.Posix.Compat

#guard toString (Fd.mk 3) == "Fd(3)"
#guard Fd.mk 3 == Fd.mk 3
#guard Fd.mk 3 != Fd.mk 4

example : Fd → IO Unit := closeFd

-- `getFileStatus` reports the size and kind of a real scratch file.
#eval show IO Unit from do
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "hello, posix!"
  handle.flush
  let st ← getFileStatus path.toString
  IO.FS.removeFile path
  unless st.size == 13 && st.isRegularFile && !st.isDirectory do
    throw (IO.userError s!"unexpected FileStatus: {repr st}")

-- `fileExist` is true for an existing file and false once it's removed.
#eval show IO Unit from do
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "x"
  handle.flush
  let existsBefore ← fileExist path.toString
  IO.FS.removeFile path
  let existsAfter ← fileExist path.toString
  unless existsBefore && !existsAfter do
    throw (IO.userError "expected fileExist to reflect creation/removal")

end Tests.System.Posix.Compat
