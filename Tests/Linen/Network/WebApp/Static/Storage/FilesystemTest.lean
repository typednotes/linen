import Linen.Network.WebApp.Static.Storage.Filesystem

/-! ### Tests for `Linen.Network.WebApp.Static.Storage.Filesystem`

    Exercises `defaultFileServerSettings.ssLookupFile` against a real
    scratch directory (`IO.FS.createTempFile`, matching this project's
    established idiom for filesystem-touching tests, e.g. `SendfileTest`):
    a plain file, a subdirectory, and a missing path. -/

open Network.WebApp.Static
open Network.WebApp.Static.Storage
open Network.HTTP.Types

namespace Tests.Network.WebApp.Static.Storage.Filesystem

#eval show IO Unit from do
  let (handle, path) ← IO.FS.createTempFile
  handle.putStr "hello static"
  handle.flush
  let some root := path.parent | throw (IO.userError "temp file has no parent directory")
  let some name := path.fileName | throw (IO.userError "temp file has no file name")
  let some namePiece := toPiece name | throw (IO.userError "temp file name is not a valid Piece")
  let settings := defaultFileServerSettings root.toString
  let result ← settings.ssLookupFile [namePiece]
  match result with
  | .lrFile file =>
    unless file.fileGetSize == "hello static".toUTF8.size do
      throw (IO.userError s!"expected size {("hello static".toUTF8.size : Nat)}, got {file.fileGetSize}")
    let resp := file.fileToResponse status200 []
    unless resp.status.statusCode == 200 && resp.headers.any (·.1 == hContentType) do
      throw (IO.userError "expected a 200 file response carrying Content-Type")
  | _ => throw (IO.userError "expected .lrFile for an existing scratch file")
  IO.FS.removeFile path

#eval show IO Unit from do
  let (_, path) ← IO.FS.createTempFile
  let some root := path.parent | throw (IO.userError "temp file has no parent directory")
  let settings := defaultFileServerSettings root.parent.get!.toString
  let some dirName := root.fileName | throw (IO.userError "temp dir has no file name")
  let some dirPiece := toPiece dirName | throw (IO.userError "temp dir name is not a valid Piece")
  let result ← settings.ssLookupFile [dirPiece]
  IO.FS.removeFile path
  match result with
  | .lrFolder => pure ()
  | _ => throw (IO.userError "expected .lrFolder for the temp file's parent directory")

#eval show IO Unit from do
  let (_, path) ← IO.FS.createTempFile
  let some root := path.parent | throw (IO.userError "temp file has no parent directory")
  IO.FS.removeFile path
  let settings := defaultFileServerSettings root.toString
  let result ← settings.ssLookupFile [unsafeToPiece "definitely-does-not-exist.txt"]
  match result with
  | .lrNotFound => pure ()
  | _ => throw (IO.userError "expected .lrNotFound for a missing path")

end Tests.Network.WebApp.Static.Storage.Filesystem
