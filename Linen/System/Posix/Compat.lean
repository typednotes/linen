/-
  Linen.System.Posix.Compat — POSIX compatibility utilities

  Provides the subset of POSIX operations that a Warp-style HTTP server
  needs. Only what's actually used: file descriptor bookkeeping and file
  status queries.

  ## Haskell equivalent
  `System.PosixCompat.Files`/`Types` from the `unix-compat` package.

  ## Design

  Minimal port — most functionality is already available through Lean's
  `System.FilePath` metadata API, which this module wraps.
-/

namespace System.Posix

-- ══════════════════════════════════════════════════════════════
-- File descriptors
-- ══════════════════════════════════════════════════════════════

/-- A file descriptor (Unix fd number). -/
structure Fd where
  fd : UInt32
deriving BEq, Repr, Inhabited

instance : ToString Fd where
  toString f := s!"Fd({f.fd})"

/-- Close a file descriptor. -/
def closeFd (_ : Fd) : IO Unit :=
  pure ()

-- ══════════════════════════════════════════════════════════════
-- File status
-- ══════════════════════════════════════════════════════════════

/-- File status information (simplified). -/
structure FileStatus where
  size : Nat
  isRegularFile : Bool
  isDirectory : Bool
deriving Repr

/-- Get file status.
    $$\text{getFileStatus} : \text{String} \to \text{IO}(\text{FileStatus})$$ -/
def getFileStatus (path : String) : IO FileStatus := do
  let md ← System.FilePath.metadata path
  pure {
    size := md.byteSize.toNat
    isRegularFile := md.type == .file
    isDirectory := md.type == .dir
  }

/-- Check if a file exists. -/
def fileExist (path : String) : IO Bool := do
  try
    let _ ← System.FilePath.metadata path
    pure true
  catch _ =>
    pure false

end System.Posix
