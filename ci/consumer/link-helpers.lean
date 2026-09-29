-- ⟪linen-link-helpers:begin⟫
-- The link-flag helpers every executable that requires `linen` needs.
-- Canonical copy: `ci/consumer/link-helpers.lean` in `typednotes/linen`, at
-- the tag the consumer pins. Paste this block, markers included, into the
-- consumer's `lakefile.lean` above its own `run_cmd`, and check it with
-- `ci/consumer/check-link-helpers.sh lakefile.lean` (from the pinned linen
-- checkout, usually `.lake/packages/linen`).
--
-- Why a copy at all: Lake links an executable with its own package's
-- `moreLinkArgs` only (Lean 4.34, `Lake/Config/LeanExe.lean`), so a
-- dependency cannot hand these to a consumer. The block is helpers only —
-- which libraries to name is the consumer's choice, in its own `run_cmd`.

/-- Ask `pkg-config` for a package's flags, split on whitespace, degrading to
    `#[]` when `pkg-config` or the package is missing — matching how `linen`
    treats optional native dependencies. -/
def pkgConfigFlags (args : Array String) : IO (Array String) := do
  try
    let out ← IO.Process.output { cmd := "pkg-config", args }
    if out.exitCode != 0 then return #[]
    -- Whitespace folded to spaces with a `Char` predicate rather than a
    -- `replace` of escape sequences: this block is also embedded in string
    -- literals (a scaffolder's copy), and a backslash does not survive that.
    let normalized := out.stdout.map fun c => if c.isWhitespace then ' ' else c
    return (normalized.splitOn " ").filter (· != "") |>.toArray
  catch _ => return #[]

/-- Link flags naming each library file outright, rather than adding its
    directory to the search path.

    On Linux, `pkg-config --variable=libdir` is typically
    `/usr/lib/<multiarch>`, which holds the *system* `libc.so`. Passing that as
    `-L` makes `ld.lld` prefer the system glibc over the one Lean bundles, and
    Lean's `Scrt1.o` references `__libc_csu_init` — removed in glibc 2.34 — so
    an executable fails to link with an undefined symbol in the C runtime.
    Naming the file shadows nothing: `.dylib` on macOS (a Homebrew libpq is
    `libpq.dylib`), `.so` on Linux, falling back to `-lfoo` when the file is
    absent. -/
def pkgAbsoluteLibs (pkg : String) : IO (Array String) := do
  let libs ← pkgConfigFlags #["--libs", pkg]
  let dirs ← pkgConfigFlags #["--variable=libdir", pkg]
  let dir : Option String := (dirs.filter (· != ""))[0]?
  let ext := if System.Platform.isOSX then "dylib" else "so"
  let mut out : Array String := #[]
  for tok in libs do
    if tok.startsWith "-L" then
      continue                                  -- deliberately dropped
    else if tok.startsWith "-l" then
      let name := (tok.drop 2).toString
      match dir with
      | some d =>
        let c : System.FilePath := (d : System.FilePath) / s!"lib{name}.{ext}"
        if ← c.pathExists then out := out.push c.toString else out := out.push tok
      | none => out := out.push tok
    else
      out := out.push tok
  return out

/-- The active macOS SDK's framework and library search paths. Lean ships its
    own `lld`, which has no default framework path, so these are required for
    `-framework` to resolve. `#[]` off macOS. -/
def macSdkArgs : IO (Array String) := do
  try
    let out ← IO.Process.output { cmd := "xcrun", args := #["--show-sdk-path"] }
    if out.exitCode != 0 then return #[]
    let sdk := out.stdout.trimAscii.copy
    if sdk.isEmpty then return #[]
    return #["-F", sdk ++ "/System/Library/Frameworks", "-L", sdk ++ "/usr/lib"]
  catch _ => return #[]

/-- What `System.Keychain` needs: Security.framework on macOS, the Windows
    credential API, libsecret elsewhere. -/
def keychainLinkArgs : IO (Array String) := do
  if System.Platform.isOSX then
    return (← macSdkArgs) ++ #["-framework", "Security", "-framework", "CoreFoundation"]
  else if System.Platform.isWindows then
    return #["-ladvapi32", "-lcredui"]
  else
    pkgAbsoluteLibs "libsecret-1"

-- OpenSSL is deliberately absent: Lean ends every executable link with its
-- own static `libssl.a`/`libcrypto.a`, and naming a distro's `libssl.so` as
-- well fails the link on glibc symbols Lean's bundled glibc predates.
-- ⟪linen-link-helpers:end⟫
