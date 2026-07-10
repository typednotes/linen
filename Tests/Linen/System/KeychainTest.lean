/-
  Tests for `Linen.System.Keychain`.

  These bindings are `@[extern]` IO actions performing real Keychain
  Services calls, so behaviour is checked with `#eval` (a thrown error fails
  the build), as in `Tests/Linen/Network/Socket/FFITest.lean` and
  `Tests/Linen/Crypto/JOSE/FFITest.lean`. Running these requires the
  `linenffi` native library, which `precompileModules` makes available to
  the interpreter.

  Every entry created here uses a clearly-scoped service name
  (`"linen-tests.System.Keychain"`) so it cannot collide with a real
  credential, and each test deletes what it created — even on failure, via
  `IO.FS.withFile`-style `try/finally` — leaving the Keychain untouched.

  Only exercised on macOS in this repository's CI/dev environment; the
  Linux/Windows backends behind the same `@[extern]` symbols are unverified.
-/
import Linen.System.Keychain

open System.Keychain

namespace Tests.System.Keychain

private def testService : String := "linen-tests.System.Keychain"

/-- Run `action` against a fresh entry, guaranteeing the entry is deleted
    (ignoring "not found") whether `action` succeeds or throws. -/
def withEntry (account : String) (action : Entry → IO Unit) : IO Unit := do
  let e := Entry.new testService account
  try
    action e
  finally
    try e.deleteCredential catch _ => pure ()

-- `setPassword` → `getPassword` round-trips exactly the stored UTF-8 text.
#eval show IO Unit from do
  withEntry "password-roundtrip" fun e => do
    e.setPassword "s3cr3t-p@ssw0rd"
    let got ← e.getPassword
    unless got == "s3cr3t-p@ssw0rd" do
      throw (IO.userError s!"expected 's3cr3t-p@ssw0rd', got {got}")

-- `setSecret`/`getSecret` round-trip arbitrary raw bytes.
#eval show IO Unit from do
  withEntry "secret-roundtrip" fun e => do
    let secret := ByteArray.mk #[0, 1, 2, 250, 251, 252, 255]
    e.setSecret secret
    let got ← e.getSecret
    unless got == secret do
      throw (IO.userError s!"expected {secret.data}, got {got.data}")

-- Storing again under the same (service, account) replaces the old value
-- rather than erroring (`SecItemAdd` duplicate ⇒ `SecItemUpdate` fallback).
#eval show IO Unit from do
  withEntry "overwrite" fun e => do
    e.setPassword "first"
    e.setPassword "second"
    let got ← e.getPassword
    unless got == "second" do
      throw (IO.userError s!"expected 'second', got {got}")

-- `deleteCredential` removes the entry: a subsequent `getPassword` throws.
#eval show IO Unit from do
  let e := Entry.new testService "delete-then-get"
  e.setPassword "temporary"
  e.deleteCredential
  let threw ← try
    _ ← e.getPassword
    pure false
  catch _ =>
    pure true
  unless threw do
    throw (IO.userError "expected getPassword to throw after deleteCredential")

-- `getPassword` on an entry that was never stored also throws.
#eval show IO Unit from do
  let e := Entry.new testService "never-stored"
  let threw ← try
    _ ← e.getPassword
    pure false
  catch _ =>
    pure true
  unless threw do
    throw (IO.userError "expected getPassword to throw for a never-stored entry")

-- `deleteCredential` on an already-deleted (or never-stored) entry throws
-- too, rather than silently succeeding — matching the documented convention.
#eval show IO Unit from do
  let e := Entry.new testService "delete-twice"
  e.setPassword "x"
  e.deleteCredential
  let threw ← try
    e.deleteCredential
    pure false
  catch _ =>
    pure true
  unless threw do
    throw (IO.userError "expected the second deleteCredential to throw")

end Tests.System.Keychain
