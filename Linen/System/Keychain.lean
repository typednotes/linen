/-
  Linen.System.Keychain — OS credential-store access

  Ported from the Rust [`keyring`](https://crates.io/crates/keyring) crate
  (`keyring-rs`), Lean-ified: `keyring`'s crate name only makes sense as a
  registry identifier, not as a module concept, so the Lean stdlib's own
  naming convention (`System.…`) is used instead — this is the same treatment
  `AGENTS.md` documents for e.g. `WaiAppStatic` → `WebApp.Static`.

  Exposes the crate's `Entry`/`Credential` façade: a small handle identifying
  a secret by `(service, account)`, with `setPassword`/`getPassword`/
  `deleteCredential` (UTF-8 text) and `setSecret`/`getSecret` (raw bytes)
  operations, dispatching at the C level to whichever native credential
  store the platform provides.

  ## FFI

  Implementation in `ffi/keychain.c` (symbols `linen_keychain_*`), which
  resolves per-platform (`#ifdef __APPLE__` / `__linux__` / `_WIN32`) inside
  a single translation unit — there is no Lean-level conditional compilation,
  since Lean has none and needs none here:

  - **macOS**: Security.framework Keychain (`kSecClassGenericPassword`),
    keyed on service+account attributes.
  - **Linux**: the D-Bus Secret Service, via libsecret's synchronous
    length-explicit API (`secret_value_new` plus
    `secret_service_{store,lookup}_sync`, *not* the `secret_password_*`
    convenience wrappers, which carry the secret as a NUL-terminated string
    and would truncate any secret containing a NUL byte). Only
    compiled/linked when `libsecret-1`'s `.pc` file is present; the link
    flags degrade to nothing otherwise.
  - **Windows**: the Win32 Credential Manager (`wincred.h`).

  The macOS and Linux branches are both exercised by this repository's test
  suite on every CI run — the Linux leg runs against a real `gnome-keyring`
  Secret Service brought up on a session D-Bus by
  `.github/actions/setup-native-deps`, because libsecret is only a client of
  that service. The Windows branch is written against the real wincred API
  but is **unverified**: CI runs no Windows leg.

  ## Error convention

  All three operations raise a plain `IO.Error` (via `IO.userError`, matching
  every other native FFI module in this library — `ffi/tls.c`, `ffi/network.c`)
  rather than returning an `Option`/`Except`: this mirrors the upstream crate,
  where `get_password`/`delete_credential` on a missing entry return
  `Err(Error::NoEntry)` rather than a plain `None`. Concretely: `getPassword`/
  `getSecret` on an entry that was never stored (or already deleted) throws;
  `deleteCredential` on an already-deleted (or never-stored) entry also
  throws, rather than silently succeeding.
-/

namespace System.Keychain

/-- Store or replace the secret for `(service, account)`, as raw bytes. -/
@[extern "linen_keychain_set"]
opaque setImpl (service : @& String) (account : @& String) (secret : @& ByteArray) : IO Unit

/-- Retrieve the secret for `(service, account)`, as raw bytes.
    Throws an `IO.Error` if no such entry exists. -/
@[extern "linen_keychain_get"]
opaque getImpl (service : @& String) (account : @& String) : IO ByteArray

/-- Delete the entry for `(service, account)`.
    Throws an `IO.Error` if no such entry exists. -/
@[extern "linen_keychain_delete"]
opaque deleteImpl (service : @& String) (account : @& String) : IO Unit

/-- A handle identifying a secret in the OS credential store by the pair
    `(service, account)` — the Lean-ified counterpart of the crate's
    `Entry::new(service, user)`. -/
structure Entry where
  /-- The service (application/site) name the secret is stored under. -/
  service : String
  /-- The account (user) name the secret is stored under. -/
  account : String
  deriving Repr, BEq

/-- Create a new credential-store handle for `(service, account)`.
    Does not itself touch the credential store — mirrors `Entry::new`. -/
def Entry.new (service account : String) : Entry :=
  { service, account }

/-- Store `secret` (raw bytes) under this entry, replacing any existing
    value. Mirrors `Entry::set_secret`. -/
def Entry.setSecret (e : Entry) (secret : ByteArray) : IO Unit :=
  setImpl e.service e.account secret

/-- Retrieve the raw-byte secret stored under this entry.
    Throws an `IO.Error` if nothing is stored. Mirrors `Entry::get_secret`. -/
def Entry.getSecret (e : Entry) : IO ByteArray :=
  getImpl e.service e.account

/-- Store `password` (UTF-8 text) under this entry, replacing any existing
    value. Mirrors `Entry::set_password`. -/
def Entry.setPassword (e : Entry) (password : String) : IO Unit :=
  e.setSecret password.toUTF8

/-- Retrieve the UTF-8 password stored under this entry.
    Throws an `IO.Error` if nothing is stored, or if the stored bytes are not
    valid UTF-8. Mirrors `Entry::get_password`. -/
def Entry.getPassword (e : Entry) : IO String := do
  let bytes ← e.getSecret
  match String.fromUTF8? bytes with
  | some s => pure s
  | none => throw (IO.userError "keychain: stored secret is not valid UTF-8")

/-- Remove the entry from the credential store.
    Throws an `IO.Error` if no such entry exists. Mirrors
    `Entry::delete_credential`. -/
def Entry.deleteCredential (e : Entry) : IO Unit :=
  deleteImpl e.service e.account

end System.Keychain
