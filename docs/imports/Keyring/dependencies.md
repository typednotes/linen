# `keyring` module dependencies

Topological order of every module of the [`keyring`](https://crates.io/crates/keyring)
Rust crate imported into `linen`, per [AGENTS.md](../../AGENTS.md)'s
crates.io-import convention.

An edge **A → B** means *module A imports module B*, so **B must be built before A**.

## Scope

Canonical source: [`open-source-cooperative/keyring-rs`](https://github.com/open-source-cooperative/keyring-rs).
The crate is a thin `Entry`/`Credential` façade (`keyring-core`) dispatching to
one of several per-OS backend crates: `keyring-macos` (Security.framework
Keychain), `keyring-secret-service`/`keyring-linux-keyutils` (Linux —
D-Bus Secret Service, resp. the kernel keyring), `keyring-windows` (Win32
Credential Manager). The public surface actually used by a typical consumer
(and the only surface ported here) is:

- `Entry::new(service, user) -> Entry`
- `Entry::set_password(&self, password: &str)`
- `Entry::get_password(&self) -> String`
- `Entry::delete_credential(&self)`
- `Entry::set_secret`/`get_secret` (raw bytes rather than a UTF-8 string) —
  ported alongside `set_password`/`get_password` since both are trivial
  wrappers around the same underlying per-OS store once the store itself is
  bound.

Per the user's explicit choice, this is a **full cross-platform port**: macOS
Keychain, Linux Secret Service, and Windows Credential Manager, even though
only the macOS backend can be built and tested in this environment.

### Precedence check (per AGENTS.md's stdlib > existing Haskell import > Hackage > crate rule)

- **Lean stdlib**: no keychain/credential-store API — nothing to reuse.
- **Already-ported Haskell modules in `linen`**: none of the existing crypto/
  network imports expose an OS credential store.
- **Hackage substitute**: researched `credential-store` (no macOS backend,
  unmaintained since 2018) and `keyring` (no Windows or generic-Linux backend,
  unmaintained since 2017) — neither covers the full cross-platform surface
  the user asked for, so per the precedence rule a direct port from the Rust
  crate is justified for all three backends.

### Build-system design (must exist before any FFI code is written)

`linen`'s existing native-FFI convention (see `lakefile.lean` and the
`ffi/network.c` shim) is: **one portable C file per concern, `#ifdef
__APPLE__` / `#ifdef __linux__` branches selecting the platform-specific
implementation inside a single translation unit**, compiled unconditionally
by Lake, with OS-specific *link* flags resolved at lakefile-elaboration time
(`ffi/network.c`'s kqueue/epoll split is the existing precedent — see its
header comment). `Crypto.Zlib`'s `macSdkLibArgs` established the pattern of
probing for a macOS-only tool (`xcrun`) and returning `#[]` elsewhere.

This import needs the same shape, extended with an explicit third branch for
Windows and OS-conditional *link* flags (frameworks and system libraries with
no `pkg-config` file, unlike every previous FFI import in this codebase):

- **One new shim, `ffi/keychain.c`**, with three `#ifdef`-guarded
  implementations of the same small function surface
  (`linen_keychain_set`/`linen_keychain_get`/`linen_keychain_delete`, each
  taking service/account/(for set) secret-bytes `Lean` byte arrays):
  - `#ifdef __APPLE__`: `SecItemAdd`/`SecItemCopyMatching`/`SecItemUpdate`/
    `SecItemDelete` against a `kSecClassGenericPassword` query keyed on
    service+account (`Security/Security.h`).
  - `#ifdef __linux__`: the D-Bus Secret Service protocol via `libsecret`
    (`secret_password_store_sync`/`secret_password_lookup_sync`/
    `secret_password_clear_sync`, `libsecret/secret.h`) — `libsecret-1` ships
    a `.pc` file, so this branch fits the existing `pkgConfig`-based
    convention unmodified.
  - `#ifdef _WIN32`: Win32 Credential Manager (`CredWriteW`/`CredReadW`/
    `CredDeleteW`, `wincred.h`).
- **`lakefile.lean` changes** (the first OS-conditional *link*-flag logic in
  this codebase — every previous native import links the same flags on every
  platform):
  - A new `keychainLinkArgs : Array String` definition, computed inside the
    existing `run_cmd` elaboration block using the **pure, compile-time**
    `System.Platform.isOSX` / `System.Platform.isWindows` constants (already
    used by Lake's own build config, `Lake/Config/LeanLib.lean` and
    `Lake/Util/NativeLib.lean`, for exactly this kind of branch — no new
    platform-detection mechanism is invented):
    - macOS: `#["-framework", "Security", "-framework", "CoreFoundation"]`.
    - Linux: `← pkgLinkFlags "libsecret-1"` (reuses the existing helper —
      returns `#[]` gracefully if `libsecret-1.pc` is absent, matching every
      other optional native dependency in this file).
    - Windows: `#["-ladvapi32", "-lcredui"]`.
  - `keychainLinkArgs` folded into `nativeLinkArgs` alongside the existing
    `pq`/`ssl`/`macSdk`/`zlib` terms.
  - A new `target keychain.o pkg : FilePath` compiling `ffi/keychain.c`,
    passing `← pkgConfig #["--cflags", "libsecret-1"]` as extra weak args (a
    no-op `#[]` on macOS/Windows, where the `#ifdef` branch never references
    libsecret headers).
  - `keychain.o` bundled into `linenffi`'s static lib alongside
    `network.o`/`postgres.o`/`jose.o`/`tls.o`/`zlib.o`.
- **One Lean module**, not three: because the C-level dispatch is already
  resolved per-platform inside the single `ffi/keychain.c` translation unit,
  the Lean side needs only one set of `@[extern]` opaque declarations — there
  is no Lean-level conditional compilation involved (Lean has none), and none
  is needed.

## Topologically sorted modules

<!-- 1. `keyring-core` (`Entry`, `Credential` trait, `Error`) — ported as
   `Linen/System/Keychain.lean`'s public `Entry`/error type, namespace
   `System.Keychain` (Lean-ified away from the crate's own name — `keyring`
   only makes sense as a crate-registry name, not a Lean module concept; the
   Lean stdlib would name this by what it does, `System.Keychain`, the same
   treatment `AGENTS.md` documents for `WaiAppStatic` → `WebApp.Static`). -->
<!-- 2. `keyring-macos` / `keyring-secret-service` / `keyring-windows` (the
   three per-OS backend crates) — collapsed into the single `ffi/keychain.c`
   shim described above; not given separate Lean modules, since Lean has no
   conditional compilation and the C-level `#ifdef` dispatch already resolves
   per platform before any Lean code runs. Only the macOS branch is
   build/test-verified in this repository's environment; the Linux
   (libsecret) and Windows (wincred) branches are written against the real
   APIs but unverified. -->
