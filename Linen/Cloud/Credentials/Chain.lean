/-
  Cloud.Credentials.Chain — the complete credential chain, every source wired

  `Cloud.Credentials.loadWith` takes its two environment-dependent sources as
  parameters, because neither belongs in that module:

  - the **OS credential store** needs the keychain FFI, and
    `Cloud.Credentials.Keychain` supplies it;
  - the **service-account key file** needs a `Transport`, because turning a key
    into a token is an RFC 7523 round-trip, and `Cloud.Credentials.Gcp`
    supplies it.

  Each of those modules wires *its own* source and leaves the other declining,
  which is right for a caller that wants only one. Neither can wire both
  without importing the other. This module is the one place that imports both,
  so it is the one place the chain `Cloud.Credentials.sourceDescriptions`
  describes can be assembled in full:

  | provider   | sources, in order                                          |
  | ---------- | ---------------------------------------------------------- |
  | `aws`      | config file → keychain → environment                       |
  | `scaleway` | config file → keychain → environment                       |
  | `gcp`      | **key file** → `gcloud` → keychain → environment           |

  Use this unless you have a reason not to: `Keychain.load` silently skips the
  key file, so a GCP service deployed the standard way — a key file named by
  `GOOGLE_APPLICATION_CREDENTIALS`, no `gcloud`, no keychain — cannot
  authenticate through it.

  ## Why a `Transport` is a parameter and not a default

  `load` takes the transport rather than reaching for
  `Transport.network` itself, so that a test can supply a canned one and so
  that a caller who has already configured retries and timeouts does not end
  up with a second, differently-configured client for this one call.
-/
import Linen.Cloud.Credentials
import Linen.Cloud.Credentials.Keychain
import Linen.Cloud.Credentials.Gcp

namespace Cloud.Credentials.Chain

open Cloud

/-- Try every source in order and return the first that yields credentials.

    For GCP that means the key file named by `GOOGLE_APPLICATION_CREDENTIALS`
    first, then `gcloud auth print-access-token`, then the keychain, then
    `GOOGLE_OAUTH_ACCESS_TOKEN` — the order
    `Cloud.Credentials.sourceDescriptions` reports. For AWS and Scaleway the
    key-file source declines and the chain is the familiar
    file → keychain → environment.

    A key file that is *named but unusable* fails the whole lookup rather than
    falling through, so a typo in `GOOGLE_APPLICATION_CREDENTIALS` or an
    expired key surfaces as itself. Every other source declines silently, since
    "no config file" and "no keychain entry" are ordinary states. -/
def loadFrom (t : Transport) (paths : Paths) (provider : Provider)
    (region : String := "") : IO (Except Error Credentials) :=
  Cloud.loadWith paths provider Keychain.forProvider (Gcp.keyFileSource t region)

/-- The complete chain, from the conventional file locations. -/
def load (t : Transport) (provider : Provider) (region : String := "") :
    IO (Except Error Credentials) := do
  loadFrom t (← Paths.default) provider region

end Cloud.Credentials.Chain
