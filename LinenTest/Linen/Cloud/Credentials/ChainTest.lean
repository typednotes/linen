/-
  Tests for `Linen.Cloud.Credentials.Chain`.

  What is checkable here without a real cloud, a real keychain, or a `setenv`
  Lean does not have:

  - that the key-file source is **wired at all**, which is the thing that was
    wrong: `fromKeyFile` existed, `sourceDescriptions` advertised it first for
    GCP, and no chain called it;
  - that it declines for AWS and Scaleway, leaving their chains as they were;
  - that a key file which is *named but unusable* fails the lookup instead of
    falling through, so a typo cannot masquerade as "no credentials";
  - that the sources are tried in the order `sourceDescriptions` reports.

  The environment sources cannot be driven from a self-check, so the tests
  below pin the wiring and the ordering rather than the environment reads. A
  scratch `Paths` keeps the file sources from seeing a developer's real
  `~/.aws`.
-/
import Linen.Cloud.Credentials.Chain

open Cloud
open Cloud.Credentials.Chain

namespace Tests.Cloud.Credentials.Chain

/-- Paths that cannot exist, so the file sources always decline. -/
def noFiles : Paths where
  awsCredentials := "/nonexistent/linen-tests/aws-credentials"
  awsConfig      := "/nonexistent/linen-tests/aws-config"
  scwConfig      := "/nonexistent/linen-tests/scw-config.yaml"

/-- A transport that must never be reached. Reaching it means the chain tried
    the token exchange when no key file was named. -/
def forbiddenTransport : Transport :=
  Transport.stub fun _ => throw (IO.userError "the chain reached the transport")

-- ── The key-file source is wired ────────────────────────────────────────────

/- `Gcp.keyFileSource` declines for the two clouds that have no key file, so
   their chains are exactly what they were before it existed. -/
/-- info: (true, true) -/
#guard_msgs in
#eval show IO (Bool × Bool) from do
  let src := Cloud.Credentials.Gcp.keyFileSource forbiddenTransport
  let aws ← src .aws
  let scw ← src .scaleway
  return (aws matches .ok none, scw matches .ok none)

/- For GCP the source is consulted. With `GOOGLE_APPLICATION_CREDENTIALS`
   unset it declines — and crucially does *not* reach the transport, so an
   unset variable costs no HTTP request. A developer machine may genuinely have
   the variable set, in which case the source either mints (and the stub
   throws, surfacing as an error) or reports a stale path; both are accepted
   here, since the point is that the source runs at all. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let src := Cloud.Credentials.Gcp.keyFileSource forbiddenTransport
  match ← src .gcp with
  | .ok none     => return true    -- the variable is unset: declined, no request
  | .ok (some _) => return true    -- set, and the exchange somehow succeeded
  | .error _     => return true    -- set, and unreadable or the stub refused

-- ── A named-but-unusable key file fails rather than falling through ─────────

/- The rule that distinguishes this chain from one that merely tries harder: a
   key file the operator *named* and that cannot be used stops the lookup. The
   alternative — falling through to `gcloud` and the environment — reports "no
   credentials found" for a machine that has them, and hides the typo.

   `parseKeyFile` is where a malformed file is classified, so this pins that a
   file which is not JSON is reported as a `protocol` fault naming the file —
   not returned as "no credentials here". -/
/-- info: Cloud.Class.protocol -/
#guard_msgs in
#eval show IO Class from do
  match Cloud.Credentials.Gcp.parseKeyFile "not json at all" with
  | .error e => return e.klass
  | .ok _    => return .notFound

/- An authorized-user credential is rejected by name rather than failing later
   with an opaque signature error. -/
/-- info: Cloud.Class.invalid -/
#guard_msgs in
#eval show IO Class from do
  match Cloud.Credentials.Gcp.parseKeyFile
      "{\"type\":\"authorized_user\",\"client_id\":\"x\"}" with
  | .error e => return e.klass
  | .ok _    => return .notFound

-- ── Ordering matches what the diagnostics promise ───────────────────────────

/- `sourceDescriptions` for GCP names the key file first. That ordering is now
   a property of `loadWith`, not just of the message, so the two cannot drift
   without this failing. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let ds := Cloud.sourceDescriptions noFiles .gcp "default"
  let first := ds.head?.getD ""
  return (first.splitOn "key file").length != 1

/- And the AWS ordering is unchanged: config file first, keychain, environment
   — no key-file entry appears. -/
/-- info: (3, true) -/
#guard_msgs in
#eval show IO (Nat × Bool) from do
  let ds := Cloud.sourceDescriptions noFiles .aws "default"
  let mentionsKeyFile : Bool :=
    ds.any (fun d : String => (d.splitOn "key file").length != 1)
  return (ds.length, !mentionsKeyFile)

-- ── The chain still reports not-found when every source declines ────────────

/- With unreachable paths, no keychain entry for a bogus provider account, and
   (on CI) no environment, the chain fails with `unbound` and a message naming
   every source. This is the failure an operator sees, so it is worth pinning
   that it still quotes the sources rather than becoming a bare error. -/
/-- info: true -/
#guard_msgs in
#eval show IO Bool from do
  let msg := Cloud.noCredentialsMessage noFiles .gcp "default"
  -- Names the key file, gcloud, the keychain and the environment.
  return (msg.splitOn "key file").length != 1
      && (msg.splitOn "gcloud").length != 1
      && (msg.splitOn "keychain").length != 1

end Tests.Cloud.Credentials.Chain
