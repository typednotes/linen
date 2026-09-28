/-
  Tests for `Control.Monad.Effect.SecretStore`.

  The flagship assertion in this file — and arguably in the whole capability
  pattern — is that **a capability granting `describe` and not `getValue` makes
  `getValue` fail to elaborate.** Both operations have the same effect type and
  differ only in a value indexing it, so no type-level effect row can draw the
  distinction at all.
-/
import Linen.Control.Monad.Effect.SecretStore

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.SecretStore

namespace Tests.Control.Monad.Effect.SecretStore

-- ── Names ───────────────────────────────────────────────────────────────────

#guard (n!"prod/db-password" : Name) == ["prod", "db-password"]
#guard Name.render n!"prod/db-password" == "prod/db-password"
#guard (n!"db-password" : Name) == ["db-password"]
#guard Name.render n!"db-password" == "db-password"

-- ── Capabilities ────────────────────────────────────────────────────────────

#guard describeOnly.consistent
#guard (readUnder n!"prod").consistent
#guard (readOne n!"db-password").consistent
#guard (rotateUnder n!"prod").consistent

/- **The bit worth staring at.** A health check, a deployment gate and a config
   validator all need to know whether a secret exists and which version is
   current; none of them need the plaintext. -/
example : describeOnly.canDescribe = true := rfl
example : describeOnly.canGetValue = false := rfl
example : describeOnly.canPut = false := rfl

example : CanDescribe describeOnly := inferInstance
example : CanList describeOnly := inferInstance
example : CanGetValue (readUnder n!"prod") := inferInstance
example : CanPut (rotateUnder n!"prod") := inferInstance

/- A rotator writes without reading: it replaces a secret's value and has no
   business seeing the old one. The mirror of the module's point. -/
example : (rotateUnder n!"prod").canGetValue = false := rfl

-- ── Permitted calls elaborate ───────────────────────────────────────────────

example : Eff [SecretStore describeOnly]
    (Except Cloud.Error (Option Cloud.Secret.Metadata)) :=
  describe n!"prod/db-password"

example : Eff [SecretStore describeOnly] (Except Cloud.Error Bool) :=
  exists? n!"prod/db-password"

example : Eff [SecretStore (readUnder n!"prod")]
    (Except Cloud.Error Cloud.Secret.Value) :=
  getValue n!"prod/db-password"

example : Eff [SecretStore (readOne n!"db-password")] (Except Cloud.Error String) :=
  getString n!"db-password"

example : Eff [SecretStore (rotateUnder n!"prod")]
    (Except Cloud.Error Cloud.Secret.Metadata) :=
  putString n!"prod/db-password" "new-value"

-- ── The flagship: metadata without plaintext ────────────────────────────────

/- **This is the distinction the module exists for.** `describe` compiles under
   `describeOnly`; `getValue` does not. Same effect type, same row — the
   difference is a *value*, which is exactly what a type-level effect row
   cannot carry. -/
/--
error: failed to synthesize instance of type class
  CanGetValue describeOnly

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  describeOnly.permits Op.getValue ["prod", "db-password"] = true
is false
-/
#guard_msgs in
#check (getValue (effs := [SecretStore describeOnly]) n!"prod/db-password")

/- `getString` is derived from `getValue`, so it is refused too — the split
   cannot be walked around by taking the convenience path. -/
/--
error: failed to synthesize instance of type class
  CanGetValue describeOnly

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  describeOnly.permits Op.getValue ["prod", "db-password"] = true
is false
-/
#guard_msgs in
#check (getString (effs := [SecretStore describeOnly]) n!"prod/db-password")

/- Nor by asking for a specific version. -/
/--
error: failed to synthesize instance of type class
  CanGetValue describeOnly

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  describeOnly.permits Op.getValue ["prod", "db-password"] = true
is false
-/
#guard_msgs in
#check (getVersion (effs := [SecretStore describeOnly]) n!"prod/db-password" "3")

/- A reader cannot write. -/
/--
error: failed to synthesize instance of type class
  CanPut (readUnder ["prod"])

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  (readUnder ["prod"]).permits Op.put ["prod", "x"] = true
is false
-/
#guard_msgs in
#check (put (effs := [SecretStore (readUnder n!"prod")]) n!"prod/x"
  (Cloud.Secret.Value.ofString "v"))

/- A rotator cannot read. -/
/--
error: failed to synthesize instance of type class
  CanGetValue (rotateUnder ["prod"])

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  (rotateUnder ["prod"]).permits Op.getValue ["prod", "x"] = true
is false
-/
#guard_msgs in
#check (getValue (effs := [SecretStore (rotateUnder n!"prod")]) n!"prod/x")

-- ── Scope escapes, as theorems ──────────────────────────────────────────────

/-- **Another environment's secrets are not covered.** The reason names are
    segments: `prod/*` is expressible, so a production service cannot read
    staging's credentials or the reverse. -/
theorem no_other_environment :
    (readUnder n!"prod").permits .getValue n!"staging/db-password" ≠ true := by decide

/-- The sibling-prefix trap, as in `Effect.ObjectStore`: a component-wise
    prefix is stricter than a byte prefix, which is the safe direction. -/
theorem no_sibling_prefix_escape :
    (readUnder n!"prod").permits .getValue n!"prod-legacy/db-password" ≠ true := by decide

/-- `readOne` really is one secret. -/
theorem read_one_is_one :
    (readOne n!"db-password").permits .getValue n!"api-token" ≠ true := by decide

/-- And `describeOnly` covers every *name* while granting no plaintext on any
    of them — the two halves of the capability are independent. -/
theorem describe_covers_everything :
    describeOnly.permits .describe n!"anything/at/all" = true := by decide

theorem describe_grants_no_value_anywhere :
    describeOnly.permits .getValue n!"anything/at/all" ≠ true := by decide

-- ── Unions ──────────────────────────────────────────────────────────────────

instance : CanDescribe (describeOnly.union (readUnder n!"prod")) := .of
instance : CanGetValue (describeOnly.union (readUnder n!"prod")) := .of

example : (describeOnly.union (readUnder n!"prod")).permits .describe n!"x" = true :=
  permits_union_left (by decide)

example : (describeOnly.union (readUnder n!"prod")).permits .getValue n!"prod/x" = true :=
  permits_union_right (by decide)

/-- The bits are a global upper bound and the scopes append, so a union of
    "describe everything" and "read prod" reads **prod only** — it does not
    silently become a plaintext reader everywhere. -/
theorem union_does_not_widen_value_access :
    (describeOnly.union (readUnder n!"prod")).permits .getValue n!"staging/x" ≠ true := by
  decide

-- ── Runtime-validated names ─────────────────────────────────────────────────

/- Evidence for `describe` is **not** evidence for `getValue`, so the
   runtime-validated path is not a way around the split. -/
#guard (ScopedName.check? describeOnly .describe n!"prod/x").isSome
#guard (ScopedName.check? describeOnly .getValue n!"prod/x").isNone
#guard (ScopedName.check? (readUnder n!"prod") .getValue n!"prod/x").isSome
#guard (ScopedName.check? (readUnder n!"prod") .getValue n!"staging/x").isNone

-- ── End to end, purely ──────────────────────────────────────────────────────

/-- A start-up check: confirm every secret exists, without reading any. -/
def healthCheck : Eff [SecretStore describeOnly] (Except Cloud.Error Bool) := do
  let a ← exists? n!"prod/db-password"
  let b ← exists? n!"prod/api-token"
  return do return (← a) && (← b)

/- **A dry run of a secret-reading program is safe to print.** The log names
   the secrets, never their values — which would not be true if the answers
   were recorded. -/
/-- info: ["describe prod/db-password", "describe prod/api-token"] -/
#guard_msgs in
#eval (dryRun healthCheck).2

def startup : Eff [SecretStore (readUnder n!"prod")] (Except Cloud.Error Nat) := do
  let _ ← describe n!"prod/db-password"
  let _ ← getValue n!"prod/db-password"
  return .ok 0

/-- info: ["describe prod/db-password", "getValue prod/db-password"] -/
#guard_msgs in
#eval (dryRun startup).2

-- ── End to end, against a backend ───────────────────────────────────────────

/-- info: (some "hunter2", true) -/
#guard_msgs in
#eval show IO (Option String × Bool) from do
  let store ← Cloud.SecretStore.inMemory
  let _ ← store.putString "prod/db-password" "hunter2"
  let program : Eff [SecretStore (readUnder n!"prod")]
      (Except Cloud.Error (Option String × Bool)) := do
    let value ← getString n!"prod/db-password"
    let present ← exists? n!"prod/db-password"
    return do return (some (← value), ← present)
  match ← runSecretStoreWith (readUnder n!"prod") store program with
  | .ok pair => return pair
  | .error _ => return (none, false)

/- A missing secret is `notFound` from the backend, not an empty value — the
   failure that otherwise surfaces as a service authenticating with a blank
   password. -/
/-- info: Cloud.Class.notFound -/
#guard_msgs in
#eval show IO Cloud.Class from do
  let store ← Cloud.SecretStore.inMemory
  let program : Eff [SecretStore (readUnder n!"prod")]
      (Except Cloud.Error Cloud.Secret.Value) :=
    getValue n!"prod/db-password"
  match ← runSecretStoreWith (readUnder n!"prod") store program with
  | .error e => return e.klass
  | .ok _ => return .protocol

/- Even *with* `canGetValue`, the value cannot be printed: `Cloud.Secret.Value`
   redacts and has no `ToJSON`. Permission controls who may read; the opaque
   type controls what may then happen to it. -/
/-- info: <redacted> -/
#guard_msgs in
#eval show IO Cloud.Secret.Value from do
  let store ← Cloud.SecretStore.inMemory
  let _ ← store.putString "prod/db-password" "hunter2"
  let program : Eff [SecretStore (readUnder n!"prod")]
      (Except Cloud.Error Cloud.Secret.Value) :=
    getValue n!"prod/db-password"
  match ← runSecretStoreWith (readUnder n!"prod") store program with
  | .ok v => return v
  | .error _ => return Cloud.Secret.Value.ofString ""

end Tests.Control.Monad.Effect.SecretStore
