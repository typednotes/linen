/-
  Tests for `Control.Monad.Effect.ObjectStore`.

  Six bands, following `HTTPTest`:

  1. The `k!` macro, and its deliberate divergence from `p!`.
  2. Permission facts, as `rfl` and `inferInstance`.
  3. Permitted calls elaborate.
  4. **Withheld permissions do not elaborate**, pinned with `#guard_msgs`.
  5. **Scope escapes stated as theorems**, which is the stronger form and does
     not rot when a compiler version rewords its diagnostics.
  6. End to end — purely through `dryRun`, and through the in-memory backend.
-/
import Linen.Control.Monad.Effect.ObjectStore

open Data.OpenUnion Control.Monad.Effect Control.Monad.Effect.ObjectStore

namespace Tests.Control.Monad.Effect.ObjectStore

-- ── The `k!` macro ──────────────────────────────────────────────────────────

#guard (k!"logs/2026/a.json" : Key) == ["logs", "2026", "a.json"]
#guard Key.render k!"logs/2026/a.json" == "logs/2026/a.json"

/- **`k!` does not filter empty segments, while `p!` does.** An S3 key is an
   opaque byte string: `a//b`, `a/` and `/a` are three different keys and all
   three must be addressable. This looks like a bug and is not. -/
#guard (k!"a//b" : Key) == ["a", "", "b"]
#guard (k!"a/" : Key) == ["a", ""]
#guard (k!"/a" : Key) == ["", "a"]

/- The split and the render are exact inverses, which is what makes
   `List String` a faithful representation rather than a lossy one. -/
#guard Key.render k!"a//b" == "a//b"
#guard Key.render k!"a/" == "a/"
#guard Key.render k!"/a" == "/a"
#guard Key.render k!"" == ""

-- ── Capabilities ────────────────────────────────────────────────────────────

/-- Read and write under `reports/`, read-only under `logs/`, nothing else. -/
abbrev analytics : Capability := tiered "analytics" k!"reports" k!"logs"

#guard analytics.consistent
#guard (readOnly "assets").consistent
#guard (readWriteDelete "assets").consistent
#guard (writeOnlyPrefix "uploads" k!"incoming").consistent

example : (readOnly "assets").canGet = true := rfl
example : (readOnly "assets").canPut = false := rfl
example : (readOnly "assets").canDelete = false := rfl

example : CanGet (readOnly "assets") := inferInstance
example : CanList (readOnly "assets") := inferInstance
example : CanPut (readWrite "assets") := inferInstance
example : CanDelete (readWriteDelete "assets") := inferInstance

/- The evidence is the proof, not a marker: a bogus instance would require
   proving `false = true`. -/
example : CanGet.proof (cap := readOnly "assets")
  = (rfl : (readOnly "assets").canGet = true) := rfl

/- **`scopes := []` grants nothing** — the deliberate divergence from
   `FileSystem`, where an empty scope list means unrestricted. A cloud
   credential's blast radius is the whole account, so that is not a default
   anyone wants. -/
example : ({ canGet := true } : Capability).permits .get "anything" k!"x" = false := rfl

-- ── Permitted calls elaborate ───────────────────────────────────────────────

example : Eff [ObjectStore (readOnly "assets")] (Except Cloud.Error ByteArray) :=
  get "assets" k!"logs/a.json"

example : Eff [ObjectStore analytics] (Except Cloud.Error Cloud.ObjectMeta) :=
  putString "analytics" k!"reports/q3.csv" "a,b,c"

example : Eff [ObjectStore analytics] (Except Cloud.Error ByteArray) :=
  get "analytics" k!"logs/2026/app.log"

example : Eff [ObjectStore (readWriteDelete "tmp")] (Except Cloud.Error Unit) :=
  delete "tmp" k!"scratch/a"

/- A `do` block: `cap` is determined by `HasObjectStore`'s `outParam`, which is
   what keeps the `decide` obligations solvable inside a continuation. -/
example : Eff [ObjectStore analytics] (Except Cloud.Error Unit) := do
  let _ ← get "analytics" k!"logs/a.log"
  let _ ← putString "analytics" k!"reports/summary.txt" "done"
  return .ok ()

-- ── Withheld permissions do not elaborate ───────────────────────────────────

/--
error: failed to synthesize instance of type class
  CanPut (readOnly "assets")

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (put (effs := [ObjectStore (readOnly "assets")]) "assets" k!"a.json"
  (String.toUTF8 "x"))

/--
error: failed to synthesize instance of type class
  CanDelete (readWrite "assets")

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
#check (delete (effs := [ObjectStore (readWrite "assets")]) "assets" k!"a.json")

/- A write-only uploader is refused on **both** counts: it lacks the read bit,
   *and* its one scope names only `.put`. The two halves of the capability are
   checked independently, so a mistake in either is caught — and the second
   error is the argument-level restriction no type-level effect row can
   express. -/
/--
error: failed to synthesize instance of type class
  CanGet (writeOnlyPrefix "uploads" ["incoming"])

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: could not synthesize default value for parameter 'hs' using tactics
---
error: Tactic `decide` proved that the proposition
  (writeOnlyPrefix "uploads" ["incoming"]).permits Op.get "uploads" ["incoming", "a"] = true
is false
-/
#guard_msgs in
#check (Control.Monad.Effect.ObjectStore.get
  (effs := [ObjectStore (writeOnlyPrefix "uploads" k!"incoming")]) "uploads" k!"incoming/a")

-- ── Scope escapes, as theorems ──────────────────────────────────────────────

/- Stated as decidable propositions rather than as diagnostic-message matches:
   the stronger statement, and one that does not rot when a compiler version
   rewords its errors. -/

/-- **A different bucket is not covered**, however similar its name. -/
theorem no_other_bucket :
    (readOnly "assets").permits .get "assets-backup" k!"a.json" ≠ true := by decide

/-- **The S3 sibling-prefix trap.** S3's own `prefix=` is a *byte* prefix, so
    `prefix=logs` matches `logs-2026/a`; the component-wise check does not.
    That is the safe direction, and the same trap `FileSystem` pins with
    `/tmp/sandbox-evil` and `HTTP` with `/v1-admin`. -/
theorem no_sibling_prefix_escape :
    analytics.permits .get "analytics" k!"logs-2026/app.log" ≠ true := by decide

/-- The read-only tier cannot be written to, even though the capability as a
    whole grants writing — a restriction on (operation, argument) *pairs*, not
    on arguments alone. -/
theorem no_write_to_readonly_tier :
    analytics.permits .put "analytics" k!"logs/a.log" ≠ true := by decide

/-- And the writable tier can be. -/
theorem write_to_writable_tier :
    analytics.permits .put "analytics" k!"reports/q3.csv" = true := by decide

/-- Nothing outside either tier. -/
theorem no_escape_outside_tiers :
    analytics.permits .get "analytics" k!"secrets/keys.txt" ≠ true := by decide

/-- A prefix does not cover its own parent: holding `reports/` is not holding
    the bucket root. -/
theorem prefix_does_not_cover_parent :
    (underPrefix "b" k!"reports").permits .get "b" k!"other.txt" ≠ true := by decide

-- ── Unions are monotone ─────────────────────────────────────────────────────

/-- The bits of a union are computed, so its permission instances are declared
    once with `.of` rather than found by search. -/
instance : CanGet ((readOnly "a").union (readWrite "b")) := .of
instance : CanPut ((readOnly "a").union (readWrite "b")) := .of

example : Eff [ObjectStore ((readOnly "a").union (readWrite "b"))]
    (Except Cloud.Error ByteArray) :=
  get "a" k!"x"

example : Eff [ObjectStore ((readOnly "a").union (readWrite "b"))]
    (Except Cloud.Error Cloud.ObjectMeta) :=
  putString "b" k!"y" "z"

/-- Combining capabilities takes nothing away — the monotonicity that makes it
    safe. -/
example : ((readOnly "a").union (readWrite "b")).permits .get "a" k!"x" = true :=
  permits_union_left (by decide)

example : ((readOnly "a").union (readWrite "b")).permits .put "b" k!"y" = true :=
  permits_union_right (by decide)

/-- A union still grants nothing outside either operand. -/
theorem union_grants_no_third_bucket :
    ((readOnly "a").union (readWrite "b")).permits .get "c" k!"x" ≠ true := by decide

-- ── Runtime-validated keys ──────────────────────────────────────────────────

/- A key computed at runtime cannot have its scope proved by `decide`. That is
   not a gap but the honest shape of the problem: `check?` validates one and
   hands back the evidence, so the operations still cannot be reached without
   it. -/
#guard (ScopedKey.check? analytics .get "analytics" ["logs", "a.log"]).isSome
#guard (ScopedKey.check? analytics .put "analytics" ["logs", "a.log"]).isNone
#guard (ScopedKey.check? analytics .get "other" ["logs", "a.log"]).isNone

/-- Evidence for one operation is not evidence for another, so the
    runtime-validated path is not a way around the (operation, argument)
    split. -/
example (sk : ScopedKey analytics .get) :
    Eff [ObjectStore analytics] (Except Cloud.Error ByteArray) := getAt sk

-- ── End to end, purely ──────────────────────────────────────────────────────

/-- A small job written against the effect. -/
def job : Eff [ObjectStore analytics] (Except Cloud.Error Nat) := do
  let _ ← get "analytics" k!"logs/2026/app.log"
  let _ ← putString "analytics" k!"reports/summary.txt" "ok"
  let _ ← list "analytics" k!"logs"
  return .ok 0

/- **The pure interpreter**: the exact request sequence, with no `IO` and no
   stub. Note the delimiter-terminated wire prefix on the `list`. -/
/-- info: ["get analytics logs/2026/app.log", "put analytics reports/summary.txt (2 bytes)", "list analytics logs/"] -/
#guard_msgs in
#eval (dryRun job).2

/- **The wire prefix is delimiter-terminated.** S3's `prefix=` is a byte
   prefix while the scope check is component-wise, so sending the bare prefix
   would let a permitted `list` enumerate keys under `logs-2026/` that a `get`
   would have been refused. -/
#guard wirePrefix k!"logs" == "logs/"
#guard wirePrefix k!"logs/2026" == "logs/2026/"
#guard wirePrefix [] == ""

-- ── End to end, against a backend ───────────────────────────────────────────

/- **The same program runs against any backend.** Here the in-memory one;
   swapping in `Cloud.ObjectStore.S3.atEndpoint` changes the argument, not the
   program. -/
/-- info: (some "hello", ["reports/greeting.txt"]) -/
#guard_msgs in
#eval show IO (Option String × List String) from do
  let store ← Cloud.ObjectStore.inMemory
  let program : Eff [ObjectStore analytics] (Except Cloud.Error (Option String)) := do
    let _ ← putString "analytics" k!"reports/greeting.txt" "hello"
    let got ← getString "analytics" k!"reports/greeting.txt"
    return got.map some
  let result ← runObjectStore analytics store program
  let keys ← store.keys
  return ((result.toOption).getD none, keys.toOption.getD [])

/- The handler checks nothing — the proofs were established at elaboration —
   so a refused request is one that never existed, not one the backend
   rejected. What the backend still decides is provider behaviour: a missing
   object is `notFound` from the store, not from the capability. -/
/-- info: Cloud.Class.notFound -/
#guard_msgs in
#eval show IO Cloud.Class from do
  let store ← Cloud.ObjectStore.inMemory
  let program : Eff [ObjectStore analytics] (Except Cloud.Error ByteArray) :=
    get "analytics" k!"logs/missing.log"
  match ← runObjectStore analytics store program with
  | .error e => return e.klass
  | .ok _ => return .protocol

/- A capability spanning two buckets dispatches per bucket. -/
/-- info: (some "from-a", some "from-b") -/
#guard_msgs in
#eval show IO (Option String × Option String) from do
  let a ← Cloud.ObjectStore.inMemory
  let b ← Cloud.ObjectStore.inMemory
  let _ ← a.putString "x" "from-a"
  let _ ← b.putString "x" "from-b"
  let cap := (readOnly "bucket-a").union (readOnly "bucket-b")
  have : CanGet cap := .of
  let program : Eff [ObjectStore cap]
      (Except Cloud.Error (Option String × Option String)) := do
    let fromA ← getString "bucket-a" k!"x" (by decide)
    let fromB ← getString "bucket-b" k!"x" (by decide)
    return .ok (fromA.toOption, fromB.toOption)
  let r ← runObjectStoreWith cap
    (fun bucket => if bucket == "bucket-a" then a else b) program
  return ((r.toOption).getD (none, none))

end Tests.Control.Monad.Effect.ObjectStore
