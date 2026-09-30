/-
  Connector — credential-free, capability-indexed operations on connected services.

  A connector is not unrestricted HTTP. Its capability fixes the provider and
  connection, and grants named operations on structured resource paths. The
  runtime interprets operations through a credential broker; no key, arbitrary
  transport, or credential-selected URL is carried by the computation.

  Resource components retain boundaries: a bucket prefix `reports` does not
  include `reports-private`; calendar and mailbox ids are components, not
  substring matches. Empty scopes grant nothing. The interpreter must enforce
  its organization/connection upper bounds as well as these static proofs, and
  must derive the provider request from the named operation and resource.
-/
import Linen.Control.Monad.Effect
import Lean.Data.Json

namespace Control.Monad.Effect.Connector

open Data.OpenUnion Control.Monad.Effect

/-- A hierarchy of provider resource identifiers, never a URL or bearer key. -/
abbrev Resource := List String

/-- Canonical broker resource components. No path traversal, separators or
    control bytes; an empty resource denotes the connection itself. -/
def Resource.valid (resource : Resource) : Bool :=
  resource.length ≤ 32 && resource.all (fun part =>
    !part.isEmpty && part.toUTF8.size ≤ 255 && part != "." && part != ".." &&
    !part.contains '/' && !part.contains '\\' && !part.contains '%' &&
    part.all (fun c => c.toNat ≥ 0x20 && !(c.toNat ≥ 0x7f && c.toNat ≤ 0x9f)))

/-- Operations are literal names, never wildcard patterns. -/
def validOperation (operation : String) : Bool :=
  !operation.isEmpty && operation.length ≤ 64 &&
    operation.all (fun c => c.isAlphanum || c == '.' || c == '-' || c == '_')

/-- One operation on an exact resource or its descendants. -/
structure Scope where
  operation : String
  root : Resource
  descendants : Bool := true
  deriving DecidableEq, Repr, Lean.ToJson, Lean.FromJson

def Scope.covers (scope : Scope) (operation : String) (resource : Resource) : Bool :=
  scope.operation == operation &&
    (if scope.descendants then scope.root.isPrefixOf resource else scope.root == resource)

/-- Every covered selector stays below the structured root, including exact
    grants. Component prefixes never become string-prefix authorization. -/
theorem Scope.covers_confined {scope : Scope} {operation : String} {resource : Resource}
    (h : scope.covers operation resource = true) : scope.root.IsPrefix resource := by
  simp only [Scope.covers, Bool.and_eq_true] at h
  cases recursive : scope.descendants
  · have same : scope.root = resource := by simpa [recursive] using h.2
    rw [same]
    exact ⟨[], by simp⟩
  · simpa [recursive] using h.2

/-- An exact grant cannot authorize a child or a substituted sibling. -/
theorem Scope.covers_exact {scope : Scope} {operation : String} {resource : Resource}
    (exactScope : scope.descendants = false) (h : scope.covers operation resource = true) :
    scope.root = resource := by
  simp [Scope.covers, exactScope] at h
  exact h.2

/-- A connection-specific capability. Limits are upper bounds, never grants.
    The broker's policy and the organization's policy may further narrow it. -/
structure Capability where
  provider : String
  connection : String
  scopes : List Scope := []
  maxRequestBytes : Nat := 1048576
  maxResponseBytes : Nat := 16777216
  deriving DecidableEq, Repr, Lean.ToJson, Lean.FromJson

/-- Decode a runtime ceiling without inheriting any constructor default.
    Explicit empty grants are valid; malformed/unknown fields never broaden it. -/
def Capability.parse (json : Lean.Json) : Except String Capability := do
  let keys ← json.getObj?
  unless keys.toList.all (fun (key, _) => ["provider", "connection", "scopes", "maxRequestBytes", "maxResponseBytes"].contains key) do
    throw "unknown capability field"
  let provider ← json.getObjValAs? String "provider"
  let connection ← json.getObjValAs? String "connection"
  let plain (s : String) := !s.isEmpty && s.length ≤ 128 && s.all (fun c => c.isAlphanum || c == '-' || c == '_')
  unless plain provider && plain connection do throw "invalid capability identity"
  let values ← json.getObjValAs? (Array Lean.Json) "scopes"
  unless values.size ≤ 128 do throw "too many capability scopes"
  let scopes ← values.toList.mapM fun value => do
    let keys ← value.getObj?
    unless keys.toList.all (fun (key, _) => ["operation", "root", "descendants"].contains key) do
      throw "unknown scope field"
    let operation ← value.getObjValAs? String "operation"
    let root ← value.getObjValAs? (List String) "root"
    let descendants ← value.getObjValAs? Bool "descendants"
    unless validOperation operation && Resource.valid root do throw "invalid capability scope"
    pure ({ operation, root, descendants } : Scope)
  let maxRequestBytes ← json.getObjValAs? Nat "maxRequestBytes"
  let maxResponseBytes ← json.getObjValAs? Nat "maxResponseBytes"
  unless maxRequestBytes > 0 && maxRequestBytes ≤ 67108864 && maxResponseBytes > 0 && maxResponseBytes ≤ 67108864 do
    throw "invalid capability byte limits"
  return { provider, connection, scopes, maxRequestBytes, maxResponseBytes }

def Capability.permits (cap : Capability) (operation : String) (resource : Resource) : Bool :=
  !operation.isEmpty && cap.scopes.any (fun scope => scope.covers operation resource)

/-- Select one operation without broadening resource grants or byte bounds. -/
def Capability.onlyOperation (cap : Capability) (operation : String) : Capability :=
  { cap with scopes := cap.scopes.filter (fun scope => scope.operation == operation) }

/-- Runtime-chosen resources still require evidence of membership in the scope. -/
structure ScopedResource (cap : Capability) (operation : String) where
  resource : Resource
  inScope : cap.permits operation resource = true

def ScopedResource.check? (cap : Capability) (operation : String) (resource : Resource) :
    Option (ScopedResource cap operation) :=
  if h : cap.permits operation resource = true then some ⟨resource, h⟩ else none

/-- The payload is operation-specific data. The trusted adapter, not the
    computation, constructs the URL/method/auth and verifies resource selectors. -/
inductive Connector (cap : Capability) : Type → Type where
  | request (operation : String) (resource : Resource)
      (permission : cap.permits operation resource = true)
      (payload : Lean.Json) : Connector cap Lean.Json

class HasConnector (effs : List (Type → Type)) (cap : outParam Capability) where
  inject : {α : Type} → Connector cap α → Union effs α

instance instHasConnectorHere {cap : Capability} {effs : List (Type → Type)} :
    HasConnector (Connector cap :: effs) cap where
  inject := Union.here

instance instHasConnectorThere {cap : Capability} {eff : Type → Type} {effs : List (Type → Type)}
    [h : HasConnector effs cap] : HasConnector (eff :: effs) cap where
  inject request := Union.there (h.inject request)

def call {effs : List (Type → Type)} {cap : Capability} [h : HasConnector effs cap]
    (operation : String) (resource : Resource) (payload : Lean.Json := Lean.Json.mkObj [])
    (permission : cap.permits operation resource = true := by decide) : Eff effs Lean.Json :=
  .impure (h.inject (.request operation resource permission payload)) .protect

def callAt {effs : List (Type → Type)} {cap : Capability} [h : HasConnector effs cap]
    (operation : String) (resource : ScopedResource cap operation)
    (payload : Lean.Json := Lean.Json.mkObj []) : Eff effs Lean.Json :=
  .impure (h.inject (.request operation resource.resource resource.inScope payload)) .protect

/-- Narrowing is semantic inclusion, independent of a scope's representation. -/
def Capability.Narrows (child parent : Capability) : Prop :=
  child.provider = parent.provider ∧ child.connection = parent.connection ∧
  child.maxRequestBytes ≤ parent.maxRequestBytes ∧ child.maxResponseBytes ≤ parent.maxResponseBytes ∧
  ∀ operation resource, child.permits operation resource = true → parent.permits operation resource = true

theorem Capability.Narrows.refl (cap : Capability) : cap.Narrows cap :=
  ⟨rfl, rfl, Nat.le_refl _, Nat.le_refl _, fun _ _ h => h⟩

theorem Capability.onlyOperation_narrows (cap : Capability) (operation : String) :
    (cap.onlyOperation operation).Narrows cap := by
  refine ⟨rfl, rfl, Nat.le_refl _, Nat.le_refl _, ?_⟩
  intro op resource permitted
  simp only [Capability.permits, Capability.onlyOperation, Bool.and_eq_true] at permitted ⊢
  obtain ⟨scope, member, covers⟩ := List.any_eq_true.mp permitted.2
  exact ⟨permitted.1, List.any_eq_true.mpr ⟨scope, (List.mem_filter.mp member).1, covers⟩⟩

theorem Capability.Narrows.trans {a b c : Capability} (ab : a.Narrows b) (bc : b.Narrows c) : a.Narrows c :=
  ⟨ab.1.trans bc.1, ab.2.1.trans bc.2.1,
    Nat.le_trans ab.2.2.1 bc.2.2.1, Nat.le_trans ab.2.2.2.1 bc.2.2.2.1,
    fun operation resource h => bc.2.2.2.2 operation resource (ab.2.2.2.2 operation resource h)⟩

/-- A decidable scope attenuation: keep the operation, restrict its root and
    optionally replace recursive access by exact access. -/
def Scope.narrows (child parent : Scope) : Bool :=
  child.operation == parent.operation &&
    (if parent.descendants then parent.root.isPrefixOf child.root
     else !child.descendants && child.root == parent.root)

theorem Scope.narrows_sound {child parent : Scope} (h : child.narrows parent = true)
    {operation : String} {resource : Resource} (hc : child.covers operation resource = true) :
    parent.covers operation resource = true := by
  simp only [Scope.narrows, Scope.covers, Bool.and_eq_true, beq_iff_eq] at h hc ⊢
  refine ⟨h.1.symm.trans hc.1, ?_⟩
  cases hp : parent.descendants <;> cases hh : child.descendants <;>
    simp [hp, hh] at h hc ⊢
  · exact h.2.symm.trans hc.2
  · simpa [hc.2] using h.2
  · exact h.2.trans hc.2

/-- A finite validator returning proof of semantic inclusion. It is sound,
    intentionally conservative for unions covered jointly by several scopes. -/
def Capability.narrows (child parent : Capability) : Bool :=
  child.provider == parent.provider && child.connection == parent.connection &&
    child.maxRequestBytes ≤ parent.maxRequestBytes && child.maxResponseBytes ≤ parent.maxResponseBytes &&
    child.scopes.all (fun scope => parent.scopes.any (scope.narrows ·))

theorem Capability.narrows_sound {child parent : Capability} (h : child.narrows parent = true) :
    child.Narrows parent := by
  simp only [Capability.narrows, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq] at h
  refine ⟨h.1.1.1.1, h.1.1.1.2, h.1.1.2, h.1.2, ?_⟩
  intro operation resource hc
  simp only [Capability.permits, Bool.and_eq_true] at hc ⊢
  obtain ⟨scope, hs, hp⟩ := List.any_eq_true.mp hc.2
  obtain ⟨ancestor, ha, hn⟩ := List.any_eq_true.mp (List.all_eq_true.mp h.2 scope hs)
  refine ⟨?_, List.any_eq_true.mpr ⟨ancestor, ha, Scope.narrows_sound hn hp⟩⟩
  simpa [h.1.1.1.1, h.1.1.1.2] using hc.1

/-- A validated attenuation carries its evidence to the session/runtime. -/
def Capability.checkNarrows? (child parent : Capability) : Option (PLift (child.Narrows parent)) :=
  if h : child.narrows parent = true then some ⟨Capability.narrows_sound h⟩ else none

/-- The four independently owned ceilings on an effect. None can substitute
    for another; all must name the same provider and connection. -/
structure Authority where
  organization : Capability
  connection : Capability
  cell : Capability
  warrant : Capability

def Authority.permits (authority : Authority) (operation : String) (resource : Resource) : Bool :=
  authority.organization.provider == authority.cell.provider &&
  authority.connection.provider == authority.cell.provider &&
  authority.warrant.provider == authority.cell.provider &&
  authority.organization.connection == authority.cell.connection &&
  authority.connection.connection == authority.cell.connection &&
  authority.warrant.connection == authority.cell.connection &&
  authority.organization.permits operation resource &&
  authority.connection.permits operation resource &&
  authority.cell.permits operation resource &&
  authority.warrant.permits operation resource

/-- Execution consumes this witness, rather than checking an unrelated string
    and then dispatching an unchecked request. -/
structure AuthorizedResource (authority : Authority) (operation : String) where
  resource : Resource
  permitted : authority.permits operation resource = true
  wellFormed : resource.valid = true
  namedOperation : validOperation operation = true

def AuthorizedResource.check? (authority : Authority) (operation : String) (resource : Resource) :
    Option (AuthorizedResource authority operation) :=
  if h : authority.permits operation resource = true then
    if v : resource.valid = true then
      if o : validOperation operation = true then some ⟨resource, h, v, o⟩ else none
    else none
  else none

/-- The four ceilings also intersect for byte bounds. -/
def Authority.maxRequestBytes (a : Authority) : Nat :=
  min a.organization.maxRequestBytes (min a.connection.maxRequestBytes (min a.cell.maxRequestBytes a.warrant.maxRequestBytes))

def Authority.maxResponseBytes (a : Authority) : Nat :=
  min a.organization.maxResponseBytes (min a.connection.maxResponseBytes (min a.cell.maxResponseBytes a.warrant.maxResponseBytes))

/-- A payload validated against the same authority and resource that execute.
    The interpreter must consume this value rather than the original JSON. -/
structure AuthorizedRequest (authority : Authority) (operation : String) where
  target : AuthorizedResource authority operation
  payload : Lean.Json
  bounded : payload.compress.toUTF8.size ≤ authority.maxRequestBytes

def AuthorizedRequest.check? (authority : Authority) (operation : String)
    (resource : Resource) (payload : Lean.Json) : Option (AuthorizedRequest authority operation) := do
  let target ← AuthorizedResource.check? authority operation resource
  if h : payload.compress.toUTF8.size ≤ authority.maxRequestBytes then
    some ⟨target, payload, h⟩ else none

/-- A disclosed response bounded by the same four ceilings as its request. -/
structure BoundedResponse (authority : Authority) where
  body : ByteArray
  bounded : body.size ≤ authority.maxResponseBytes

def BoundedResponse.check? (authority : Authority) (body : ByteArray) : Option (BoundedResponse authority) :=
  if h : body.size ≤ authority.maxResponseBytes then some ⟨body, h⟩ else none

theorem BoundedResponse.organization_bounded {a : Authority} (r : BoundedResponse a) :
    r.body.size ≤ a.organization.maxResponseBytes := Nat.le_trans r.bounded (Nat.min_le_left _ _)

theorem BoundedResponse.connection_bounded {a : Authority} (r : BoundedResponse a) :
    r.body.size ≤ a.connection.maxResponseBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_left _ _))

theorem BoundedResponse.cell_bounded {a : Authority} (r : BoundedResponse a) :
    r.body.size ≤ a.cell.maxResponseBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _) (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_left _ _)))

theorem BoundedResponse.warrant_bounded {a : Authority} (r : BoundedResponse a) :
    r.body.size ≤ a.warrant.maxResponseBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _) (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_right _ _)))

theorem AuthorizedRequest.organization_bounded {a : Authority} {op : String}
    (r : AuthorizedRequest a op) : r.payload.compress.toUTF8.size ≤ a.organization.maxRequestBytes :=
  Nat.le_trans r.bounded (Nat.min_le_left _ _)

theorem AuthorizedRequest.connection_bounded {a : Authority} {op : String}
    (r : AuthorizedRequest a op) : r.payload.compress.toUTF8.size ≤ a.connection.maxRequestBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_left _ _))

theorem AuthorizedRequest.cell_bounded {a : Authority} {op : String}
    (r : AuthorizedRequest a op) : r.payload.compress.toUTF8.size ≤ a.cell.maxRequestBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _)
    (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_left _ _)))

theorem AuthorizedRequest.warrant_bounded {a : Authority} {op : String}
    (r : AuthorizedRequest a op) : r.payload.compress.toUTF8.size ≤ a.warrant.maxRequestBytes :=
  Nat.le_trans r.bounded (Nat.le_trans (Nat.min_le_right _ _)
    (Nat.le_trans (Nat.min_le_right _ _) (Nat.min_le_right _ _)))

theorem AuthorizedResource.organization_permits {authority : Authority} {operation : String}
    (authorized : AuthorizedResource authority operation) :
    authority.organization.permits operation authorized.resource = true := by
  have h := authorized.permitted
  simp only [Authority.permits, Bool.and_eq_true] at h
  exact h.1.1.1.2

theorem AuthorizedResource.connection_permits {authority : Authority} {operation : String}
    (authorized : AuthorizedResource authority operation) :
    authority.connection.permits operation authorized.resource = true := by
  have h := authorized.permitted
  simp only [Authority.permits, Bool.and_eq_true] at h
  exact h.1.1.2

theorem AuthorizedResource.cell_permits {authority : Authority} {operation : String}
    (authorized : AuthorizedResource authority operation) :
    authority.cell.permits operation authorized.resource = true := by
  have h := authorized.permitted
  simp only [Authority.permits, Bool.and_eq_true] at h
  exact h.1.2

theorem AuthorizedResource.warrant_permits {authority : Authority} {operation : String}
    (authorized : AuthorizedResource authority operation) :
    authority.warrant.permits operation authorized.resource = true := by
  have h := authorized.permitted
  simp only [Authority.permits, Bool.and_eq_true] at h
  exact h.2

end Control.Monad.Effect.Connector
