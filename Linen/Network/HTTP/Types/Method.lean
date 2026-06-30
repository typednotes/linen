/-
  Linen.Network.HTTP.Types.Method — HTTP request methods
-/

namespace Network.HTTP.Types

/-- Standard HTTP methods (RFC 7231 + RFC 5789).
    $$\text{StdMethod} = \text{GET} \mid \text{POST} \mid \text{HEAD} \mid \ldots$$ -/
inductive StdMethod where
  | GET | POST | HEAD | PUT | DELETE | TRACE | CONNECT | OPTIONS | PATCH
deriving BEq, Repr, Inhabited

instance : ToString StdMethod where
  toString
    | .GET     => "GET"
    | .POST    => "POST"
    | .HEAD    => "HEAD"
    | .PUT     => "PUT"
    | .DELETE  => "DELETE"
    | .TRACE   => "TRACE"
    | .CONNECT => "CONNECT"
    | .OPTIONS => "OPTIONS"
    | .PATCH   => "PATCH"

/-- An HTTP method: either a standard method or a custom string. -/
inductive Method where
  | standard : StdMethod → Method
  | custom : String → Method
deriving BEq, Repr

instance : ToString Method where
  toString
    | .standard m => toString m
    | .custom s => s

/-- Parse a string to a Method. Known methods return `standard`, others `custom`. -/
def parseMethod (s : String) : Method :=
  match s with
  | "GET"     => .standard .GET
  | "POST"    => .standard .POST
  | "HEAD"    => .standard .HEAD
  | "PUT"     => .standard .PUT
  | "DELETE"  => .standard .DELETE
  | "TRACE"   => .standard .TRACE
  | "CONNECT" => .standard .CONNECT
  | "OPTIONS" => .standard .OPTIONS
  | "PATCH"   => .standard .PATCH
  | other     => .custom other

/-- Render a method to its canonical string form. -/
@[inline] def renderMethod (m : Method) : String := toString m

-- ── parseMethod roundtrip theorems ──────────────────────────────────────────

/-- Parsing "GET" yields `Method.standard .GET`. -/
theorem parseMethod_GET : parseMethod "GET" = .standard .GET := by rfl

/-- Parsing "POST" yields `Method.standard .POST`. -/
theorem parseMethod_POST : parseMethod "POST" = .standard .POST := by rfl

/-- Parsing "HEAD" yields `Method.standard .HEAD`. -/
theorem parseMethod_HEAD : parseMethod "HEAD" = .standard .HEAD := by rfl

/-- Parsing "PUT" yields `Method.standard .PUT`. -/
theorem parseMethod_PUT : parseMethod "PUT" = .standard .PUT := by rfl

/-- Parsing "DELETE" yields `Method.standard .DELETE`. -/
theorem parseMethod_DELETE : parseMethod "DELETE" = .standard .DELETE := by rfl

/-- Parsing "TRACE" yields `Method.standard .TRACE`. -/
theorem parseMethod_TRACE : parseMethod "TRACE" = .standard .TRACE := by rfl

/-- Parsing "CONNECT" yields `Method.standard .CONNECT`. -/
theorem parseMethod_CONNECT : parseMethod "CONNECT" = .standard .CONNECT := by rfl

/-- Parsing "OPTIONS" yields `Method.standard .OPTIONS`. -/
theorem parseMethod_OPTIONS : parseMethod "OPTIONS" = .standard .OPTIONS := by rfl

/-- Parsing "PATCH" yields `Method.standard .PATCH`. -/
theorem parseMethod_PATCH : parseMethod "PATCH" = .standard .PATCH := by rfl

/-- Parsing an unknown string yields `Method.custom`. -/
theorem parseMethod_custom (s : String)
    (h : s ≠ "GET" ∧ s ≠ "POST" ∧ s ≠ "HEAD" ∧ s ≠ "PUT" ∧ s ≠ "DELETE"
       ∧ s ≠ "TRACE" ∧ s ≠ "CONNECT" ∧ s ≠ "OPTIONS" ∧ s ≠ "PATCH") :
    parseMethod s = .custom s := by
  simp only [parseMethod]
  split <;> simp_all

-- ═══════════════════════════════════════════════════════════
-- RFC 9110 §9.2: Method Properties
-- ═══════════════════════════════════════════════════════════

/-- RFC 9110 §9.2.1: Safe methods do not modify server state.
    $$\text{isSafe}(m) \iff m \in \{\text{GET}, \text{HEAD}, \text{OPTIONS}, \text{TRACE}\}$$ -/
def Method.isSafe : Method → Bool
  | .standard .GET     => true
  | .standard .HEAD    => true
  | .standard .OPTIONS => true
  | .standard .TRACE   => true
  | _                  => false

/-- RFC 9110 §9.2.2: Idempotent methods can be repeated without different outcomes.
    $$\text{isIdempotent}(m) \iff \text{isSafe}(m) \lor m \in \{\text{PUT}, \text{DELETE}\}$$ -/
def Method.isIdempotent : Method → Bool
  | .standard .PUT    => true
  | .standard .DELETE => true
  | m                 => m.isSafe

/-- RFC 9110 §9.2.1: GET is safe. -/
theorem Method.get_is_safe : (Method.standard .GET).isSafe = true := by rfl

/-- RFC 9110 §9.2.1: HEAD is safe. -/
theorem Method.head_is_safe : (Method.standard .HEAD).isSafe = true := by rfl

/-- RFC 9110 §9.2.1: OPTIONS is safe. -/
theorem Method.options_is_safe : (Method.standard .OPTIONS).isSafe = true := by rfl

/-- RFC 9110 §9.2.1: TRACE is safe. -/
theorem Method.trace_is_safe : (Method.standard .TRACE).isSafe = true := by rfl

/-- RFC 9110 §9.2.1: POST is not safe. -/
theorem Method.post_not_safe : (Method.standard .POST).isSafe = false := by rfl

/-- RFC 9110 §9.2.1: PATCH is not safe. -/
theorem Method.patch_not_safe : (Method.standard .PATCH).isSafe = false := by rfl

/-- RFC 9110 §9.2.2: PUT is idempotent. -/
theorem Method.put_is_idempotent : (Method.standard .PUT).isIdempotent = true := by rfl

/-- RFC 9110 §9.2.2: DELETE is idempotent. -/
theorem Method.delete_is_idempotent : (Method.standard .DELETE).isIdempotent = true := by rfl

/-- RFC 9110 §9.2.2: POST is not idempotent. -/
theorem Method.post_not_idempotent : (Method.standard .POST).isIdempotent = false := by rfl

/-- RFC 9110 §9.2.2: PATCH is not idempotent. -/
theorem Method.patch_not_idempotent : (Method.standard .PATCH).isIdempotent = false := by rfl

/-- RFC 9110 §9.2.2: All safe methods are idempotent.
    Proof by case analysis on `Method`: for each `standard` variant,
    either `isSafe` is false (contradiction) or `isIdempotent` reduces to `isSafe`. -/
theorem Method.safe_implies_idempotent (m : Method) (h : m.isSafe = true) :
    m.isIdempotent = true := by
  match m with
  | .standard .GET     => rfl
  | .standard .HEAD    => rfl
  | .standard .OPTIONS => rfl
  | .standard .TRACE   => rfl
  | .standard .PUT     => simp [Method.isSafe] at h
  | .standard .POST    => simp [Method.isSafe] at h
  | .standard .DELETE  => simp [Method.isSafe] at h
  | .standard .CONNECT => simp [Method.isSafe] at h
  | .standard .PATCH   => simp [Method.isSafe] at h
  | .custom _          => simp [Method.isSafe] at h

end Network.HTTP.Types
