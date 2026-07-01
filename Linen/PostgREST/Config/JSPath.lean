/-
  `PostgREST.Config.JSPath` — JWT claim path parsing

  Parses and follows JSON path expressions used to extract claims from JWTs.
  For example, `.role` extracts the top-level `role` claim, and
  `.user.permissions` navigates nested objects. Mirrors PostgREST's
  `PostgREST.Config.JSPath` module.

  A `JSPath` is a list of segments, each either a string key or an integer
  index:
  $$\text{JSPath} = [\text{JSPathSegment}],\quad
    \text{JSPathSegment} \in \{ \text{key}(s),\; \text{index}(n) \}$$
  Parsing splits on `.` and classifies each part:
  $$\text{parse}(\texttt{.user.permissions}) =
    [\text{key}(\texttt{user}), \text{key}(\texttt{permissions})]$$
  `follow` walks a flat key-value association list using the first segment;
  deeper paths require a proper JSON traversal layer.
-/

namespace PostgREST.Config

-- ── Path segment ──────────────────────────────────────────

/-- A single segment of a JSON path.
    $$\text{JSPathSegment} \in \{ \text{key}(s : \text{String}),\;
      \text{index}(i : \mathbb{N}) \}$$ -/
inductive JSPathSegment where
  /-- A string key for navigating into a JSON object. -/
  | key (name : String)
  /-- A numeric index for navigating into a JSON array. -/
  | index (i : Nat)
  deriving BEq, Repr

instance : ToString JSPathSegment where
  toString
    | .key name => name
    | .index i  => s!"[{i}]"

-- ── Path ──────────────────────────────────────────

/-- A parsed JSON path: a list of segments for navigating a JSON value.
    $$\text{JSPath} = \{ \text{segments} : [\text{JSPathSegment}] \}$$ -/
structure JSPath where
  segments : List JSPathSegment
  deriving BEq, Repr, Inhabited

instance : ToString JSPath where
  toString p :=
    let segs := p.segments.map toString
    "." ++ String.intercalate "." segs

/-- Whether this path is empty (no segments). -/
def JSPath.isEmpty (p : JSPath) : Bool :=
  p.segments.isEmpty

/-- The number of segments in this path. -/
def JSPath.depth (p : JSPath) : Nat :=
  p.segments.length

-- ── Parsing ──────────────────────────────────────────

/-- Parse a path string like `".role"` or `".user.permissions"`.
    $$\text{parse}(s) = \text{classify}(\text{split}(s, \texttt{.}))$$

    Leading dot is stripped. Each part is classified as an `index` if it
    parses as a natural number, otherwise as a `key`.

    Examples:
    - `".role"` → `[key "role"]`
    - `".user.permissions"` → `[key "user", key "permissions"]`
    - `".items.0.name"` → `[key "items", index 0, key "name"]` -/
def JSPath.parse (s : String) : JSPath :=
  let stripped := if s.startsWith "." then (s.drop 1).toString else s
  let parts := stripped.splitOn "."
  { segments := parts.filterMap fun p =>
    if p.isEmpty then none
    else match p.toNat? with
    | some n => some (.index n)
    | none => some (.key p) }

-- ── Path following ──────────────────────────────────────────

/-- Follow a single-key path through a flat key-value association list.
    $$\text{follow}([\text{key}(k)], \text{claims}) = \text{claims}[k]$$

    For paths deeper than one level, a proper JSON value traversal is
    needed. This function handles the common case of a single top-level key
    lookup (e.g. `.role`). -/
def JSPath.follow (path : JSPath) (claims : List (String × String)) : Option String :=
  match path.segments with
  | [.key k] => claims.lookup k
  | _ => none

/-- Follow a path through nested association lists (for two-level paths).
    For deeper nesting, a full JSON library is required. -/
def JSPath.followNested (path : JSPath) (claims : List (String × String))
    (nestedLookup : String → List (String × String)) : Option String :=
  match path.segments with
  | [.key k] => claims.lookup k
  | [.key k1, .key k2] => do
    let _ ← claims.lookup k1
    let nested := nestedLookup k1
    nested.lookup k2
  | _ => none

-- ── Default path ──────────────────────────────────────────

/-- The default JWT role claim path: `.role`. -/
def defaultRoleClaimPath : JSPath :=
  { segments := [.key "role"] }

end PostgREST.Config
