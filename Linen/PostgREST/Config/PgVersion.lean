/-
  `PostgREST.Config.PgVersion` — PostgreSQL version parsing

  Parsing and comparison of PostgreSQL version numbers. PostgreSQL encodes
  its version as a single integer (e.g. 150004 for 15.0.4); this module
  provides structured parsing and minimum-version checks. Mirrors
  PostgREST's `PostgREST.Config.PgVersion` module.

  `PGVersion` stores major, minor, and patch as natural numbers:
  $$\text{PGVersion} = \{ \text{major} : \mathbb{N},\;
    \text{minor} : \mathbb{N},\; \text{patch} : \mathbb{N} \}$$
  `fromVersionNum` decodes PostgreSQL's integer version format:
  $$n \mapsto \langle n / 10000,\; (n \bmod 10000) / 100,\; n \bmod 100 \rangle$$
  Minimum supported version is PostgreSQL 9.6.
-/

namespace PostgREST.Config

-- ── PGVersion ──────────────────────────────────────────

/-- A parsed PostgreSQL version number.
    $$\text{PGVersion} = \{ \text{major},\; \text{minor},\;
      \text{patch} \} \subset \mathbb{N}^3$$ -/
structure PGVersion where
  pgvMajor : Nat
  pgvMinor : Nat
  pgvPatch : Nat := 0
  deriving BEq, Repr

/-- Render a version as `"major.minor.patch"`. -/
def PGVersion.toString (v : PGVersion) : String :=
  s!"{v.pgvMajor}.{v.pgvMinor}.{v.pgvPatch}"

instance : ToString PGVersion := ⟨PGVersion.toString⟩

instance : Ord PGVersion where
  compare a b :=
    match compare a.pgvMajor b.pgvMajor with
    | .eq => match compare a.pgvMinor b.pgvMinor with
      | .eq => compare a.pgvPatch b.pgvPatch
      | ord => ord
    | ord => ord

instance : Inhabited PGVersion := ⟨{ pgvMajor := 0, pgvMinor := 0, pgvPatch := 0 }⟩

-- ── Parsing ──────────────────────────────────────────

/-- Parse from PostgreSQL's integer version number encoding.
    $$\text{fromVersionNum}(n) = \langle n / 10000,\;
      (n \bmod 10000) / 100,\; n \bmod 100 \rangle$$

    Examples:
    - `150004` → `15.0.4`
    - `140009` → `14.0.9`
    - `90600`  → `9.6.0` -/
def PGVersion.fromVersionNum (n : Nat) : PGVersion :=
  { pgvMajor := n / 10000
    pgvMinor := (n % 10000) / 100
    pgvPatch := n % 100 }

/-- Convert a version back to PostgreSQL's integer encoding.
    $$\text{toVersionNum}(v) = v.\text{major} \times 10000
      + v.\text{minor} \times 100 + v.\text{patch}$$ -/
def PGVersion.toVersionNum (v : PGVersion) : Nat :=
  v.pgvMajor * 10000 + v.pgvMinor * 100 + v.pgvPatch

/-- Parse from a dotted version string (e.g. `"15.0.4"`). Returns `none` if
    the string cannot be parsed. -/
def PGVersion.parse (s : String) : Option PGVersion :=
  let parts := s.splitOn "."
  match parts with
  | [maj, min_, pat] => do
    let major ← maj.toNat?
    let minor ← min_.toNat?
    let patch ← pat.toNat?
    return { pgvMajor := major, pgvMinor := minor, pgvPatch := patch }
  | [maj, min_] => do
    let major ← maj.toNat?
    let minor ← min_.toNat?
    return { pgvMajor := major, pgvMinor := minor }
  | [maj] => do
    let major ← maj.toNat?
    return { pgvMajor := major, pgvMinor := 0 }
  | _ => none

-- ── Version checks ──────────────────────────────────────────

/-- The minimum supported PostgreSQL version (9.6). -/
def pgVersionMin : PGVersion := { pgvMajor := 9, pgvMinor := 6 }

/-- Whether a version meets the minimum requirement.
    $$\text{isSupported}(v) \iff v \geq 9.6$$ -/
def PGVersion.isSupported (v : PGVersion) : Bool :=
  v.pgvMajor > pgVersionMin.pgvMajor ||
  (v.pgvMajor == pgVersionMin.pgvMajor && v.pgvMinor >= pgVersionMin.pgvMinor)

/-- Whether a version is at least the given major version. -/
def PGVersion.isAtLeastMajor (v : PGVersion) (major : Nat) : Bool :=
  v.pgvMajor >= major

/-- Whether a version is at least the given major.minor version. -/
def PGVersion.isAtLeast (v : PGVersion) (major minor : Nat) : Bool :=
  v.pgvMajor > major ||
  (v.pgvMajor == major && v.pgvMinor >= minor)

end PostgREST.Config
