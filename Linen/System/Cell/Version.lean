/- Semantic release identities and selection rules for precompiled Lean cells. -/
import Lean.Data.Json

namespace System.Cell

open Lean (FromJson ToJson)

/-- Major, minor, and patch numbers shared by stable and prerelease versions. -/
structure VersionCore where
  /-- Incompatible public contract changes. -/
  major : Nat
  /-- Backward-compatible additions. -/
  minor : Nat
  /-- Backward-compatible fixes. -/
  patch : Nat
  deriving DecidableEq, Ord, Repr

/-- Numeric prerelease identifiers sort before identifiers with letters or hyphens. -/
inductive PrereleaseIdentifier where
  | numeric (value : Nat)
  | text (value : String)
  deriving DecidableEq, Ord, Repr

/-- A canonical Semantic Versioning 2.0.0 release version. -/
structure Version where
  private mk ::
  /-- The three numeric version components. -/
  core : VersionCore
  /-- Empty for a stable release. -/
  prerelease : List PrereleaseIdentifier
  /-- Metadata retained in the exact release identity and ignored in precedence. -/
  buildMetadata : List String
  deriving DecidableEq, Repr

private def identifierChar (c : Char) : Bool :=
  c.isDigit || ('A' ≤ c && c ≤ 'Z') || ('a' ≤ c && c ≤ 'z') || c == '-'

private def digits (s : String) : Bool := !s.isEmpty && s.all Char.isDigit

private def number (s : String) : Except String Nat := do
  unless digits s && (s == "0" || !s.startsWith "0") do
    throw "expected a nonnegative decimal version component without leading zeroes"
  let some value := s.toNat? | throw "invalid numeric version component"
  return value

private def identifiers (suffix : String) : Except String (List String) := do
  let parts := suffix.splitOn "."
  unless parts.all (fun s => !s.isEmpty && s.all identifierChar) do
    throw "expected nonempty dot-separated version identifiers using ASCII letters, digits, or hyphens"
  return parts

/-- Parse a SemVer 2.0.0 version such as `1.2.0-rc.1+build.007`. -/
def Version.parse (s : String) : Except String Version := do
  let (version, buildMetadata) ← match s.splitOn "+" with
    | [core] => pure (core, [])
    | [core, build] => pure (core, ← identifiers build)
    | _ => throw "a semantic version has at most one build metadata separator"
  let core :: suffix := version.splitOn "-" | throw "expected a semantic version"
  let [major, minor, patch] := core.splitOn "." | throw "expected major.minor.patch"
  let prerelease ← match suffix with
    | [] => pure []
    | parts => (← identifiers ("-".intercalate parts)).mapM fun part =>
        if digits part then return .numeric (← number part) else return .text part
  return ⟨⟨← number major, ← number minor, ← number patch⟩, prerelease, buildMetadata⟩

/-- Construct a stable release version from its numeric components. -/
def Version.stable (core : VersionCore) : Version := ⟨core, [], []⟩

instance : ToString Version := ⟨fun v =>
  let core := s!"{v.core.major}.{v.core.minor}.{v.core.patch}"
  let pre := v.prerelease.map fun
    | .numeric n => toString n
    | .text part => part
  core ++ (if pre.isEmpty then "" else "-" ++ ".".intercalate pre) ++
    (if v.buildMetadata.isEmpty then "" else "+" ++ ".".intercalate v.buildMetadata)⟩

instance : ToJson Version := ⟨fun v => .str (toString v)⟩
instance : FromJson Version := ⟨fun j => j.getStr? >>= Version.parse⟩

/-- Compare by SemVer precedence, which ignores build metadata. Use equality
    when checking whether two versions designate the exact same release. -/
def Version.precedence (a b : Version) : Ordering :=
  match compare a.core b.core with
  | .eq =>
    match a.prerelease, b.prerelease with
    | [], [] => .eq
    | [], _ => .gt
    | _, [] => .lt
    | left, right => compare left right
  | order => order

/-- A stable cell name within one project's release catalog. -/
structure ReleaseName where
  private mk ::
  /-- The lowercase dotted name, such as `math.addition`. -/
  value : String
  deriving DecidableEq, Repr

/-- Validate a name of up to 128 ASCII characters. Each dot-separated
    component starts with a lowercase letter and uses letters, digits, and hyphens. -/
def ReleaseName.parse (s : String) : Except String ReleaseName := do
  let lower (c : Char) := 'a' ≤ c && c ≤ 'z'
  unless s.utf8ByteSize ≤ 128 && (s.splitOn ".").all (fun part =>
      lower (part.toList.headD ' ') && part.all (fun c => lower c || c.isDigit || c == '-')) do
    throw "expected a lowercase dotted cell release name of at most 128 characters"
  return ⟨s⟩

instance : ToString ReleaseName := ⟨ReleaseName.value⟩
instance : ToJson ReleaseName := ⟨fun n => .str n.value⟩
instance : FromJson ReleaseName := ⟨fun j => j.getStr? >>= ReleaseName.parse⟩

/-- The human-facing identity signed into each native variant of a release. -/
structure Release where
  /-- Stable name within the project. -/
  name : ReleaseName
  /-- Exact version, including any prerelease and build metadata. -/
  version : Version
  deriving DecidableEq, Repr, ToJson, FromJson

instance : ToString Release := ⟨fun r => s!"{r.name}@{r.version}"⟩

/-- Select an exact version, or the highest stable compatible release. -/
inductive VersionRequirement where
  /-- Match the full version including prerelease and build metadata. -/
  | exact (version : Version)
  /-- Stable caret range. The first nonzero component fixes the compatibility boundary. -/
  | compatible (minimum : VersionCore)
  deriving DecidableEq, Repr

/-- Parse an exact version such as `1.2.0-rc.1` or a stable caret range such
    as `^1.2.0`. Caret range lower bounds use only major, minor and patch. -/
def VersionRequirement.parse (text : String) : Except String VersionRequirement := do
  if !text.startsWith "^" then return .exact (← Version.parse text)
  let minimum ← Version.parse (text.drop 1).toString
  unless minimum.prerelease.isEmpty && minimum.buildMetadata.isEmpty do
    throw "a caret range requires a stable major.minor.patch lower bound"
  return .compatible minimum.core

instance : ToString VersionRequirement := ⟨fun
  | .exact version => toString version
  | .compatible minimum => "^" ++ toString (Version.stable minimum)⟩

instance : ToJson VersionRequirement := ⟨fun r => .str (toString r)⟩
instance : FromJson VersionRequirement := ⟨fun j => j.getStr? >>= VersionRequirement.parse⟩

/-- Test exact equality or stable caret compatibility. For example, `^1.2.0`
    stops before `2.0.0`, `^0.2.0` before `0.3.0`, and `^0.0.2` before `0.0.3`. -/
def VersionRequirement.matches (requirement : VersionRequirement) (candidate : Version) : Bool :=
  match requirement with
  | .exact version => candidate == version
  | .compatible minimum =>
    candidate.prerelease.isEmpty && compare candidate.core minimum != .lt &&
      candidate.core.major == minimum.major &&
      (minimum.major != 0 || candidate.core.minor == minimum.minor) &&
      (minimum.major != 0 || minimum.minor != 0 || candidate.core.patch == minimum.patch)

end System.Cell
