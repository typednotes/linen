/-
  Linen.System.Git.Remote — branch names and repository URLs, checked

  Two grammars a service must apply before it hands anything to `git` or to
  a hosting API on someone else's behalf:

  - `isBranchName`: what `git check-ref-format --branch` accepts, so a name
    can never be read as an option, a revision expression (`..`, `@{`, `^`)
    or a path outside `refs/heads/`;
  - `Repository.parse`: a repository URL as written for `git clone` —
    `https://github.com/owner/repo(.git)`, `https://gitlab.com/group/…/project(.git)`,
    another `https` host, and `file:///abs/path` only when asked for (tests)
    — reduced to its host, its path segments and one canonical clone URL. No
    userinfo, port, query, fragment or dot segment gets through, so the URL
    cannot smuggle credentials or point `git` at a local path or a different
    host than it shows.

  ## Provenance
  Moved from the sibling services `lode` and `lun` (`Validate.lean`), which
  must agree on both grammars — lode hands lun the repositories and branches
  it writes to — and kept identical copies. One definition makes the
  agreement a fact rather than a convention.
-/

namespace System.Git

-- ── Branch names ────────────────────────────────────────────────────────────

/-- A branch name, per `git check-ref-format --branch`: no control
    characters, space or `~^:?*[\`; no `..`, `@{`, `//`; no component
    starting with `.` or ending in `.lock`; not starting with `-` or `/`, not
    ending with `/` or `.`, not `@`; at most 255 characters. -/
def isBranchName (s : String) : Bool :=
  let forbidden (c : Char) := c.toNat < 0x20 || c.toNat == 0x7f || " ~^:?*[\\".contains c
  !s.isEmpty && s.length ≤ 255 && s != "@" && !s.any forbidden &&
    (s.splitOn "..").length == 1 &&
    (s.splitOn "@{").length == 1 && (s.splitOn "//").length == 1 &&
    !s.startsWith "-" && !s.startsWith "/" && !s.endsWith "/" && !s.endsWith "." &&
    (s.splitOn "/").all fun comp => !comp.isEmpty && !comp.startsWith "." && !comp.endsWith ".lock"

-- ── Repository URLs ─────────────────────────────────────────────────────────

/-- Where a repository is hosted, which decides how it is fetched (and, for
    the two hosting services, through which API). -/
inductive Host where
  /-- `github.com`. -/
  | github
  /-- `gitlab.com`. -/
  | gitlab
  /-- Any other `https` host (lowercased): plain `git`. -/
  | other (host : String)
  /-- A local repository (`file://`), accepted only when asked for. -/
  | local
  deriving DecidableEq, Repr

/-- The hosting service's conventional name (`github`, `gitlab`), for the
    two services that have one. -/
def Host.provider? : Host → Option String
  | .github => some "github"
  | .gitlab => some "gitlab"
  | _ => none

/-- A parsed repository URL. -/
structure Repository where
  /-- Where it is hosted. -/
  host : Host
  /-- The path segments on the host (`owner/repo`, `group/…/project`,
      without `.git`), or the components of a local repository's absolute
      path. -/
  segments : List String
  /-- The canonical URL to clone: `https://{host}/{segments}.git`, or the
      `file://` URL as given. -/
  cloneUrl : String
  deriving DecidableEq, Repr

/-- One path segment: `[A-Za-z0-9._-]+`, not `.` or `..`. -/
def Repository.isSegment (s : String) : Bool :=
  !s.isEmpty && s != "." && s != ".." &&
    s.all fun c => c.isAlphanum || c == '.' || c == '_' || c == '-'

/-- Parse a repository URL, the way it is written for `git clone`:
    `https://github.com/owner/repo(.git)` (exactly two segments),
    `https://gitlab.com/group/…/project(.git)`, or another `https` host; a
    trailing `/` and `.git` are dropped. `file:///abs/path` only when
    `allowLocal`. At most 1024 characters. -/
def Repository.parse (url : String) (allowLocal : Bool := false) : Except String Repository := do
  if url.length > 1024 then throw "the repository URL is too long"
  let strip (s : String) : String :=
    let s := if s.endsWith "/" then (s.dropEnd 1).toString else s
    if s.endsWith ".git" then (s.dropEnd 4).toString else s
  if url.startsWith "file://" then
    unless allowLocal do throw "file:// repositories are only accepted in local mode"
    let path := (url.drop 7).toString
    let segs := (path.splitOn "/").drop 1
    unless path.startsWith "/" && segs.all isSegment do
      throw "a file:// URL must name an absolute path of plain components"
    return { host := .local, segments := segs, cloneUrl := url }
  unless url.startsWith "https://" do throw "the repository URL must be https://"
  let rest := strip (url.drop 8).toString
  match rest.splitOn "/" with
  | [] | [_] => throw "the repository URL names no repository"
  | hostName :: segs =>
    unless !hostName.isEmpty && hostName.all (fun c => c.isAlphanum || c == '.' || c == '-') do
      throw "the repository host must be a plain DNS name (no userinfo or port)"
    unless segs.all isSegment do
      throw "the repository path must be plain components (no query, fragment or dot segments)"
    let host : Host := match hostName.toLower with
      | "github.com" => .github
      | "gitlab.com" => .gitlab
      | h => .other h
    if host == .github && segs.length != 2 then
      throw "a GitHub repository URL is https://github.com/OWNER/REPO"
    let cloneUrl := s!"https://{hostName.toLower}/{"/".intercalate segs}.git"
    return { host, segments := segs, cloneUrl }

end System.Git
