/-
  Tests for `Linen.System.GitFn.Descriptor`: commits, descriptors, their
  validation and JSON.
-/
import Linen.System.GitFn.Descriptor

open System.GitFn Lean

namespace Tests.System.GitFn.Descriptor

deriving instance BEq for Except

def sha1 : String := "0123456789abcdef0123456789abcdef01234567"

#guard CommitSha.isValid sha1
#guard CommitSha.isValid (String.ofList (List.replicate 64 'a'))
#guard !CommitSha.isValid "0123456789ABCDEF0123456789abcdef01234567"
#guard !CommitSha.isValid "main" && !CommitSha.isValid "HEAD"
#guard !CommitSha.isValid (sha1.drop 1).toString

def sha : CommitSha := (CommitSha.ofString? sha1).get!
def fn : GitFn :=
  { repo := "https://github.com/acme/geo", commit := sha, project := "lean", name := `Geo.shift, type := "Geo.Point → Nat → Geo.Point" }

#guard fn.validate == .ok ()
#guard ({ fn with repo := "--upload-pack=evil" }).validate ==
  .error "repository `--upload-pack=evil` must not start with `-`"
#guard ({ fn with project := "/etc" }).validate == .error "project path `/etc` must be relative"
#guard ({ fn with project := "a/../../b" }).validate == .error "project path `a/../../b` must not contain `..`"
#guard ({ fn with project := "a//b" }).validate == .error "project path `a//b` has an empty component"
#guard ({ fn with name := .anonymous }).validate == .error "the function name is empty"
#guard ({ fn with type := "  " }).validate == .error "the declared type is empty"
#guard projectComponents "." == .ok [] && projectComponents "a/b" == .ok ["a", "b"]

-- JSON round trip, and rejection of what does not validate.
#guard (fromJson? (toJson fn) : Except String GitFn) == .ok fn
-- (Lean core's JSON objects keep their keys sorted.)
#guard (toJson fn).compress ==
  "{\"commit\":\"0123456789abcdef0123456789abcdef01234567\",\"name\":\"Geo.shift\",\"project\":\"lean\",\"repo\":\"https://github.com/acme/geo\",\"type\":\"Geo.Point → Nat → Geo.Point\"}"
#guard (fromJson? (Json.mkObj [("repo", "r"), ("commit", "main"), ("name", "f"), ("type", "Nat")]) :
  Except String GitFn) == .error "`main` is not a full commit SHA"
#guard (fromJson? (Json.mkObj [("repo", "-x"), ("commit", sha1), ("name", "f"), ("type", "Nat")]) :
  Except String GitFn) == .error "repository `-x` must not start with `-`"

-- A SHA resolves to itself without asking git.
#eval show IO Unit from do
  unless (← resolve "unused" sha1 (git := "/nonexistent")) == .ok sha do
    throw (IO.userError "a SHA should resolve to itself")

end Tests.System.GitFn.Descriptor
