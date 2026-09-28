/-
  Tests for `Linen.System.Git.Remote`: what `git check-ref-format --branch`
  refuses, and the repository URLs that parse (and to what) or do not.
-/
import Linen.System.Git.Remote

open System.Git

namespace Tests.System.Git.Remote

/-! ### Branch names -/

#guard isBranchName "main" && isBranchName "feature/x-1" && isBranchName "release-1.2"
#guard isBranchName "a" && isBranchName "é/ü"
#guard !isBranchName ""
#guard !isBranchName "-rf"                          -- would be a flag to git
#guard !isBranchName "a..b" && !isBranchName "a b" && !isBranchName "a~1" && !isBranchName "a:b"
#guard !isBranchName "a^" && !isBranchName "a?" && !isBranchName "a*" && !isBranchName "a[" && !isBranchName "a\\b"
#guard !isBranchName "a//b" && !isBranchName "/a" && !isBranchName "a/" && !isBranchName "a."
#guard !isBranchName ".hidden" && !isBranchName "a/.b" && !isBranchName "x.lock" && !isBranchName "a/x.lock/b"
#guard !isBranchName "@" && !isBranchName "a@{1}"
#guard isBranchName "a@b"                            -- `@` alone and `@{` only
#guard !isBranchName "a\tb" && !isBranchName "a\x7fb"
#guard isBranchName (String.ofList (List.replicate 255 'a')) && !isBranchName (String.ofList (List.replicate 256 'a'))

/-! ### Repository URLs -/

#guard (Repository.parse "https://github.com/owner/repo").toOption ==
  some { host := .github, segments := ["owner", "repo"], cloneUrl := "https://github.com/owner/repo.git" }
#guard (Repository.parse "https://github.com/owner/repo.git/").toOption.map (·.cloneUrl) ==
  some "https://github.com/owner/repo.git"
#guard (Repository.parse "https://GitHub.com/a/b").toOption.map (·.host) == some .github
#guard (Repository.parse "https://GitLab.com/g/sub/p").toOption.map (·.host) == some .gitlab
#guard (Repository.parse "https://gitlab.com/g/sub/p").toOption.map (·.segments) == some ["g", "sub", "p"]
#guard (Repository.parse "https://Git.Example.org/x").toOption ==
  some { host := .other "git.example.org", segments := ["x"], cloneUrl := "https://git.example.org/x.git" }
#guard (Repository.parse "http://github.com/o/r").toOption.isNone                -- not https
#guard (Repository.parse "https://user:pw@github.com/o/r").toOption.isNone       -- userinfo
#guard (Repository.parse "https://github.com:22/o/r").toOption.isNone            -- port
#guard (Repository.parse "https://github.com/o/r?x=1").toOption.isNone           -- query
#guard (Repository.parse "https://github.com/o/r#main").toOption.isNone          -- fragment
#guard (Repository.parse "https://github.com/o/r/tree/main").toOption.isNone     -- not OWNER/REPO
#guard (Repository.parse "https://github.com/o").toOption.isNone
#guard (Repository.parse "https://example.org/a/../b").toOption.isNone           -- dot segment
#guard (Repository.parse "https://example.org/a//b").toOption.isNone             -- empty segment
#guard (Repository.parse "https://github.com").toOption.isNone
#guard (Repository.parse "ssh://git@github.com/o/r").toOption.isNone
#guard (Repository.parse ("https://example.org/" ++ String.ofList (List.replicate 1100 'a'))).toOption.isNone
#guard (Repository.parse "file:///tmp/x").toOption.isNone                        -- local mode only
#guard (Repository.parse "file:///tmp/x" (allowLocal := true)).toOption ==
  some { host := .local, segments := ["tmp", "x"], cloneUrl := "file:///tmp/x" }
#guard (Repository.parse "file://relative/x" (allowLocal := true)).toOption.isNone
#guard (Repository.parse "file:///tmp/../etc" (allowLocal := true)).toOption.isNone

#guard Host.github.provider? == some "github" && Host.gitlab.provider? == some "gitlab"
#guard (Host.other "x").provider? == none && Host.local.provider? == none

#guard Repository.isSegment "a.b_c-1" && !Repository.isSegment "." && !Repository.isSegment ".."
#guard !Repository.isSegment "" && !Repository.isSegment "a b" && !Repository.isSegment "a%20"

end Tests.System.Git.Remote
