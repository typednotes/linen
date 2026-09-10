#!/usr/bin/env bash
#
# Validate a release tag and print its release notes.
#
#   ci/check-release.sh v0.16.0
#
# Checks two things that are easy to get wrong by hand and expensive to get
# wrong in public:
#
#   1. The tag matches `version` in `lakefile.lean`. These are bumped
#      separately, so they drift; a release whose tag disagrees with the
#      version the library reports about itself is worse than no release.
#   2. `CHANGELOG.md` has a non-empty section for that version. A release with
#      empty notes tells a reader nothing and cannot be fixed retroactively
#      without confusing anyone who already read it.
#
# On success the notes go to stdout and diagnostics to stderr, so the workflow
# can capture one without the other. On failure it exits non-zero and says
# which check failed and how to fix it.
#
# Runnable locally before tagging, which is the point of it being a script
# rather than inline YAML.

set -euo pipefail

tag="${1:-}"
if [[ -z "$tag" ]]; then
  echo "usage: $0 <tag>   (e.g. $0 v0.16.0)" >&2
  exit 2
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# ── The tag ────────────────────────────────────────────────────────────────

if [[ ! "$tag" =~ ^v[0-9]+\.[0-9]+\.[0-9]+(-[0-9A-Za-z.]+)?$ ]]; then
  echo "error: '$tag' is not a version tag." >&2
  echo "       Expected vMAJOR.MINOR.PATCH, optionally with a prerelease" >&2
  echo "       suffix — v0.16.0, or v0.17.0-rc1." >&2
  exit 1
fi
version="${tag#v}"

# ── Check 1: the tag matches the lakefile ──────────────────────────────────

declared="$(grep -oE 'version := v!"[^"]+"' lakefile.lean | head -1 \
            | sed -E 's/.*v!"([^"]+)".*/\1/')"

if [[ -z "$declared" ]]; then
  echo "error: could not find 'version := v!\"...\"' in lakefile.lean." >&2
  exit 1
fi

if [[ "$declared" != "$version" ]]; then
  echo "error: tag and lakefile disagree about the version." >&2
  echo "         tag $tag  =>  $version" >&2
  echo "    lakefile.lean  =>  $declared" >&2
  echo "" >&2
  echo "  Bump 'version := v!\"$version\"' in lakefile.lean, or tag" >&2
  echo "  v$declared instead. Whichever is wrong, fix it before" >&2
  echo "  publishing: the library reports the lakefile's version to its" >&2
  echo "  consumers, so a mismatch is a lie that outlives the release." >&2
  exit 1
fi

# ── Check 2: the CHANGELOG has notes for it ────────────────────────────────

# Anchored with `index` rather than a regex so the version's dots need no
# escaping, and matched on `## [version]` alone so it works whether the
# heading separates the date with a hyphen or an em dash.
notes="$(awk -v ver="$version" '
  index($0, "## [" ver "]") == 1 { inside = 1; next }
  inside && index($0, "## [") == 1 { exit }
  inside { print }
' CHANGELOG.md)"

# Trim leading and trailing blank lines.
notes="$(printf '%s\n' "$notes" | sed -e '/./,$!d' | sed -e :a -e '/^\n*$/{$d;N;};/\n$/ba')"

if [[ -z "$notes" ]]; then
  echo "error: CHANGELOG.md has no notes for $version." >&2
  echo "" >&2
  echo "  Add a section headed '## [$version] - $(date -u +%Y-%m-%d)'," >&2
  echo "  above the previous release and below [Unreleased]." >&2
  exit 1
fi

# ── Report ─────────────────────────────────────────────────────────────────

echo "ok: $tag matches lakefile.lean, and CHANGELOG.md has notes for it." >&2
if [[ "$version" == *-* ]]; then
  echo "note: '$version' has a prerelease suffix; publishing as a prerelease." >&2
fi

printf '%s\n' "$notes"
