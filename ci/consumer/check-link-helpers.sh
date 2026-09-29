#!/usr/bin/env bash
#
# Check that a consumer's copy of linen's link-flag helpers matches the
# canonical block at the linen revision it pins.
#
#   ci/consumer/check-link-helpers.sh path/to/consumer/lakefile.lean
#
# Run it from the linen checkout the consumer resolves (usually
# `.lake/packages/linen`), so the canonical side is the pinned tag's rather
# than whatever `main` says today. Exit 0 in sync, 1 drifted or missing,
# 2 on a usage error.
#
# Why this exists: Lake gives an executable only its own package's
# `moreLinkArgs`, so every consumer carries a copy of these helpers, and six
# sibling repositories had each drifted from the others — one had dropped
# `pkgAbsoluteLibs` and broken every Linux link it produced.

set -euo pipefail

consumer="${1:-}"
if [[ -z "$consumer" ]]; then
  echo "usage: $0 <consumer lakefile.lean>" >&2
  exit 2
fi
if [[ ! -f "$consumer" ]]; then
  echo "error: $consumer: no such file" >&2
  exit 2
fi

here="$(cd "$(dirname "$0")" && pwd)"
canonical="$here/link-helpers.lean"

extract() {
  # Everything between the markers, inclusive. A Lean string literal holding
  # the block (a scaffolder's copy) escapes its quotes; undo that.
  sed -n '/⟪linen-link-helpers:begin⟫/,/⟪linen-link-helpers:end⟫/p' "$1" | sed 's/\\"/"/g'
}

a=$(mktemp); b=$(mktemp)
trap 'rm -f "$a" "$b"' EXIT
extract "$canonical" > "$a"
extract "$consumer" > "$b"

if [[ ! -s "$a" ]]; then
  echo "error: no marked block in $canonical — this linen checkout is broken" >&2
  exit 1
fi
if [[ ! -s "$b" ]]; then
  echo "error: $consumer has no ⟪linen-link-helpers:begin⟫ … ⟪linen-link-helpers:end⟫ block." >&2
  echo "  Paste $canonical into it, markers included." >&2
  exit 1
fi

if diff -u "$a" "$b" > /dev/null; then
  echo "linen link helpers: in sync ($(wc -l < "$a" | tr -d ' ') lines)"
else
  echo "error: $consumer's linen link helpers differ from the canonical block." >&2
  echo "  Canonical: $canonical (at this linen checkout's revision)." >&2
  echo >&2
  diff -u "$a" "$b" >&2 || true
  exit 1
fi
