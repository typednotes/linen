#!/usr/bin/env bash
# Shared Typednotes release gate with a Linen-specific manual capacity exception.
# Usage: bash ci/require-main-ci.sh <CI workflow filename> <version tag>
# Trust boundary: GitHub's Actions API and the fetched Git refs. A PR result,
# manual CI, or another commit's success cannot authorize publication.
set -euo pipefail

fail() { printf '::error::%s\n' "$*" >&2; exit 1; }
[[ $# == 2 ]] || fail 'Expected a CI workflow filename and a version tag'
workflow=$1
tag=$2
wait_seconds=${CI_WAIT_SECONDS:-7200}
poll_seconds=${CI_POLL_SECONDS:-15}
[[ "$wait_seconds" =~ ^(0|[1-9][0-9]{0,4})$ && "$wait_seconds" -le 7200 ]] || fail 'CI_WAIT_SECONDS must be 0..7200'
[[ "$poll_seconds" =~ ^[1-9][0-9]?$ && "$poll_seconds" -le 60 ]] || fail 'CI_POLL_SECONDS must be 1..60'
[[ "$workflow" =~ ^[A-Za-z0-9_-]+\.ya?ml$ ]] || fail 'Invalid CI workflow filename'
[[ "$tag" =~ ^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(-[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?(\+[0-9A-Za-z]+([.-][0-9A-Za-z]+)*)?$ ]] \
  || fail "Expected a version tag (vX.Y.Z, optionally prerelease/build): $tag"
[[ "${GITHUB_REPOSITORY:-}" =~ ^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$ ]] \
  || fail 'GITHUB_REPOSITORY must identify the publishing repository'
[[ -n "${GH_TOKEN:-}" ]] || fail 'GH_TOKEN with actions:read is required'
for tool in git gh jq; do
  command -v "$tool" >/dev/null 2>&1 || fail "Required tool is missing: $tool"
done
reason=${RUNNER_CAPACITY_REASON:-}
if [[ -n "$reason" ]]; then
  [[ "$workflow" == lean_action_ci.yml && "${GITHUB_EVENT_NAME:-}" == workflow_dispatch ]] \
    || fail 'The runner capacity exception requires manual Linen release dispatch'
  [[ ${#reason} -le 300 && "$reason" =~ [^[:space:]] && ! "$reason" =~ [[:cntrl:]] ]] \
    || fail 'The runner capacity reason must be a nonblank printable line of at most 300 characters'
fi

# Use the actual checkout, including workflow_dispatch's selected tag, never
# GITHUB_SHA (which can name the default branch for a manual dispatch).
sha=$(git rev-parse --verify 'HEAD^{commit}')
tag_sha=$(git rev-parse --verify "refs/tags/$tag^{commit}") \
  || fail "Version tag does not exist: $tag"
[[ "$sha" == "$tag_sha" ]] || fail "Checkout HEAD does not match $tag"
git show-ref --verify --quiet refs/remotes/origin/main \
  || fail 'origin/main is missing; checkout must use fetch-depth: 0'
git merge-base --is-ancestor "$sha" refs/remotes/origin/main \
  || fail "Tag $tag is not reachable from origin/main"

# Do not filter by success: a newer pending/failed attempt must block an older
# green attempt. The workflow endpoint also binds the evidence to its filename.
endpoint="repos/$GITHUB_REPOSITORY/actions/workflows/$workflow/runs?event=push&branch=main&head_sha=$sha&per_page=1"
deadline=$((SECONDS + wait_seconds))
result='successful main CI'
while true; do
  runs=$(gh api --method GET "$endpoint") || fail 'Unable to read main CI runs'
  jq -e '.workflow_runs | type == "array" and length <= 1' <<< "$runs" >/dev/null \
    || fail 'Malformed main CI evidence'
  state=$(jq -r --arg sha "$sha" '
    if (.workflow_runs | length) == 0 then "missing"
    else .workflow_runs[0] |
      if .event != "push" or .head_branch != "main" or .head_sha != $sha then "invalid"
      elif .status == "completed" then (if .conclusion == "success" then "success" else "failed" end)
      elif (.status == "queued" or .status == "in_progress" or .status == "waiting" or .status == "pending" or .status == "requested") and .conclusion == null then "pending"
      else "invalid" end
    end' <<< "$runs") || fail 'Malformed main CI evidence'
  case "$state" in
    success) break ;;
    failed)
      [[ -n "$reason" ]] \
        || fail "Latest push-to-main run of $workflow for $sha did not succeed; rerun CI and then this publisher."
      evidence=$(jq -cer '.workflow_runs[0] | select(.conclusion == "failure") |
        [.id, .run_attempt] | select(all(.[]; type == "number" and . >= 1 and floor == .))' <<< "$runs") \
        || fail 'Capacity exception requires a completed failed CI attempt with a run ID'
      waived=$(bash "$(dirname "$0")/verify-runner-capacity.sh" "$sha" "$evidence") \
        || fail 'Cannot waive CI: not all unsuccessful jobs are verified hosted runner allocation failures'
      latest=$(gh api --method GET "$endpoint") || fail 'Unable to recheck latest main CI'
      jq -e --argjson attempt "$evidence" --arg sha "$sha" '
        .workflow_runs | type == "array" and length == 1 and
        (.[0] | [.id, .run_attempt] == $attempt and .event == "push" and
         .head_branch == "main" and .head_sha == $sha and
         .status == "completed" and .conclusion == "failure")' <<< "$latest" >/dev/null \
        || fail 'Latest main CI changed during the capacity check; retry this publisher'
      printf '::warning::Waiving jobs for which GitHub did not assign a hosted runner.\n'
      printf 'Waived jobs:\n%s\n' "$waived"
      printf 'Capacity exception reason: %s\n' "$reason"
      result='main CI with a verified hosted runner capacity exception'
      break ;;
    invalid) fail 'Main CI evidence has the wrong identity or status' ;;
  esac
  remaining=$((deadline - SECONDS))
  (( remaining > 0 )) || fail "Timed out waiting for push-to-main $workflow for $sha. Push main or rerun CI, then retry this publisher."
  printf 'Waiting for exact-commit main CI (%s; %ss remaining)\n' "$state" "$remaining"
  delay=$poll_seconds
  (( delay <= remaining )) || delay=$remaining
  sleep "$delay"
done
printf 'Verified %s at %s against %s (%s)\n' "$tag" "$sha" "$result" "$workflow"
if [[ -n "${GITHUB_OUTPUT:-}" ]]; then
  printf 'sha=%s\ntag=%s\n' "$sha" "$tag" >> "$GITHUB_OUTPUT"
fi
