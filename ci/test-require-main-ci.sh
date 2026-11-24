#!/usr/bin/env bash
# Offline behavioral checks for the shared release gate. Optional arguments are
# other copies to exercise; no GitHub request, push or production checkout edit.
# Bounded-wait and Linen-specific hosted runner capacity tests.
set -euo pipefail
for tool in bash git gh jq mktemp; do
  command -v "$tool" >/dev/null 2>&1 \
    || { printf 'error: required test tool is missing: %s\n' "$tool" >&2; exit 2; }
done
root=$(cd "$(dirname "$0")/.." && pwd)
if [[ $# == 0 ]]; then set -- "$root/ci/require-main-ci.sh"; fi
scratch=$(mktemp -d "${TMPDIR:-/tmp}/main-ci-gate.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin" "$scratch/repo"

# The mock accepts only the documented read-only evidence queries.
cat > "$scratch/bin/gh" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
if [[ $# == 6 && "$1 $2 $3 $4 $5" == 'api --method GET --paginate --slurp' &&
      "$6" == 'repos/typednotes/fixture/actions/runs/123/jobs?filter=all&per_page=100' ]]; then
  printf '%s\n' "$API_HISTORY"
  exit
fi
[[ $# == 4 && "$1" == api && "$2" == --method && "$3" == GET ]]
case "$4" in
  "$EXPECTED_ENDPOINT") ;;
  'repos/typednotes/fixture/actions/runs/123/jobs?filter=latest&per_page=100')
    printf '%s\n' "$API_JOBS"; exit ;;
  'repos/typednotes/fixture/check-runs/456/annotations?per_page=100')
    printf '%s\n' "$API_ANNOTATIONS"; exit ;;
  'repos/typednotes/fixture/check-runs/457/annotations?per_page=100')
    printf '%s\n' "$API_SECOND_ANNOTATIONS"; exit ;;
  *) exit 1 ;;
esac
printf '%s\n' "$4" >> "$MOCK_CALLS"
[[ "${API_FAIL:-0}" == 0 ]] || exit 1
[[ "${API_NEXT_FAIL:-0}" != 1 || $(wc -l < "$MOCK_CALLS") -le 1 ]] || exit 1
if [[ -n "${API_NEXT_RESPONSE:-}" && $(wc -l < "$MOCK_CALLS") -gt 1 ]]; then
  printf '%s\n' "$API_NEXT_RESPONSE"
else
  printf '%s\n' "$API_RESPONSE"
fi
MOCK
cat > "$scratch/bin/sleep" <<'MOCK'
#!/usr/bin/env bash
# Offline transition tests need no actual delay.
if [[ "${CI_TEST_REAL_SLEEP:-0}" == 1 ]]; then exec /bin/sleep "$@"; fi
exit 0
MOCK
chmod +x "$scratch/bin/sleep"
chmod +x "$scratch/bin/gh"
export PATH="$scratch/bin:$PATH" GH_TOKEN=offline-test GITHUB_REPOSITORY=typednotes/fixture
export GIT_AUTHOR_NAME='CI fixture' GIT_AUTHOR_EMAIL=ci@example.invalid
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME" GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
export MOCK_CALLS="$scratch/calls"
export CI_WAIT_SECONDS=0 CI_POLL_SECONDS=1
unset API_NEXT_RESPONSE API_NEXT_FAIL CI_TEST_REAL_SLEEP RUNNER_CAPACITY_REASON GITHUB_EVENT_NAME
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE
cd "$scratch/repo"
git init -q -b main
git -c core.hooksPath=/dev/null -c commit.gpgSign=false commit -q --allow-empty -m base
base=$(git rev-parse HEAD)
git -c core.hooksPath=/dev/null -c tag.gpgSign=false tag -a v1.2.3 -m fixture
git -c tag.gpgSign=false tag v1.2.3-rc.1
git -c core.hooksPath=/dev/null -c commit.gpgSign=false commit -q --allow-empty -m tip
tip=$(git rev-parse HEAD)
git update-ref refs/remotes/origin/main "$tip"
git checkout -q --detach "$base"
# Simulates a manual dispatch from newer main releasing an older tested tag.
export GITHUB_SHA="$tip"

good=$(jq -nc --arg sha "$base" '{workflow_runs:[{event:"push",head_branch:"main",head_sha:$sha,status:"completed",conclusion:"success"}]}')
checks=0
expect() {
  local expected=$1 label=$2 workflow=${3:-ci.yml} tag=${4:-v1.2.3} rc=0
  local checkout_sha
  checkout_sha=$(git rev-parse HEAD)
  export EXPECTED_ENDPOINT="repos/$GITHUB_REPOSITORY/actions/workflows/$workflow/runs?event=push&branch=main&head_sha=$checkout_sha&per_page=1"
  export GITHUB_OUTPUT="$scratch/output"
  rm -f "$GITHUB_OUTPUT" "$MOCK_CALLS"
  bash "$helper" "$workflow" "$tag" > "$scratch/log" 2>&1 || rc=$?
  if [[ "$expected" == pass ]]; then
    [[ "$rc" == 0 ]] || { cat "$scratch/log" >&2; printf 'FAILED: %s\n' "$label" >&2; exit 1; }
    [[ -s "$MOCK_CALLS" ]] || { printf 'Gate passed without API evidence\n' >&2; exit 1; }
    [[ "$(cat "$GITHUB_OUTPUT")" == "$(printf 'sha=%s\ntag=%s' "$base" "$tag")" ]]
  else
    [[ "$rc" != 0 ]] || { printf 'FAILED: gate accepted %s\n' "$label" >&2; exit 1; }
    [[ ! -s "$GITHUB_OUTPUT" ]] || { printf 'Denied gate emitted publish outputs\n' >&2; exit 1; }
  fi
  checks=$((checks + 1))
}

for helper in "$@"; do
  [[ "$helper" == /* ]] || helper="$root/$helper"
  export API_RESPONSE="$good" API_FAIL=0
  expect pass 'annotated tag; actual checkout differs from dispatch GITHUB_SHA'
  expect pass 'Lean workflow identity' lean_action_ci.yml
  expect pass 'lightweight prerelease tag' ci.yml v1.2.3-rc.1
  for change in '.workflow_runs = []' '.workflow_runs[0].status = "queued" | .workflow_runs[0].conclusion = null' '.workflow_runs[0].status = "in_progress" | .workflow_runs[0].conclusion = null' '.workflow_runs[0].status = "waiting" | .workflow_runs[0].conclusion = null' '.workflow_runs[0].status = "pending" | .workflow_runs[0].conclusion = null' '.workflow_runs[0].status = "requested" | .workflow_runs[0].conclusion = null'; do
    export API_RESPONSE API_NEXT_RESPONSE="$good" CI_WAIT_SECONDS=5
    API_RESPONSE=$(jq -c "$change" <<< "$good")
    expect pass "wait for $change then exact-commit success"
    [[ $(wc -l < "$MOCK_CALLS") == 2 ]]
  done
  unset API_NEXT_RESPONSE
  pending=$(jq -c '.workflow_runs[0].status = "in_progress" | .workflow_runs[0].conclusion = null' <<< "$good")
  for change in '.workflow_runs[0].conclusion = "failure"' '.workflow_runs[0].conclusion = "cancelled"' '.workflow_runs[0].head_sha = "another-commit"' '.workflow_runs[0].event = "workflow_dispatch"' '.workflow_runs[0].head_branch = "feature"' '.workflow_runs[0].status = "unknown"' '.workflow_runs = [{},{}]'; do
    export API_RESPONSE="$pending" API_NEXT_RESPONSE CI_WAIT_SECONDS=5
    API_NEXT_RESPONSE=$(jq -c "$change" <<< "$good")
    expect fail "pending then $change"
    [[ $(wc -l < "$MOCK_CALLS") == 2 ]]
  done
  export API_RESPONSE="$pending" API_NEXT_RESPONSE='not json'
  expect fail 'pending then malformed API response'
  unset API_NEXT_RESPONSE
  export API_NEXT_FAIL=1
  expect fail 'API access fails while waiting'
  unset API_NEXT_FAIL
  export CI_WAIT_SECONDS=1 CI_TEST_REAL_SLEEP=1
  expect fail 'bounded timeout while valid exact-commit CI remains pending'
  [[ $(wc -l < "$MOCK_CALLS") -ge 2 ]]
  unset CI_TEST_REAL_SLEEP
  export CI_WAIT_SECONDS=0
  for change in \
    '.workflow_runs[0].status = "queued"' \
    '.workflow_runs[0].status = "in_progress"' \
    '.workflow_runs[0].conclusion = "failure"' \
    '.workflow_runs[0].conclusion = "cancelled"' \
    '.workflow_runs[0].conclusion = "skipped"' \
    '.workflow_runs[0].conclusion = null' \
    '.workflow_runs[0].event = "pull_request"' \
    '.workflow_runs[0].event = "workflow_dispatch"' \
    '.workflow_runs[0].head_branch = "feature"' \
    '.workflow_runs[0].head_sha = "another-commit"' \
    'del(.workflow_runs[0].head_sha)' \
    '.workflow_runs = []' \
    'del(.workflow_runs)' \
    '.workflow_runs = {}'; do
    export API_RESPONSE
    API_RESPONSE=$(jq -c "$change" <<< "$good")
    expect fail "$change"
  done
  export API_RESPONSE='not json'
  expect fail 'malformed API response'
  # API per_page=1 exposes only the latest attempt: if that attempt failed,
  # an older successful attempt is never considered by the gate.
  export API_RESPONSE
  API_RESPONSE=$(jq -c '.workflow_runs[0].conclusion = "failure"' <<< "$good")
  expect fail 'latest attempt failed despite an older success'
  if [[ -f "$(dirname "$helper")/verify-runner-capacity.sh" ]]; then
    export API_RESPONSE API_JOBS API_HISTORY API_ANNOTATIONS API_SECOND_ANNOTATIONS
    API_RESPONSE=$(jq -c '.workflow_runs[0] += {conclusion:"failure",id:123,run_attempt:2}' <<< "$good")
    jobs=$(jq -nc --arg sha "$base" '
      ["build (ubuntu-24.04)", "build (ubuntu-24.04-arm)", "build (macos-latest)",
       "Consumer build (ubuntu-24.04)", "Consumer build (ubuntu-24.04-arm)",
       "A host that cannot seal DuckDB fails loudly"] |
      map({name:., head_sha:$sha, run_id:123, run_attempt:1, status:"completed", conclusion:"success"}) |
      . + [{name:"Consumer build (macos-latest)", head_sha:$sha, run_id:123, run_attempt:2,
        status:"completed", conclusion:"cancelled", runner_id:0, runner_name:"", steps:[],
        id:456, check_run_url:"https://api.github.com/repos/typednotes/fixture/check-runs/456"}] |
      {total_count:length, jobs:.}')
    annotations='[
      {"annotation_level":"failure","message":"The job was not acquired by Runner of type hosted even after multiple attempts"},
      {"annotation_level":"notice","message":"Due to capacity constraints, jobs targeting macOS arm64 runners may experience longer queue times."}
    ]'
    history=$(jq -c '[.jobs |= (to_entries | map(.value + {id:(.value.id // (500 + .key))}))]' <<< "$jobs")
    API_JOBS=$jobs API_ANNOTATIONS=$annotations API_HISTORY=$history
    expect fail 'capacity cancellation is blocking without opt-in' lean_action_ci.yml
    export RUNNER_CAPACITY_REASON='Runner shortage after retry' GITHUB_EVENT_NAME=push
    expect fail 'tag push cannot waive capacity' lean_action_ci.yml
    export GITHUB_EVENT_NAME=workflow_dispatch
    expect fail 'capacity exception is specific to Linen CI'
    expect pass 'manual reason plus confirmed shortage and all other jobs passed' lean_action_ci.yml
    grep -Fq 'Capacity exception reason: Runner shortage after retry' "$scratch/log"
    API_HISTORY=$(jq -c '.[0] | .jobs += [.jobs[6] + {id:789,run_attempt:1,conclusion:"failure"}] |
      .total_count += 1 | [.]' <<< "$history")
    expect fail 'runner shortage cannot hide a test failure from a prior attempt' lean_action_ci.yml
    API_HISTORY=$(jq -c '.[0].total_count += 1' <<< "$history")
    expect fail 'truncated history cannot authorize a waiver' lean_action_ci.yml
    API_JOBS=$(jq -c '.jobs[0] += (.jobs[6] |
      del(.name) | .id = 457 | .check_run_url |= sub("456$"; "457"))' <<< "$jobs")
    API_HISTORY=$(jq -c '.[0].jobs[0] += {id:457,runner_id:0,runner_name:"",steps:[],
      run_attempt:2,conclusion:"cancelled"}' <<< "$history")
    API_SECOND_ANNOTATIONS=$(jq -c '.[0:1]' <<< "$annotations")
    expect pass 'verified Linux and macOS runner cancellations are both waivable' lean_action_ci.yml
    grep -Fxq 'build (ubuntu-24.04)' "$scratch/log"
    grep -Fxq 'Consumer build (macos-latest)' "$scratch/log"
    API_SECOND_ANNOTATIONS='[]'
    expect fail 'every waived cancellation needs independent API evidence' lean_action_ci.yml
    for change in \
      '.jobs[6].conclusion = "failure"' \
      '.jobs[6].runner_id = 42' \
      '.jobs[6].steps = [{name:"Set up job"}]' \
      '.jobs[6].run_attempt = 1' \
      '.jobs[0].conclusion = "failure"' \
      '.jobs[2].conclusion = "cancelled"' \
      '.jobs[0].head_sha = "another-commit"' \
      '.jobs[0].run_id = 124' \
      '.jobs[0].name = .jobs[1].name' \
      'del(.jobs[0])' \
      '.total_count = 8'; do
      API_JOBS=$(jq -c "$change" <<< "$jobs")
      expect fail "capacity exception refuses $change" lean_action_ci.yml
    done
    API_JOBS=$jobs API_HISTORY=$history
    for change in '[]' '.[1:]' '. + [{annotation_level:"failure",message:"Job cancelled"}]'; do
      API_ANNOTATIONS=$(jq -c "$change" <<< "$annotations")
      expect fail "capacity exception refuses annotations $change" lean_action_ci.yml
    done
    API_ANNOTATIONS=$annotations
    export API_NEXT_RESPONSE="$pending"
    expect fail 'capacity exception refuses a superseding CI run' lean_action_ci.yml
    unset API_NEXT_RESPONSE
    export RUNNER_CAPACITY_REASON='   '
    expect fail 'capacity exception rejects a blank reason' lean_action_ci.yml
    unset RUNNER_CAPACITY_REASON GITHUB_EVENT_NAME
  fi
  export API_RESPONSE="$good" API_FAIL=1
  expect fail 'API access refused/network failure'
  export API_FAIL=0
  export CI_WAIT_SECONDS=invalid
  expect fail 'invalid wait timeout'
  export CI_WAIT_SECONDS=7201
  expect fail 'wait timeout exceeds the bound'
  export CI_WAIT_SECONDS=0 CI_POLL_SECONDS=61
  expect fail 'poll interval exceeds the bound'
  export CI_WAIT_SECONDS=0 CI_POLL_SECONDS=0
  expect fail 'invalid poll interval'
  export CI_POLL_SECONDS=1
  expect fail 'non-version manual input' ci.yml main
  expect fail 'missing version tag' ci.yml v9.9.9
  expect fail 'workflow path injection' '../ci.yml'
  expect fail 'version tag injection' ci.yml 'v1.2.3;exit 0'
  expect fail 'leading-zero version' ci.yml v01.2.3
  export GH_TOKEN=''
  expect fail 'missing Actions token'
  export GH_TOKEN=offline-test
  export GITHUB_REPOSITORY='invalid/repo/endpoint'
  expect fail 'invalid repository identity'
  export GITHUB_REPOSITORY=typednotes/fixture

  git checkout -q --detach "$tip"
  API_RESPONSE=$(jq -c --arg sha "$tip" '.workflow_runs[0].head_sha = $sha' <<< "$good")
  expect fail 'checkout is main, not selected tag'
  git checkout -q --detach "$base"
  export API_RESPONSE="$good"
  git update-ref -d refs/remotes/origin/main
  expect fail 'missing main ref/shallow checkout'
  git update-ref refs/remotes/origin/main "$tip"

  git checkout -q --orphan fixture-unrelated
  git -c core.hooksPath=/dev/null -c commit.gpgSign=false commit -q --allow-empty -m unrelated
  git -c tag.gpgSign=false tag v4.5.6
  API_RESPONSE=$(jq -c --arg sha "$(git rev-parse HEAD)" '.workflow_runs[0].head_sha = $sha' <<< "$good")
  expect fail 'tag is outside main ancestry' ci.yml v4.5.6
  git checkout -q --detach "$base"
  git tag -d v4.5.6 >/dev/null
  git branch -D fixture-unrelated >/dev/null
  printf 'ok: behavioral release gate checks: %s\n' "$helper"
done
printf 'ok: %s offline gate cases\n' "$checks"
