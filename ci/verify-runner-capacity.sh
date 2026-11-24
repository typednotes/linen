#!/usr/bin/env bash
# Validate hosted runner allocation failures for Linen's exact-commit gate.
# Called by require-main-ci.sh after it verifies the tag, run and manual input.
set -euo pipefail

sha=$1
attempt=$2
run_id=$(jq -er '.[0]' <<< "$attempt")
repo="repos/$GITHUB_REPOSITORY"
jobs=$(gh api --method GET "$repo/actions/runs/$run_id/jobs?filter=latest&per_page=100")
waived=$(jq -cer --arg sha "$sha" --arg repo "$repo" --argjson attempt "$attempt" '
  def expected:
    ["build (ubuntu-24.04)", "build (ubuntu-24.04-arm)", "build (macos-latest)",
     "Consumer build (ubuntu-24.04)", "Consumer build (ubuntu-24.04-arm)",
     "Consumer build (macos-latest)", "A host that cannot seal DuckDB fails loudly"];
  select(.total_count == (expected | length) and
    (.jobs | map(.name) | sort) == (expected | sort) and
    any(.jobs[]; .conclusion == "success") and
    any(.jobs[]; .conclusion == "cancelled") and
    all(.jobs[];
      .run_id == $attempt[0] and .head_sha == $sha and .status == "completed" and
      (.run_attempt | type == "number" and . >= 1 and . <= $attempt[1] and floor == .) and
      (if .conclusion != "success" then
         .run_attempt == $attempt[1] and .conclusion == "cancelled" and
         .runner_id == 0 and .runner_name == "" and .steps == [] and
         (.id | type == "number" and . >= 1 and floor == .) and
         .check_run_url == ("https://api.github.com/" + $repo + "/check-runs/" + (.id | tostring))
       else true end))) |
  [.jobs[] | select(.conclusion == "cancelled") | {id, name}] |
  select((map(.id) | unique | length) == length)
' <<< "$jobs")

# A rerun that never got a runner must not erase an earlier observed failure
# of the same job. Check the entire run history, rejecting truncated evidence.
attempt_no=$(jq -er '.[1]' <<< "$attempt")
if (( attempt_no > 1 )); then
  history=$(gh api --method GET --paginate --slurp "$repo/actions/runs/$run_id/jobs?filter=all&per_page=100")
  jq -e --arg sha "$sha" --argjson attempt "$attempt" --argjson waived "$waived" '
    type == "array" and length > 0 and
    (.[0].total_count as $count |
      all(.[]; .total_count == $count and (.jobs | type == "array")) and
      ([.[].jobs[]] |
        length == $count and (map(.id) | unique | length) == $count and
        all(.[]; .run_id == $attempt[0] and .head_sha == $sha) and
        ( . as $jobs | all($waived[]; .id as $id | any($jobs[]; .id == $id))) and
        all(.[]; .name as $name |
          if any($waived[]; .name == $name) then
            .status == "completed" and
            (.conclusion == "success" or
              (.conclusion == "cancelled" and .runner_id == 0 and .runner_name == "" and .steps == []))
          else true end)))
  ' <<< "$history" >/dev/null
fi

check_ids=$(jq -er '.[].id' <<< "$waived")
while IFS= read -r check_id; do
  annotations=$(gh api --method GET "$repo/check-runs/$check_id/annotations?per_page=100")
  jq -e '
    "The job was not acquired by Runner of type hosted even after multiple attempts" as $reason |
    type == "array" and length > 0 and length < 100 and
    any(.[]; .annotation_level == "failure" and .message == $reason) and
    all(.[]; if .annotation_level == "failure" then .message == $reason
             else .annotation_level == "warning" or .annotation_level == "notice" end)
  ' <<< "$annotations" >/dev/null
done <<< "$check_ids"
jq -er '.[].name' <<< "$waived"
