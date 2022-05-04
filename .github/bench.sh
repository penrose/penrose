#!/usr/bin/env bash
set -eo pipefail

function get_run() {
  gh run list --branch="$1" --jq='.[]' --limit=1 --workflow=Build \
    --json=conclusion,databaseId,event,headSha,status
}

function check_run() {
  # https://github.com/koalaman/shellcheck/wiki/SC2155

  local sha_local
  sha_local=$(git rev-parse "$1")
  local sha_remote
  sha_remote=$(jq --raw-output '.headSha' <<< "$3")

  if [[ "$sha_remote" != "$sha_local" ]]; then
    echo "commit for latest \`$1\` run does not match local branch:"
    echo "  $sha_local (local)"
    echo "  $sha_remote (remote)"
    false
  fi

  local event
  event=$(jq --raw-output '.event' <<< "$3")

  if [[ "$event" != "$2" ]]; then
    echo "event for latest \`$1\` run is \`$event\`, not \`$2\` as expected"
    false
  fi

  local status
  status=$(jq --raw-output '.status' <<< "$3")
  local completed
  completed=completed

  if [[ "$status" != "$completed" ]]; then
    echo "latest \`$1\` run status is \`$status\`, not \`$completed\`"
    false
  fi

  local conclusion
  conclusion=$(jq --raw-output '.conclusion' <<< "$3")
  local success
  success=success

  if [[ "$conclusion" != "$success" ]]; then
    echo "latest \`$1\` run conclusion is \`$conclusion\`, not \`$success\`"
    false
  fi
}

artifact=diagrams
dir=.github/artifacts/"$artifact"

function download_diagrams() {
  local dir_branch="$dir/$1"

  if [[ -e "$dir_branch" ]]; then
    echo "\`$dir_branch\` already exists, skipping download"
  else
    local run
    run=$(get_run "$1")
    check_run "$1" "$2" "$run"
    local id
    id=$(jq '.databaseId' <<< "$run")
    gh run download "$id" --dir="$dir_branch" --name="$artifact"
  fi
}

download_diagrams main push
branch_name=$(git symbolic-ref --short HEAD)
download_diagrams "$branch_name" pull_request

icdiff "$dir/main/aggregateData.json" "$dir/$branch_name/aggregateData.json"
