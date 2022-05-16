#!/usr/bin/env bash
set -eo pipefail

function get_run() {
  local branch="$1"

  gh run list --branch="$branch" --jq='.[]' --limit=1 --workflow=Build \
    --json=conclusion,databaseId,event,headSha,status
}

function check_run() {
  local branch="$1"
  local trigger="$2"
  local run="$3"

  # https://github.com/koalaman/shellcheck/wiki/SC2155

  local sha_local
  sha_local=$(git rev-parse "$branch")
  local sha_remote
  sha_remote=$(jq --raw-output '.headSha' <<< "$run")

  if [[ "$sha_remote" != "$sha_local" ]]; then
    echo "latest \`$branch\` run commit does not match local branch:"
    echo "  $sha_local (local)"
    echo "  $sha_remote (remote)"
    false
  fi

  local event
  event=$(jq --raw-output '.event' <<< "$run")

  if [[ "$event" != "$trigger" ]]; then
    echo "latest \`$branch\` run event is \`$event\`, not \`$trigger\`"
    false
  fi

  local status
  status=$(jq --raw-output '.status' <<< "$run")
  local completed=completed

  if [[ "$status" != "$completed" ]]; then
    echo "latest \`$branch\` run status is \`$status\`, not \`$completed\`"
    false
  fi

  local conclusion
  conclusion=$(jq --raw-output '.conclusion' <<< "$run")
  local succ=success

  if [[ "$conclusion" != "$succ" ]]; then
    echo "latest \`$branch\` run conclusion is \`$conclusion\`, not \`$succ\`"
    false
  fi
}

artifact=diagrams
dir=.github/artifacts/"$artifact"/

function visualize() {
  local branch="$1"
  local event="$2"

  local commit
  commit=$(git rev-parse "$branch")
  local dir_commit="$dir$commit"/

  if [[ -e "$dir_commit" ]]; then
    echo "\`$dir_commit\` already exists, skipping download"
  else
    local run
    run=$(get_run "$branch")
    check_run "$branch" "$event" "$run"

    local id
    id=$(jq '.databaseId' <<< "$run")
    gh run download "$id" --dir="$dir_commit" --name="$artifact"
  fi

  pushd packages/automator/
  local out=out/
  mkdir -p "$out"

  local out_branch="$out$branch"/
  yarn start render ../../"$dir_commit" "$out_branch"
  open "$out_branch"vis.html
  popd
}

visualize main push
branch_name=$(git symbolic-ref --short HEAD)
visualize "$branch_name" pull_request
