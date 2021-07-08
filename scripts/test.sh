#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../" || exit

test_options=""
for i in "$@"; do
  test_options="$test_options --test-option=$i"
done

# shellcheck disable=SC2086
cabal --ghc-options -Wwarn test --test-show-details=direct $test_options
