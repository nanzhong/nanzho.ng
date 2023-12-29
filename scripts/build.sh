#!/usr/bin/env bash

set -euo pipefail

sass --style=compressed theme/static/scss:theme/static/css
mkdir -p output
emacs --script export.el
find posts -type f | grep -v .org | xargs -i cp '{}' 'output/{}'
