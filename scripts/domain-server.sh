#!/usr/bin/env bash

# Serves style/element programs in src/ locally

scriptdir="$(dirname "$0")"
cd "$scriptdir"

npx http-server ../src -p 9090 -d false -i false --cors -c-1 || echo "Ensure npx is able to run"