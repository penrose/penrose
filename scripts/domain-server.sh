#!/usr/bin/env bash

# Serves style/element programs in src/ locally

scriptdir="$(dirname "$0")"
cd "$scriptdir"

http-server ../src -p 9090 -d false -i false --cors -c-1 || echo "Please run npm install -g http-server"