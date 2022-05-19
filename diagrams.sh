#!/usr/bin/env bash
rm -rf docker/clone/
docker build .
git clone . docker/clone/
docker run -v "$PWD"/docker/clone/:/penrose/ "$(docker build -q .)"
