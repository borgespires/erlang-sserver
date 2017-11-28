#!/bin/bash

# release application artifact
docker build -t build_erlang . --file=build.dockerfile
docker run --rm --volume="$PWD/_artifacts:/artifacts" build_erlang

# build docker image to deploy
docker build -t sserver . --file=prod.dockerfile