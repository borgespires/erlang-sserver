#!/bin/bash

PORT=$1
docker run --rm -it -p$PORT:8080 sserver