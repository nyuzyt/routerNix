#!/usr/bin/env sh

# usage: ag -l | entr -r ./build_and_run.sh

nix-build
status=$?
if test "$status" -eq 0
then
    cd result && bin/server;
else
    echo "Waiting for file changes..."
fi
