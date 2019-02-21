#!/usr/bin/env sh

nix build
status=$?
if test "$status" -eq 0
then
    cd result && bin/server;
else
    echo "Waiting for file changes..."
fi
