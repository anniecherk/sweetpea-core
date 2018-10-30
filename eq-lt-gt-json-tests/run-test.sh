#!/usr/bin/env bash

curl -Ss -d @"$1" localhost:8080/experiments/build-cnf > "$1.cnf"
cryptominisat5 "$1.cnf" > "$1.sol"
cat "$1.sol"
rm "$1.cnf" "$1.sol"


cat "$1"
