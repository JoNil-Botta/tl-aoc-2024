#!/usr/bin/env bash
# Fetch all AoC 2024 inputs. Requires the session cookie in ~/.aoc-session.
set -euo pipefail
mkdir -p inputs
for d in $(seq 1 25); do
  n=$(printf "%02d" "$d")
  if [ ! -s "inputs/day${n}.txt" ]; then
    curl -s -H "Cookie: session=$(cat ~/.aoc-session)" \
      -o "inputs/day${n}.txt" "https://adventofcode.com/2024/day/${d}/input"
    sleep 1
  fi
done
echo "done"
