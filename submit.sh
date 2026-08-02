#!/usr/bin/env bash
# Submit an answer: submit.sh <day> <level> <answer>
set -euo pipefail
day="$1"; level="$2"; answer="$3"
resp=$(curl -s -H "Cookie: session=$(cat ~/.aoc-session)" -X POST \
  -d "level=${level}&answer=${answer}" \
  "https://adventofcode.com/2024/day/${day}/answer")
echo "$resp" | grep -oz '<article[^>]*>.*</article>' | sed 's/<[^>]*>//g' | head -2
if echo "$resp" | grep -qi "right answer"; then exit 0; else exit 1; fi
