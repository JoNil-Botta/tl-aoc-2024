#!/usr/bin/env bash
# Submit an answer: submit.sh <day> <level> <answer>
set -euo pipefail
day="$1"; level="$2"; answer="$3"
resp=$(curl -s -H "Cookie: session=$(cat ~/.aoc-session)" -X POST \
  -d "level=${level}&answer=${answer}" \
  "https://adventofcode.com/2024/day/${day}/answer")
echo "$resp" | grep -o '<article[^>]*>.*</article>' | sed 's/<[^>]*>//g' | head -2
echo "$resp" | grep -qi "right answer" && exit 0 || exit 1
