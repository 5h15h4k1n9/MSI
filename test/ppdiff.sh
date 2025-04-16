#!/bin/bash

# Цвета
RED='\033[0;31m'
GREEN='\033[0;32m'
BOLD_GREEN='\033[1;32m'
NC='\033[0m'

if [ "$#" -lt 2 ]; then
  echo -e "${RED}Usage: $0 actual.txt expected.txt [--only-diff]${NC}"
  exit 1
fi

actual_file="$1"
expected_file="$2"
only_diff=false

if [ "$3" == "--only-diff" ]; then
  only_diff=true
fi

printf "${BOLD_GREEN}      %-6s | %-60s | %-60s${NC}\n" "Line" "Actual" "Expected"
printf "%s\n" "      --------------------------------------------------------------------------------------------------------------------------------------"

max_lines=$(awk 'END{print NR}' "$actual_file")
expected_lines=$(awk 'END{print NR}' "$expected_file")
if [ "$expected_lines" -gt "$max_lines" ]; then
  max_lines=$expected_lines
fi

for ((i=1; i<=max_lines; i++)); do
  actual_line=$(sed -n "${i}p" "$actual_file")
  expected_line=$(sed -n "${i}p" "$expected_file")

  if [ "$actual_line" == "$expected_line" ]; then
    if ! $only_diff; then
      printf "${GREEN}      %-6s | %-60s | %-60s${NC}\n" "$i" "$actual_line" "$expected_line"
    fi
  else
    printf "${RED}      %-6s | %-60s | %-60s${NC}\n" "$i" "$actual_line" "$expected_line"
  fi
done
