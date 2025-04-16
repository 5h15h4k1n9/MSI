#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 <path_to_yaml_file>"
  exit 1
fi

yaml_file="$1"

if [ ! -f "$yaml_file" ]; then
  echo "File not found: $yaml_file"
  exit 1
fi

name=$(sed -n 's/^name:[[:space:]]*\(.*\)$/\1/p' "$yaml_file")

if [ -n "$name" ]; then
  echo "$name"
else
  exit 1
fi
