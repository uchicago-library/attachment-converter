#!/usr/bin/env bash
if [[ "${OSTYPE}" = darwin* ]]; then
  # OSX
    base64 -i "$@"
else
  # Linux
    base64 -w 0 "$@"
fi
