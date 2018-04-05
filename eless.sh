#!/usr/bin/env bash
# If already running inside an emacs shell, use emacs view-mode as a
# pager. Otherwise just use less.

set -e

_eless_stdin() {
  local _eless_tmp
  _eless_tmp=$(mktemp)
  trap 'rm -f "$_eless_tmp"' ERR
  cat - > "$_eless_tmp"
  _eless_run "$_eless_tmp"
  rm -f "$_eless_tmp"
}

_eless_run() {
  emacsclient -e "(xterm-color-view-file \"$1\")"
}

if [[ -z "$INSIDE_EMACS" ]]; then
  exec less "$@"
elif [[ $# -eq 0 ]] || [[ "$1" == "-" ]]; then
  echo "Reading into emacs..." 1>&2
  _eless_stdin
else
  _eless_run "$1"
fi
