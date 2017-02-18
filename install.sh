#!/usr/bin/env bash
set -e -x
here=$(dirname "${BASH_SOURCE[0]}")

echo "(import <dotfiles> { }).homedir" > "$HOME/default.nix"

"$(nix-build "$here" -A nix-home --no-out-link)"/bin/nix-home
