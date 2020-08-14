#!/usr/bin/env bash
set -e -x
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

echo "(import ${here}).homedir" > "$HOME/default.nix"

"$(nix-build "$here" -A nix-home --no-out-link)"/bin/nix-home
