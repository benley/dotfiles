#!bash

# nixpkgs does some hilarious tricks to make haskell libraries work
if _GHC=$(type -p ghc 2>/dev/null) \
    && [[ $(readlink -f "$_GHC") =~ /nix\/store/ ]]; then
  eval "$(egrep ^export "$(type -p ghc)")"
fi
