{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8104" }:

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./xmonad-benley.nix {}
