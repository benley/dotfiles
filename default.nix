{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

rec {
  inherit (pkgs) callPackage;

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  mkHome = nixhomeLib.mkHome;

  homedir = import ./homedir.nix { inherit lib mkHome; };
}
