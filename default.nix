{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

let vimPackages = import ./vim/vimPackages.nix pkgs; in

rec {
  inherit (pkgs) callPackage;

  kubernetes-client = callPackage ./pkgs/kubernetes-client { };

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  homedir = callPackage ./homedir.nix { mkHome = nixhomeLib.mkHome; };

  vim = vimPackages.vim;

  neovim = vimPackages.neovim;

  sddm-theme-breeze-custom = callPackage ./pkgs/sddm-theme-breeze-custom { };

  slim-themes = callPackage ./pkgs/slim-themes { };

  steamcontroller-udev-rules = callPackage ./pkgs/steamcontroller-udev-rules { };

  # yaml2json = callPackage ./pkgs/yaml2json { };
}
