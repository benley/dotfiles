{ nixpkgs ? import <nixpkgs> {} }:

with { inherit (nixpkgs) pkgs lib; };

let vimPackages = import ./vim/vimPackages.nix pkgs; in

rec {
  inherit (pkgs) callPackage;

  emacs = callPackage ./emacs.nix { };

  eless = pkgs.writeScriptBin "eless" (builtins.readFile ./eless.sh);

  kubernetes-client = callPackage ./pkgs/kubernetes-client { };

  nix-home = callPackage ./pkgs/nix-home { };

  nixhomeLib = callPackage (import "${nix-home}/nix/lib/nixhome") {};

  hddfancontrol = callPackage ./pkgs/hddfancontrol { };

  homedir = callPackage ./homedir.nix { mkHome = nixhomeLib.mkHome; };

  vim = vimPackages.vim;

  neovim = vimPackages.neovim;

  sddm-theme-breeze-custom = callPackage ./pkgs/sddm-theme-breeze-custom { };

  slim-themes = callPackage ./pkgs/slim-themes { };

  steamcontroller-udev-rules = callPackage ./pkgs/steamcontroller-udev-rules { };

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-small wrapfig capt-of cm-super;
  };

  # yaml2json = callPackage ./pkgs/yaml2json { };
}
