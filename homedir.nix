{ pkgs, lib, mkHome, ... }:

let
  # Given a baseDir containing various dotfiles, produce a mapping of
  #     { ".name_of_file" = /path/to/.name_of_file/on/disk; ... }
  # to be used as a home directory overlay for nix-home
  genOverlay = basedir:
    (lib.listToAttrs
      (map (f: lib.nameValuePair f "${basedir}/${f}")
           (lib.attrNames (builtins.readDir basedir))));
in

mkHome {
  user = "benley";
  files = genOverlay ./cfg //
    {
      ".config/dunst/dunstrc".content = pkgs.callPackage ./dunstrc.nix {};
    };
}
