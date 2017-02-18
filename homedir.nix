{ lib, mkHome, ... }:

let
  genOverlay = basedir:
    (lib.listToAttrs
      (map (f: lib.nameValuePair f "${basedir}/${f}")
           (lib.attrNames (builtins.readDir basedir))));
in

# Homedir overlay definition to use with nix-home
mkHome {
  user = "benley";
  files = genOverlay ./cfg //
    {
      ".config/awesome/debian".link = "/etc/xdg/awesome/debian";
    };
}
