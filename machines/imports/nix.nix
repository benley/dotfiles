{ config, pkgs, ... }:

{
  nix = {
    daemonIOSchedClass = "idle";
    settings = {
      extra-experimental-features = [ "nix-command" "flakes" ];
      keep-outputs = true;
      keep-derivations = true;
      sandbox = true;
      auto-optimise-store = true;
      trusted-users = [ "root" "benley" ];
      trusted-substituters = [
        "https://cache.nixos.org"
        "https://hydra.nixos.org"
        "https://binarycache.nix.sciesnet.net/ci-cache"
      ];
      trusted-public-keys = [
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
        "ci-5950x:rOCrQglma4VRn+sIVlA7kbBO7tVkRUTZzMLNpYVnwQE="  # XKRD
      ];
    };
  };
}
