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
        https://cache.nixos.org
        https://hydra.nixos.org
      ];
      trusted-public-keys = [
        "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      ];
    };
  };
}
