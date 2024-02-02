{ config, pkgs, ... }:

{
  nix = {
    nixPath = [
      "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos"
      "nixos-config=/home/benley/p/dotfiles/machines/${config.networking.hostName}/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    daemonIOSchedClass = "idle";
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
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
