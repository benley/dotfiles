{ config, pkgs, ... }:

{
  nix = {
    nixPath = [
      "nixpkgs=https://github.com/benley/nixpkgs/archive/nixos-benley.tar.gz"
      "nixos-config=/home/benley/p/dotfiles/machines/${config.networking.hostName}/configuration.nix"
      #"/nix/var/nix/profiles/per-user/root/channels/nixos"
      #"/nix/var/nix/profiles/per-user/root/channels"
    ];
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    useSandbox = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
    trustedBinaryCaches = [
      https://cache.nixos.org
      https://hydra.nixos.org
    ];
    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };
}
