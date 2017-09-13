{ config, pkgs, ... }:

{
  nix = {
    nixPath = [
      "nixos-config=/home/benley/p/dotfiles/machines/${config.networking.hostName}/configuration.nix"
      "dotfiles=/home/benley/p/dotfiles"
      "nixpkgs=/home/benley/p/nixpkgs"
      "/nix/var/nix/profiles/per-user/root/channels"
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
      http://ein.local:5150
    ];
    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
      "ein:HHuyPwBPbAKcBKmyMsqHMWVLjFxE71MZCTZWxPWRtck="
    ];
  };
}
