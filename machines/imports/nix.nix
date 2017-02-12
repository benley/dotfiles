{ config, pkgs, ... }:

let
  nixpkgs_github_repo = "benley/nixpkgs";
  nixpkgs_branch = "nixos-benley";
in

{
  nix = {
    nixPath = [
      # Instead of using a nixos channel, I follow a specific nixpkgs branch on github.
      "nixpkgs=https://github.com/${nixpkgs_github_repo}/archive/${nixpkgs_branch}.tar.gz"

      # I like to keep my dotfiles repo in my homedir for easy editing
      "nixos-config=/home/benley/p/dotfiles/machines/${config.networking.hostName}/configuration.nix"
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
