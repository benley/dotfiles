{ config, pkgs, ... }:

{
  # holy fuck this takes FOREVER to compile
  # nixpkgs.config.virtualbox.enableExtensionPack = true;

  virtualisation.virtualbox.host.enable = true;
}
