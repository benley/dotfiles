{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
    # yubikey-manager
    # yubikey-manager-qt
    # yubikey-neo-manager
    # yubioath-flutter # build broken 11/4/2024
    yubico-piv-tool
  ];

  services.udev.packages = with pkgs; [
    # libu2f-host
    # yubikey-personalization
  ];

  services.pcscd.enable = true;
}
