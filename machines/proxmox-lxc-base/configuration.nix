{ config, lib, pkgs, ... }:

{
  services.sshd.enable = true;
  services.openssh.settings.PermitRootLogin = lib.mkDefault "prohibit-password";
  services.getty.autologinUser = lib.mkDefault "root";

  system.stateVersion = "23.05";
}
