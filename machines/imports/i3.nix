{ config, pkgs, ... }:

{
  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.configFile = ../../.config/i3/config;

  environment.systemPackages = with pkgs; [
    dmenu
    i3status
    roxterm
  ];
}
