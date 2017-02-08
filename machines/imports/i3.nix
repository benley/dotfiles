{ config, pkgs, ... }:

{
  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.configFile = ../../.config/i3/config;

  services.compton.enable = true;

  environment.systemPackages = with pkgs; [
    dmenu
    i3status
    roxterm

    adapta-gtk-theme
    hicolor_icon_theme
  ];

}
