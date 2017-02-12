{ config, pkgs, ... }:

{
  services.xserver.wacom.enable = true;

  # http://linuxwacom.sourceforge.net/wiki/index.php/Consumer_Tablet_ExpressKey_Mapping_Issue
  services.xserver.inputClassSections = [
    ''
      Identifier "Wacom Bamboo 16FG 4x5 Pad pad GNOME compatibility"
      MatchDriver "wacom"
      MatchProduct "Wacom Bamboo 16FG 4x5 Pad pad"

      Option "Button1" "1"
      Option "Button5" "2"
      Option "Button4" "3"
      Option "Button3" "4"
    ''
  ];
}
