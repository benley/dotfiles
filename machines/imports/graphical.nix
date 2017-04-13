{ config, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  environment.systemPackages = with pkgs; [
    dropbox
    firefox
    google-chrome
    glxinfo
    inkscape
    insync
    skype
    slack
    steam
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    xsettingsd  # So I can use dump_xsettings
    vlc
    vscode
  ];

  services.xserver.enable = true;

  networking.networkmanager.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
}
