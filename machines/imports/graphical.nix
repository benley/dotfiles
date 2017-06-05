{ config, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  environment.systemPackages = with pkgs; [
    dropbox
    firefox
    glxinfo
    google-chrome
    inkscape
    insync
    minecraft
    skype
    slack
    steam
    transmission_gtk
    vlc
    vscode
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    # xsettingsd  # for dump_xsettings
    # vdpauinfo
    # libva       # for the vainfo command
  ];

  services.printing.enable = true;

  services.xserver.enable = true;

  networking.networkmanager.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Pulseaudio with bluetooth support enabled:
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
}
