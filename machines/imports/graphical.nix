{ config, pkgs, ... }:

{
  imports = [
    ./fonts.nix
  ];

  environment.systemPackages = with pkgs; [
    dropbox-cli
    firefox
    glxinfo
    google-chrome
    inkscape
    insync
    minecraft
    slack
    steam
    transmission_gtk
    vlc
    vscode
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    zathura       # keyboard-driven PDF viewer
    # xsettingsd  # for dump_xsettings
    # vdpauinfo
    # libva       # for the vainfo command
  ];

  services.printing.enable = true;

  services.xserver.enable = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [
    "interface-name:docker*"
    "interface-name:veth*"
  ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Pulseaudio with bluetooth support enabled:
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  environment.variables = {
    XCURSOR_SIZE = "64";
    XCURSOR_THEME = "breeze_cursors";
  };
}
