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
    insync
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
}
