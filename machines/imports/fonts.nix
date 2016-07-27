{ config, pkgs, ... }:

{
  fonts.fonts = with pkgs; [
    corefonts
    anonymousPro
    aurulent-sans
    bakoma_ttf
    cantarell_fonts
    crimson
    dejavu_fonts
    dina-font
    dosemu_fonts
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    freefont_ttf
    hasklig
    inconsolata
    liberation_ttf
    meslo-lg
    powerline-fonts
    proggyfonts
    source-code-pro
    source-sans-pro
    source-serif-pro
    terminus_font
    tewi-font
    ttf_bitstream_vera
    ubuntu_font_family
    unifont
    vistafonts
  ];
}
