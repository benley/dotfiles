{ config, pkgs, inputs, ... }:

let my-fonts = inputs.my-fonts.packages."${pkgs.system}"; in

{
  fonts.packages = with pkgs; [
    my-fonts.pragmataPro
    corefonts
    anonymousPro
    aurulent-sans
    bakoma_ttf
    cantarell-fonts
    crimson
    dejavu_fonts
    dina-font
    dosemu_fonts
    emacs-all-the-icons-fonts
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    freefont_ttf
    font-awesome
    go-font
    hack-font
    hasklig
    ibm-plex
    inconsolata
    iosevka
    meslo-lg
    mononoki
    montserrat
    nerd-fonts.symbols-only
    noto-fonts
    noto-fonts-emoji
    oxygenfonts
    profont
    roboto
    source-code-pro
    source-sans-pro
    source-serif-pro
    symbola
    terminus_font
    tewi-font
    ttf_bitstream_vera
    ubuntu_font_family
  ];

  fonts.fontconfig.defaultFonts = {
    monospace = [ "Cousine" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Serif" ];
  };

  fonts.fontconfig.localConf = builtins.readFile ./local-fontconfig.conf;
}
