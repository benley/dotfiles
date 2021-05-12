{ config, pkgs, ... }:

let localFonts = pkgs.callPackage "/home/benley/benley@gmail.com/Fonts" {}; in

{
  fonts.fonts = with pkgs; [
    localFonts.pragmataPro
    corefonts
    anonymousPro
    aurulent-sans
    bakoma_ttf
    cantarell_fonts
    crimson
    # dejavu_fonts
    dina-font
    dosemu_fonts
    emacs-all-the-icons-fonts
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    freefont_ttf
    font-awesome-ttf
    go-font
    #google-fonts  # Like 1800 fonts. Whoa.
    hack-font
    hasklig
    ibm-plex
    inconsolata
    iosevka
    liberation_ttf
    meslo-lg
    mononoki
    montserrat
    # nerdfonts  # This is like 6,000 fonts and 3+ gigabytes, seriously
    noto-fonts
    noto-fonts-emoji
    oxygenfonts
    # powerline-fonts
    profont
    # proggyfonts  # bitmapped, too small on modern screens
    roboto
    source-code-pro
    source-sans-pro
    source-serif-pro
    symbola
    terminus_font
    tewi-font
    ttf_bitstream_vera
    ubuntu_font_family
    unifont
    # vistafonts  # broken?
  ];

  # Seems like penultimate.enable may break emoji support somehow?
  # https://github.com/NixOS/nixpkgs/issues/53139
  # fonts.fontconfig.penultimate.enable = false;
  fonts.fontconfig.defaultFonts = {
    monospace = [ "Cousine" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Serif" ];
  };

  fonts.fontconfig.localConf = builtins.readFile ./local-fontconfig.conf;
}
