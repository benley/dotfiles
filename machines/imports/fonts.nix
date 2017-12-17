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
    emacs-all-the-icons-fonts
    fantasque-sans-mono
    fira
    fira-code
    fira-mono
    freefont_ttf
    font-awesome-ttf
    font-droid
    #google-fonts  # Like 1800 fonts. Whoa.
    hack-font
    hasklig
    inconsolata
    iosevka
    liberation_ttf
    meslo-lg
    mononoki
    montserrat
    # nerdfonts  # This is like 6,000 fonts and 3+ gigabytes, seriously
    noto-fonts
    noto-fonts-emoji  # Build broken?
    oxygenfonts
    powerline-fonts
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
    vistafonts
  ];

  fonts.fontconfig.localConf = builtins.readFile ./fonts.conf;
}
