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
    #google-fonts  # Like 1800 fonts. Whoa.
    hack-font
    hasklig
    ibm-plex
    inconsolata
    iosevka
    # liberation_ttf  # default
    meslo-lg
    # monoid  # broken?
    mononoki
    montserrat
    (nerdfonts.override { fonts = ["NerdFontsSymbolsOnly"]; })
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
    # unifont  # default
    # vistafonts  # broken?
  ];

  fonts.fontconfig.defaultFonts = {
    monospace = [ "Cousine" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Serif" ];
  };

  fonts.fontconfig.localConf = builtins.readFile ./local-fontconfig.conf;
}
