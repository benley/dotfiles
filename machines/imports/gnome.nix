{ config, pkgs, ... }:

{
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;

  environment.systemPackages = with pkgs; [
    adapta-backgrounds  # post-16.09
    adapta-gtk-theme
    arc-gtk-theme
    arc-icon-theme
    clearlooks-phenix
    elementary-icon-theme
    faba-icon-theme
    flat-plat
    gnome-breeze
    gnome.gnome_icon_theme
    gnome3.adwaita-icon-theme
    gnome3.adwaita-icon-theme
    gtk-engine-murrine
    gtk_engines
    hicolor_icon_theme
    mate.mate-icon-theme
    mate.mate-icon-theme-faenza
    mate.mate-themes
    moka-icon-theme
    numix-gtk-theme
    numix-icon-theme
    oxygen-gtk2
    oxygen-gtk3
    kde4.oxygen_icons
    paper-gtk-theme
    paper-icon-theme
    sound-theme-freedesktop
    tango-icon-theme
    theme-vertex
    vanilla-dmz
    xfce.gtk_xfce_engine
    xfce.xfce4icontheme
    xorg.xcursorthemes
    zuki-themes

    gnome3.cheese
    gnome3.gnome-video-effects
  ];
}
