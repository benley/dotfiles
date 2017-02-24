{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.kde5.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnome3.cheese          # KDE seems to lack a webcam app?
    kde5.gwenview          # photo viewer
    kde5.ark               # archive thinger
    krita                  # gimp-alike
    kde5.okular            # PDF viewer
    kde5.spectacle         # screenshot
    redshift-plasma-applet
    redshift
  ];
}
