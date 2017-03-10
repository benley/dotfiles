{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gnome3.cheese     # KDE seems to lack a webcam app?
    gwenview          # photo viewer
    ark               # archive thinger
    krita             # gimp-alike
    okular            # PDF viewer
    spectacle         # screenshot
    redshift-plasma-applet
    redshift
  ];
}
