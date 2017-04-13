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
    kgpg
    krita             # gimp-alike
    okular            # PDF viewer
    spectacle         # screenshot
    redshift-plasma-applet
    redshift
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  environment.variables = {
    # make gtk3 apps shut the hell up about the gnome accessibility bus
    NO_AT_BRIDGE = "1";
  };
}
