{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.xmonad.extraPackages = haskellPackages: [
      haskellPackages.taffybar
    ];
  };

  environment.systemPackages = with pkgs; [
    dmenu             # For xmonad
    dzen2             # For xmonad
    haskellPackages.xmobar # For xmonad
    haskellPackages.taffybar
    xcompmgr          # For xmonad

    gnome3.cheese     # KDE seems to lack a webcam app?
    gwenview          # photo viewer
    ark               # archive thinger
    kgpg
    krita             # gimp-alike
    #latte-dock       # broken?
    okular            # PDF viewer
    spectacle         # screenshot
    redshift-plasma-applet
    redshift
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  environment.variables = {
    # make gtk3 apps shut the hell up about the gnome accessibility bus
    NO_AT_BRIDGE = "1";

    # https://github.com/NixOS/nixpkgs/issues/27050#issuecomment-315324541
    QT_PLUGIN_PATH = [ "${pkgs.plasma-desktop}/lib/qt-5.9/plugins/kcms" ];
  };
}
