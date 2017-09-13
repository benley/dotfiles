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

  systemd.user.services.xautolock = {
    description = "xautolock";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.xautolock}/bin/xautolock \
          -time 10 \
          -locker '${pkgs.i3lock}/bin/i3lock -n -e -f -c "#263238" -i $HOME/.background-image' \
          -notify 15 \
          -notifier "${pkgs.libnotify}/bin/notify-send -u critical -t 14000 -- 'Locking screen in 15 seconds'" \
          -detectsleep
      '';
      RestartSec = 3;
      Restart = "always";
    };
  };

  systemd.user.services.xss-lock = {
    description = "Trigger xautolock on suspend";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.xss-lock}/bin/xss-lock \
          -- ${pkgs.xautolock}/bin/xautolock -locknow
      '';
      RestartSec = 3;
      Restart = "always";
    };
  };

  environment.systemPackages = with pkgs; [
    dmenu             # For xmonad
    dunst             # notifications daemon
    #dzen2
    #haskellPackages.xmobar
    haskellPackages.taffybar
    j4-dmenu-desktop  # dmenu .desktop app launcher
    libnotify         # includes notify-send
    networkmanager_dmenu
    networkmanagerapplet
    qt5ct             # Set QT themes without running Plasma
    xautolock         # so I can xautolock -locknow
    xcompmgr
    xorg.xbacklight

    gnome3.cheese     # KDE seems to lack a webcam app?
    gwenview          # photo viewer
    ark               # archive thinger
    kate
    kgpg
    krita             # gimp-alike
    #latte-dock       # broken?
    #okular           # PDF viewer
    qt5.qttools       # includes qdbusviewer
    spectacle         # screenshot
    redshift-plasma-applet
    redshift
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  environment.variables = {
    # make gtk3 apps shut the hell up about the gnome accessibility bus
    NO_AT_BRIDGE = "1";

    # https://github.com/NixOS/nixpkgs/issues/27050#issuecomment-315324541
    # QT_PLUGIN_PATH = [ "${pkgs.plasma-desktop}/lib/qt-5.9/plugins/kcms" ];

    # Make QT theming work at all
    QT_QPA_PLATFORMTHEME = "qt5ct";
  };
}
