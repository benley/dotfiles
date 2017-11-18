{ config, pkgs, lib, ... }:

let dotfiles = import ../.. {}; in

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.sddm.theme = lib.mkForce "breeze-custom";
    desktopManager.plasma5.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.xmonad.extraPackages = haskellPackages: [
      haskellPackages.taffybar
    ];

  };

  programs.light.enable = true;  # backlight control helper
  programs.qt5ct.enable = true;  # Qt theme/font/icon config for non-kde envs

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
    description = "xss-lock";
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
    dotfiles.sddm-theme-breeze-custom

    dmenu             # For xmonad
    dunst             # notifications daemon
    #dzen2
    #haskellPackages.xmobar
    taffybar
    j4-dmenu-desktop  # dmenu .desktop app launcher
    libnotify         # includes notify-send
    networkmanager_dmenu
    networkmanagerapplet
    pasystray
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
  };
}
