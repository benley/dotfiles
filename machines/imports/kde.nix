{ config, pkgs, lib, ... }:

let dotfiles = import ../.. {}; in

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    displayManager.sddm.theme = lib.mkForce "breeze-custom";
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.xmonad.extraPackages = haskellPackages: [
      haskellPackages.taffybar
    ];
  };

  services.upower.enable = true;

  services.compton.enable = true;  # X11 compositor

  programs.light.enable = true;  # backlight control helper
  programs.qt5ct.enable = true;  # Qt theme/font/icon config for non-kde envs

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableExtraSocket = true;
    # enableBrowserSocket = true;  # What is this for?
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

  environment.pathsToLink = [ "/share" ];

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
    lxqt.pavucontrol-qt
    xautolock         # so I can xautolock -locknow
    xorg.xbacklight

    gnome3.cheese     # KDE seems to lack a webcam app?
    gwenview          # photo viewer
    ark               # archive thinger
    kate
    kgpg
    krita             # gimp-alike
    konsole
    qt5.qttools       # includes qdbusviewer
    spectacle         # screenshot
    dolphin
    kdeApplications.dolphin-plugins

    breeze-icons
    hicolor_icon_theme
    breeze-gtk
    breeze-qt5
    breeze-qt4

    ksshaskpass
    pinentry_qt5
  ];

  services.dbus.packages = with pkgs; [ gnome3.dconf dunst ];
  systemd.packages = with pkgs; [ dunst ];

  environment.variables = {
    # make gtk3 apps shut the hell up about the gnome accessibility bus
    NO_AT_BRIDGE = "1";

    # https://github.com/NixOS/nixpkgs/issues/27050#issuecomment-315324541
    # QT_PLUGIN_PATH = [ "${pkgs.plasma-desktop}/lib/qt-5.9/plugins/kcms" ];

    # Enable GTK applications to load SVG icons
    GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass.out}/bin/ksshaskpass";
}
