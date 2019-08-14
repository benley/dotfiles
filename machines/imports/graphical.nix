{ config, pkgs, lib, ... }:

{
  imports = [
    ./fonts.nix
  ];

  # services.dunst.enable = true;
  # services.dunst.config = pkgs.callPackage ../../dunstrc.nix {};

  # TODO: did this (or equivlent) get merged upstream for sddm?
  environment.pathsToLink = [ "/share" ];

  environment.etc = {
    "opt/chrome/policies/managed/test_policy.json" = {
      text = builtins.toJSON {
        "ExtensionInstallBlacklist" = [
          "khpfeaanjngmcnplbdlpegiifgpfgdco"  # Smart Card Connector (breaks gpg/ssh agent!)
        ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    (hunspellWithDicts [pkgs.hunspellDicts.en-us])
    # arandr
    # battery-monitor
    # blueman
    discord
    dropbox-cli
    firefox
    fritzing
    glxinfo
    google-chrome
    graphviz
    inkscape
    insync
    minecraft
    # nixnote2  # evernote client
    pinta
    remmina  # RDP/VNC/NX/Spice client
    signal-desktop
    slack
    steam
    # linux-steam-integration  # build broken?
    # texlive
    transmission_gtk
    # vivaldi
    # libsForQt5.vlc
    gnome_mplayer
    gnome3.gnome_themes_standard  # I think this fixes some "can't find theme engine adwaita" erorrs

    gnomeExtensions.appindicator
    gnomeExtensions.dash-to-dock
    gnomeExtensions.dash-to-panel
    # gnomeExtensions.icon-hider
    # gnomeExtensions.no-title-bar
    # gnomeExtensions.system-monitor
    gnomeExtensions.topicons-plus
    # gnomeExtensions.tilingnome
    # vscode
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    # zathura       # keyboard-driven PDF viewer (build broken)
    # xsettingsd  # for dump_xsettings
    # vdpauinfo
    # libva       # for the vainfo command

    # sddm-theme-breeze-custom

    alsaUtils         # amixer, used in .xmonad.hs
    # dmenu             # For xmonad
    rofi              # maybe replace dmenu?
    #dzen2
    #haskellPackages.xmobar
    feh               # For scaling / setting background image
    # taffybar
    # j4-dmenu-desktop  # dmenu .desktop app launcher
    # libnotify         # includes notify-send
    networkmanager_dmenu
    # networkmanagerapplet
    # pasystray
    # lxqt.pavucontrol-qt  # This is nicer, but pasystray wants to launch regular pavucontrol
    # pavucontrol
    # xautolock         # so I can xautolock -locknow
    xorg.xbacklight
    xorg.xmodmap

    gimp
    gnome3.cheese     # KDE seems to lack a webcam app?
    # gwenview          # photo viewer
    # ark               # archive thinger
    # kate
    # kgpg
    krita             # gimp-alike
    # konsole
    # kupfer            # task launcher a la QuickSilver
    # kupfer-plugin-google-search
    # qt5.qttools       # includes qdbusviewer
    # spectacle         # screenshot
    # dolphin
    # kdeApplications.dolphin-plugins

    breeze-icons
    hicolor_icon_theme
    breeze-gtk
    breeze-qt5
    # breeze-qt4  # gone, I think

    # ksshaskpass
    # pinentry_qt5
    pinentry_gnome
    # kwalletcli      # includes pinentry-kwallet
    gnome3.seahorse   # gnome-wallet manager gui

    # stumpish
    ddccontrol
    # udiskie
  ];

  environment.sessionVariables = {
    # This is probably a terrible idea but there doesn't seem to be a
    # great alternative if I want xcursor stuff to work in non-gtk
    # emacs, xterm, etc.
    LD_LIBRARY_PATH = ["${pkgs.xorg.libXcursor}/lib"];
  };

  environment.variables = {
    XCURSOR_SIZE = "64";
    XCURSOR_THEME = "breeze_cursors";

    # make gtk3 apps shut the hell up about the gnome accessibility bus
    # https://github.com/NixOS/nixpkgs/issues/16327
    #NO_AT_BRIDGE = "1";
    # ^^^^ not needed if sesrvices.gnome3.at-spi2-core.enable == true

    # Enable GTK applications to load SVG icons
    GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

    # Redundant when programs.qt5ct.enable == true
    # QT_QPA_PLATFORMTHEME = "qt5ct";

    # QT_FONT_DPI = toString (config.services.xserver.dpi / 2);
    # QT_SCALE_FACTOR = "2.0";
    # QT_AUTO_SCREEN_SCALE_FACTOR = "0";

    # gtk3 is too dumb to notice the 240dpi display, so force it to scale
    # GDK_SCALE = "2";

    # ... but freetype sure as heck notices, so now we compensate for that
    # GDK_DPI_SCALE = "0.5";
  };

  services.gnome3.at-spi2-core.enable = true;
  # services.gnome3.gnome-keyring.enable = true;
  # security.pam.services.gdm.enableGnomeKeyring = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [
    "interface-name:docker*"
    "interface-name:veth*"
  ];

  # This would enable CUPS, which I don't seem to actually use
  # services.printing.enable = true;

  services.xserver = {
    enable = true;
    updateDbusEnvironment = true;
    desktopManager.gnome3.enable = true;
    displayManager.gdm.enable = true;
    # displayManager.lightdm.enable = true;
    # displayManager.lightdm.background = "${/home/benley/Downloads/Clean-Desktop-Wallpaper-12.jpg}";

    windowManager.session = [{
      name = "exwm";
      start = ''
        emacs -mm &
        waitPID=$!
      '';
    }];

    # Commands to run just before starting my window manager:
    displayManager.sessionCommands = lib.concatStringsSep "\n" [
      # status-notifier-watcher needs to be up and running before any
      # apps try to create indicator icons, and before taffybar goes looking for it
      # "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher &"
      # "${pkgs.plasma5.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1 &"
      # "xsetroot -cursor_name left_ptr"
      "${pkgs.insync}/bin/insync start &"
      # "${pkgs.dropbox-cli}/bin/dropbox start &"
      # "${pkgs.taffybar}/bin/taffybar &"
      # "${pkgs.networkmanagerapplet}/bin/nm-applet --indicator &"
      # "${pkgs.networkmanagerapplet}/bin/nm-applet &"
      # "${pkgs.pasystray}/bin/pasystray -a &"
      # "${pkgs.blueman}/bin/blueman-applet &"
      # "${pkgs.kupfer}/bin/kupfer --no-splash &"
      # "${pkgs.battery-monitor}/bin/battery-monitor &"
      # "${pkgs.udiskie}/bin/udiskie --tray --notify --no-automount &"
      # "setxkbmap"  # is this still necessary?
    ];
    # windowManager.xmonad.enable = true;
    # windowManager.xmonad.enableContribAndExtras = true;
    # windowManager.xmonad.extraPackages = haskellPackages: [
    #   haskellPackages.taffybar
    # ];
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;  # With bluetooth support

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableExtraSocket = true;
    # enableBrowserSocket = true;  # What is this for?
  };

  # programs.light.enable = true;  # backlight control helper
  # programs.qt5ct.enable = true;  # Qt theme/font/icon config for non-kde envs

  #services.compton.enable = true;  # X11 compositor

  services.upower.enable = true;

  services.dbus.packages = with pkgs; [ gnome3.dconf blueman ];

  services.dbus.socketActivated = true;

  # systemd.user.services.xautolock = {
  #   description = "xautolock";
  #   wantedBy = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  #   serviceConfig = {
  #     ExecStart = ''
  #       ${pkgs.xautolock}/bin/xautolock \
  #         -time 10 \
  #         -locker '${pkgs.i3lock}/bin/i3lock -n -e -f -c "#263238" -i $HOME/.background-image' \
  #         -notify 15 \
  #         -notifier "${pkgs.libnotify}/bin/notify-send -u critical -t 14000 -- 'Locking screen in 15 seconds'" \
  #         -detectsleep
  #     '';
  #     RestartSec = 3;
  #     Restart = "always";
  #   };
  # };

  # systemd.user.services.xss-lock = {
  #   description = "xss-lock";
  #   wantedBy = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  #   serviceConfig = {
  #     ExecStart = ''
  #       ${pkgs.xss-lock}/bin/xss-lock \
  #         -- ${pkgs.xautolock}/bin/xautolock -locknow
  #     '';
  #     RestartSec = 3;
  #     Restart = "always";
  #   };
  # };

  # systemd.user.services.taffybar = {
  #   description = "taffybar";
  #   wantedBy = [ "graphical-session.target" ];
  #   wants = [ "status-notifier-watcher.service" ];
  #   after = [ "status-notifier-watcher.service" ];
  #   partOf = [ "graphical-session.target" ];
  #   # environment.GTK_THEME = "Breeze-Dark:dark";
  #   # path = config.environment.profiles;
  #   path = [ pkgs.upower ];
  #   serviceConfig = {
  #     ExecStart = "${pkgs.taffybar}/bin/taffybar";
  #     RestartSec = 3;
  #     Restart = "always";
  #     MemoryLimit = "512M";
  #   };
  # };

  # systemd.user.services.status-notifier-watcher = {
  #   description = "status-notifier-watcher";
  #   wantedBy = [ "graphical-session.target" ];
  #   partOf = [ "graphical-session.target" ];
  #   serviceConfig.ExecStart =
  #       "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
  # };

  # programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass.out}/bin/ksshaskpass";

  # Probably don't want this on headless machines, but workstations/laptops
  # sure. This makes sure that things like my user dbus session don't persist
  # across multiple logout/login cycles and mess things up.
  services.logind.extraConfig = ''
    KillUserProcesses=yes
  '';

}
