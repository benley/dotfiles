{ config, pkgs, lib, ... }:

{
  imports = [
    ./fonts.nix
    ./wacom.nix
    ./workstation.nix
  ];

  # services.dunst.enable = true;
  # services.dunst.config = pkgs.callPackage ../../dunstrc.nix {};

  # https://github.com/NixOS/nixpkgs/issues/47173
  # environment.pathsToLink = [ "/share" ];

  environment.etc = {
    "opt/chrome/policies/managed/test_policy.json" = {
      text = builtins.toJSON {
        "ExtensionInstallBlacklist" = [
          "khpfeaanjngmcnplbdlpegiifgpfgdco"  # Smart Card Connector (breaks gpg/ssh agent!)
        ];
      };
    };
  };

  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [
    (hunspellWithDicts [pkgs.hunspellDicts.en-us])
    # arandr
    # battery-monitor
    # blueman
    discord
    # dropbox-cli
    firefox-wayland
    fritzing
    glxinfo
    google-chrome
    graphviz
    inkscape
    insync-v3
    minecraft
    # nixnote2  # evernote client
    pinta
    remmina  # RDP/VNC/NX/Spice client
    signal-desktop
    slack
    # texlive
    transmission_gtk
    # vivaldi
    # libsForQt5.vlc
    gnome_mplayer
    nordic  # GTK theme
    gnome3.gnome-themes-extra  # I think this fixes some "can't find theme engine adwaita" erorrs
    gnome3.gnome-tweaks
    gnome3.dconf-editor
    gnomeExtensions.appindicator
    # gnomeExtensions.material-shell
    gnomeExtensions.sound-output-device-chooser
    # vscode
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    zathura       # keyboard-driven PDF viewer
    # xsettingsd  # for dump_xsettings
    vdpauinfo
    libva-utils       # for the vainfo command

    alsaUtils         # amixer, used in .xmonad.hs
    # dmenu             # For xmonad
    # rofi              # maybe replace dmenu?
    # dzen2
    # feh               # For scaling / setting background image
    # taffybar
    # j4-dmenu-desktop  # dmenu .desktop app launcher
    # libnotify         # includes notify-send
    # networkmanager_dmenu
    # networkmanagerapplet
    # pasystray
    # lxqt.pavucontrol-qt  # This is nicer, but pasystray wants to launch regular pavucontrol
    # pavucontrol
    # xautolock         # so I can xautolock -locknow
    # xorg.xbacklight
    xorg.xmodmap

    gimp
    # gnome3.cheese     # KDE seems to lack a webcam app?
    # gwenview          # photo viewer
    # ark               # archive thinger
    # kate
    # kgpg
    # krita             # breaks update-desktop-database until https://invent.kde.org/graphics/krita/-/merge_requests/663 is backported
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
    # gnome3.seahorse   # gnome-wallet manager gui

    # stumpish
    ddccontrol
    # udiskie
    powertop

    # alacritty

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

  # Redundant I think?  Enabled with gnome by default.
  # services.gnome.at-spi2-core.enable = true;

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

    desktopManager.gnome = {
      enable = true;
      extraGSettingsOverridePackages = [ pkgs.gnome3.mutter ];
      extraGSettingsOverrides = ''
        [org.gnome.mutter]
        experimental-features=['scale-monitor-framebuffer']

        [org.gnome.desktop.interface]
        text-scaling-factor='1.37'
      '';
    };

    displayManager.gdm.enable = true;

    # Commands to run just before starting my window manager:
    displayManager.sessionCommands = lib.concatStringsSep "\n" [
      # status-notifier-watcher needs to be up and running before any
      # apps try to create indicator icons, and before taffybar goes looking for it
      # "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher &"
      # "${pkgs.plasma5.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1 &"
      # "xsetroot -cursor_name left_ptr"
      # "${pkgs.insync}/bin/insync start &"
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

  programs.adb.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableExtraSocket = true;
  };

  services.upower.enable = true;

  services.dbus.packages = with pkgs; [
    # gnome3.dconf  # redundant
    # blueman       # I don't think I'm using this (gnome has its own bluetooth applet and stuff)
  ];

  # Removed in the next release after 20.09
  # services.dbus.socketActivated = true;

  # Probably don't want this on headless machines, but workstations/laptops
  # sure. This makes sure that things like my user dbus session don't persist
  # across multiple logout/login cycles and mess things up.
  services.logind.extraConfig = ''
    KillUserProcesses=yes
  '';
}
