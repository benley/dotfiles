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
  programs.steam.remotePlay.openFirewall = true;
  programs.steam.dedicatedServer.openFirewall = true;

  environment.systemPackages = with pkgs; [
    (hunspellWithDicts [pkgs.hunspellDicts.en-us])
    # arandr
    # battery-monitor
    # blueman
    discord
    # dropbox-cli
    element-desktop
    firefox-wayland
    fritzing
    glxinfo
    (google-chrome.override {
      commandLineArgs = "--enable-features=TouchpadOverscrollHistoryNavigation";
    })
    graphviz
    inkscape
    insync-v3
    minecraft
    moonlight-qt
    # nixnote2  # evernote client
    pinta
    remmina  # RDP/VNC/NX/Spice client
    signal-desktop
    slack
    # texlive
    transmission-gtk
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
    xorg.xdpyinfo
    xorg.xev
    xlsfonts
    xsel
    zathura       # keyboard-driven PDF viewer
    zoom-us
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
    pavucontrol
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
    hicolor-icon-theme
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
    razergenie
  ];

  hardware.openrazer.enable = true;
  hardware.openrazer.users = [ "bstaffin" "benley" ];

  environment.sessionVariables = {
    # This is probably a terrible idea but there doesn't seem to be a
    # great alternative if I want xcursor stuff to work in non-gtk
    # emacs, xterm, etc.
    LD_LIBRARY_PATH = ["${pkgs.xorg.libXcursor}/lib"];
  };

  environment.variables = {
    # Enable GTK applications to load SVG icons
    GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [
    "interface-name:docker*"
    "interface-name:veth*"
  ];

  services.printing.enable = true;

  services.xserver = {
    enable = true;
    updateDbusEnvironment = true;
    desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;  # With bluetooth support

  hardware.bluetooth.enable = true;

  programs.adb.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableExtraSocket = true;
  };

  programs.gnome-terminal.enable = true;  # omfg how is this not the default with gnome

  # Probably don't want this on headless machines, but workstations/laptops
  # sure. This makes sure that things like my user dbus session don't persist
  # across multiple logout/login cycles and mess things up.
  services.logind.extraConfig = ''
    KillUserProcesses=yes
  '';

  services.powermate.enable = true;
}
