{ config, pkgs, lib, ... }:

let dotfiles = import ../.. {}; in

{
  imports = [
    ./fonts.nix
  ];

  # TODO: did this (or equivlent) get merged upstream for sddm?
  environment.pathsToLink = [ "/share" ];

  environment.systemPackages = with pkgs; [
    dropbox-cli
    firefox
    glxinfo
    google-chrome
    inkscape
    insync
    minecraft
    signal-desktop
    slack
    steam
    transmission_gtk
    vivaldi
    vlc
    vscode
    xlibs.xdpyinfo
    xlibs.xev
    xlsfonts
    xsel
    zathura       # keyboard-driven PDF viewer
    # xsettingsd  # for dump_xsettings
    # vdpauinfo
    # libva       # for the vainfo command

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

  environment.variables = {
    XCURSOR_SIZE = "64";
    XCURSOR_THEME = "breeze_cursors";

    # make gtk3 apps shut the hell up about the gnome accessibility bus
    # https://github.com/NixOS/nixpkgs/issues/16327
    #NO_AT_BRIDGE = "1";

    # Enable GTK applications to load SVG icons
    GDK_PIXBUF_MODULE_FILE = "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  services.gnome3.at-spi2-core.enable = true;

  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [
    "interface-name:docker*"
    "interface-name:veth*"
  ];

  services.printing.enable = true;

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

  programs.light.enable = true;  # backlight control helper
  programs.qt5ct.enable = true;  # Qt theme/font/icon config for non-kde envs

  services.compton.enable = true;  # X11 compositor

  services.upower.enable = true;

  services.dbus.packages = with pkgs; [ gnome3.dconf dunst ];
  services.dbus.socketActivated = true;

  systemd.packages = with pkgs; [ dunst ];

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

  systemd.user.services.taffybar = {
    description = "taffybar";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.taffybar}/bin/taffybar";
      RestartSec = 3;
      Restart = "always";
      MemoryLimit = "512M";
      Environment = "GTK_THEME=Breeze-Dark:dark";
    };
  };

  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass.out}/bin/ksshaskpass";
}
