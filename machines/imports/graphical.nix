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
    bitwarden
    chrysalis  # keyboardio config editor (requires services.udev.packages entry also)
    discord
    firefox-wayland
    fritzing
    glxinfo
    (google-chrome.override {
      commandLineArgs = "--enable-features=TouchpadOverscrollHistoryNavigation";
    })
    graphviz
    pdfarranger
    prismlauncher
    remmina  # RDP/VNC/NX/Spice client
    signal-desktop
    slack
    telegram-desktop
    transmission-gtk
    gnome_mplayer
    nordic  # GTK theme
    gnome3.gnome-themes-extra  # I think this fixes some "can't find theme engine adwaita" erorrs
    gnome3.gnome-tweaks
    gnome3.dconf-editor
    gnomeExtensions.appindicator
    gnomeExtensions.dash-to-panel
    gnomeExtensions.just-perfection
    gnomeExtensions.sound-output-device-chooser
    gnomeExtensions.vitals
    gnomeExtensions.freon
    lm_sensors  # for freon
    xorg.xdpyinfo
    xorg.xev
    xlsfonts
    xsel
    # zathura       # keyboard-driven PDF viewer
    zoom-us
    pdfpc           # PDF viewer for presentations
    vdpauinfo
    libva-utils     # for the vainfo command

    alsaUtils       # amixer, used in .xmonad.hs
    pavucontrol

    gimp

    breeze-icons
    hicolor-icon-theme
    breeze-gtk
    breeze-qt5
    pinentry-gnome3    # for rbw

    ddccontrol
    powertop

    razergenie
    steam-run
    appimage-run
  ];

  hardware.openrazer.enable = true;
  hardware.openrazer.users = [ "bstaffin" "benley" ];

  environment.sessionVariables = {
    # This is probably a terrible idea but there doesn't seem to be a
    # great alternative if I want xcursor stuff to work in non-gtk
    # emacs, xterm, etc.
    # LD_LIBRARY_PATH = ["${pkgs.xorg.libXcursor}/lib"];
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
  hardware.opengl.driSupport32Bit = true;

  # TODO: what was I doing with uinput?
  hardware.uinput.enable = true;

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

  services.udev.packages = [pkgs.chrysalis];
}
