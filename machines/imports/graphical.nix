{ config, pkgs, lib, ... }:

{
  imports = [
    ./fonts.nix
    ./workstation.nix
  ];

  environment.etc = {
    "opt/chrome/policies/managed/test_policy.json" = {
      text = builtins.toJSON {
        AuthServerAllowlist = "*.memcompute.com,*.memsql.com,*.singlestore.com";
        ExtensionInstallBlocklist = [
          "khpfeaanjngmcnplbdlpegiifgpfgdco"  # Smart Card Connector (breaks gpg/ssh agent!)
        ];
      };
    };
  };

  programs.steam.enable = true;
  programs.steam.remotePlay.openFirewall = true;
  programs.steam.dedicatedServer.openFirewall = true;

  programs.chrysalis.enable = true;

  environment.systemPackages = with pkgs; [
    (hunspell.withDicts (d: [d.en-us]))
    bitwarden-desktop
    vesktop
    firefox
    # fritzing
    # glxinfo
    (google-chrome.override {
      commandLineArgs = "--enable-features=TouchpadOverscrollHistoryNavigation --disable-features=ExtensionManifestV2Unsupported,ExtensionManifestV2Disabled";
    })
    graphviz
    pdfarranger
    prismlauncher
    remmina  # RDP/VNC/NX/Spice client
    signal-desktop-bin
    slack
    telegram-desktop
    transmission_4-gtk
    nordic  # GTK theme
    gnome-themes-extra  # I think this fixes some "can't find theme engine adwaita" erorrs
    gnome-tweaks
    dconf-editor
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

    alsa-utils       # amixer, used in .xmonad.hs
    pavucontrol

    gimp

    # pinentry-gnome3    # for rbw

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

    # tell electron apps to use Wayland
    NIXOS_OZONE_WL = "1";
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

  services.printing = {
    enable = true;
    drivers = [ pkgs.hplip ];
    # Stop cups from automatitically adding printers it detects on the network,
    # usually in a broken state
    browsed.enable = false;
  };

  services.desktopManager.gnome.enable = true;
  services.displayManager.gdm.enable = true;

  services.xserver = {
    enable = true;
    updateDbusEnvironment = true;
  };

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  # TODO: what was I doing with uinput?
  hardware.uinput.enable = true;

  hardware.bluetooth.enable = true;

  programs.adb.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableExtraSocket = true;
  };

  programs.gnome-terminal.enable = true;  # omfg how is this not the default with gnome

  services.logind.settings.Login = {
    # Probably don't want this on headless machines, but workstations/laptops
    # sure. This makes sure that things like my user dbus session don't persist
    # across multiple logout/login cycles and mess things up.
    KillUserProcesses = true;
  };

  services.geoclue2.geoProviderUrl = "https://api.beacondb.net/v1/geolocate";

}
