{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/fonts.nix
    ../imports/nix.nix
    ../imports/package-overrides.nix
    ../imports/redshift.nix
    ../imports/users.nix
    ../imports/virtualbox.nix
    ../imports/wacom.nix
  ];

  boot.loader = {
    gummiboot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  boot.supportedFilesystems = [ "zfs" ];

  boot.initrd.luks = {
    mitigateDMAAttacks = true;
    devices = [
      { device = "/dev/sda4"; name = "crypted";    allowDiscards = true; }
      { device = "/dev/sda5"; name = "cryptswap1"; allowDiscards = true; }
    ];
  };

  boot.kernelPackages = pkgs.linuxPackages_4_3;
  boot.kernelParams = [
    # https://help.ubuntu.com/community/AppleKeyboard
    # https://wiki.archlinux.org/index.php/Apple_Keyboard
    "hid_apple.fnmode=1"
    "hid_apple.iso_layout=0"
    "hid_apple.swap_opt_cmd=0"
  ];

  networking = {
    # ZFS requires this I guess?
    hostId = "286d0d3e";
    hostName = "wharrgarbl";
    firewall.enable = true;
    firewall.allowPing = true;
    networkmanager.enable = true;
  };

  i18n = {
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    acpi
    glxinfo
    pciutils
    usbutils
    xlibs.xbacklight
    xlibs.xdpyinfo
    xlsfonts
  ];

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    socketActivation = false;
  };

  # services.openssh.enable = true;
  # services.printing.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    # Gnome is a dick and overrides these, so just set them in gnome:
    #xkbModel = "applealu_ansi";
    #xkbOptions = "ctrl:nocaps";
    xkbVariant = "mac";
    vaapiDrivers = [ pkgs.vaapiIntel ];

    # Populate /etc/X11 since you're going to look there anyway
    exportConfiguration = true;

    displayManager.gdm.enable = true;
    desktopManager.gnome3.enable = true;

    libinput = {
      enable = true;
      naturalScrolling = true;
      tapping = true;
      tappingDragLock = true;
    };
  };

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
