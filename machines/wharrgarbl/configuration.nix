{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/redshift.nix
    ../imports/virtualbox.nix
    ../imports/wacom.nix
  ];

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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
  };

  i18n.consoleFont = "sun12x22";

  environment.systemPackages = with pkgs; [
    xlibs.xbacklight
  ];

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    socketActivation = false;
  };

  services.xserver = {
    xkbModel = "applealu_ansi";
    xkbOptions = "ctrl:nocaps";
    xkbVariant = "mac";
    vaapiDrivers = [ pkgs.vaapiIntel ];

    displayManager.gdm.enable = true;
    desktopManager.gnome3.enable = true;

    libinput = {
      enable = true;
      naturalScrolling = true;
      tapping = true;
      tappingDragLock = true;
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
