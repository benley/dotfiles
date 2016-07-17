{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix  ];

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

  time.timeZone = "America/Los_Angeles";

  environment.systemPackages = with pkgs; [
    acpi
    dstat
    file
    glxinfo
    htop
    iotop
    pciutils
    sysstat
    tmux
    usbutils
    vimHugeX
    xlibs.xbacklight
    xlibs.xdpyinfo
    xlsfonts
  ];

  fonts = {
    fonts = with pkgs; [
      corefonts
      anonymousPro
      aurulent-sans
      bakoma_ttf
      cantarell_fonts
      crimson
      dejavu_fonts
      dina-font
      dosemu_fonts
      fantasque-sans-mono
      fira
      fira-code
      fira-mono
      freefont_ttf
      hasklig
      inconsolata
      liberation_ttf
      meslo-lg
      powerline-fonts
      proggyfonts
      source-code-pro
      source-sans-pro
      source-serif-pro
      terminus_font
      tewi-font
      ttf_bitstream_vera
      ubuntu_font_family
      unifont
      vistafonts
    ];
  };

  programs.bash.enableCompletion = true;

  nix = {
    nixPath = [
      "/nix/var/nix/profiles/per-user/root/channels/nixos"
      #"nixos-config=/etc/nixos/configuration.nix"
      "nixos-config=/home/benley/p/dotfiles/machines/wharrgarbl/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
    buildCores = 0;
    daemonIONiceLevel = 7;
    daemonNiceLevel = 10;
    useChroot = true;
    extraOptions = ''
      auto-optimise-store = true
    '';
    trustedBinaryCaches = [
      https://cache.nixos.org
      https://hydra.nixos.org
    ];
    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  nixpkgs.config = {
    allowUnfree = true;
    virtualbox.enableExtensionPack = true;
    chromium = {
      gnomeSupport = true;
      enablePepperFlash = true;
      enablePepperPDF = true;
      enableWideVine = true;
    };
  };

  virtualisation.virtualbox.host.enable = true;

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

  services.redshift = {
    enable = true;
    latitude = "37.77493";
    longitude = "-122.41942";
    temperature.day = 6500;
    temperature.night = 3500;
  };

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "vboxusers" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";

}
