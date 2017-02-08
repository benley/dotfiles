{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/nix.nix
  ];

  # Samus audio support is supposed to just work in linux >= 4.9
  boot.kernelPackages = pkgs.linuxPackages_4_9;

  boot.initrd.kernelModules = [
    "intel_agp"
    "i915"
  ];

  boot.extraModprobeConfig = ''
    options snd_soc_sst_bdw_rt5677_mach index=0
    options snd-hda-intel index=1
  '';
  #  options snd slots=snd_soc_sst_bdw_rt5677_mach,snd-hda-intel

  boot.initrd.luks = {
    mitigateDMAAttacks = true;
    devices = [
      { device = "/dev/disk/by-uuid/d2de07e6-a0fe-4cd0-a1e5-9e1921763d00"; name = "crypted"; allowDiscards = true; }
    ];
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    zfsSupport = true;
  };

  boot.supportedFilesystems = [ "zfs" ];

  networking.firewall = {
    enable = true;
    allowPing = true;
  };

  networking.hostName = "samus";
  networking.networkmanager.enable = true;
  networking.hostId = "8425e439";

  nixpkgs.config.allowUnfree = true;

  i18n = {
    consoleFont = "sun12x22";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    google-chrome
    wget
    vimHugeX

    vdpauinfo
    libva
  ];

  programs.bash.enableCompletion = true;

  # services.openssh.enable = true;
  services.printing.enable = true;

  services.xserver = {
    enable = true;
    # useGlamor = true;
    desktopManager.kde5.enable = true;
    displayManager.sddm.enable = true;
    wacom.enable = true;
    # layout = "us";
    # xkbOptions = "eurosign:e";
    synaptics = {
      enable = true;
      buttonsMap = [ 1 3 2 ];
      fingersMap = [ 1 3 2 ];
      palmDetect = true;
      tapButtons = true;
      twoFingerScroll = true;
      vertEdgeScroll = false;
      horizontalScroll = true;
    };

    # Physical DPI of samus screen is ~239,
    # but telling KDE that makes everything quite huge
    # monitorSection = ''
    #   DisplaySize 272 181
    # '';

    # So instead let's try an even multiple of 96dpi:
    dpi = 192;

    # Believe it or not, "intel" is deprecated nowadays
    videoDrivers = [ "modesetting" ];
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];

    enable = true;
    driSupport = true;
    driSupport32Bit = true;
    s3tcSupport = true;
  };

  environment.variables = {
    VDPAU_DRIVER = "va_gl";
  };

  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;
  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  users.extraUsers.benley = {
    isNormalUser = true;
    uid = 1000;
    description = "Benjamin Staffin";
    extraGroups = [ "wheel" "vboxusers" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
