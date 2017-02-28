{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/fonts.nix
    ../imports/kde.nix
    ../imports/nix.nix
    ../imports/package-overrides.nix
    ../imports/users.nix
    ../imports/wacom.nix
  ];

  # Samus audio support is supposed to just work in linux >= 4.9
  boot.kernelPackages = pkgs.linuxPackages_4_9;

  # Make the cryptsetup password prompt readable
  boot.earlyVconsoleSetup = true;

  boot.plymouth.enable = true;

  boot.initrd.kernelModules = [
    "intel_agp"
    "i915"
  ];

  # Set builtin audio as the primary sound card, and HDMI audio as secondary.
  boot.extraModprobeConfig = ''
    options snd_soc_sst_bdw_rt5677_mach index=0
    options snd-hda-intel index=1
  '';
  # This probably accomplishes the same thing as the above:
  # options snd slots=snd_soc_sst_bdw_rt5677_mach,snd-hda-intel

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

  i18n = {
    consoleFont = "ter-132b";
    consolePackages = [ pkgs.terminus_font ];
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    dropbox
    google-chrome
    insync
    minecraft
    slack
    steam

    glxinfo
    vdpauinfo
    libva  # Just for the vainfo command
    xorg.xdpyinfo
  ];

  services.openssh.enable = false;

  services.printing.enable = true;

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
    nssmdns = true;
  };

  services.xserver = {
    enable = true;
    useGlamor = true;
    wacom.enable = true; # does this do anything on samus?
    layout = "us";
    xkbModel = "chromebook";
    xkbOptions = "";  # drop the default "terminate:ctrl_alt_bksp"

    # KDE exposes more configuration for synaptics, but libinput has *way*
    # better defaults.
    libinput.enable = true;

    # If you want to use synaptics instead, start here.
    # synaptics = {
    #   enable = true;
    #   buttonsMap = [ 1 3 2 ];
    #   fingersMap = [ 1 3 2 ];
    #   palmDetect = true;
    #   tapButtons = true;
    #   twoFingerScroll = true;
    #   vertEdgeScroll = false;
    #   horizontalScroll = true;
    # };

    # Physical DPI of samus screen is ~239,
    # but telling KDE that makes everything quite huge
    # monitorSection = "DisplaySize 272 181";

    # So instead let's try an even multiple of 96dpi:
    dpi = 192;

    # Believe it or not, "intel" is deprecated nowadays
    videoDrivers = [ "modesetting" ];

    # FDE means I type my password immediately after grub, so enabling sddm
    # autologin is a sane thing to do.
    displayManager.sddm.autoLogin.enable = true;
    displayManager.sddm.autoLogin.user = "benley";
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
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;

  # I'm not sure if these next two are necessary:
  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
