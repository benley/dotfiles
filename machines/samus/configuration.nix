#### SAMUS (Chromebook Pixel 2 LS) ####

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../imports/defaults.nix
    ../imports/graphical.nix
    ../imports/wacom.nix
  ];

  # Make the cryptsetup password prompt readable
  boot.earlyVconsoleSetup = true;

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    zfsSupport = true;
  };

  boot.supportedFilesystems = [ "zfs" ];

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
    devices = [{
      name = "crypted";
      device = "/dev/disk/by-uuid/d2de07e6-a0fe-4cd0-a1e5-9e1921763d00";
      allowDiscards = true;
    }];
  };

  networking.hostId = "8425e439";
  networking.hostName = "samus";
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;

  i18n.consoleFont = "ter-132b";
  i18n.consolePackages = [ pkgs.terminus_font ];

  services.xserver = {
    useGlamor = true;
    wacom.enable = true; # does this do anything on samus?
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
  };

  hardware.opengl = {
    extraPackages = with pkgs;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];
    extraPackages32 = with pkgs.pkgsi686Linux;
        [ vaapiIntel libvdpau libvdpau-va-gl vaapiVdpau ];

    s3tcSupport = true;
  };

  environment.variables = {
    # There is no dedicated i965 vpdau driver, so this tells libvpdau to use
    # libva as its backend.
    VDPAU_DRIVER = "va_gl";
  };

  hardware.bluetooth.enable = true;

  # I'm not sure if these next two are necessary:
  hardware.enableAllFirmware = true;
  hardware.cpu.intel.updateMicrocode = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";

  zramSwap.enable = true;

  # FIXME: does this belong here?
  systemd.mounts = [{
    where = "/var/lib/docker";
    what = "rpool/docker";
    type = "zfs";
    requiredBy = [ "docker.service" ];
    before = ["docker.service"];
    after = ["zfs-import-pool0.service"];
  }];

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "zfs";
}
